module Language.HeMan.Backend
  (Stmt(..), Tail(..), Block, Thread,
   backend)
  where

import qualified Language.HeMan.Syntax as Front
  (Stmt(..), DExpr(..), VDecl, Prim(..),
   ArithOp(..), ArithUnop(..), RelnOp(..), Block, Var)
import Control.Monad.RWS
import Control.Monad.Writer
import Control.Monad.State
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe
import Data.Bits

type Prgm = [Block]

type Label = Int
type ThreadName = Label
type Block = (Label, ThreadName, [Stmt], Tail)
type Thread = (ThreadName, [Front.VDecl])

data Stmt = Decl Front.VDecl Front.DExpr
          | Assign Front.DExpr Front.DExpr
          | Exp Front.DExpr
          | Spawn Label [(Front.VDecl, Front.DExpr)]
          deriving (Eq, Ord, Show)

data Tail = If Front.DExpr Tail Tail
          | Goto Label
          | GotoWait Label
          | Exit
          deriving (Eq, Ord, Show)

backend :: Front.Block -> ([Block],[Thread])
backend = collectFrees . (mapFst (optimize . simplifyJumps)) . flattenPrgm
--backend = collectFrees . flattenPrgm

mapFst f (x,y) = (f x,y)

constantExpr expr = case expr of
  Front.NumLit n -> True
  Front.StringLit s -> True
  _ -> False

--{{{ flattenPrgm

type Flattener = RWS ThreadName ([Block],[Thread]) Label

flattenPrgm stmts = ((0,0,s,t):blocks,(0,[]):thds) where
  ((s,t),_,(blocks,thds)) = runRWS (flattenStmts stmts [] Exit) 0 1
  
registerEvent event = [Front.Exp $
                       Front.Call (Front.CFn "register_event")
                       [Front.CurThread, event]]

fresh = do
  x <- get
  modify (+1)
  return x
fresh2 = do
  x <- fresh
  y <- fresh
  return (x,y)

newBlock blockL stmts tail = do
  threadL <- ask
  tell ([(blockL,threadL,stmts,tail)],mempty)

newThread threadL vdecls = tell (mempty,[(threadL,vdecls)])

inThread threadL = local (\_ -> threadL)

{- flattenStmts takes a list of Stmts, a list of succeeding Stmts, and the Tail
for that block, and returns a new list of Stmts and a new Tail. -}

flattenStmts :: [Front.Stmt] -> [Stmt] -> Tail -> Flattener ([Stmt],Tail)
flattenStmts [] aStmts tail = return (aStmts,tail)
flattenStmts stmts aStmts tail =
  flattenStmt (last stmts) (init stmts) aStmts tail

{- flattenStmt takes a Stmt, a list of preceding Stmts, a list of succeeding
Stmts, and a Tail, and returns a new list of Stmts and a new Tail.

Variable suffix conventions:
T = tail
L = label
-}

flattenStmt :: Front.Stmt -> [Front.Stmt] -> [Stmt] -> Tail ->
               Flattener ([Stmt],Tail)
flattenStmt stmt bStmts aStmts tail =
  case stmt of
    Front.Decl vdecl expr -> continue $ Decl vdecl expr
    Front.Exp expr -> continue $ Exp expr
    Front.Assign expr expr' -> continue $ Assign expr expr'
    Front.If expr cs as -> 
      do seqL <- fresh
         newBlock seqL aStmts tail
         (conL,altL) <- fresh2
         (bs,tail') <- flattenStmts bStmts [] (If expr (Goto conL) (Goto altL))
         (cs',conT) <- flattenStmts cs [] (Goto seqL)
         (as',altT) <- flattenStmts as [] (Goto seqL)
         newBlock conL cs' conT
         newBlock altL as' altT
         return (bs,tail')
    Front.While expr ss ->
      do (seqL,whileL) <- fresh2
         newBlock seqL aStmts tail
         let whileT = If expr (Goto whileL) (Goto seqL)
         (bs,tail') <- flattenStmts bStmts [] whileT
         (ws,whileT') <- flattenStmts ss [] whileT
         newBlock whileL ws whileT'
         return (bs,tail')
    Front.Exit ->
      do (bs, tail') <- flattenStmts bStmts [] Exit
         return (bs, tail')
    Front.Wait expr ->
      do seqL <- fresh
         newBlock seqL aStmts tail
         let waitT = GotoWait seqL
         (bs,tail') <- flattenStmts (bStmts ++ (registerEvent expr)) [] waitT
         return (bs,tail')
    Front.Spawn (vs,ss) args ->
      do threadL <- fresh
         let spawn = Spawn threadL (zip vs args)
         (bs,tail') <- flattenStmts bStmts (spawn:aStmts) tail
         (ts,threadT) <- inThread threadL $ flattenStmts ss [] Exit
         inThread threadL $ newBlock threadL ts threadT
         newThread threadL vs
         return (bs,tail')
  where continue s = flattenStmts bStmts (s:aStmts) tail

--}}}
--{{{ simplifyJumps

{- simplifyJumps removes unnecessary Gotos from the output of flattenPrgm. -}

simplifyJumps bs = execWriter $ mapM_ (rewriteTail rs) bs'
  where (_,rs,bs') = runRWS (mapM_ simplify bs) () Map.empty

rewriteTail rs b = case b of
  -- Recover empty GotoWait targets.
  (l,t,ss,GotoWait g) | walk (Goto g) rs /= (Goto g) ->
    tell [b,(g,t,[],walk (Goto g) rs)]
  -- Redirect tails.
  (l,t,ss,tail) -> tell [(l,t,ss,walk tail rs)]

type Simplifier = RWS () [Block] (Map.Map Tail Tail)

addRedirect l l' = do
  redirects <- get 
  modify $ Map.insert l (walk l' redirects)

simplify :: Block -> Simplifier ()
simplify b = case b of
  -- Don't remove the empty head block of a thread.
  (l,t,[],_) | l == t -> tell [b]
  -- Remove and redirect empty blocks.
  (l,t,[],Goto g) -> do
    redirects <- get
    let isLoop = (walk (Goto g) redirects == Goto l)
    if isLoop then tell [b] else addRedirect (Goto l) (Goto g)
  (l,t,[],If e c a) -> addRedirect (Goto l) (If e c a)
  (l,t,[],Exit) -> addRedirect (Goto l) Exit
  -- No optimizations to perform.
  _ -> tell [b]

walk x xs = case (x,Map.lookup x xs) of
  (_,Just (If e t t')) -> walk (If e (walk t xs) (walk t' xs)) xs
  (_,Just x') -> walk x' xs
  (If e t t',Nothing) -> If e (walk t xs) (walk t' xs)
  (_,Nothing) -> x

--}}}
--{{{ optimize

{- optimize performs block-local constant propagation and constant folding, and
block folding, in order to further reduce code size. -}

optimize bs = map (\b -> evalState (optimizeBlock b) []) bs

type Optimizer = State [(Front.Var,Front.DExpr)]

optimizeBlock (l,t,ss,tail) = do
  ss' <- mapM optimizeStmt ss
  tail' <- optimizeTail [] tail
  return (l,t,ss',tail')
  
optimizeExpr expr = 
  case expr of
    Front.Var var -> do
      env <- get
      return $ fromMaybe expr (lookup expr env)
    Front.Call prim exprs -> do
      exprs' <- mapM optimizeExpr exprs
      return $ Front.Call prim exprs'
    Front.Arith op e1 e2 -> do
      e1' <- optimizeExpr e1
      e2' <- optimizeExpr e2
      return . constFold $ Front.Arith op e1' e2'
    Front.ArithUnop op e -> do
      e' <- optimizeExpr e
      return . constFold $ Front.ArithUnop op e'
    Front.RelnOp op e1 e2 -> do
      e1' <- optimizeExpr e1
      e2' <- optimizeExpr e2
      return . constFold $ Front.RelnOp op e1' e2'
    _ -> return expr

optimizeStmt stmt = 
  case stmt of
    Decl (var,typ) expr -> do
      expr' <- optimizeExpr expr
      insertVar (Front.Var var) expr'
      return $ Decl (var,typ) expr'
    Assign v@(Front.Var var) expr -> do
      expr' <- optimizeExpr expr
      insertVar v expr'
      return $ Assign v expr'
    Exp expr -> do
      expr' <- optimizeExpr expr
      return $ Exp expr'
    Spawn l args -> do
      exprs' <- mapM optimizeExpr (map snd args)
      return $ Spawn l (zip (map fst args) exprs')

optimizeTail env (If e c a) = do
  e' <- optimizeExpr e
  -- Constant true/false: replace with branch.
  if e' == Front.NumLit 0 then optimizeTail env a else
    if constantExpr e' then optimizeTail env c else case e' of
      -- Redundantly true.
      Front.RelnOp op e1 e2 | impliedR env (op,e1,e2) -> optimizeTail env c
      -- Otherwise, recur on branches.
      _ -> do
        c' <- optimizeTail (assumptions e' ++ env) c
        a' <- optimizeTail env a
        return (If e' c' a')
  where impliedR env e = normalizeR e `elem` env
optimizeTail _ t = return t

{- Helpers for propagation. -}

insertVar v@(Front.Var var) expr = case expr of
  -- Don't propagate potentially effectful calls.
  Front.Call _ _ -> return ()
  -- Insert, and invalidate expressions with var on RHS, including expr.
  _ -> do
    modify (\xs -> (v,expr):xs)
    modify $ filter (\(_,e) -> not (exprContains var e))
    return ()

exprContains var expr = case expr of
  Front.Call prim exprs -> any (exprContains var) exprs
  Front.Arith op e e' -> any (exprContains var) [e,e']
  Front.ArithUnop op e -> exprContains var e
  Front.RelnOp op e e' -> any (exprContains var) [e,e']
  Front.Var v -> v == var
  _ -> False

{- Helpers for If cleanup. -}

assumptions expr = case expr of
  Front.Arith Front.And e e' -> concatMap assumptions [e,e']
  Front.RelnOp op e e' -> [normalizeR (op,e,e')]
  _ -> []

-- Translate RelnOps to a normal form, increasing likelihood of a simplification.
normalizeR e = case e of
  (Front.Gt,a,b) -> (Front.Lt,b,a)
  (Front.Geq,a,b) -> (Front.Leq,b,a)
  (Front.Eq,a,b) | b > a -> (Front.Eq,b,a)
  (Front.Neq,a,b) | b > a -> (Front.Neq,b,a)
  _ -> e

{- Perform constant folding. Effectful computations don't occur in the RHS. -}

fromBool True = Front.NumLit 1
fromBool False = Front.NumLit 0

constFold (Front.RelnOp op e e') = case (op,e,e') of
  (Front.Eq,x,y) | x == y -> Front.NumLit 1
  (Front.Eq,Front.NumLit x,Front.NumLit y) | x /= y -> Front.NumLit 0
  (Front.Lt,x,y) | x == y -> Front.NumLit 0
  (Front.Lt,Front.NumLit x,Front.NumLit y) -> fromBool (x < y)
  (Front.Gt,x,y) | x == y -> Front.NumLit 0
  (Front.Gt,Front.NumLit x,Front.NumLit y) -> fromBool (x > y)
  (Front.Leq,x,y) | x == y -> Front.NumLit 1
  (Front.Leq,Front.NumLit x,Front.NumLit y) -> fromBool (x <= y)
  (Front.Geq,x,y) | x == y -> Front.NumLit 1
  (Front.Geq,Front.NumLit x,Front.NumLit y) -> fromBool (x >= y)
  (Front.Neq,x,y) | x == y -> Front.NumLit 0
  (Front.Neq,Front.NumLit x,Front.NumLit y) | x /= y -> Front.NumLit 1
  _ -> Front.RelnOp op e e'

constFold (Front.Arith op e e') = case (op,e,e') of
  (Front.And,Front.NumLit x,y) -> if x == 0 then Front.NumLit 0 else y
  (Front.And,x,Front.NumLit y) -> if y == 0 then Front.NumLit 0 else x
  (Front.Or,Front.NumLit x,y) -> fromBool (x /= 0)
  (Front.Or,x,Front.NumLit y) -> fromBool (y /= 0)
  (Front.Xor,x,y) | x == y -> Front.NumLit 0
  (Front.Xor,Front.NumLit x,Front.NumLit y) -> Front.NumLit (x `xor` y)
  (Front.Plus,Front.NumLit x,Front.NumLit y) -> Front.NumLit (x + y)
  (Front.Times,Front.NumLit 0,_) -> Front.NumLit 0
  (Front.Times,_,Front.NumLit 0) -> Front.NumLit 0
  (Front.Times,Front.NumLit x,Front.NumLit y) -> Front.NumLit (x * y)
  (Front.Minus,x,y) | x == y -> Front.NumLit 0
  (Front.Minus,Front.NumLit x,Front.NumLit y) -> Front.NumLit (x - y)
  (Front.Div,x,Front.NumLit 1) -> x
  (Front.Div,Front.NumLit x,Front.NumLit y) -> Front.NumLit (x `div` y)
  (Front.Mod,x,Front.NumLit 1) -> Front.NumLit 0
  (Front.Mod,Front.NumLit x,Front.NumLit y) -> Front.NumLit (x `mod` y)
  (Front.Rsh,Front.NumLit x,Front.NumLit y) ->
    Front.NumLit (shiftR x (fromInteger y))
  (Front.Rsh,x,Front.NumLit 0) -> x
  (Front.Rsh,Front.NumLit 0,_) -> Front.NumLit 0
  (Front.Lsh,Front.NumLit x,Front.NumLit y) ->
    Front.NumLit (shiftL x (fromInteger y))
  (Front.Lsh,x,Front.NumLit 0) -> x
  (Front.Lsh,Front.NumLit 0,_) -> Front.NumLit 0
  _ -> Front.Arith op e e'

constFold (Front.ArithUnop op e) = case (op,e) of
  (Front.Negate,Front.NumLit x) -> Front.NumLit (-x)
  (Front.Not,Front.NumLit 0) -> Front.NumLit 1
  (Front.Not,Front.NumLit _) -> Front.NumLit 0
  _ -> Front.ArithUnop op e

--}}}
--{{{ collectFrees

{- collectFrees determines, for each thread, the variables which must be
retained across multiple blocks, and adds those to the list of thread-local
variables. 

*** This pass assumes all declared variables have a single type. ***
We enforce this constraint in the front-end. -}

collectFrees (blocks,threads) = (blocks,map (collectThread blocks) threads)

collectThread :: [Block] -> Thread -> Thread
collectThread blocks (thread,vdecls) = (thread,vdecls')
  where threadBlocks = filter (\(_,t,_,_) -> t == thread) blocks
        (decls,frees) = mconcat $ map collectBlock threadBlocks
        frees' = frees Set.\\ Set.fromList (map fst vdecls)
        getDecl free = (free,fromJust $ lookup free (Set.toList decls))
        -- fromJust exception <=> encountered a truly free variable
        vdecls' = vdecls ++ map getDecl (Set.toList frees')

type Collector = Writer (Set.Set Front.VDecl,Set.Set Front.Var)

collectBlock (label,thread,stmts,tail) = (decls,frees')
  where collect = do mapM_ collectStmt stmts
                     collectTail tail
        ((),(decls,frees)) = runWriter collect
        frees' = frees Set.\\ Set.map fst decls

collectStmt s = case s of
  Decl vdecl expr ->
    do tell (Set.singleton vdecl,mempty)
       collectExpr expr
  Assign expr expr' ->
    do collectExpr expr
       collectExpr expr'
  Spawn _ exprs -> mapM_ (collectExpr . snd) exprs
  Exp expr -> collectExpr expr

collectExpr e = case e of
  Front.Call _ exprs -> mapM_ collectExpr exprs
  Front.Arith _ expr expr' ->
    do collectExpr expr
       collectExpr expr'
  Front.ArithUnop _ expr -> collectExpr expr
  Front.RelnOp _ expr expr' ->
    do collectExpr expr
       collectExpr expr'
  Front.Var var -> tell (mempty,Set.singleton var)
  Front.Constant _ -> return ()
  Front.NumLit _ -> return ()
  Front.StringLit _ -> return ()
  Front.CurThread -> return ()

collectTail t = case t of
  If expr tail tail' ->
    do collectExpr expr
       collectTail tail
       collectTail tail'
  Goto _ -> return ()
  GotoWait _ -> return ()
  Exit -> return ()

--}}}
--{{{ Tests
{-

testFlat = backend [Front.Decl ("x",Front.Int) (Front.Var "y")]

testIf = backend [Front.Exp (Front.NumLit 5),
                  Front.If (Front.NumLit 6)
                          [Front.Exp $ Front.Call (Front.CFn "cfun") []]
                          [Front.Exp $ Front.Call (Front.CFn "afun") []],
                  Front.Exp (Front.NumLit 10)]

testWhile = backend [Front.Exp (Front.NumLit 5),
                     Front.While (Front.NumLit 6)
                                [Front.Exp $ Front.Call (Front.CFn "rep") [],
                                 Front.Exp $ Front.Call (Front.CFn "rep2") []],
                     Front.Exp (Front.NumLit 10)]

testExit = backend [Front.Exp (Front.NumLit 5),
                    Front.Exit,
                    Front.Exp (Front.NumLit 6)]

testIfExit = backend [Front.Exp (Front.NumLit 5),
                      Front.If (Front.NumLit 6)
                              [Front.Exit]
                              [Front.Exp $ Front.Call (Front.CFn "afun") []],
                      Front.Exp (Front.NumLit 10)]

testWait = backend [Front.Exp (Front.NumLit 10),
                    Front.Wait (Front.Call (Front.CFn "wfun") []),
                    Front.Exp (Front.NumLit 11)]

testSpawn = backend [Front.Exp (Front.NumLit 5),
                     Front.Spawn ([("x",Front.Int),("y",Front.Bool)],
                       [Front.Exp (Front.NumLit 10),
                        Front.Exp (Front.NumLit 15)])
                       [Front.Var "arg1",Front.Var "arg2"],
                     Front.Exp (Front.NumLit 20)]

testSpawnIf = backend [Front.Exp (Front.NumLit 5),
                       Front.Spawn ([("x",Front.Int),("y",Front.Bool)],
                         [Front.Exp (Front.NumLit 0),
                          Front.If (Front.NumLit 6)
                                  [Front.Exit]
                                  [Front.Exp $ Front.NumLit 100],
                          Front.Exp (Front.NumLit 1)])
                         [Front.Var "arg1",Front.Var "arg2"],
                       Front.If (Front.NumLit 7)
                               [Front.Exp $ Front.Call (Front.CFn "cfun") []]
                               [Front.Exp $ Front.Call (Front.CFn "afun") []],
                       Front.Exp (Front.NumLit 8)]

testOptimize = backend [Front.If (Front.NumLit 6)
                                [Front.Exit]
                                [Front.If (Front.NumLit 7)
                                         [Front.Exp (Front.NumLit 8)]
                                         [Front.Exp (Front.NumLit 9)]],
                        Front.Exp (Front.NumLit 10)]

testCollect = backend [Front.Decl ("y",Front.Int) (Front.NumLit 2),
                       Front.Decl ("x",Front.Int) (Front.NumLit 2),
                       Front.Exp $
                         Front.Arith Front.Plus (Front.Var "y") (Front.Var "x"),
                       Front.If (Front.Var "y")
                               [Front.Exit]
                               [Front.Exp $ Front.Var "y"]]
-}

--}}}
