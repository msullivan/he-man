module Language.Foo.Backend
  (Stmt(..), Tail(..), Block, Thread,
   backend)
  where

import qualified Language.Foo.Syntax as Front
  (Stmt(..), Expr(..), VDecl, Prim(..), Block, Var)
import Control.Monad.RWS
import Control.Monad.Writer
import qualified Data.Set as Set
import Data.Maybe

type Prgm = [Block]

type Label = Int
type ThreadName = Label
type Block = (Label, ThreadName, [Stmt], Tail)
type Thread = (ThreadName, [Front.VDecl])

data Stmt = Decl Front.VDecl Front.Expr
          | Assign Front.Expr Front.Expr
          | Exp Front.Expr
          | Spawn Label [(Front.VDecl, Front.Expr)]
          deriving (Eq, Ord, Show)

data Tail = If Front.Expr Tail Tail
          | Goto Label
          | GotoWait Label
          | Exit
          deriving (Eq, Ord, Show)

backend :: Front.Block -> ([Block],[Thread])
backend = collectFrees . (mapFst optimizeJumps) . flattenPrgm

mapFst f (x,y) = (f x,y)

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
--{{{ optimizeJumps

{- optimizeJumps removes unnecessary Gotos from the output of flattenPrgm. -}

optimizeJumps bs = map (redirect ls) bs'
  where (bs',ls) = optimize bs []
        redirect ls (x,vs,ss,tail) = (x,vs,ss,walk tail ls)

optimize [] xs = ([],xs)
optimize (b:bs) ls =
  let targets = map snd ls in
  case b of
  (label,vs,[],_) | Goto label `elem` targets ->
    let (bs',ls') = optimize bs ls in (b:bs',ls')
  (label,vs,stmts,If (Front.NumLit 0) c a) ->
    optimize ((label,vs,stmts,a):bs) ls
  (label,vs,stmts,If e c a) | knownConstantExpr e ->
    optimize ((label,vs,stmts,c):bs) ls
  (0,_,_,_) -> let (bs',ls') = optimize bs ls in (b:bs',ls')
  (label,vs,[],Goto g) ->
    optimize bs ((Goto label,walk (Goto g) ls):ls)
  (label,vs,[],Exit) ->
    optimize bs ((Goto label,Exit):ls)
  (label,vs,[],If e t t') ->
    optimize bs ((Goto label,walk (If e t t') ls):ls)
  _ -> let (bs',ls') = optimize bs ls in (b:bs',ls')

walk x xs = case (x,lookup x xs) of
  (_,Just (If e t t')) -> walk (If e (walk t xs) (walk t' xs)) xs
  (_,Just x') -> walk x' xs
  (If e t t',Nothing) -> If e (walk t xs) (walk t' xs)
  (_,Nothing) -> x

knownConstantExpr expr =
  case expr of
    Front.NumLit n -> True
    Front.StringLit s -> True
    _ -> False

--}}}
--{{{ collectFrees

{- collectFrees determines, for each thread, the variables which must be
retained across multiple blocks, and adds those to the list of thread-local
variables. 

*** This pass assumes all declared variables are unique! ***
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
