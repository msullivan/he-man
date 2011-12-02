module Back where

import qualified Lang
import Control.Monad.RWS
import Control.Monad.Writer
import qualified Data.Set as Set
import Data.Maybe

type Prgm = [Block]

type Label = Int
type ThreadName = Label
type Block = (Label, ThreadName, [Stmt], Tail)
type Thread = (ThreadName, [Lang.VDecl])

data Stmt = Decl Lang.VDecl Lang.Expr
          | Assign Lang.Expr Lang.Expr
          | Exp Lang.Expr
          | Spawn Label [(Lang.VDecl, Lang.Expr)]
          deriving (Eq, Ord, Show)

data Tail = If Lang.Expr Tail Tail
          | Goto Label
          | GotoWait Label
          | Exit
          deriving (Eq, Ord, Show)

--{{{ flattenPrgm

type Flattener = RWS ThreadName ([Block],[Thread]) Label

flattenPrgm stmts = ((0,0,s,t):blocks,(0,[]):thds) where
  ((s,t),_,(blocks,thds)) = runRWS (flattenStmts stmts [] Exit) 0 1
  
registerEvent event = [Lang.Exp $
                       Lang.Call (Lang.CFn "register_event")
                       [Lang.CurThread, event]]

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

flattenStmts :: [Lang.Stmt] -> [Stmt] -> Tail -> Flattener ([Stmt],Tail)
flattenStmts [] aStmts tail = return (aStmts,tail)
flattenStmts stmts aStmts tail =
  flattenStmt (last stmts) (init stmts) aStmts tail

{- flattenStmt takes a Stmt, a list of preceding Stmts, a list of succeeding
Stmts, and a Tail, and returns a new list of Stmts and a new Tail.

Variable suffix conventions:
T = tail
L = label
-}

flattenStmt :: Lang.Stmt -> [Lang.Stmt] -> [Stmt] -> Tail ->
               Flattener ([Stmt],Tail)
flattenStmt stmt bStmts aStmts tail =
  case stmt of
    Lang.Decl vdecl expr -> continue $ Decl vdecl expr
    Lang.Exp expr -> continue $ Exp expr
    Lang.Assign expr expr' -> continue $ Assign expr expr'
    Lang.If expr cs as -> 
      do seqL <- fresh
         newBlock seqL aStmts tail
         (bs,_) <- flattenStmts bStmts [] tail
         (cs',conT) <- flattenStmts cs [] (Goto seqL)
         (as',altT) <- flattenStmts as [] (Goto seqL)
         (conL,altL) <- fresh2
         newBlock conL cs' conT
         newBlock altL as' altT
         return (bs,If expr (Goto conL) (Goto altL))
    Lang.While expr ss ->
      do (seqL,whileL) <- fresh2
         newBlock seqL aStmts tail
         let whileT = If expr (Goto whileL) (Goto seqL)
         (bs,_) <- flattenStmts bStmts [] whileT
         (ws,whileT') <- flattenStmts ss [] whileT
         newBlock whileL ws whileT'
         return (bs,whileT)
    Lang.Exit ->
      do (bs, tail) <- flattenStmts bStmts [] Exit
         return (bs, tail)
    Lang.Wait expr ->
      do seqL <- fresh
         newBlock seqL aStmts tail
         (bs,_) <- flattenStmts (bStmts ++ (registerEvent expr)) [] tail
         return (bs,GotoWait seqL)
    Lang.Spawn (vs,ss) args ->
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

-- TODO fuse small non-empty blocks
-- TODO eliminate blocks never jumped to (optimized If tail)

optimizeJumps bs = map (redirect ls) bs'
  where (bs',ls) = optimize bs []

optimize [] xs = ([],xs)
optimize (b:bs) ls =
  let targets = map snd ls in
  case b of
  (label,vs,[],_) | Goto label `elem` targets ->
    let (bs',ls') = optimize bs ls in (b:bs',ls')
  (label,vs,[],Goto g) ->
    let (bs',ls') = optimize bs ((Goto label,walk (Goto g) ls):ls) in (bs',ls')
  (label,vs,[],Exit) ->
    optimize bs ((Goto label,Exit):ls)
  (label,vs,stmts,If (Lang.NumLit 0) c a) ->
    optimize ((label,vs,stmts,a):bs) ls
  (label,vs,stmts,If e c a) | knownConstantExpr e ->
    optimize ((label,vs,stmts,c):bs) ls
  (label,vs,[],If e (Goto g) (Goto g')) ->
    let tail = walk (Goto g) ls
        tail' = walk (Goto g') ls
        (bs',ls') = optimize bs ((Goto label,If e tail tail'):ls) in
    ((label,vs,[],If e tail tail'):bs',ls')
  _ -> let (bs',ls') = optimize bs ls in (b:bs',ls')

walk x xs = case lookup x xs of
  Just x' -> walk x' xs
  Nothing -> x

redirect ls (x,vs,ss,tail) = (x,vs,ss,help tail) where
  help tail = case tail of
    Goto x -> walk (Goto x) ls
    GotoWait x -> GotoWait x
    If e t t' -> If e (help t) (help t')
    Exit -> Exit

knownConstantExpr expr =
  case expr of
    Lang.NumLit n -> True
    Lang.StringLit s -> True
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

type Collector = Writer (Set.Set Lang.VDecl,Set.Set Lang.Var)

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
  Lang.Call _ exprs -> mapM_ collectExpr exprs
  Lang.Arith _ expr expr' ->
    do collectExpr expr
       collectExpr expr'
  Lang.ArithUnop _ expr -> collectExpr expr
  Lang.RelnOp _ expr expr' ->
    do collectExpr expr
       collectExpr expr'
  Lang.Var var -> tell (mempty,Set.singleton var)
  Lang.Constant _ -> return ()
  Lang.NumLit _ -> return ()
  Lang.StringLit _ -> return ()

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

mapFst f (x,y) = (f x,y)
runPasses = collectFrees . (mapFst optimizeJumps) . flattenPrgm

testFlat = runPasses [Lang.Decl ("x",Lang.Int) (Lang.Var "y")]

testIf = runPasses [Lang.Exp (Lang.NumLit 5),
                    Lang.If (Lang.NumLit 6)
                            [Lang.Exp $ Lang.Call (Lang.CFn "cfun") []]
                            [Lang.Exp $ Lang.Call (Lang.CFn "afun") []],
                    Lang.Exp (Lang.NumLit 10)]

testWhile = runPasses [Lang.Exp (Lang.NumLit 5),
                       Lang.While (Lang.NumLit 6)
                                  [Lang.Exp $ Lang.Call (Lang.CFn "rep") [],
                                   Lang.Exp $ Lang.Call (Lang.CFn "rep2") []],
                       Lang.Exp (Lang.NumLit 10)]

testExit = runPasses [Lang.Exp (Lang.NumLit 5),
                      Lang.Exit,
                      Lang.Exp (Lang.NumLit 6)]

testIfExit = runPasses [Lang.Exp (Lang.NumLit 5),
                        Lang.If (Lang.NumLit 6)
                                [Lang.Exit]
                                [Lang.Exp $ Lang.Call (Lang.CFn "afun") []],
                        Lang.Exp (Lang.NumLit 10)]

testWait = runPasses [Lang.Exp (Lang.NumLit 10),
                      Lang.Wait (Lang.Call (Lang.CFn "wfun") []),
                      Lang.Exp (Lang.NumLit 11)]

testSpawn = runPasses [Lang.Exp (Lang.NumLit 5),
                       Lang.Spawn ([("x",Lang.Int),("y",Lang.Bool)],
                         [Lang.Exp (Lang.NumLit 10),
                          Lang.Exp (Lang.NumLit 15)])
                         [Lang.Var "arg1",Lang.Var "arg2"],
                       Lang.Exp (Lang.NumLit 20)]

testSpawnIf = runPasses [Lang.Exp (Lang.NumLit 5),
                         Lang.Spawn ([("x",Lang.Int),("y",Lang.Bool)],
                           [Lang.Exp (Lang.NumLit 0),
                            Lang.If (Lang.NumLit 6)
                                    [Lang.Exit]
                                    [Lang.Exp $ Lang.NumLit 100],
                            Lang.Exp (Lang.NumLit 1)])
                           [Lang.Var "arg1",Lang.Var "arg2"],
                         Lang.If (Lang.NumLit 7)
                                 [Lang.Exp $ Lang.Call (Lang.CFn "cfun") []]
                                 [Lang.Exp $ Lang.Call (Lang.CFn "afun") []],
                         Lang.Exp (Lang.NumLit 8)]

testOptimize = runPasses [Lang.If (Lang.NumLit 6)
                                  [Lang.Exit]
                                  [Lang.If (Lang.NumLit 7)
                                           [Lang.Exp (Lang.NumLit 8)]
                                           [Lang.Exp (Lang.NumLit 9)]],
                          Lang.Exp (Lang.NumLit 10)]

testCollect = runPasses [Lang.Decl ("y",Lang.Int) (Lang.NumLit 2),
                         Lang.Decl ("x",Lang.Int) (Lang.NumLit 2),
                         Lang.Exp $
                           Lang.Arith Lang.Plus (Lang.Var "y") (Lang.Var "x"),
                         Lang.If (Lang.Var "y")
                                 [Lang.Exit]
                                 [Lang.Exp $ Lang.Var "y"]]

--}}}
