module Back where

import qualified Lang
import Control.Monad.Writer
import Control.Monad.State
import Debug.Trace
import Data.Maybe

type Flattener = State Label

type Prgm = [Block]

type Label = Int
type Block = (Label, [Lang.VDecl], [Stmt], Tail)

data Stmt = Decl Lang.VDecl Lang.Expr
          | Assign Lang.Expr Lang.Expr
          | Spawn Label [Lang.Expr]
          | Exp Lang.Expr
          deriving (Eq, Ord, Show)

data Tail = If Lang.Expr Tail Tail
          | Goto Label
          | GotoWait Label
          | Exit
          deriving (Eq, Ord, Show)

--{{{ flattenPrgm

flattenPrgm stmts = [makeBlock 0 [] s t] ++ bbs where
  ((s,t,bbs),_) = runState (flattenStmts stmts [] Exit) (mainB + 1)
  mainB = 0
  
makeBlock name decls stmts tail = (name,decls,stmts,tail)

-- TODO
spawnThread name args = [Exp $ Lang.Call (Lang.CFn "spawn") args']
  where args' = (Lang.NumLit $ fromIntegral name):args

-- TODO
registerEvent event = [Lang.Exp $ Lang.Call (Lang.CFn "register") [event]]

fresh = do
  x <- get
  modify (1+)
  return x

fresh2 = do
  x <- fresh
  y <- fresh
  return (x,y)

{- flattenStmts takes a list of Stmts, a list of succeeding Stmts, and the Tail
for that block, and returns a new list of Stmts, a new Tail, and a list of new
blocks. -}

flattenStmts :: [Lang.Stmt] -> [Stmt] -> Tail ->
                Flattener ([Stmt],Tail,[Block])
flattenStmts [] aStmts tail = return (aStmts,tail,[])
flattenStmts stmts aStmts tail =
  flattenStmt (last stmts) (init stmts) aStmts tail

{- flattenStmt takes a Stmt, a list of preceding Stmt, a list of succeeding
Stmt, and a Tail, and returns a new list of Stmt, a new Tail, and a list of new
blocks. 

Variable suffix conventions:
T = tail
B = basic block
L = label
-}

flattenStmt :: Lang.Stmt -> [Lang.Stmt] -> [Stmt] -> Tail ->
               Flattener ([Stmt],Tail,[Block])
flattenStmt stmt bStmts aStmts tail =
  case stmt of
    Lang.Decl vdecl expr -> continue $ Decl vdecl expr
    Lang.Exp expr -> continue $ Exp expr
    Lang.Assign expr expr' -> continue $ Assign expr expr'
    Lang.If expr cs as -> 
      do seqL <- fresh
         let seqB = makeBlock seqL [] aStmts tail
         (bs,_,bBs) <- flattenStmts bStmts [] tail
         (cs',conT,conBs) <- flattenStmts cs [] (Goto seqL)
         (as',altT,altBs) <- flattenStmts as [] (Goto seqL)
         (conL,altL) <- fresh2
         let conB = makeBlock conL [] cs' conT
             altB = makeBlock altL [] as' altT
             ifT = If expr (Goto conL) (Goto altL)
         return (bs,ifT,[seqB,conB,altB] ++ bBs ++ conBs ++ altBs)
    Lang.While expr ss ->
      do (seqL,whileL) <- fresh2
         let seqB = makeBlock seqL [] aStmts tail
             whileT = If expr (Goto whileL) (Goto seqL)
         (bs,_,bBs) <- flattenStmts bStmts [] whileT
         (ws,whileT',whileBs) <- flattenStmts ss [] whileT
         let whileB = makeBlock whileL [] ws whileT'
         return (bs,whileT,[seqB,whileB] ++ bBs ++ whileBs)
    Lang.Exit ->
      do (bs,_,bBs) <- flattenStmts bStmts [] Exit
         return (bs,Exit,bBs)
    Lang.Wait expr ->
      do seqL <- fresh
         let seqB = makeBlock seqL [] aStmts tail
             waitT = GotoWait seqL 
         (bs,_,bBs) <- flattenStmts (bStmts ++ (registerEvent expr)) [] waitT
         return (bs,waitT,seqB:bBs)
    Lang.Spawn (vs,ss) args ->
      do threadL <- fresh
         let spawn = spawnThread threadL args
         (bs,_,bBs) <- flattenStmts bStmts (spawn ++ aStmts) tail
         (ts,tbt,threadBs) <- flattenStmts ss [] Exit
         let threadB = makeBlock threadL vs ts Exit
         return (bs,tail,[threadB] ++ threadBs ++ bBs)
  where continue s = flattenStmts bStmts (s:aStmts) tail

--}}}
--{{{ optimizeJumps

{- optimizeJumps removes unnecessary Gotos from the output of flattenPrgm. -}

-- TODO fuse small non-empty blocks
-- TODO eliminate blocks never jumped to (optimized If tail)

optimizeJumps bs = map (fMapTail (flip walk xs)) bs'
  where (bs',xs) = optimize bs []

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
  (label,vs,[],If e (Goto g) (Goto g')) ->
    let tail = walk (Goto g) ls
        tail' = walk (Goto g') ls
        (bs',ls') = optimize bs ((Goto label,If e tail tail'):ls) in
    ((label,vs,[],If e tail tail'):bs',ls')
  _ -> let (bs',ls') = optimize bs ls in (b:bs',ls')

walk x xs = case lookup x xs of
  Just x' -> walk x' xs
  Nothing -> x

fMapTail f (x,vs,ss,tail) = (x,vs,ss,help tail) where
  help tail = case tail of
    Goto x -> f (Goto x)
    GotoWait x -> GotoWait x
    If e t t' -> f (If e (help t) (help t'))
    Exit -> Exit

--}}}
--{{{ Tests

testFlat = flattenPrgm [Lang.Decl ("x",Lang.Int) (Lang.Var "y")]

testIf = flattenPrgm [Lang.Exp (Lang.NumLit 5),
                      Lang.If (Lang.NumLit 6)
                              [Lang.Exp $ Lang.Call (Lang.CFn "cfun") []]
                              [Lang.Exp $ Lang.Call (Lang.CFn "afun") []],
                      Lang.Exp (Lang.NumLit 10)]

testWhile = flattenPrgm [Lang.Exp (Lang.NumLit 5),
                         Lang.While (Lang.NumLit 6)
                                    [Lang.Exp $ Lang.Call (Lang.CFn "rep") [],
                                     Lang.Exp $ Lang.Call (Lang.CFn "rep2") []],
                         Lang.Exp (Lang.NumLit 10)]

testExit = flattenPrgm [Lang.Exp (Lang.NumLit 5),
                        Lang.Exit,
                        Lang.Exp (Lang.NumLit 6)]

testIfExit = flattenPrgm [Lang.Exp (Lang.NumLit 5),
                          Lang.If (Lang.NumLit 6)
                                  [Lang.Exit]
                                  [Lang.Exp $ Lang.Call (Lang.CFn "afun") []],
                          Lang.Exp (Lang.NumLit 10)]

testWait = flattenPrgm [Lang.Exp (Lang.NumLit 10),
                        Lang.Wait (Lang.Call (Lang.CFn "wfun") []),
                        Lang.Exp (Lang.NumLit 11)]

testSpawn = flattenPrgm [Lang.Exp (Lang.NumLit 5),
                         Lang.Spawn ([("x",Lang.Int),("y",Lang.Bool)],
                           [Lang.Exp (Lang.NumLit 10),
                            Lang.Exp (Lang.NumLit 15)])
                           [Lang.Var "arg1",Lang.Var "arg2"],
                         Lang.Exp (Lang.NumLit 20)]

testOptimize = flattenPrgm [Lang.If (Lang.NumLit 6)
                                    [Lang.Exit]
                                    [Lang.If (Lang.NumLit 7)
                                             [Lang.Exp (Lang.NumLit 8)]
                                             [Lang.Exp (Lang.NumLit 9)]],
                            Lang.Exp (Lang.NumLit 10)]

--}}}
