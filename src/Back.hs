module Back where

import qualified Lang
import Control.Monad.Writer
import Control.Monad.State
import Debug.Trace
import Data.Maybe
import Data.Functor.Identity

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

makeBlock name decls stmts tail = (name,decls,stmts,tail)
spawnThread name args = [Exp $ Lang.Call (Lang.CFn "spawn") args']
  where args' = (Lang.NumLit $ fromIntegral name):args

fresh = do
  x <- get
  modify (1+)
  return x

fresh2 = do
  x <- fresh
  y <- fresh
  return (x,y)

{-
flattenPrgm prgm = 
  let (stmts,blocks) = runWriter $ flattenStmt "main" prgm 
      mainBlock = makeBlock "main" [] stmts Exit in
  mainBlock:blocks
-}

flattenPrgm stmts = [makeBlock 0 [] s t] ++ bbs where
  ((s,t,bbs),_) = runState (flattenStmts stmts [] Exit) (mainB + 1)
  mainB = 0
  
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
blocks. -}

flattenStmt :: Lang.Stmt -> [Lang.Stmt] -> [Stmt] -> Tail ->
               Flattener ([Stmt],Tail,[Block])
flattenStmt stmt bStmts aStmts tail =
  case stmt of
    Lang.Decl vdecl expr -> continue $ Decl vdecl expr
    Lang.Exp expr -> continue $ Exp expr
    Lang.Assign expr expr' -> continue $ Assign expr expr'
    Lang.If expr cs as -> 
      do seqB <- fresh
         let sbb = makeBlock seqB [] aStmts tail
         (bs,bbt,bbbs) <- flattenStmts bStmts [] tail
         (cs',cbt,cbbs) <- flattenStmts cs [] (Goto seqB)
         (as',abt,abbs) <- flattenStmts as [] (Goto seqB)
         (conB,altB) <- fresh2
         let cbb = makeBlock conB [] cs' cbt
         let abb = makeBlock altB [] as' abt
         return 
           (bs,
            If expr (Goto conB) (Goto altB),
            [sbb,cbb,abb] ++ bbbs ++ cbbs ++ abbs)
    Lang.While expr ss ->
      do (seqB,whileB) <- fresh2
         let sbb = makeBlock seqB [] aStmts tail
             wseq = If expr (Goto whileB) (Goto seqB)
         (bs,bbt,bbbs) <- flattenStmts bStmts [] wseq
         (ws,wbt,wbbs) <- flattenStmts ss [] wseq
         let wbb = makeBlock whileB [] ws wbt
         return (bs,wseq,[sbb,wbb] ++ bbbs ++ wbbs)
    Lang.Exit ->
      do (bs,bbt,bbbs) <- flattenStmts bStmts [] Exit
         return (bs,Exit,bbbs)
    Lang.Wait expr ->
      do seqB <- fresh
         let sbb = makeBlock seqB [] aStmts tail
             wseq = GotoWait seqB 
         (bs,bbt,bbbs) <- flattenStmts (bStmts ++ [Lang.Exp expr]) [] wseq
         return (bs,wseq,sbb:bbbs)
    Lang.Spawn (vs,ss) args ->
      do threadB <- fresh
         let spawn = spawnThread threadB args
         (bs,bbt,bbbs) <- flattenStmts bStmts (spawn ++ aStmts) tail
         (ts,tbt,tbbs) <- flattenStmts (ss) [] Exit
         let tbb = makeBlock threadB vs ts Exit
         return (bs,Exit,[tbb] ++ tbbs ++ bbbs)
  where continue s = flattenStmts bStmts (s:aStmts) tail

--}}}
--{{{ optimizeJumps

{- optimizeJumps removes unnecessary Gotos from the output of flattenPrgm. -}

-- TODO combine small non-empty blocks
-- TODO remove unneeded blocks which appear in If tails

optimizeJumps :: Prgm -> Prgm
optimizeJumps bs = traceShow xs $ map (fMapTail (flip walk xs)) bs'
  where
  (bs',xs) = optimize [] bs
  optimize xs [] = ([],xs)
  optimize xs (b:bs) = case b of
    (x,vs,[],Goto x') ->
      if Goto x `elem` map snd xs
        then optimize ((Goto x,walk (Goto x') xs):xs) bs
        else let (bs',xs') = optimize xs bs in ((x,vs,[],walk (Goto x') xs):bs',xs')
    (x,vs,[],Exit) -> optimize ((Goto x,Exit):xs) bs
    (x,vs,[],If e (Goto x') (Goto x'')) ->
      let xs' = (Goto x,If e (walk (Goto x') xs) (walk (Goto x'') xs)):xs in
      if Goto x `elem` map snd xs'
        then optimize xs' bs
        else let (bs',xs'') = optimize xs' bs in
             ((x,vs,[],If e (walk (Goto x') xs) (walk (Goto x'') xs)):bs',xs'')
    _ -> let (bs',xs') = optimize xs bs in (b:bs',xs')


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

testOptimize = optimizeJumps $
  flattenPrgm[Lang.If (Lang.NumLit 6)
                      [Lang.Exit]
                      [Lang.If (Lang.NumLit 7)
                               [Lang.Exp (Lang.NumLit 8)]
                               [Lang.Exp (Lang.NumLit 9)]],
              Lang.Exp (Lang.NumLit 10)]

--}}}
