module Back where

import qualified Lang
import Control.Monad.Writer
import Debug.Trace
import Data.Maybe

type Prgm = [Block]

type BlockName = String
type Block = (BlockName, [Lang.VDecl], [Stmt], Tail)

data Stmt = Decl Lang.VDecl Lang.Expr
          | Assign Lang.Expr Lang.Expr
          | Spawn BlockName [Lang.Expr]
          | Exp Lang.Expr
          deriving (Eq, Ord, Show)

data Tail = If Lang.Expr Tail Tail
          | Goto BlockName
          | GotoWait BlockName
          | Exit
          deriving (Eq, Ord, Show)

--{{{ flattenPrgm

makeBlock name decls stmts tail = (name,decls,stmts,tail)
spawnThread name args = [Exp $ Lang.Call (Lang.CFn "spawn") args']
  where args' = (Lang.Var name):args
mainBlock = "main"

{-
flattenPrgm prgm = 
  let (stmts,blocks) = runWriter $ flattenStmt "main" prgm 
      mainBlock = makeBlock "main" [] stmts Exit in
  mainBlock:blocks
-}

flattenPrgm stmts = [makeBlock mainBlock [] s t] ++ bbs
  where (s,t,bbs) = flattenStmts stmts [] Exit

{- flattenStmts takes a list of Stmts, a list of succeeding Stmts, and the Tail
for that block, and returns a new list of Stmts, a new Tail, and a list of new
blocks. -}

flattenStmts :: [Lang.Stmt] -> [Stmt] -> Tail -> ([Stmt],Tail,[Block])
flattenStmts [] aStmts tail = (aStmts,tail,[])
flattenStmts stmts aStmts tail = flattenStmt (last stmts) (init stmts) aStmts tail

{- flattenStmt takes a Stmt, a list of preceding Stmt, a list of succeeding
Stmt, and a Tail, and returns a new list of Stmt, a new Tail, and a list of new
blocks. -}

flattenStmt :: Lang.Stmt -> [Lang.Stmt] -> [Stmt] -> Tail -> ([Stmt],Tail,[Block])
flattenStmt stmt bStmts aStmts tail =
  case stmt of
    Lang.Decl vdecl expr -> continue $ Decl vdecl expr
    Lang.Exp expr -> continue $ Exp expr
    Lang.Assign expr expr' -> continue $ Assign expr expr'
    Lang.If expr cs as -> 
      (bs,If expr (Goto "c") (Goto "a"),[sbb,cbb,abb] ++ bbbs ++ cbbs ++ abbs)
      where sbb = makeBlock "ifseq" [] aStmts tail
            (bs,bbt,bbbs) = flattenStmts bStmts [] tail
            (cs',cbt,cbbs) = flattenStmts cs [] (Goto "ifseq")
            (as',abt,abbs) = flattenStmts as [] (Goto "ifseq")
            cbb = makeBlock "c" [] cs' cbt
            abb = makeBlock "a" [] as' abt
    Lang.While expr ss -> (bs,wseq,[sbb,wbb] ++ bbbs ++ wbbs)
      where sbb = makeBlock "whileseq" [] aStmts tail
            wseq = If expr (Goto "w") (Goto "whileseq")
            (bs,bbt,bbbs) = flattenStmts bStmts [] wseq
            (ws,wbt,wbbs) = flattenStmts ss [] wseq
            wbb = makeBlock "w" [] ws wbt
    Lang.Exit -> (bs,Exit,bbbs)
      where (bs,bbt,bbbs) = flattenStmts bStmts [] Exit
    Lang.Wait expr -> (bs,wseq,sbb:bbbs)
      where sbb = makeBlock "waitseq" [] aStmts tail
            wseq = GotoWait "waitseq"
            (bs,bbt,bbbs) = flattenStmts (bStmts ++ [Lang.Exp expr]) [] wseq
    Lang.Spawn (vs,ss) args -> (bs,Exit,[tbb] ++ tbbs ++ bbbs)
      where spawn = spawnThread "newthread" args
            (bs,bbt,bbbs) = flattenStmts bStmts (spawn ++ aStmts) tail
            (ts,tbt,tbbs) = flattenStmts (ss) [] Exit
            tbb = makeBlock "newthread" vs ts Exit
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

{-
[("main",[],[Exp (NumLit 5)],If (NumLit 6) (Goto "c") (Goto "a")),
 ("ifseq",[],[Exp (NumLit 10)],Exit),
 ("c",[],[Exp (Call (CFn "cfun") [])],Goto "ifseq"),
 ("a",[],[Exp (Call (CFn "afun") [])],Goto "ifseq")]
-}
testIf = flattenPrgm [Lang.Exp (Lang.NumLit 5),
                      Lang.If (Lang.NumLit 6)
                              [Lang.Exp $ Lang.Call (Lang.CFn "cfun") []]
                              [Lang.Exp $ Lang.Call (Lang.CFn "afun") []],
                      Lang.Exp (Lang.NumLit 10)]

{-
[("main",[],[Exp (NumLit 5)],If (NumLit 6) (Goto "w") (Goto "whileseq")),
 ("whileseq",[],[Exp (NumLit 10)],Exit),
 ("w",[],[Exp (Call (CFn "rep") []),Exp (Call (CFn "rep2") [])],If (NumLit 6) (Goto "w") (Goto
  "whileseq"))]
-}
testWhile = flattenPrgm [Lang.Exp (Lang.NumLit 5),
                         Lang.While (Lang.NumLit 6)
                                    [Lang.Exp $ Lang.Call (Lang.CFn "rep") [],
                                     Lang.Exp $ Lang.Call (Lang.CFn "rep2") []],
                         Lang.Exp (Lang.NumLit 10)]

{-
[("main",[],[Exp (NumLit 5)],Exit)]
-}
testExit = flattenPrgm [Lang.Exp (Lang.NumLit 5),
                        Lang.Exit,
                        Lang.Exp (Lang.NumLit 6)]

{-
[("main",[],[Exp (NumLit 5)],If (NumLit 6) (Goto "c") (Goto "a")),
 ("ifseq",[],[Exp (NumLit 10)],Exit),
 ("c",[],[],Exit),
 ("a",[],[Exp (Call (CFn "afun") [])],Goto "ifseq")]
-}
testIfExit = flattenPrgm [Lang.Exp (Lang.NumLit 5),
                          Lang.If (Lang.NumLit 6)
                                  [Lang.Exit]
                                  [Lang.Exp $ Lang.Call (Lang.CFn "afun") []],
                          Lang.Exp (Lang.NumLit 10)]

{-
[("main",[],[Exp (NumLit 10),Exp (Call (CFn "wfun") [])],GotoWait "waitseq"),
 ("waitseq",[],[Exp (NumLit 11)],Exit)]
-}
testWait = flattenPrgm [Lang.Exp (Lang.NumLit 10),
                        Lang.Wait (Lang.Call (Lang.CFn "wfun") []),
                        Lang.Exp (Lang.NumLit 11)]

{-
[("main",[],[Exp (NumLit 5),
             Exp (Call (CFn "spawn") [Var "newthread",Var "arg1",Var "arg2"]),
             Exp (NumLit 20)],Exit),
 ("newthread",[("x",Int),("y",Bool)],[Exp (NumLit 10),Exp (NumLit 15)],Exit)]
-}
testSpawn = flattenPrgm [Lang.Exp (Lang.NumLit 5),
                         Lang.Spawn ([("x",Lang.Int),("y",Lang.Bool)],
                           [Lang.Exp (Lang.NumLit 10),
                            Lang.Exp (Lang.NumLit 15)])
                           [Lang.Var "arg1",Lang.Var "arg2"],
                         Lang.Exp (Lang.NumLit 20)]

{-
[("main",[],[],If (NumLit 6) (Goto "c") (Goto "a")),
 ("ifseq",[],[],Exit),
 ("c",[],[],Exit),
 ("a",[],[],If (NumLit 7) (Goto "c") (Goto "a")),
 ("ifseq",[],[],Goto "ifseq"),
 ("c",[],[],Exit),
 ("a",[],[],Exit)]
-}

testOptimize = optimizeJumps $
  flattenPrgm[Lang.If (Lang.NumLit 6)
                      [Lang.Exit]
                      [Lang.If (Lang.NumLit 7)
                               [Lang.Exp (Lang.NumLit 8)]
                               [Lang.Exp (Lang.NumLit 9)]],
              Lang.Exp (Lang.NumLit 10)]

{-
[("main",[],[],If (NumLit 6) Exit (If (NumLit 7) (Goto "c2") (Goto "a2"))),
 ("a1",[],[],If (NumLit 7) (Goto "c2") (Goto "a2")),
 ("ifseq2",[],[Exp (NumLit 10)],Exit),
 ("c2",[],[Exp (NumLit 8)],Goto "ifseq2"),
 ("a2",[],[Exp (NumLit 9)],Goto "ifseq2")]
-}

testOptimize' = optimizeJumps $
  [("main",[],[],If (Lang.NumLit 6) (Goto "c1") (Goto "a1")),
   ("c1",[],[],Exit),
   ("a1",[],[],If (Lang.NumLit 7) (Goto "c2") (Goto "a2")),
   ("ifseq2",[],[Exp (Lang.NumLit 10)],Exit),
   ("c2",[],[Exp (Lang.NumLit 8)],Goto "ifseq2"),
   ("a2",[],[Exp (Lang.NumLit 9)],Goto "ifseq2")]

--}}}
