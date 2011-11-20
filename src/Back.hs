module Back where

import qualified Lang
import Control.Monad.Writer
import Debug.Trace

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

{-
data Stmt = Decl VDecl Expr
          | Assign Expr Expr
          | While Expr [Stmt]
          | If Expr [Stmt] [Stmt]
          | Spawn ThreadCode [Expr]
          | Exp Expr
          | Wait Expr
          | Exit
          deriving (Eq, Ord, Show)
-}

-- Helpers

makeBlock name decls stmts tail = (name,decls,stmts,tail)
--spawnThread name args = [Exp $ Call (CFn "spawn") (Lang.Var name):args

-- Grammar

{-
flattenPrgm prgm = 
  let (stmts,blocks) = runWriter $ flattenStmt "main" prgm 
      mainBlock = makeBlock "main" [] stmts Exit in
  mainBlock:blocks
-}

flattenPrgm stmts = (makeBlock "main" [] s t):bbs
  where (s,t,bbs) = flattenStmts stmts [] Exit

{- flattenStmts takes a list of Stmts, a list of succeeding Stmts, and the Tail
for that block, and returns 
-}

flattenStmts :: [Lang.Stmt] -> [Stmt] -> Tail -> ([Stmt],Tail,[Block])
flattenStmts [] aStmts tail = (aStmts,tail,[])
flattenStmts stmts aStmts tail = flattenStmt (last stmts) (init stmts) aStmts tail

{- flattenStmt takes a Statement, a list of preceding Statements, a list of
succeeding Statements, and a Tail, and returns a new list of Statements, a new
Tail, and a list of new blocks.
-}

flattenStmt :: Lang.Stmt -> [Lang.Stmt] -> [Stmt] -> Tail -> ([Stmt],Tail,[Block])
flattenStmt stmt bStmts aStmts tail =
  case stmt of
    Lang.Decl vdecl expr -> continue $ Decl vdecl expr
    Lang.Exp expr -> continue $ Exp expr
    Lang.Assign expr expr' -> continue $ Assign expr expr'
    Lang.If expr cs as -> 
      (bs,If expr (Goto "c") (Goto "a"),([sbb,cbb,abb] ++ bbbs ++ cbbs ++ abbs))
        where sbb  = makeBlock "ifseq" [] aStmts tail
              (bs,bbt,bbbs) = flattenStmts bStmts [] tail -- FIXME what about bbt?
              (cs',cbt,cbbs) = flattenStmts cs [] (Goto "ifseq")
              (as',abt,abbs) = flattenStmts as [] (Goto "ifseq")
              cbb = makeBlock "c" [] cs' cbt
              abb = makeBlock "a" [] as' abt
    --Lang.Exit -> makeBlock "foo" [] aStmts 
    --Lang.Spawn (vds,ss) args -> continue $ [spawnThread "blah" args],
    --Lang.While expr stmts -> 
  where continue s = flattenStmts bStmts (s:aStmts) tail

-- TESTS

test1 = flattenPrgm [Lang.Decl ("x",Lang.Int) (Lang.Var "y")]

{-
[("main",[],[Exp (NumLit 5)],If (NumLit 6) (Goto "c") (Goto "a")),
 ("ifseq",[],[Exp (NumLit 10)],Exit),
 ("c",[],[Exp (Call (CFn "cfun") [])],Goto "ifseq"),
 ("a",[],[Exp (Call (CFn "afun") [])],Goto "ifseq")]
-}
test2 = flattenPrgm [Lang.Exp (Lang.NumLit 5),
                     Lang.If (Lang.NumLit 6)
                             [Lang.Exp $ Lang.Call (Lang.CFn "cfun") []]
                             [Lang.Exp $ Lang.Call (Lang.CFn "afun") []],
                     Lang.Exp (Lang.NumLit 10)]

