module Back where

import qualified Lang
import Control.Monad.Writer

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

flattenPrgm stmts = flattenStmts stmts []

{- flattenStmts takes a list of Stmts, a list of succeeding Stmts, and the Tail
for that block, and returns a list of blocks.
-}

flattenStmts [] aStmts tail = [makeBlock "foo" [] aStmts tail]
flattenStmts stmts aStmts tail = flattenStmt (last stmts) (init stmts) aStmts tail

{- flattenStmt takes a Statement, a list of preceding Statements, a list of
succeeding Statements, and a Tail, and returns a new list of Statements and a
list of new blocks.
-}

flattenStmt stmt bStmts aStmts tail = 
  case stmt of
    Lang.Decl vdecl expr -> continue $ Decl vdecl expr
    Lang.Exp expr -> continue $ Exp expr
    Lang.Assign expr expr' -> continue $ Assign expr expr'
    --Lang.While expr stmts -> 
    --Lang.Exit -> 
    --Lang.Spawn (vds,ss) args -> ([spawnThread "blah" args],
  where continue s = flattenStmts bStmts (s:aStmts) tail


