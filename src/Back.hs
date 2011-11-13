module Back where

import qualified Lang

type Prgm' = [Block']

type BlockName = String
type Block = (BlockName, [Lang.VDecl], [Stmt], Tail)

data Stmt = Decl VDecl Lang.Expr
          | Assign Var Lang.Expr
          | Spawn BlockName [Expr]
          | Exp Lang.Expr
          deriving (Eq, Ord, Show)

type Tail = If Lang.Expr Tail Tail
          | Goto BlockName
          | GotoWait BlockName
          | Exit
          deriving (Eq, Ord, Show)


