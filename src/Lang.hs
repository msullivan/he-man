module Lang where

type Block = [Stmt]
type Var = String
type VDecl = (Var, Type)
type ThreadCode = ([VDecl], Block)

data Stmt = Decl VDecl Expr
          | While Expr Block
          | If Expr Block Block
          | Spawn ThreadCode [Expr]
          | Assign Expr Expr
          | Exp Expr
          | Wait Expr
          | Exit
          deriving (Eq, Ord, Show)

data Type = Int | Bool | String | FD | Buffer | Event -- | ThreadT
          deriving (Eq, Ord, Show)
data Expr = Call Prim [Expr]
          | Arith ArithOp Expr Expr
          | ArithUnop ArithUnop Expr
          | RelnOp RelnOp Expr Expr
          | Constant String
          | NumLit Integer
          | StringLit String
          | Var Var
          deriving (Eq, Ord, Show)
data ArithOp = Plus | Times | Minus | Div | Mod
             | And | Or | Xor
             | Rsh | Lsh
             deriving (Eq, Ord, Show)
data ArithUnop = Negate | Not
               deriving (Eq, Ord, Show)
data RelnOp = Eq | Less | Greater -- more
             deriving (Eq, Ord, Show)
data Prim = CFn String
          deriving (Eq, Ord, Show)


instance Num Expr where
  fromInteger = NumLit
  (+) = Arith Plus
  (-) = Arith Minus
  (*) = Arith Times
  negate = ArithUnop Negate
  abs = error "full of lies"
  signum = error "full of lies"
