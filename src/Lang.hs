module Lang where

{-
Echo server:

setup -> accept/spawn ->
  thread_setup ->
  read -> write | die
  write -> write | read | die

main = do
  fd <- var FD "fd" (setup_listener port)
  e <- var Event "e" (reg_event fd READ)
  while true { do
    fd' <- call accept fd
    spawn child_code fd'
  }

child_code = Thread [(Int,fd)] $ do
  e <- var Event "e" (setup_connection fd)
  buf <- var Buffer "buf" (new_buf 4096)
  while true { do
    amount_read <- read e buf 4096
    write e buf amount_read
  }

-}

type Block = [Stmt]
type Var = String
type VDecl = (Var, Type)
type ThreadCode = ([VDecl], Block)

data Type = Int | Bool | String | FD | Buffer | Event -- | ThreadT
          deriving (Eq, Ord, Show)
data Stmt = Decl VDecl Expr
          | While Expr Block
          | If Expr Block Block
          | Assign Expr Expr
          | Exp Expr
          | Wait Expr
          | Exit
          deriving (Eq, Ord, Show)
data Expr = Call Prim [Expr]
          | Thread ThreadCode [Expr]
          | Arith ArithOp Expr Expr
          | ArithUnop ArithUnop Expr
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
