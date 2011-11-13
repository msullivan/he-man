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

type Prgm = [Stmt]
type Var = String
data Type = Int | Bool | ThreadT | FD | Buffer | Event
data Stmt = Decl Type Var Expr 
          | While Expr [Stmt]
          | If Expr [Stmt] [Stmt] [Stmt]
          | Assign Var Expr
data Expr = Call Prim 
          | Thread [Stmt]
data Prim = Unit
