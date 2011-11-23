module Echo where

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

import Lang
import Sugar

------------------------------ Library code
kAF_INET = Constant "AF_INET"
kINADDR_ANY = Constant "INADDR_ANY"
kSOCK_STREAM = Constant "SOCK_STREAM"
kEVENT_RD = Constant "EVENT_RD"
kEVENT_WR = Constant "EVENT_WR"
kEVENT_RDWR = Constant "EVENT_RDWR"


do_nb_action :: Expr -> Prog Expr -> Prog Expr
do_nb_action e action = do
  res <- action
  ifE' (res .< 0) $ do
    while (res .< 0) $ do
      wait e
      res .=. action
  return res

accept fd e = do_nb_action e (sock_accept fd)
do_read fd e buf size = do_nb_action e (sock_read fd buf size)
do_write fd e buf size = do_nb_action e (sock_write fd buf size)

-- TODO: error checking
full_write fd e buf size = do
  amt_written <- var "amt_written" Int 0
  while (amt_written .< size) $ do
    amt <- do_write fd e (buf + amt_written) (size - amt_written)
    amt_written .= amt_written + amt

------------------------------ The application
q_limit = 1024
port = 2023
bufsize = 4096

setup_connection :: Expr -> Prog Expr
setup_connection fd = do
  make_nb fd
  e <- var "event" Event =<< (reg_event fd kEVENT_RDWR)
  return e

-- This is kind of lame
child_args = [("child_fd", Int)]
child_fd = Var "child_fd"
child_code = (child_args, compile child_body)

child_body = do
  e <- (setup_connection child_fd)
  buf <- var "buf" Buffer =<< (new_buf bufsize)
  while 1 $ do
    amount_read <- do_read child_fd e buf bufsize
    ifE' (amount_read .== 0) exit
    full_write child_fd e buf amount_read

setup_listener :: Expr -> Prog (Expr, Expr)
setup_listener port = do
  fd <- var "fd" Int =<< (socket kAF_INET kSOCK_STREAM 0)
  make_nb fd
  sock_bind fd kAF_INET kINADDR_ANY port
  sock_listen fd q_limit
  e <- var "event" Event =<< (reg_event fd kEVENT_RD)
  return (fd, e)

main_loop = do
  (fd, e) <- setup_listener port
  while 1 $ do
    fd' <- accept fd e
    spawn child_code [fd']
main_loop_code = compile main_loop
