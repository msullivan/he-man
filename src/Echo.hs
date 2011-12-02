module Echo where

import Lang
import Back
import Pretty
import Sugar
import Lib
import Codegen

q_limit = 1024
port = 2023
bufsize = 4096

setup_connection :: Expr -> Prog Expr
setup_connection fd = do
  make_nb fd
  e <- var "event" Event =<< (mk_nb_event fd kEVENT_RDWR)
  return e

setup_listener :: Expr -> Prog (Expr, Expr)
setup_listener port = do
  fd <- var "fd" Int =<< (socket kAF_INET kSOCK_STREAM 0)
  make_nb fd
  sock_bind_v4 fd kINADDR_ANY port
  sock_listen fd q_limit
  e <- var "event" Event =<< (mk_nb_event fd kEVENT_RD)
  return (fd, e)

child_code = declare_thread [("child_fd", Int)] $
  \[child_fd] -> do
  e <- (setup_connection child_fd)
  buf <- var "buf" Buffer =<< (new_buf bufsize)
  while 1 $ do
    amount_read <- do_read child_fd e buf bufsize
    ifE' (amount_read .== 0) exit
    amount_written <- full_write child_fd e buf amount_read
    ifE' (amount_written .< amount_read) exit

main_loop = do
  (fd, e) <- setup_listener port
  while 1 $ do
    fd' <- accept fd e
    spawn child_code [fd']
main_loop_code = compile main_loop
main_loop_back = runPasses $ compile main_loop
testPretty = pretty $ fst main_loop_back
