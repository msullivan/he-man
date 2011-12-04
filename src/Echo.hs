module Echo where

import Lang
import Sugar
import Lib
import Pretty (pretty)
import Back (backend)
import Codegen (codegen)

q_limit = 1024
port = 2023
bufsize = 4096

setup_connection :: Expr -> Prog (Expr, Expr)
setup_connection fd = do
  make_nb fd
  e <- mk_nb_event fd kEVENT_RDWR
  return (fd, e)

setup_listener :: Expr -> Prog (Expr, Expr)
setup_listener port = do
  fd <- socket kAF_INET kSOCK_STREAM 0
  make_nb fd
  sock_bind_v4 fd kINADDR_ANY port
  sock_listen fd q_limit
  e <- mk_nb_event fd kEVENT_RD
  return (fd, e)

child_code = declare_thread [("child_fd", Int)] $
  \[child_fd] -> do
  ev <- setup_connection child_fd
  buf <- new_buf bufsize
  while 1 $ do
    amount_read <- do_read ev buf bufsize
    ifE' (amount_read .== 0) exit
    amount_written <- full_write ev buf amount_read
    ifE' (amount_written .< amount_read) exit

main_loop = do
  ev <- setup_listener port
  while 1 $ do
    fd' <- accept ev
    spawn child_code [fd']

compile file p = do
  writeFile file (codegen $ backend $ desugar p)

main_loop_all = codegen $ backend $ desugar main_loop
main_loop_back = pretty $ fst . backend $ desugar main_loop

