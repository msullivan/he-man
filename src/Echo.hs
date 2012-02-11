module Echo where

import Language.HeMan

q_limit = 1024
port = 2023
bufsize = 4096

setup_connection :: FdE -> Prog (FdE, EventE)
setup_connection fd = do
  make_nb fd
  e <- mk_nb_event fd kEVENT_RDWR
  return (fd, e)

setup_listener :: IntE -> Prog (FdE, EventE)
setup_listener port = do
  fd <- socket kAF_INET kSOCK_STREAM 0
  set_sock_reuse fd
  make_nb fd
  sock_bind_v4 fd kINADDR_ANY port
  sock_listen fd q_limit
  e <- mk_nb_event fd kEVENT_RD
  return (fd, e)

child_code = declare_thread ("child_fd", FD) $
  \child_fd -> do
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
    spawn child_code fd'

