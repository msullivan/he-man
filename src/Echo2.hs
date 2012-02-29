module Echo where

import Language.HeMan

q_limit = 1024
port = 2083
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

writer_code = declare_thread (("child_fd", FD), ("channel", Channel)) $
  \(fd, ch) -> do
    ev <- setup_connection fd
    while 1 $ do
      (amount_read, buf) <- channel_recv_buf ch
      ifE' (amount_read .== 0) $ do
        dec_refcount ch
        exit
      amount_written <- full_write ev buf amount_read
      dec_buf_refcount buf
      --ifE' (amount_written .< amount_read) exit
    exit

duplicate_buf buf sz =
  do new_buf <- new_rc_buf sz
     memcpy new_buf buf sz
     return new_buf

child_code = declare_thread ("child_fd", FD) $
  \child_fd -> do
  ev <- setup_connection child_fd
  ch <- new_channel
  inc_refcount ch
  write_fd <- dup child_fd
  spawn writer_code (write_fd, ch)
  buf <- new_buf bufsize
  while 1 $ do
    amount_read <- do_read ev buf bufsize
    ifE' (amount_read .== 0) $ do
      channel_send ch 0 (int_to_data 0)
      dec_refcount ch
      exit
    new_buf <- duplicate_buf buf amount_read
    channel_send ch amount_read (buf_to_data new_buf)    

main_loop = do
  ev <- setup_listener port
  while 1 $ do
    fd' <- accept ev
    spawn child_code fd'

