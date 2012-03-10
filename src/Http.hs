module Http where

import Language.HeMan

q_limit = 1024
port = 8080
bufsize = 511

http_parse :: BufferE -> IntE -> Prog IntE
http_parse buf len =
  callName "parse_result" (CFn "http_parse") Int (buf, len)
get_file_size :: FdE -> Prog IntE
get_file_size fd =
  callName "file_size" (CFn "get_file_size") Int fd
http_make_hdr :: BufferE -> IntE -> IntE -> Prog IntE
http_make_hdr buf len fsize =
  callName "hdr_length" (CFn "http_make_hdr") Int (buf, len, fsize)

setup_connection :: FdE -> Prog (FdE, EventE)
setup_connection fd = do
  make_nb fd
  set_sock_cork fd
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

parse_request buf ev = do
  parse_result <- var "parse_result" Int 0
  request_size <- var "request_size" Int 0
  while (parse_result .== 0) $ do
    amt_read <- do_read ev buf (bufsize - request_size)
    ifE' (amt_read .== 0) exit
    request_size .= request_size + amt_read
    parse_result .=. http_parse buf request_size
  return parse_result

sendfile :: FdE -> FdE -> IntE -> Prog IntE
sendfile out_fd in_fd len =
  callName "amt_written" (CFn "sendfile") Int (out_fd, in_fd, num 0, len)

full_sendfile (out_fd, e) in_fd size = do
  amt_written <- var "total_written" Int 0
  failed <- var "write_failed" Int 0
  while (amt_written .< size .&& failed .== 0) $ do
    prepare_event e kEVENT_WR
    amt <- sendfile out_fd in_fd (size - amt_written)
    ifE (amt .== -1 .&& errno .== kEAGAIN) (wait e) $
      (ifE (amt .<= 0) (failed .= 1)
       (amt_written .= amt_written + amt))
  return amt_written

child_code = declare_thread (FD) $
  \child_fd -> do
  ev <- setup_connection child_fd
  buf <- new_buf (bufsize+1) -- pfbbt.
  parse_result <- parse_request buf ev
  -- XXX: error messages or something
  ifE' (parse_result .> 1 .|| parse_result .< 0) exit

  file_fd <- open buf kO_RDONLY
  ifE' (isFailure file_fd) exit
  let cleanup = close file_fd >> exit

  file_size <- get_file_size file_fd
  hdr_length <- http_make_hdr buf bufsize file_size
  amount_written <- full_write ev buf hdr_length
  ifE' (amount_written .< hdr_length) cleanup
  
  full_sendfile ev file_fd file_size
  cleanup

main_loop = do
  ev <- setup_listener port
  while 1 $ do
    fd' <- accept ev
    spawn child_code fd'
