module Http where

import Language.HeMan

q_limit = 1024
port = 8080
bufsize = 4096*4

http_parse buf len =
  callName "parse_result" (CFn "http_parse") Int [buf, len]

response_header = "HTTP/1.0 200 OK\r\n\r\n"

setup_connection :: Expr -> Prog (Expr, Expr)
setup_connection fd = do
  make_nb fd
  e <- mk_nb_event fd kEVENT_RDWR
  return (fd, e)

setup_listener :: Expr -> Prog (Expr, Expr)
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

child_code = declare_thread [("child_fd", Int)] $
  \[child_fd] -> do
  ev <- setup_connection child_fd
  buf <- new_buf (bufsize+1) -- pfbbt.
  parse_result <- parse_request buf ev
  -- XXX: error messages or something
  ifE' (parse_result .> 1 .|| parse_result .< 0) exit

  file_fd <- open buf kO_RDONLY
  ifE' (file_fd .< 0) exit
  let cleanup = close file_fd >> exit

  let hdr_length = NumLit $ toInteger $ length response_header
  header <- var "output_header" String (StringLit response_header)
  amount_written <- full_write ev header hdr_length
  ifE' (amount_written .< hdr_length) cleanup
  
  while 1 $ do
    amount_read <- file_read file_fd buf bufsize
    ifE' (amount_read .== 0) cleanup
    amount_written <- full_write ev buf amount_read
    ifE' (amount_written .< amount_read) cleanup


main_loop = do
  ev <- setup_listener port
  while 1 $ do
    fd' <- accept ev
    spawn child_code [fd']
