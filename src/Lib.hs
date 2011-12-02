module Lib where

import Lang
import Sugar

-- Coreish runtime functions
mk_nb_event fd modes =
  call (CFn "mk_nb_event") Event [CurThread, fd, modes]
-- This is kind of lame
new_buf size =
  call (CFn "new_buf") Buffer [CurThread, size]

-- Sugar for individual functions and whatnot
socket domain typ protcol =
  call (CFn "socket") Int [domain, typ, protcol]
make_nb fd = call (CFn "make_socket_non_blocking") Int [fd]
sock_bind_v4 fd addr port =
  call (CFn "sock_bind_v4") Int [fd, addr, port]
sock_listen fd q_limit =
  call (CFn "listen") Int [fd, q_limit]
sock_accept fd =
  call (CFn "accept") Int [fd, 0, 0]
sock_read fd buf len =
  call (CFn "read") Int [fd, buf, len]
sock_write fd buf len =
  call (CFn "write") Int [fd, buf, len]
close fd =
  call (CFn "close") Int [fd]

-- For debugging
print_int n  =
  call (CFn "print_int") Int [n]


-- TODO: a bunch more

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

full_write fd e buf size = do
  amt_written <- var "amt_written" Int 0
  failed <- var "write_failed" Int 0
  while (amt_written .< size .&& failed .== 0) $ do
    amt <- do_write fd e (buf + amt_written) (size - amt_written)
    amt_written .= amt_written + amt
    ifE' (amt .== 0) $ do failed .= 1
  return amt_written
