module Language.HeMan.Lib where

import Language.HeMan.Syntax

-- Coreish runtime functions
mk_nb_event :: IntE -> IntE -> Prog EventE
mk_nb_event fd modes =
  callName "event" (CFn "mk_nb_event") Event (curThread, fd, modes)
-- This is kind of lame
new_buf :: IntE -> Prog BufferE
new_buf size =
  callName "buf" (CFn "new_buf") Buffer (curThread, size)

-- Sugar for individual functions and whatnot
socket :: IntE -> IntE -> IntE -> Prog IntE
socket domain typ protcol =
  callName "sock_fd" (CFn "socket") Int (domain, typ, protcol)

set_sock_reuse :: IntE -> Prog IntE
set_sock_reuse fd = call (CFn "set_sock_reuse") Int (fd)

make_nb :: IntE -> Prog IntE
make_nb fd = call (CFn "make_socket_non_blocking") Int (fd)

sock_bind_v4 :: IntE -> IntE -> IntE -> Prog IntE
sock_bind_v4 fd addr port =
  call (CFn "sock_bind_v4") Int (fd, addr, port)

sock_listen :: IntE -> IntE -> Prog IntE
sock_listen fd q_limit =
  call (CFn "listen") Int (fd, q_limit)

sock_accept :: IntE -> Prog IntE
sock_accept fd =
  callName "accepted_fd" (CFn "accept") Int (fd, num 0, num 0)

sock_read :: IntE -> BufferE -> IntE -> Prog IntE
sock_read fd buf len =
  callName "amt_read" (CFn "read") Int (fd, buf, len)

sock_write :: IntE -> BufferE -> IntE -> Prog IntE
sock_write fd buf len =
  callName "amt_written" (CFn "write") Int (fd, buf, len)

close :: IntE -> Prog IntE
close fd =
  call (CFn "close") Int (fd)

open :: BufferE -> IntE -> Prog IntE  
open path flags =
  call (CFn "open") Int (path, flags)

file_read :: IntE -> BufferE -> IntE -> Prog IntE
file_read fd buf len =
  callName "amt_read" (CFn "read") Int (fd, buf, len)
file_write :: IntE -> BufferE -> IntE -> Prog IntE
file_write fd buf len =
  callName "amt_written" (CFn "write") Int (fd, buf, len)


-- For debugging
print_int n  =
  call (CFn "print_int") Int (n)

errno = Constant "errno" -- weeeee

-- TODO: a bunch more

------------------------------ Library code
kAF_INET = constant "AF_INET"
kINADDR_ANY = constant "INADDR_ANY"
kSOCK_STREAM = constant "SOCK_STREAM"
kEVENT_RD = constant "EVENT_RD"
kEVENT_WR = constant "EVENT_WR"
kEVENT_RDWR = constant "EVENT_RDWR"

kO_RDONLY = constant "O_RDONLY"

kEAGAIN = constant "EAGAIN"


do_nb_action :: Expr Event -> Prog IntE -> Prog IntE
do_nb_action e action = do
  res <- action
  ifE' (res .< 0) $ do
    while (res .< 0) $ do
      wait e
      res .=. action
  return res

accept (fd, e) = do_nb_action e (sock_accept fd)
do_read (fd, e) buf size = do_nb_action e (sock_read fd buf size)
do_write (fd, e) buf size = do_nb_action e (sock_write fd buf size)

full_write ev buf size = do
  amt_written <- var "total_written" Int 0
  failed <- var "write_failed" Int 0
  while (amt_written .< size .&& failed .== 0) $ do
    amt <- do_write ev (buf +* amt_written) (size - amt_written)
    amt_written .= amt_written + amt
    ifE' (amt .== 0) $ do failed .= num 1
  return amt_written

full_write_good_but_broken (fd, e) buf size = do
  amt_written <- var "total_written" Int 0
  failed <- var "write_failed" Int 0
  while (amt_written .< size .&& failed .== 0) $ do
    amt <- sock_write fd (buf +* amt_written) (size - amt_written)
    ifE (amt .== -1) (wait e) $
      (ifE (amt .== 0) (failed .= num 1)
       (amt_written .= amt_written + amt))
  return amt_written
