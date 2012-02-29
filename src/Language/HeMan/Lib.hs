module Language.HeMan.Lib where

import Language.HeMan.Syntax

-- Coreish runtime functions
mk_nb_event :: FdE -> IntE -> Prog EventE
mk_nb_event fd modes =
  callName "event" (CFn "mk_nb_event") Event (curThread, fd, modes)
-- This is kind of lame
new_buf :: IntE -> Prog BufferE
new_buf size =
  callName "buf" (CFn "new_buf") Buffer (curThread, size)
new_rc_buf :: IntE -> Prog BufferE
new_rc_buf size =
  callName "buf" (CFn "new_rc_buf") Buffer (curThread, size)

prepare_event :: EventE -> IntE -> Prog ()
prepare_event fd modes =
  call' (CFn "prepare_event") (fd, modes)

-- Sugar for individual functions and whatnot
socket :: IntE -> IntE -> IntE -> Prog FdE
socket domain typ protcol =
  callName "sock_fd" (CFn "socket") FD (domain, typ, protcol)

set_sock_reuse :: FdE -> Prog IntE
set_sock_reuse fd = call (CFn "set_sock_reuse") Int (fd)

make_nb :: FdE -> Prog ()
make_nb fd = call' (CFn "make_socket_non_blocking") (fd)

sock_bind_v4 :: FdE -> IntE -> IntE -> Prog IntE
sock_bind_v4 fd addr port =
  call (CFn "sock_bind_v4") Int (fd, addr, port)

sock_listen :: FdE -> IntE -> Prog IntE
sock_listen fd q_limit =
  call (CFn "listen") Int (fd, q_limit)

sock_accept :: FdE -> Prog FdE
sock_accept fd =
  callName "accepted_fd" (CFn "accept") FD (fd, num 0, num 0)

sock_read :: FdE -> BufferE -> IntE -> Prog IntE
sock_read fd buf len =
  callName "amt_read" (CFn "read") Int (fd, buf, len)

sock_write :: FdE -> BufferE -> IntE -> Prog IntE
sock_write fd buf len =
  callName "amt_written" (CFn "write") Int (fd, buf, len)

close :: FdE -> Prog IntE
close fd =
  call (CFn "close") Int (fd)

open :: BufferE -> IntE -> Prog FdE
open path flags =
  call (CFn "open") FD (path, flags)

file_read :: FdE -> BufferE -> IntE -> Prog IntE
file_read fd buf len =
  callName "amt_read" (CFn "read") Int (fd, buf, len)
file_write :: FdE -> BufferE -> IntE -> Prog IntE
file_write fd buf len =
  callName "amt_written" (CFn "write") Int (fd, buf, len)

-- Things for channels and other bullshit
inc_refcount :: Expr a -> Prog ()
inc_refcount p = call' (CFn "inc_refcount") (p)
dec_refcount :: Expr a -> Prog ()
dec_refcount p = call' (CFn "dec_refcount") (p)
inc_buf_refcount :: Expr Buffer -> Prog ()
inc_buf_refcount p = call' (CFn "inc_buf_refcount") (p)
dec_buf_refcount :: Expr Buffer -> Prog ()
dec_buf_refcount p = call' (CFn "dec_buf_refcount") (p)

-- We don't want people doing any lower level operations on
-- messages.
read_msg :: Expr Msg -> Prog (IntE, Expr Data)
read_msg m =
  do tag <- callName "tag" (CFn "read_msg_tag") Int (m)
     payload <- callName "payload" (CFn "read_msg_payload") Data (m)
     return (tag, payload)

new_channel :: Prog ChannelE
new_channel =
  callName "ch" (CFn "new_channel") Channel ()
channel_send :: ChannelE -> IntE -> DataE -> Prog ()
channel_send ch tag payload =
  call' (CFn "channel_send") (ch, tag, payload)
channel_recv :: (ChannelE, EventE) -> Prog (IntE, Expr Data)
channel_recv (ch, e) =
  do wait e
     msg <- call (CFn "channel_recv") Msg ch
     read_msg msg

buf_to_data :: BufferE -> Prog DataE
buf_to_data b = callName "buf" (CFn "buf_to_data") Data (b)
data_to_buf :: DataE -> Prog BufferE
data_to_buf d = callName "data" (CFn "data_to_buf") Buffer (d)


-- For debugging
print_int n  =
  call' (CFn "print_int") (n)

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

-- This implementation depends on level triggered semantics
do_nb_action :: ExprFailable a =>
                Type a -> IntE -> Expr Event -> Prog (Expr a) ->
                Prog (Expr a)
do_nb_action t mode e action = do
  res <- var "result" t (E (NumLit (-1))) -- HACK! bad!
  while (isFailure res) $ do
    prepare_event e mode
    wait e
    res .=. action
  return res

accept (fd, e) = do_nb_action FD kEVENT_RD e (sock_accept fd)
do_read (fd, e) buf size = do_nb_action Int kEVENT_RD e
                           (sock_read fd buf size)
do_write (fd, e) buf size = do_nb_action Int kEVENT_WR e
                            (sock_write fd buf size)

-- TODO remove this
full_write_naive ev buf size = do
  amt_written <- var "total_written" Int 0
  failed <- var "write_failed" Int 0
  while (amt_written .< size .&& failed .== 0) $ do
    amt <- do_write ev (buf +* amt_written) (size - amt_written)
    amt_written .= amt_written + amt
    ifE' (amt .== 0) $ do failed .= 1
  return amt_written

-- This is "good" in the sense that by incorporating EAGAIN and short
-- returns into one loop, it reduces the number of loops we generate,
-- hopefully allowing us to produce more idiomatic looking code.
full_write (fd, e) buf size = do
  amt_written <- var "total_written" Int 0
  failed <- var "write_failed" Int 0
  while (amt_written .< size .&& failed .== 0) $ do
    prepare_event e kEVENT_WR
    amt <- sock_write fd (buf +* amt_written) (size - amt_written)
    ifE (amt .== -1) (wait e) $
      (ifE (amt .== 0) (failed .= 1)
       (amt_written .= amt_written + amt))
  return amt_written
