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
import Control.Applicative

kAF_INET = Constant "AF_INET"
kINADDR_ANY = Constant "INADDR_ANY"
kSOCK_STREAM = Constant "SOCK_STREAM"
kEVENT_RD = Constant "EVENT_RD"

q_limit = 1024
port = 2023
bufsize = 4096

accept fd e = do
  fd' <- sock_accept fd
  ifE' (fd' .< 0) $ do
    while (fd' .< 0) $ do
      wait e
      fd'' <- sock_accept fd
      fd' .= fd''
  return fd'


setup_listener :: Expr -> Prog Expr
setup_listener port = do
  fd <- var "fd" Int =<< (socket kAF_INET kSOCK_STREAM 0)
  make_nb fd
  sock_bind fd kAF_INET kINADDR_ANY port
  sock_listen fd q_limit
  return fd

main_loop = do
  fd <- setup_listener port
  e <- var "event" Event =<< (reg_event fd kEVENT_RD)
  while 1 $ do
    fd' <- accept fd e
    spawn child_code [fd']


child_code = undefined
