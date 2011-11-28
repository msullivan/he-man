module Sugar where

import Lang
import Control.Monad.RWS

type Prog = RWS () [Stmt] Int

compile :: Prog () -> Block
compile prog = block
  where (_, block) = execRWS prog () 0

-- Generate a fresh integer for use in naming things
freshName :: Prog Int
freshName = do
  n <- get
  put (n+1)
  return n
  
-- Add a statement to the current block
add :: Stmt -> Prog ()
add s = tell [s]

--- Wrappers around all of the important language features
var :: String -> Type -> Expr -> Prog Expr
var name t e = do
  v <- freshName
  let name' = name ++ "_" ++ show v
  add $ Decl (name', t) e
  return $ Var name'

infixl 0 .=
infixl 0 .=.
(.=) :: Expr -> Expr -> Prog ()
l .= r = do
  add $ Assign l r

(.=.) :: Expr -> Prog Expr -> Prog ()
l .=. r = do
  r' <- r
  l .= r'

exit :: Prog ()
exit = add Exit

wait :: Expr -> Prog ()
wait = add . Wait

call :: Prim -> Type -> [Expr] -> Prog Expr
call fn t args = var "tmp" t (Call fn args)

spawn :: ThreadCode -> [Expr] -> Prog ()
spawn thread args = add $ Spawn thread args

while :: Expr -> Prog a -> Prog ()
while e body = do
  (_, bodyStmts) <- listen body
  add $ While e bodyStmts

ifE :: Expr -> Prog a -> Prog b -> Prog ()
ifE e thenBody elseBody = do
  (_, thenStmts) <- listen thenBody
  (_, elseStmts) <- listen elseBody
  add $ If e thenStmts elseStmts
  
ifE' :: Expr -> Prog a -> Prog ()
ifE' e thenBody = ifE e thenBody (return ())

(.<) = RelnOp Less
(.>) = RelnOp Greater
(.==) = RelnOp Eq

-- Helper to construct a ThreadCode - kind of annoying
declare_thread :: [VDecl] -> ([Expr] -> Prog ()) -> ThreadCode
declare_thread decls f = (decls, compile prog)
  where prog = f (map (Var . fst) decls)

-- Sugar for individual functions and whatnot
socket domain typ protcol =
  call (CFn "socket") Int [domain, typ, protcol]
make_nb fd = call (CFn "make_non_blocking") Int [fd]
sock_bind fd family addr port =
  call (CFn "bind") Int [fd, family, addr, port]
reg_event fd modes =
  call (CFn "reg_event") Int [fd, modes]
sock_listen fd q_limit =
  call (CFn "listen") Int [fd, q_limit]
sock_accept fd =
  call (CFn "accept") Int [fd]
sock_read fd buf len =
  call (CFn "read") Int [fd, buf, len]
sock_write fd buf len =
  call (CFn "write") Int [fd, buf, len]

new_buf size =
  call (CFn "new_buf") Buffer [size]


-- TODO: a bunch more
