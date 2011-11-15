module Sugar where

import Lang
import Control.Monad.State
import Control.Monad.Writer

type Prog = StateT Int (Writer [Stmt])

freshName :: Prog Int
freshName = do
  n <- get
  put (n+1)
  return n
  
add :: Stmt -> Prog ()
add s = tell [s]

var :: String -> Type -> Expr -> Prog Expr
var name t e = do
  v <- freshName
  let name' = name ++ "_" ++ show v
  add $ Decl (name', t) e
  return $ Var name'
  
(.=) :: Expr -> Expr -> Prog ()
l .= r = add $ Assign l r

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

-- Sugar for individual functions and whatnot
socket domain typ protcol =
  call (CFn "socket") Int [domain, typ, protcol]
make_nb fd = call (CFn "make_non_blocking") Int [fd]
bind fd family addr port =
  call (CFn "socket") Int [fd, family, addr, port]
reg_event fd modes =
  call (CFn "reg_event") Int [fd, modes]
-- TODO: a bunch more
