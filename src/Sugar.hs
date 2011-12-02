module Sugar where

import Lang
import Control.Monad.RWS

type Prog = RWS () [Stmt] Int

desugar :: Prog () -> Block
desugar prog = block
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

extract :: Prog a -> Prog (a, [Stmt])
extract m = censor (const []) (listen m)

while :: Expr -> Prog a -> Prog ()
while e body = do
  (_, bodyStmts) <- extract body
  add $ While e bodyStmts

ifE :: Expr -> Prog a -> Prog b -> Prog ()
ifE e thenBody elseBody = do
  (_, thenStmts) <- extract thenBody
  (_, elseStmts) <- extract elseBody
  add $ If e thenStmts elseStmts
  
ifE' :: Expr -> Prog a -> Prog ()
ifE' e thenBody = ifE e thenBody (return ())

infixr 2 .||
infixr 3 .&&
infix  4 .==, .<, .> --, ./=, .<=, .>=

(.<) = RelnOp Less
(.>) = RelnOp Greater
(.==) = RelnOp Eq
(.&&) = Arith And
(.||) = Arith Or

-- Helper to construct a ThreadCode - kind of annoying
declare_thread :: [VDecl] -> ([Expr] -> Prog ()) -> ThreadCode
declare_thread decls f = (decls, desugar prog)
  where prog = f (map (Var . fst) decls)
