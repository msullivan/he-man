module Language.HeMan.Syntax where

import Control.Monad.RWS

--{{{ Front-end language

type Block = [Stmt]
type Var = String
type VDecl = (Var, Type)
type ThreadCode = ([VDecl], Block)

data Stmt = Decl VDecl DExpr
          | While DExpr Block
          | If DExpr Block Block
          | Spawn ThreadCode [DExpr]
          | Assign DExpr DExpr
          | Exp DExpr
          | Wait DExpr
          | Exit
          deriving (Eq, Ord, Show)

data Type = Int | Bool | String | FD | Buffer | Event -- | ThreadT
          deriving (Eq, Ord, Show)
data DExpr = Call Prim [DExpr]
          | Arith ArithOp DExpr DExpr
          | ArithUnop ArithUnop DExpr
          | RelnOp RelnOp DExpr DExpr
          | Constant String
          | NumLit Integer
          | StringLit String
          | Var Var
          | CurThread
          deriving (Eq, Ord, Show)
data ArithOp = Plus | Times | Minus | Div | Mod
             | And | Or | Xor
             | Rsh | Lsh
             deriving (Eq, Ord, Show)
data ArithUnop = Negate | Not
               deriving (Eq, Ord, Show)
data RelnOp = Eq | Less | Greater -- more
             deriving (Eq, Ord, Show)
data Prim = CFn String
          deriving (Eq, Ord, Show)

instance Num DExpr where
  fromInteger = NumLit
  (+) = Arith Plus
  (-) = Arith Minus
  (*) = Arith Times
  negate = ArithUnop Negate
  abs = error "full of lies"
  signum = error "full of lies"

--}}}
--{{{ Monadic sugar

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
var :: String -> Type -> DExpr -> Prog DExpr
var name t e = do
  v <- freshName
  let name' = name ++ "_" ++ show v
  add $ Decl (name', t) e
  return $ Var name'

infixl 0 .=
infixl 0 .=.
(.=) :: DExpr -> DExpr -> Prog ()
l .= r = do
  add $ Assign l r

(.=.) :: DExpr -> Prog DExpr -> Prog ()
l .=. r = do
  r' <- r
  l .= r'

exit :: Prog ()
exit = add Exit

wait :: DExpr -> Prog ()
wait = add . Wait

callName :: String -> Prim -> Type -> [DExpr] -> Prog DExpr
callName name fn t args = var name t (Call fn args)

call :: Prim -> Type -> [DExpr] -> Prog DExpr
call fn t args = callName "tmp" fn t args

spawn :: ThreadCode -> [DExpr] -> Prog ()
spawn thread args = add $ Spawn thread args

extract :: Prog a -> Prog (a, [Stmt])
extract m = censor (const []) (listen m)

while :: DExpr -> Prog a -> Prog ()
while e body = do
  (_, bodyStmts) <- extract body
  add $ While e bodyStmts

ifE :: DExpr -> Prog a -> Prog b -> Prog ()
ifE e thenBody elseBody = do
  (_, thenStmts) <- extract thenBody
  (_, elseStmts) <- extract elseBody
  add $ If e thenStmts elseStmts
  
ifE' :: DExpr -> Prog a -> Prog ()
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
declare_thread :: [VDecl] -> ([DExpr] -> Prog ()) -> ThreadCode
declare_thread decls f = (decls, desugar prog)
  where prog = f (map (Var . fst) decls)

--}}}
