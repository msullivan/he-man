{-#LANGUAGE EmptyDataDecls, FlexibleInstances #-}

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

data Type = Int | Bool | FD | Buffer | Event -- | ThreadT
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

-- Now for some phantom type bullshit

newtype Expr a = E DExpr
               deriving (Eq, Ord, Show)

data FD
data Buffer
data Event
data Thread

type IntE = Expr Int
type BoolE = Expr Bool
type BufferE = Expr Buffer
type EventE = Expr Event

-- Lift functions of DExprs to ones over Exprs
typ1 :: (DExpr -> DExpr) -> (Expr a -> Expr b)
typ1 f (E e1) = E (f e1)
typ2 :: (DExpr -> DExpr -> DExpr) -> (Expr a -> Expr b -> Expr c)
typ2 f (E e1) (E e2) = E (f e1 e2)

class ArgPacket a where
  toDExprList :: a -> [DExpr]
  makeVars :: [String] -> a

instance ArgPacket (Expr a) where
  toDExprList (E x) = [x]
  makeVars [x] = E $ Var x
instance ArgPacket (Expr a, Expr b) where
  toDExprList (E x1, E x2) = [x1, x2]
  makeVars [x1, x2] = (E $ Var x1, E $ Var x2)
instance ArgPacket (Expr a, Expr b, Expr c) where
  toDExprList (E x1, E x2, E x3) = [x1, x2, x3]
  makeVars [x1, x2, x3] = (E $ Var x1, E $ Var x2, E $ Var x3)
instance ArgPacket (Expr a, Expr b, Expr c, Expr d) where
  toDExprList (E x1, E x2, E x3, E x4) = [x1, x2, x3, x4]
  makeVars [x1, x2, x3, x4] = (E $ Var x1, E $ Var x2, E $ Var x3, E $ Var x4)

infixr 2 .||
infixr 3 .&&
infix  4 .==, .<, .> --, ./=, .<=, .>=
infixl 6 +*

(.<) :: IntE -> IntE -> BoolE
(.<) = typ2 $ RelnOp Less
(.>) :: IntE -> IntE -> BoolE
(.>) = typ2 $ RelnOp Greater
(.==) :: Expr a -> Expr a -> BoolE
(.==) = typ2 $ RelnOp Eq
(.&&) :: BoolE -> BoolE -> BoolE
(.&&) = typ2 $ Arith And
(.||) :: BoolE -> BoolE -> BoolE
(.||) = typ2 $ Arith Or


instance Num (Expr Int) where
  fromInteger = E . NumLit
  (+) = typ2 $ Arith Plus
  (-) = typ2 $ Arith Minus
  (*) = typ2 $ Arith Times
  negate = typ1 $ ArithUnop Negate
  abs = error "abs unimplemented"
  signum = error "signum unimplemented"
-- This is /super/ dubious. We want to be able to write "while 1", so
-- we overload fromInteger.  We /could/ try to give sensible meanings
-- to the arithmetic operations, but we really don't want to encourage
-- that sort of behavior.
instance Num (Expr Bool) where
  fromInteger = E . NumLit
  (+) = error "trying to use Num things on BoolE: don't do that"
  (-) = error "trying to use Num things on BoolE: don't do that"
  (*) = error "trying to use Num things on BoolE: don't do that"
  negate = error "trying to use Num things on BoolE: don't do that"
  abs = error "trying to use Num things on BoolE: don't do that"
  signum = error "trying to use Num things on BoolE: don't do that"



-- Pointer arithmetic
(+*) :: BufferE -> IntE -> BufferE
(+*) = typ2 $ Arith Plus

num :: Int -> IntE
num = E . NumLit . toInteger

curThread :: Expr Thread
curThread = E CurThread

constant :: String -> IntE
constant = E . Constant

stringLit :: String -> BufferE
stringLit = E . StringLit


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
-- Is there a way to do type checking here...?
var :: String -> Type -> Expr a -> Prog (Expr a)
var name t (E e) = do
  v <- freshName
  let name' = name ++ "_" ++ show v
  add $ Decl (name', t) e
  return $ E $ Var name'

infixl 0 .=
infixl 0 .=.
(.=) :: Expr a -> Expr a -> Prog ()
(E l) .= (E r) = do
  add $ Assign l r

(.=.) :: Expr a -> Prog (Expr a) -> Prog ()
l .=. r = do
  r' <- r
  l .= r'

exit :: Prog ()
exit = add Exit

wait :: EventE  -> Prog ()
wait (E e) = add $ Wait e

spawn :: ArgPacket a => ThreadCode -> a -> Prog ()
spawn thread args = add $ Spawn thread (toDExprList args)

extract :: Prog a -> Prog (a, [Stmt])
extract m = censor (const []) (listen m)

while :: BoolE -> Prog a -> Prog ()
while (E e) body = do
  (_, bodyStmts) <- extract body
  add $ While e bodyStmts

ifE :: BoolE -> Prog a -> Prog b -> Prog ()
ifE (E e) thenBody elseBody = do
  (_, thenStmts) <- extract thenBody
  (_, elseStmts) <- extract elseBody
  add $ If e thenStmts elseStmts
  
ifE' :: BoolE -> Prog a -> Prog ()
ifE' e thenBody = ifE e thenBody (return ())


callName :: ArgPacket a => String -> Prim -> Type -> a -> Prog (Expr b)
callName name fn t args = var name t (E $ Call fn (toDExprList args))

call :: ArgPacket a => Prim -> Type -> a -> Prog (Expr b)
call fn t args = callName "tmp" fn t args


-- Helper to construct a ThreadCode - kind of annoying
declare_thread :: ArgPacket a => [VDecl] -> (a -> Prog ()) -> ThreadCode
declare_thread decls f = (decls, desugar prog)
  where prog = f (makeVars (map fst decls))

--}}}
