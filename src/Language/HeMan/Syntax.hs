{-#LANGUAGE GADTs, EmptyDataDecls, FlexibleInstances, TypeSynonymInstances,
   MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances #-}

module Language.HeMan.Syntax where

import Control.Monad.RWS

--{{{ Front-end language

type Block = [Stmt]
type Var = String
type VDecl = (Var, IType)
type TVDecl a = (Var, Type a)
type IThreadCode = ([VDecl], Block)
data ThreadCode a = Thr IThreadCode

-- Some dummy types for the phantom types
data FD
data Buffer
data Event
data Thread
data Data
data Msg
data Channel

data Stmt = Decl VDecl IExpr
          | While IExpr Block
          | If IExpr Block Block
          | Spawn IThreadCode [IExpr]
          | Assign IExpr IExpr
          | Exp IExpr
          | Wait IExpr
          | Exit
          deriving (Eq, Ord, Show)

data IType = IInt | IBool | IFD | IBuffer | IEvent | IData | IMsg | IChannel
          deriving (Eq, Ord, Show)

data IExpr = Call Prim [IExpr]
          | Arith ArithOp IExpr IExpr
          | ArithUnop ArithUnop IExpr
          | RelnOp RelnOp IExpr IExpr
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
data RelnOp = Eq | Lt | Gt | Leq | Geq | Neq
             deriving (Eq, Ord, Show)
data Prim = CFn String
          deriving (Eq, Ord, Show)

-- Now for some phantom type bullshit
-- I think I should win an award for "worst reason to choose to use
-- GADTs": I want types to stay constructors so they get syntax
-- highlighted differently.
data Type a where
  Int :: Type Int
  Bool :: Type Bool
  FD :: Type FD
  Buffer :: Type Buffer
  Event :: Type Event
  Data :: Type Data
  Msg :: Type Msg
  Channel :: Type Channel

toIType :: Type a -> IType
toIType Int = IInt
toIType Bool = IBool
toIType FD = IFD
toIType Buffer = IBuffer
toIType Event = IEvent
toIType Data = IData
toIType Msg = IMsg
toIType Channel = IChannel

newtype Expr a = E IExpr
               deriving (Eq, Ord, Show)

type IntE = Expr Int
type FdE = Expr FD
type BoolE = Expr Bool
type BufferE = Expr Buffer
type EventE = Expr Event
type ChannelE = Expr Channel
type DataE = Expr Data

-- Lift functions over IExprs to ones over Exprs
typ1 :: (IExpr -> IExpr) -> (Expr a -> Expr b)
typ1 f (E e1) = E (f e1)
typ2 :: (IExpr -> IExpr -> IExpr) -> (Expr a -> Expr b -> Expr c)
typ2 f (E e1) (E e2) = E (f e1 e2)

unsafeExprCoerce :: Expr a -> Expr b
unsafeExprCoerce (E x) = E x

-- TODO: Document this trick.
class ArgPacket exps tys | exps -> tys, tys -> exps where
  toIExprList :: exps -> [IExpr]
  makeVars :: [String] -> exps
  toITypeList :: tys -> [IType]

-- This is some ugly shit.
-- Nullary and unary arg packets
instance ArgPacket () () where
  toIExprList () = []
  makeVars [] = ()
  toITypeList () = []
instance ArgPacket (Expr a) (Type a) where
  toIExprList (E e) = [e]
  makeVars [x] = E $ Var x
  toITypeList t = [toIType t]

-- Generic binary packets; this makes us need UndecidableInstances
instance (ArgPacket b c) => ArgPacket (Expr a, b) (Type a, c) where
  toIExprList (E e, es) = e : toIExprList es
  makeVars (x : xs) = (E $ Var x, makeVars xs)
  toITypeList (t, ts) = toIType t : toITypeList ts

{-
instance ArgPacket (Expr a, Expr b) (Type a, Type b) where
  toIExprList (E e1, E e2) = [e1, e2]
  makeVars [x1, x2] = (E $ Var x1, E $ Var x2)
  toITypeList (t1, t2) = [toIType t1, toIType t2]
-}

-- Hacky 3,4,5-ary packets
instance ArgPacket (Expr a, Expr b, Expr c) (Type a, Type b, Type c) where
  toIExprList (E e1, E e2, E e3) = [e1, e2, e3]
  makeVars [x1, x2, x3] = (E $ Var x1, E $ Var x2, E $ Var x3)
  toITypeList (t1, t2, t3) = [toIType t1, toIType t2, toIType t3]
instance ArgPacket
         (Expr a, Expr b, Expr c, Expr d)
         (Type a, Type b, Type c, Type d) where
  toIExprList (E e1, E e2, E e3, E e4) = [e1, e2, e3, e4]
  makeVars [x1, x2, x3, x4] = (E $ Var x1, E $ Var x2, E $ Var x3, E $ Var x4)
  toITypeList (t1, t2, t3, t4) =
    [toIType t1, toIType t2, toIType t3, toIType t4]
instance ArgPacket
         (Expr a, Expr b, Expr c, Expr d, Expr e)
         (Type a, Type b, Type c, Type d, Type e) where
  toIExprList (E e1, E e2, E e3, E e4, E e5) = [e1, e2, e3, e4, e5]
  makeVars [x1, x2, x3, x4, x5] = 
    (E $ Var x1, E $ Var x2, E $ Var x3, E $ Var x4, E $ Var x5)
  toITypeList (t1, t2, t3, t4, t5) =
    [toIType t1, toIType t2, toIType t3, toIType t4, toIType t5]

infixr 2 .||
infixr 3 .&&
infix  4 .==, .<, .>, ./=, .<=, .>=
infixl 6 +*

(.<) :: IntE -> IntE -> BoolE
(.<) = typ2 $ RelnOp Lt
(.>) :: IntE -> IntE -> BoolE
(.>) = typ2 $ RelnOp Gt
(.<=) :: IntE -> IntE -> BoolE
(.<=) = typ2 $ RelnOp Leq
(.>=) :: IntE -> IntE -> BoolE
(.>=) = typ2 $ RelnOp Geq
(.==) :: Expr a -> Expr a -> BoolE
(.==) = typ2 $ RelnOp Eq
(./=) :: Expr a -> Expr a -> BoolE
(./=) = typ2 $ RelnOp Neq

(.&&) :: BoolE -> BoolE -> BoolE
(.&&) = typ2 $ Arith And
(.||) :: BoolE -> BoolE -> BoolE
(.||) = typ2 $ Arith Or
-- Pointer arithmetic
(+*) :: BufferE -> IntE -> BufferE
(+*) = typ2 $ Arith Plus
notE :: BoolE -> BoolE
notE = typ1 $ ArithUnop Not

-- Class for types we can do a failure test on
class ExprFailable a where
  isFailure :: Expr a -> BoolE
instance ExprFailable Int where
  isFailure n = n .< 0
instance ExprFailable FD where
  isFailure (E fd) = (E fd) .< 0

instance Num IntE where
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
instance Num BoolE where
  fromInteger = E . NumLit
  (+) = error "trying to use Num things on BoolE: don't do that"
  (-) = error "trying to use Num things on BoolE: don't do that"
  (*) = error "trying to use Num things on BoolE: don't do that"
  negate = error "trying to use Num things on BoolE: don't do that"
  abs = error "trying to use Num things on BoolE: don't do that"
  signum = error "trying to use Num things on BoolE: don't do that"

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
var :: String -> Type a -> Expr a -> Prog (Expr a)
var name t (E e) = do
  v <- freshName
  let name' = name ++ "_" ++ show v
  add $ Decl (name', toIType t) e
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

spawn :: ArgPacket exps tys =>
         ThreadCode exps -> exps ->
         Prog ()
spawn (Thr thread) args = add $ Spawn thread (toIExprList args)

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

call' :: ArgPacket a b => Prim -> a -> Prog ()
call' fn args = add $ Exp (Call fn (toIExprList args))

callName :: ArgPacket a b => String -> Prim -> Type c -> a -> Prog (Expr c)
callName name fn t args = var name t (E $ Call fn (toIExprList args))

call :: ArgPacket exps tys =>
        Prim -> Type c -> exps ->
        Prog (Expr c)
call fn t args = callName "tmp" fn t args

callE :: ArgPacket a b => Prim -> Type c -> a -> (Expr c)
callE fn t args = E $ Call fn (toIExprList args)


-- Helper to construct a ThreadCode
declare_thread :: (ArgPacket exps tys) =>
                  tys -> (exps -> Prog ()) ->
                  ThreadCode exps
declare_thread decls f = Thr (decls', desugar prog)
  where prog = f (makeVars (map fst decls'))
        arg_names = map (\n -> "arg_" ++ show n) [0..]
        decls' = zip arg_names (toITypeList decls)
--}}}
