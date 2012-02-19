\section{Backend}
This module translates the front-end language (as desugared by
\tt{Language.HeMan.Syntax}) to a back-end language with explicit control flow,
which is straightforwardly translated to C in \tt{Language.HeMan.Codegen}.

\begin{code}
module Language.HeMan.Backend
  (Stmt(..), Tail(..), Block, Thread, backend, printCFG) where

import qualified Language.HeMan.Syntax as Front
  (Stmt(..), DExpr(..), VDecl, Prim(..),
   ArithOp(..), ArithUnop(..), RelnOp(..), Block, Var)
import Control.Monad.RWS
import Control.Monad.Writer
import Control.Monad.State
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe
import Data.Bits
import Data.List
import Debug.Trace as DT
\end{code}

In this representation, a program is a set of labeled statement blocks. We
replace all control flow with conditional and unconditional jumps. Thread code
is no longer inlined at the spawn site.
\begin{code}
type Prgm = [Block]

type Label = Int
type ThreadName = Label
type Block = (Label, ThreadName, [Stmt], Tail)
type Thread = (ThreadName, [Front.VDecl])

data Stmt = Decl Front.VDecl Front.DExpr
          | Assign Front.DExpr Front.DExpr
          | Exp Front.DExpr
          | Spawn Label [(Front.VDecl, Front.DExpr)]
          deriving (Eq, Ord, Show)

data Tail = If Front.DExpr ([Stmt],Tail) ([Stmt],Tail)
          | Goto Label
          | GotoWait Front.DExpr Label
          | Exit
          deriving (Eq, Ord, Show)
\end{code}

The backend first translates the front-end language, then runs a series of
optimization passes, then performs closure conversion on each thread, collecting
variables needed across multiple blocks.
\begin{code}
backend :: Front.Block -> ([Block],[Thread])
backend = collectFrees . (mapFst optimizations) . flattenPrgm
  where optimizations = fuseBlocks . optimize . simplifyJumps
        --optimizations = id

mapFst f (x,y) = (f x,y)

constantExpr expr = case expr of
  Front.NumLit n -> True
  Front.StringLit s -> True
  _ -> False
\end{code}

\subsection{\tt{flattenPrgm}} % {{{
\begin{code}
type Flattener = RWS ThreadName ([Block],[Thread]) Label

flattenPrgm stmts = ((0,0,s,t):blocks,(0,[]):thds) where
  ((s,t),_,(blocks,thds)) = runRWS (flattenStmts stmts [] Exit) 0 1
  
registerEvent event = [Front.Exp $
                       Front.Call (Front.CFn "register_event")
                       [Front.CurThread, event]]

fresh = do
  x <- get
  modify (+1)
  return x
fresh2 = do
  x <- fresh
  y <- fresh
  return (x,y)

newBlock blockL stmts tail = do
  threadL <- ask
  tell ([(blockL,threadL,stmts,tail)],mempty)

newThread threadL vdecls = tell (mempty,[(threadL,vdecls)])

inThread threadL = local (\_ -> threadL)

{- flattenStmts takes a list of Stmts, a list of succeeding Stmts, and the Tail
for that block, and returns a new list of Stmts and a new Tail. -}

flattenStmts :: [Front.Stmt] -> [Stmt] -> Tail -> Flattener ([Stmt],Tail)
flattenStmts [] aStmts tail = return (aStmts,tail)
flattenStmts stmts aStmts tail =
  flattenStmt (last stmts) (init stmts) aStmts tail

{- flattenStmt takes a Stmt, a list of preceding Stmts, a list of succeeding
Stmts, and a Tail, and returns a new list of Stmts and a new Tail.

Variable suffix conventions:
T = tail
L = label
-}

flattenStmt :: Front.Stmt -> [Front.Stmt] -> [Stmt] -> Tail ->
               Flattener ([Stmt],Tail)
flattenStmt stmt bStmts aStmts tail =
  case stmt of
    Front.Decl vdecl expr -> continue $ Decl vdecl expr
    Front.Exp expr -> continue $ Exp expr
    Front.Assign expr expr' -> continue $ Assign expr expr'
    Front.If expr cs as -> 
      do seqL <- fresh
         newBlock seqL aStmts tail
         (conL,altL) <- fresh2
         (bs,tail') <-
           flattenStmts bStmts [] (If expr ([],Goto conL) ([],Goto altL))
         (cs',conT) <- flattenStmts cs [] (Goto seqL)
         (as',altT) <- flattenStmts as [] (Goto seqL)
         newBlock conL cs' conT
         newBlock altL as' altT
         return (bs,tail')
    Front.While expr ss ->
      do (seqL,whileL) <- fresh2
         newBlock seqL aStmts tail
         let whileT = If expr ([],Goto whileL) ([],Goto seqL)
         (bs,tail') <- flattenStmts bStmts [] whileT
         (ws,whileT') <- flattenStmts ss [] whileT
         newBlock whileL ws whileT'
         return (bs,tail')
    Front.Exit ->
      do (bs, tail') <- flattenStmts bStmts [] Exit
         return (bs, tail')
    Front.Wait expr ->
      do seqL <- fresh
         newBlock seqL aStmts tail
         let waitT = GotoWait expr seqL
         (bs,tail') <- flattenStmts bStmts [] waitT
         return (bs,tail')
    Front.Spawn (vs,ss) args ->
      do threadL <- fresh
         let spawn = Spawn threadL (zip vs args)
         (bs,tail') <- flattenStmts bStmts (spawn:aStmts) tail
         (ts,threadT) <- inThread threadL $ flattenStmts ss [] Exit
         inThread threadL $ newBlock threadL ts threadT
         newThread threadL vs
         return (bs,tail')
  where continue s = flattenStmts bStmts (s:aStmts) tail
\end{code}

% }}}
\subsection{\tt{simplifyJumps}} % {{{
This pass reduces the output of \tt{flattenPrgm}, by replacing series of
\tt{Goto}s by their ultimate target, and nesting iterated \tt{If}s. This
eliminates nearly all empty blocks, and the resulting control flow is easier to
analyze in later optimization passes.
\begin{code}
simplifyJumps bs = execWriter $ mapM_ (rewriteTail rs) bs'
  where (_,rs,bs') = runRWS (mapM_ simplify bs) () Map.empty
\end{code}

We process each block with \tt{simplify}, while building a map of new jump
targets (in State) and a list of blocks retained by this pass (in Writer).  When
we encounter an empty block, we do not retain it, and add to the map what to
replace jumps to that block with. Exceptions:
\begin{enumerate}
\item The first block in a thread must be retained, since threads are identified
with their first block.
\item Blocks with self-loops are not removed, to avoid infinite loops when
redirecting jumps.
\end{enumerate}
To simplify redirection, we always recursively \tt{walk} jump targets through
the current redirections before adding them to the map.
\begin{code}
type Simplifier = RWS () [Block] (Map.Map Tail Tail)

simplify :: Block -> Simplifier ()
simplify b = case b of
  (l,t,[],_) | l == t -> tell [b]
  (l,t,[],Goto g) -> doRedirect (Goto l) (Goto g) b
  (l,t,[],If e c a) -> doRedirect (Goto l) (If e c a) b
  (l,t,[],Exit) -> doRedirect (Goto l) Exit b
  _ -> tell [b]

doRedirect t t' b = do
  redirects <- get 
  let tail = walk t' redirects
  if occursCheck t tail then tell [b] else modify (Map.insert t tail)

walk x xs = case (x,Map.lookup x xs) of
  (_,Just (If e ([],t) ([],t'))) ->
    walk (If e ([],walk t xs) ([],walk t' xs)) xs
  (_,Just x') -> walk x' xs
  (If e ([],t) ([],t'),Nothing) -> If e ([],walk t xs) ([],walk t' xs)
  (_,Nothing) -> x

occursCheck l t = case t of
  Goto t' -> l == t
  If _ (_,t') (_,t'') -> occursCheck l t' || occursCheck l t''
  _ -> False
\end{code}

After running \tt{simplify} on each block producing a list \tt{rs} of redirects,
we run each remaining block through \tt{rewriteTail} to redirect all jumps as
appropriate. Because we do not redirect any \tt{GotoWait} jumps, we must also
recover any targets of these that we may have removed.
\begin{code}
rewriteTail rs b = case b of
  -- Recover empty GotoWait targets.
  (l,t,ss,GotoWait _ g) | walk (Goto g) rs /= (Goto g) ->
    tell [b,(g,t,[],walk (Goto g) rs)]
  (l,t,ss,tail) -> tell [(l,t,ss,walk tail rs)]
\end{code}

% }}}
\subsection{\tt{optimize}} % {{{
This pass performs traditional local optimizations with the goal of eliminating
never-taken branches, to reduce code size and enable block fusion in later
passes. In particular, we perform constant propagation and constant folding, as
well as dead code elimination on block tails.

We traverse each block sequentially, carrying around an environment of known
variable values, replacing occurrences of those variables with their values,
and performing constant folding where appropriate.
\begin{code}
optimize bs = map (\b -> evalState (optimizeBlock b) []) bs

type Optimizer = State [(Front.Var,Front.DExpr)]

optimizeBlock (l,t,ss,tail) = do
  ss' <- mapM optimizeStmt ss
  tail' <- optimizeTail [] tail
  return (l,t,ss',tail')

optimizeExpr expr = 
  case expr of
    Front.Var var -> do
      env <- get
      return $ fromMaybe expr (lookup expr env)
    Front.Call prim exprs -> do
      exprs' <- mapM optimizeExpr exprs
      return $ Front.Call prim exprs'
    Front.Arith op e1 e2 -> do
      e1' <- optimizeExpr e1
      e2' <- optimizeExpr e2
      return . constFold $ Front.Arith op e1' e2'
    Front.ArithUnop op e -> do
      e' <- optimizeExpr e
      return . constFold $ Front.ArithUnop op e'
    Front.RelnOp op e1 e2 -> do
      e1' <- optimizeExpr e1
      e2' <- optimizeExpr e2
      return . constFold $ Front.RelnOp op e1' e2'
    _ -> return expr

optimizeStmt stmt = 
  case stmt of
    Decl (var,typ) expr -> do
      expr' <- optimizeExpr expr
      insertVar (Front.Var var) expr'
      return $ Decl (var,typ) expr'
    Assign v@(Front.Var var) expr -> do
      expr' <- optimizeExpr expr
      insertVar v expr'
      return $ Assign v expr'
    Exp expr -> do
      expr' <- optimizeExpr expr
      return $ Exp expr'
    Spawn l args -> do
      exprs' <- mapM optimizeExpr (map snd args)
      return $ Spawn l (zip (map fst args) exprs')
\end{code}

When optimizing tail-position \tt{If}s, we symbolically propagate relational
expressions into the ``true'' branch, to eliminate redundantly nested \tt{If}s.
For simplicity, we do not do the same on the ``false'' branch, which occurs
infrequently in our generated code. Relational expressions are first
``normalized'' to increase the likelihood of a simplification.
\begin{code}
optimizeTail env (If e ([],c) ([],a)) = do
  e' <- optimizeExpr e
  -- Constant true/false.
  if e' == Front.NumLit 0 then optimizeTail env a else do
  if constantExpr e' then optimizeTail env c else do
  case e' of
    -- Redundantly true.
    Front.RelnOp op e1 e2 | impliedR env (op,e1,e2) -> optimizeTail env c
    -- Otherwise, recur on branches.
    _ -> do
      c' <- optimizeTail (assumptions e' ++ env) c
      a' <- optimizeTail env a
      return (If e' ([],c') ([],a'))
  where impliedR env e = normalizeR e `elem` env
optimizeTail env (GotoWait e l) = do
  e' <- optimizeExpr e
  return (GotoWait e' l)
optimizeTail _ t = return t

assumptions expr = case expr of
  Front.Arith Front.And e e' -> concatMap assumptions [e,e']
  Front.RelnOp op e e' -> [normalizeR (op,e,e')]
  Front.ArithUnop Front.Not (Front.RelnOp op e e') ->
    [normalizeR (negateR op,e,e')]
  _ -> []

normalizeR e = case e of
  (Front.Gt,a,b) -> (Front.Lt,b,a)
  (Front.Geq,a,b) -> (Front.Leq,b,a)
  (Front.Eq,a,b) | b > a -> (Front.Eq,b,a)
  (Front.Neq,a,b) | b > a -> (Front.Neq,b,a)
  _ -> e

negateR op = case op of
  Front.Gt -> Front.Leq
  Front.Geq -> Front.Lt
  Front.Eq -> Front.Neq
  Front.Neq -> Front.Eq
  Front.Lt -> Front.Geq
  Front.Leq -> Front.Gt
\end{code}

\tt{insertVar} adds assignments to the current environment, provided they do not
contain effects. Once an assignment to \tt{x} is processed, we then invalidate
any assignments referencing the previous value of \tt{x}.
\begin{code}
insertVar v@(Front.Var var) expr = 
  if containsCall expr then return () else do
  modify (\xs -> (v,expr):xs)
  modify $ filter (\(_,e) -> not (containsVar var e))

containsCall expr = case expr of
  Front.Call _ _ -> True
  Front.Arith _ e e' -> any containsCall [e,e']
  Front.ArithUnop _ e -> containsCall e
  Front.RelnOp _ e e' -> any containsCall [e,e']
  _ -> False

containsVar var expr = case expr of
  Front.Call prim exprs -> any (containsVar var) exprs
  Front.Arith op e e' -> any (containsVar var) [e,e']
  Front.ArithUnop op e -> containsVar var e
  Front.RelnOp op e e' -> any (containsVar var) [e,e']
  Front.Var v -> v == var
  _ -> False
\end{code}

\tt{constFold} performs constant folding whenever possible.
\begin{code}
fromBool True = Front.NumLit 1
fromBool False = Front.NumLit 0

constFold expr@(Front.RelnOp op e e') =
  if any containsCall [e,e'] then expr else case (op,e,e') of
  (Front.Eq,x,y) | x == y -> Front.NumLit 1
  (Front.Eq,Front.NumLit x,Front.NumLit y) | x /= y -> Front.NumLit 0
  (Front.Lt,x,y) | x == y -> Front.NumLit 0
  (Front.Lt,Front.NumLit x,Front.NumLit y) -> fromBool (x < y)
  (Front.Gt,x,y) | x == y -> Front.NumLit 0
  (Front.Gt,Front.NumLit x,Front.NumLit y) -> fromBool (x > y)
  (Front.Leq,x,y) | x == y -> Front.NumLit 1
  (Front.Leq,Front.NumLit x,Front.NumLit y) -> fromBool (x <= y)
  (Front.Geq,x,y) | x == y -> Front.NumLit 1
  (Front.Geq,Front.NumLit x,Front.NumLit y) -> fromBool (x >= y)
  (Front.Neq,x,y) | x == y -> Front.NumLit 0
  (Front.Neq,Front.NumLit x,Front.NumLit y) | x /= y -> Front.NumLit 1
  _ -> Front.RelnOp op e e'

constFold expr@(Front.Arith op e e') =
  if any containsCall [e,e'] then expr else case (op,e,e') of
  (Front.And,Front.NumLit x,y) -> if x == 0 then Front.NumLit 0 else y
  (Front.And,x,Front.NumLit y) -> if y == 0 then Front.NumLit 0 else x
  (Front.Or,Front.NumLit x,y) -> fromBool (x /= 0)
  (Front.Or,x,Front.NumLit y) -> fromBool (y /= 0)
  (Front.Xor,x,y) | x == y -> Front.NumLit 0
  (Front.Xor,Front.NumLit x,Front.NumLit y) -> Front.NumLit (x `xor` y)
  (Front.Plus,Front.NumLit x,Front.NumLit y) -> Front.NumLit (x + y)
  (Front.Times,Front.NumLit 0,_) -> Front.NumLit 0
  (Front.Times,_,Front.NumLit 0) -> Front.NumLit 0
  (Front.Times,Front.NumLit x,Front.NumLit y) -> Front.NumLit (x * y)
  (Front.Minus,x,y) | x == y -> Front.NumLit 0
  (Front.Minus,Front.NumLit x,Front.NumLit y) -> Front.NumLit (x - y)
  (Front.Div,x,Front.NumLit 1) -> x
  (Front.Div,Front.NumLit x,Front.NumLit y) -> Front.NumLit (x `div` y)
  (Front.Mod,x,Front.NumLit 1) -> Front.NumLit 0
  (Front.Mod,Front.NumLit x,Front.NumLit y) -> Front.NumLit (x `mod` y)
  (Front.Rsh,Front.NumLit x,Front.NumLit y) ->
    Front.NumLit (shiftR x (fromInteger y))
  (Front.Rsh,x,Front.NumLit 0) -> x
  (Front.Rsh,Front.NumLit 0,_) -> Front.NumLit 0
  (Front.Lsh,Front.NumLit x,Front.NumLit y) ->
    Front.NumLit (shiftL x (fromInteger y))
  (Front.Lsh,x,Front.NumLit 0) -> x
  (Front.Lsh,Front.NumLit 0,_) -> Front.NumLit 0
  _ -> Front.Arith op e e'

constFold expr@(Front.ArithUnop op e) = 
  if containsCall e then expr else case (op,e) of
  (Front.Negate,Front.NumLit x) -> Front.NumLit (-x)
  (Front.Not,Front.NumLit 0) -> Front.NumLit 1
  (Front.Not,Front.NumLit _) -> Front.NumLit 0
  _ -> Front.ArithUnop op e
\end{code}

% }}}
\subsection{\tt{fuseBlocks}} % {{{
This pass reduces the number of blocks in the output code by fusing existing
ones. Note that we cannot remove \tt{GotoWait} jumps, and due to
\tt{simplifyJumps}, no inlinable empty blocks remain.
\begin{code}
fuseBlocks bs = fuseUnique uniquelyTargeted bs'
  where cfg = buildCFG bs
        -- Exit block fusion.
        exitBlocks = filter isExitBlock bs
        (bs',cfg') = execState (mapM_ fuseExits exitBlocks) (bs,cfg)
        -- Uniquely-targeted block fusion.
        uniquelyTargeted = map (\(x,[y]) -> (y,x))
          (Map.toList $ Map.filter (\ls -> length ls == 1) cfg')
\end{code}

First, we find all \tt{Exit} blocks and inline them where they are called; they
contain very little code, so this allows threads to clean up without yielding
first.
\begin{code}
fuseExits :: Block -> State ([Block],Map.Map Label [Label]) ()
fuseExits b = if getLabel b == getThread b then return () else do
  (bs,cfg) <- get
  case Map.lookup (getLabel b) cfg of
    Nothing -> return ()
    Just xs -> do
    let (froms,bs') = partition (\x -> getLabel x `elem` xs) bs
    let (froms',failed) = partitionMaybe (\f -> fuse f b) froms
    if null failed
      then do
        let cfg' = Map.delete (getLabel b) cfg
        let bs'' = filter (/= b) bs'
        put (froms' ++ bs'',cfg')
      else do
        let cfg' = Map.insert (getLabel b) (map getLabel failed) cfg
        put (froms' ++ failed ++ bs',cfg')

partitionMaybe :: (a -> Maybe b) -> [a] -> ([b],[a])
partitionMaybe f [] = ([],[])
partitionMaybe f (x:xs) =
  let (js,ns) = partitionMaybe f xs in
  case f x of
    Nothing -> (js,x:ns)
    Just x' -> (x':js,ns)

isExitBlock (_,_,_,Exit) = True
isExitBlock _ = False
\end{code}

Next, we find all uniquely-targeted blocks and inline them at the call site, as
long as the target block is not the first block in a thread.
\begin{code}
fuseUnique [] bs = bs
fuseUnique ((from,to):rs) bs =
  let ([toBlock],bs') = partition (\b -> getLabel b == to) bs in
  if getLabel toBlock == getThread toBlock then fuseUnique rs bs else
  let ([fromBlock],bs'') = partition (\b -> getLabel b == from) bs' in
  case fuse fromBlock toBlock of
    Nothing -> fuseUnique rs bs
    Just fromBlock' -> fuseUnique (updateTargets (from,to) rs) (fromBlock':bs'')

updateTargets (from,to) = map (\(f,t) -> if f == to then (from,t) else (f,t))
\end{code}

\tt{buildCFG} generates a map from labels to all blocks targeting them.
\begin{code}
buildCFG bs = execState (mapM_ getTargets bs) Map.empty

getTargets :: Block -> State (Map.Map Label [Label]) ()
getTargets (l,t,ss,tail) = case tail of
  If expr (_,tail) (_,tail') -> do
    getTargets (l,t,ss,tail)
    getTargets (l,t,ss,tail')
  Goto l' -> modify $ Map.insertWith (++) l' [l]
  GotoWait _ l' -> modify $ Map.insertWith (++) l' [l]
  Exit -> return ()
\end{code}

\tt{fuse} takes two blocks and inlines the second at every jump to it in the
first. It returns the new block if fusion succeeds, or \tt{Nothing} otherwise.
We cannot inline at \tt{GotoWait} jumps.
\begin{code}
fuse (fromL,fromT,fromSs,fromTail) (toL,_,toSs,toTail) =
  case runState (fuseIf fromTail toL toSs toTail) False of
    (_,False) -> Nothing
    (newTail,True) -> Just (fromL,fromT,fromSs,newTail)

-- State is True if any replacements occur.
fuseIf :: Tail -> Label -> [Stmt] -> Tail -> State Bool Tail
fuseIf tail l newSs newTail = case tail of
  If e (cs,Goto cl) a | cl == l -> do
    put True
    fuseIf (If e (cs ++ newSs,newTail) a) l newSs newTail
  If e c (as,Goto al) | al == l -> do
    put True
    fuseIf (If e c (as ++ newSs,newTail)) l newSs newTail
  If e (cs,c) (as,a) -> do
    c' <- fuseIf c l newSs newTail
    a' <- fuseIf a l newSs newTail
    return $ If e (cs,c') (as,a')
  _ -> return tail

getLabel (l,t,ss,tail) = l
getThread (l,t,ss,tail) = t
\end{code}

% }}}
\subsection{\tt{collectFrees}} % {{{
\begin{code}

{- collectFrees determines, for each thread, the variables which must be
retained across multiple blocks, and adds those to the list of thread-local
variables. 

*** This pass assumes all declared variables have a single type. ***
We enforce this constraint in the front-end. -}

collectFrees (blocks,threads) = (blocks,map (collectThread blocks) threads)

collectThread :: [Block] -> Thread -> Thread
collectThread blocks (thread,vdecls) = (thread,vdecls')
  where threadBlocks = filter (\(_,t,_,_) -> t == thread) blocks
        (decls,frees) = mconcat $ map collectBlock threadBlocks
        frees' = frees Set.\\ Set.fromList (map fst vdecls)
        getDecl free = (free,fromJust $ lookup free (Set.toList decls))
        -- fromJust exception <=> encountered a truly free variable
        vdecls' = vdecls ++ map getDecl (Set.toList frees')

type Collector = Writer (Set.Set Front.VDecl,Set.Set Front.Var)

collectBlock (label,thread,stmts,tail) = (decls,frees')
  where collect = do mapM_ collectStmt stmts
                     collectTail tail
        ((),(decls,frees)) = runWriter collect
        frees' = frees Set.\\ Set.map fst decls

collectStmt s = case s of
  Decl vdecl expr ->
    do tell (Set.singleton vdecl,mempty)
       collectExpr expr
  Assign expr expr' ->
    do collectExpr expr
       collectExpr expr'
  Spawn _ exprs -> mapM_ (collectExpr . snd) exprs
  Exp expr -> collectExpr expr

collectExpr e = case e of
  Front.Call _ exprs -> mapM_ collectExpr exprs
  Front.Arith _ expr expr' ->
    do collectExpr expr
       collectExpr expr'
  Front.ArithUnop _ expr -> collectExpr expr
  Front.RelnOp _ expr expr' ->
    do collectExpr expr
       collectExpr expr'
  Front.Var var -> tell (mempty,Set.singleton var)
  Front.Constant _ -> return ()
  Front.NumLit _ -> return ()
  Front.StringLit _ -> return ()
  Front.CurThread -> return ()

collectTail t = case t of
  If expr (ss,tail) (ss',tail') ->
    do collectExpr expr
       mapM collectStmt ss
       mapM collectStmt ss'
       collectTail tail
       collectTail tail'
  Goto _ -> return ()
  GotoWait _ _ -> return ()
  Exit -> return ()
\end{code}

% }}}
\subsection{CFG printer} % {{{
This function prints a DOT representation of the emitted code's control flow
graph.

\begin{code}
printCFG (bs,_) = do
  let cfg = Map.toList (buildCFG bs)
  putStrLn "digraph G {"
  mapM_ (\(to,froms) ->
    mapM_ (\from -> putStr (show from ++ " -> " ++ show to ++ "\n"))
    froms) cfg
  putStrLn "}"
\end{code}

% }}}
\subsection{Tests} % {{{
\begin{code}
{-

testFlat = backend [Front.Decl ("x",Front.Int) (Front.Var "y")]

testIf = backend [Front.Exp (Front.NumLit 5),
                  Front.If (Front.NumLit 6)
                          [Front.Exp $ Front.Call (Front.CFn "cfun") []]
                          [Front.Exp $ Front.Call (Front.CFn "afun") []],
                  Front.Exp (Front.NumLit 10)]

testWhile = backend [Front.Exp (Front.NumLit 5),
                     Front.While (Front.NumLit 6)
                                [Front.Exp $ Front.Call (Front.CFn "rep") [],
                                 Front.Exp $ Front.Call (Front.CFn "rep2") []],
                     Front.Exp (Front.NumLit 10)]

testExit = backend [Front.Exp (Front.NumLit 5),
                    Front.Exit,
                    Front.Exp (Front.NumLit 6)]

testIfExit = backend [Front.Exp (Front.NumLit 5),
                      Front.If (Front.NumLit 6)
                              [Front.Exit]
                              [Front.Exp $ Front.Call (Front.CFn "afun") []],
                      Front.Exp (Front.NumLit 10)]

testWait = backend [Front.Exp (Front.NumLit 10),
                    Front.Wait (Front.Call (Front.CFn "wfun") []),
                    Front.Exp (Front.NumLit 11)]

testSpawn = backend [Front.Exp (Front.NumLit 5),
                     Front.Spawn ([("x",Front.Int),("y",Front.Bool)],
                       [Front.Exp (Front.NumLit 10),
                        Front.Exp (Front.NumLit 15)])
                       [Front.Var "arg1",Front.Var "arg2"],
                     Front.Exp (Front.NumLit 20)]

testSpawnIf = backend [Front.Exp (Front.NumLit 5),
                       Front.Spawn ([("x",Front.Int),("y",Front.Bool)],
                         [Front.Exp (Front.NumLit 0),
                          Front.If (Front.NumLit 6)
                                  [Front.Exit]
                                  [Front.Exp $ Front.NumLit 100],
                          Front.Exp (Front.NumLit 1)])
                         [Front.Var "arg1",Front.Var "arg2"],
                       Front.If (Front.NumLit 7)
                               [Front.Exp $ Front.Call (Front.CFn "cfun") []]
                               [Front.Exp $ Front.Call (Front.CFn "afun") []],
                       Front.Exp (Front.NumLit 8)]

testOptimize = backend [Front.If (Front.NumLit 6)
                                [Front.Exit]
                                [Front.If (Front.NumLit 7)
                                         [Front.Exp (Front.NumLit 8)]
                                         [Front.Exp (Front.NumLit 9)]],
                        Front.Exp (Front.NumLit 10)]

testCollect = backend [Front.Decl ("y",Front.Int) (Front.NumLit 2),
                       Front.Decl ("x",Front.Int) (Front.NumLit 2),
                       Front.Exp $
                         Front.Arith Front.Plus (Front.Var "y") (Front.Var "x"),
                       Front.If (Front.Var "y")
                               [Front.Exit]
                               [Front.Exp $ Front.Var "y"]]
-}
\end{code}
% }}}
