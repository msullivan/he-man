module Codegen where

import Lang hiding(Stmt,Decl,While,If,Spawn,Assign,Exp,Wait,Exit,Block)
import Back
import CLib
import Data.List
import Data.Maybe

codegen :: ([Block], [Thread]) -> CFile
codegen (blocks, threads) =
  cFile (map translateThread threads ++ 
                  map (translateBlock threads) blocks)

--{{{ Helpers

hasVar vars x = isJust $ find (\(y, _) -> x == y) vars

blockName id = "block" ++ show id
threadName id = "thread" ++ show id ++ "_t"

--}}}
--{{{ Grammar

translateThread (name, vars) =
  cTypedef (cStructType sname (map declVar vars)) [] tydefName
  where declVar (x, t) = cDecl [translateType t] [] x Nothing
        sname = threadName name
        -- This is a frumious hack to sneak in a toplevel invocation of
        -- DECLARE_THREAD
        tydefName = sname ++ ";\nDECLARE_THREAD(" ++ sname ++ ")"

translateBlock threads (id, thread, stmts, tail) =
  cFunction (blockName id) [([cType "thread_t"],[cPtr],"thread")] [cInt]
  (cCompound (
      [Right $ cDecl [threadType] [cPtr] "priv" 
       (Just (cCast threadType [cPtr] (cVar "thread")))] ++
      concatMap (translateStmt vars) stmts ++
      translateTail vars tail))
    where Just vars = lookup thread threads
          threadType = cType (threadName thread)

translateExpr vars expr =
  case expr of
    Var x -> if hasVar vars x then cArrow (cVar "priv") x else cVar x
    Call (CFn f) args -> cCall (cVar f) (map trans args)
    Arith op e e' -> (translateArith op) (trans e) (trans e')
    ArithUnop op e -> (translateArithUnop op) (trans e)
    RelnOp op e e' -> (translateRelnOp op) (trans e) (trans e')
    Constant s -> cVar s
    NumLit n -> cIntConst n
    StringLit s -> cStrConst s
    CurThread -> cVar "thread"
  where trans = translateExpr vars

translateStmt vars stmt =
  case stmt of
    Decl (x, t) e ->
      if hasVar vars x then
        [Left $ cExpr (cAssign (cVar x) (transE e))]
      else
        [Right $ cDecl [translateType t] [] x (Just $ transE e)]
    Assign e1 e2 -> [Left $ cExpr (cAssign (transE e1) (transE e2))]
    Exp e -> [Left $ cExpr $ transE e]
    Spawn thd args -> makeT:setupTVars ++ setupTRun where
      thread = threadName thd
      makeT = Right $ cDecl [cType thread] [cPtr] "new_thread" $ 
        Just $ cCall (cVar $ "mk_" ++ thread) []
      setupTVars = map (\((var,_),arg) ->
        Left $ cExpr $ cAssign (cArrow (cVar "new_thread") var) (transE arg))
        args
      setupTRun = [Left $ cExpr $ cAssign
          (cDot (cArrow (cVar "new_thread") "thread") "cont")
          (cVar $ blockName thd),
        Left $ cExpr $ cCall (cVar "make_runnable") $
          [cAddr (cArrow (cVar "new_thread") "thread")]]
    where transE = translateExpr vars

translateTail vars tail =
  case tail of
    Exit -> [cleanup, return 0]
    Goto target -> [jump target, return 1]
    GotoWait target -> [jump target, return 0]
    If e t1 t2 -> [Left $
      cIfThen (translateExpr vars e) (cCompound (translateTail vars t1))
      (Just (cCompound (translateTail vars t2)))]
    where return n = Left $ cReturn (cIntConst n)
          jump target = Left $ cExpr (cAssign
                                      (cArrow (cVar "thread") "cont")
                                      (cVar (blockName target)))
          cleanup = Left $ cExpr (cCall (cVar "free_thread") [cVar "thread"])

translateType t =
  case t of
    Int -> cInt
    Bool -> cBool
    FD -> cInt
    Event -> cType "event_t"
    -- TODO
    String -> cType "string"
    Buffer -> cType "Buffer"

--}}}
--{{{ Operators

translateArith op =
  case op of
    Plus -> cAdd
    Times -> cMul
    Minus -> cSub
    Div -> cDiv
    Mod -> cMod
    And -> cLand
    Or -> cLor
    Xor -> cXor -- XXX bitwise!
    Rsh -> cShr
    Lsh -> cShl

translateArithUnop op =
  case op of
    Negate -> cNeg
    Not -> cLnot

translateRelnOp op =
  case op of
    Eq -> cEq
    Less -> cLt
    Greater -> cGt

--}}}
