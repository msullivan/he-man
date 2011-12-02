module Codegen where

import Lang hiding(Stmt,Decl,While,If,Spawn,Assign,Exp,Wait,Exit,Block)
import Back
import CLib
import Data.List
import Data.Maybe

codegen :: ([Block], [Thread]) -> CFile
codegen (blocks, threads) =
  cFile (concatMap translateThread threads ++ 
                  map (translateBlock threads) blocks)

--{{{ Helpers

hasVar vars x = isJust $ find (\(y, _) -> x == y) vars

blockName id = "block" ++ show id
threadName id = "thread" ++ show id

--}}}
--{{{ Grammar

translateExpr vars expr =
  case expr of
    Var x -> if hasVar vars x then cArrow (cVar "thread_priv") x else cVar x
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
        Left $ cExpr (cAssign (cVar x) (transE e))
      else
        Right $ cDecl [translateType t] [] x (Just $ transE e)
    Assign e1 e2 -> Left $ cExpr (cAssign (transE e1) (transE e2))
    Exp e -> Left $ cExpr $ transE e
    -- TODO Spawn
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

translateThread (name, vars) =
  [cStructDecl sname (map declVar vars),
   cTypedef (cStruct sname) [cPtr] tydef_name]
  where declVar (x, t) = cDecl [translateType t] [] x Nothing
        sname = threadName name
        -- This is a frumious hack to sneak in a toplevel invocation of
        -- DECLARE_THREAD
        tydef_name = sname ++ "_p; DECLARE_THREAD(" ++ sname ++ ")"

translateBlock threads (id, thread, stmts, tail) =
  cFunction (blockName id) [([cType "thread"],[cPtr],"thread")] [cInt]
  (cCompound (
      [Right $ cDecl [threadType] [] "thread_priv" 
       (Just (cCast threadType (cVar "thread")))] ++
      map (translateStmt vars) stmts ++
      translateTail vars tail))
    where Just vars = lookup thread threads
          threadType = cType (threadName thread ++ "_p")

translateType t =
  case t of
    Int -> cInt
    Bool -> cBool
    -- TODO
    String -> cType "string"
    FD -> cType "FD"
    Buffer -> cType "Buffer"
    Event -> cType "Event"

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
