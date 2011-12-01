module Codegen where

import Lang hiding(Stmt,Decl,While,If,Spawn,Assign,Exp,Wait,Exit,Block)
import Back
import CLib
import Data.List
import Data.Maybe
import qualified Text.PrettyPrint.HughesPJ

translateType Int = cInt
translateType Bool = cBool

hasVar vars x = isJust $ find (\(y, _) -> x == y) vars

blockName id = "block" ++ show id
threadName id = "thread" ++ show id

translateExpr vars expr =
  case expr of
    Var x -> if hasVar vars x then cArrow (cVar "thread_priv") x else cVar x
    Call (CFn f) args -> cCall (cVar f) (map trans args)
    NumLit n -> cIntConst n
    -- TODO all of it, I think
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
    where transE = translateExpr vars

translateTail vars tail =
  case tail of
    Exit -> [return 0]
    Goto target -> [jump target, return 1]
    GotoWait target -> [jump target, return 0]
    If e t1 t2 -> [Left $
      cIfThen (translateExpr vars e) (cCompound (translateTail vars t1))
      (Just (cCompound (translateTail vars t2)))]

    where return n = Left $ cReturn (cIntConst n)
          jump target = Left $ cExpr (cAssign
                                      (cArrow (cVar "thread") "cont")
                                      (cVar (blockName target)))

translateThread (name, vars) =
  [cStructDecl sname (map declVar vars),
   cTypedef (cStruct sname) [cPtr] (sname ++ "_p")]
  where declVar (x, t) = cDecl [translateType t] [] x Nothing
        sname = threadName name

translateBlock threads (id, thread, stmts, tail) =
  cFunction (blockName id) [([cType "thread"],[cPtr],"thread")] [cInt]
  (cCompound (
      [Right $ cDecl [threadType] [] "thread_priv" 
       (Just (cCast threadType (cVar "thread")))] ++
      map (translateStmt vars) stmts ++
      translateTail vars tail))
    where Just vars = lookup thread threads
          threadType = cType (threadName thread ++ "_p")

translateProgram :: ([Block], [Thread]) -> Text.PrettyPrint.HughesPJ.Doc
translateProgram (blocks, threads) =
  pretty $ cFile (concatMap translateThread threads ++ 
                  map (translateBlock threads) blocks)
