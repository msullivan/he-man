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

translateExpr vars expr =
  case expr of
    Var x -> if hasVar vars x then cArrow (cVar "thread") x else cVar x
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
    Exit -> [Left (cReturn (cIntConst 0))]

translateBlock threads (id, thread, stmts, tail) =
  cFunction ("block" ++ show id) [] [cInt]
  (cCompound (map (translateStmt vars) stmts 
              ++ translateTail vars tail))
    where Just vars = lookup thread threads

translateProgram :: ([Block], [Thread]) -> Text.PrettyPrint.HughesPJ.Doc
translateProgram (blocks, threads) =
  pretty $ cFile (map (translateBlock threads) blocks)
