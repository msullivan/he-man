{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module BackPretty where

import qualified Lang
import Back
import Text.PrettyPrint
import Data.List

class PP a where
  pretty :: a -> Doc

instance PP a => PP [a] where
  pretty xs = vcat $ map pretty xs

commaSeparated docs = hcat $ intersperse comma docs
block header body = header <> lbrace $$ nest 2 body $$ rbrace

instance PP Block where
  pretty (label,thread,stmts,tail) =
    block (text "Thread" <+> int thread <> comma <+>
           text "Block" <+> int label <> space)
          (vcat (map pretty stmts) $$ pretty tail)

instance PP Stmt where
  pretty s = case s of
    Decl (var,typ) expr ->
      text (show typ) <+> text var <+> equals <+> pretty expr
    Assign expr expr' ->
      pretty expr <+> colon <> equals <+> pretty expr'
    Spawn label exprs ->
      text "Spawn" <+> text (show label) <>
        (parens $ commaSeparated $ map pretty exprs)
    Exp expr -> pretty expr

instance PP Lang.Expr where
  pretty e = case e of
    Lang.Call (Lang.CFn prim) exprs ->
      text prim <> (parens $ commaSeparated (map pretty exprs))
    Lang.Arith op expr expr' ->
      pretty expr <+> pretty op <+> pretty expr'
    Lang.ArithUnop op expr ->
      pretty op <+> pretty expr
    Lang.RelnOp op expr expr' ->
      pretty expr <+> pretty op <+> pretty expr'
    Lang.Constant c -> text c
    Lang.NumLit i -> integer i
    Lang.StringLit s -> text s
    Lang.Var v -> text v

instance PP Tail where
  pretty t = case t of
    If expr tail tail' ->
      text "If" <+> parens (pretty expr)
        $$ nest 2 (pretty tail)
        $$ nest 2 (pretty tail')
    GotoWait label -> text "GotoWait" <+> int label
    Goto label -> text "Goto" <+> int label
    Exit -> text "Exit"

instance PP Lang.ArithOp where
  pretty o = case o of
    Lang.Plus -> text "+"
    Lang.Times -> text "*"
    Lang.Minus -> text "-"
    Lang.Div -> text "/"
    Lang.Mod -> text "%"
    Lang.And -> text "&"
    Lang.Or -> text "|"
    Lang.Xor -> text "^"
    Lang.Rsh -> text ">>"
    Lang.Lsh -> text "<<"

instance PP Lang.ArithUnop where
  pretty o = case o of
    Lang.Negate -> text "-"
    Lang.Not -> text "!"

instance PP Lang.RelnOp where
  pretty o = case o of
    Lang.Eq -> text "="
    Lang.Less -> text "<"
    Lang.Greater -> text ">"

