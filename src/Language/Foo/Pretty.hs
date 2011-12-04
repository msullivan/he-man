{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Language.Foo.Pretty (pretty) where

import qualified Language.Foo.Syntax as Lang
import Language.Foo.Backend
import Text.PrettyPrint
import Data.List
import qualified Language.C

commaSeparated docs = hcat $ intersperse comma docs
block header body = header <> lbrace $$ nest 2 body $$ rbrace

--{{{ Class declaration

class PP a where
  pretty :: a -> Doc

instance PP a => PP [a] where
  pretty xs = vcat $ map pretty xs

instance PP (Language.C.CTranslationUnit Language.C.NodeInfo) where
  pretty = Language.C.pretty

--}}}
--{{{ Front-end grammar

instance PP Lang.VDecl where
  pretty (var,typ) = text (show typ) <+> text var

instance PP Lang.Stmt where
  pretty s = case s of
    Lang.Decl (var,typ) expr ->
      text (show typ) <+> text var <+> equals <+> pretty expr
    Lang.While expr stmts ->
      block (text "while" <+> parens (pretty expr) <> space)
            (pretty stmts)
    Lang.If expr stmts stmts' ->
      text "if" <+> parens (pretty expr) <+> lbrace
        $$ nest 2 (pretty stmts) $$ text "} else {"
        $$ nest 2 (pretty stmts') $$ rbrace
    Lang.Assign expr expr' ->
      pretty expr <+> colon <> equals <+> pretty expr'
    Lang.Spawn (vdecls,stmts) exprs ->
      block (text "spawn" <> parens (commaSeparated $ args))
            (pretty stmts) where
      args = zipWith (\x y -> pretty x <+> equals <+> pretty y) vdecls exprs
    Lang.Exp expr -> pretty expr
    Lang.Wait expr -> text "wait" <> parens (pretty expr)
    Lang.Exit -> text "exit"

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
    Lang.StringLit s -> doubleQuotes (text s)
    Lang.Var v -> text v
    Lang.CurThread -> text "curThread"

--}}}
--{{{ Back-end grammar

instance PP Block where
  pretty (label,thread,stmts,tail) =
    block (text "Thread" <+> int thread <> comma <+>
           text "Block" <+> int label <> space)
          (pretty stmts $$ pretty tail)

instance PP Stmt where
  pretty s = case s of
    Decl vdecl expr ->
      pretty vdecl <+> equals <+> pretty expr
    Assign expr expr' ->
      pretty expr <+> colon <> equals <+> pretty expr'
    Spawn label exprs ->
      text "spawn" <+> text (show label) <>
        (parens $ commaSeparated $ map (pretty . snd) exprs)
    Exp expr -> pretty expr

instance PP Tail where
  pretty t = case t of
    If expr tail tail' ->
      text "if" <+> parens (pretty expr)
        $$ nest 2 (pretty tail)
        $$ nest 2 (pretty tail')
    GotoWait label -> text "gotoWait" <+> int label
    Goto label -> text "goto" <+> int label
    Exit -> text "exit"

--}}}
--{{{ Operators

instance PP Lang.ArithOp where
  pretty o = case o of
    Lang.Plus -> text "+"
    Lang.Times -> text "*"
    Lang.Minus -> text "-"
    Lang.Div -> text "/"
    Lang.Mod -> text "%"
    Lang.And -> text "&&"
    Lang.Or -> text "||"
    Lang.Xor -> text "^"
    Lang.Rsh -> text ">>"
    Lang.Lsh -> text "<<"

instance PP Lang.ArithUnop where
  pretty o = case o of
    Lang.Negate -> text "-"
    Lang.Not -> text "!"

instance PP Lang.RelnOp where
  pretty o = case o of
    Lang.Eq -> text "=="
    Lang.Less -> text "<"
    Lang.Greater -> text ">"

--}}}
