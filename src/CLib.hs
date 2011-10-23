-- Wrappers for Language.C

module CLib where

import Language.C hiding (cChar)
import qualified Language.C
--import Language.C.Data.Ident

-- Types {{{

cVoid = CTypeSpec $ CVoidType undefNode
cChar = CTypeSpec $ CCharType undefNode
cShort = CTypeSpec $ CShortType undefNode
cInt = CTypeSpec $ CIntType undefNode
cLong = CTypeSpec $ CLongType undefNode
cSigned = CTypeSpec $ CSignedType undefNode
cUnsig = CTypeSpec $ CUnsigType undefNode
cBool = CTypeSpec $ CBoolType undefNode
-- TODO structs and unions
-- TODO enums
--cTypedef ident = CTypeDef ident undefNode

-- }}}
-- Identifiers {{{

-- TODO better to use namesStartingFrom :: Int -> [Name]
noname = head $ newNameSupply

ident name = mkIdent nopos name noname

-- }}}
-- Declarations {{{

cDeclr ident declrs = CDeclr (Just ident) declrs Nothing [] undefNode
cDeclrFun params = CFunDeclr (Right (params,False)) [] undefNode

cFunction ident params return body = CFDefExt $ CFunDef
  [return] (cDeclr ident [cDeclrFun params]) [] body undefNode

-- }}}
-- Expressions {{{

-- Assignment operators
cAssignOp assop lhs rhs = CAssign assop lhs rhs undefNode 
cAssign = cAssignOp CAssignOp
cMulEq = cAssignOp CMulAssOp
cDivEq = cAssignOp CDivAssOp
cModEq = cAssignOp CRmdAssOp
cAddEq = cAssignOp CAddAssOp
cSubEq = cAssignOp CSubAssOp
cShlEq = cAssignOp CShlAssOp
cShrEq = cAssignOp CShrAssOp
cAndEq = cAssignOp CAndAssOp
cXorEq = cAssignOp CXorAssOp
cOrEq = cAssignOp COrAssOp

-- TODO CCond

-- Binary operators
cBinaryOp binop expr1 expr2 = CBinary binop expr1 expr2 undefNode
cMul = cBinaryOp CMulOp
cDiv = cBinaryOp CDivOp
cMod = cBinaryOp CRmdOp
cAdd = cBinaryOp CAddOp
cSub = cBinaryOp CSubOp
cShl = cBinaryOp CShlOp
cShr = cBinaryOp CShrOp
cLt = cBinaryOp CLeOp
cGt = cBinaryOp CGrOp
cLeq = cBinaryOp CLeqOp
cGeq = cBinaryOp CGeqOp
cEq = cBinaryOp CEqOp
cNeq = cBinaryOp CNeqOp
cAnd = cBinaryOp CAndOp
cXor = cBinaryOp CXorOp
cOr = cBinaryOp COrOp
cLand = cBinaryOp CLndOp
cLor = cBinaryOp CLorOp

-- TODO CCast

-- Unary operators
cUnaryOp unop expr = CUnary unop expr undefNode
cPreInc = cUnaryOp CPreIncOp
cPreDec = cUnaryOp CPreDecOp
cPostInc = cUnaryOp CPostIncOp
cPostDec = cUnaryOp CPostDecOp
cAddr = cUnaryOp CAdrOp
cDeref = cUnaryOp CIndOp
cNot = cUnaryOp CCompOp
cLnot = cUnaryOp CNegOp

cSizeofExpr expr = CSizeofExpr expr undefNode
-- TODO sizeoftype

cIndex arr ind = CIndex arr ind undefNode
cCall fun args = CCall fun args undefNode
cDot expr ident = CMember expr ident False undefNode
cArrow expr ident = CMember expr ident True undefNode
cVar ident = CVar ident undefNode

cIntConst x = CConst $ CIntConst (cInteger x) undefNode
cCharConst x = CConst $ CCharConst (Language.C.cChar x) undefNode
cStrConst x = CConst $ CStrConst (cString x) undefNode

-- }}}
-- Statements {{{

cCase expr stmt = CCase expr stmt undefNode
cDefault stmt = CDefault stmt undefNode
cExpr expr = CExpr (Just expr) undefNode
cCompound stmts = CCompound [] stmts undefNode
-- TODO avoid wrapping block items with CBlockItem constructors
-- CIf
cSwitch expr stmt = CSwitch expr stmt undefNode
-- TODO work on this
cReturn expr = CReturn (Just expr) undefNode
cReturnVoid = CReturn Nothing undefNode

-- }}}

-- Temp

{-
-}

cTopLevel ns = CTranslUnit ns undefNode

-- Tests {{{

test1 = print $ pretty $ cTopLevel
  [CDeclExt (CDecl [cInt]
                   [(Just (cDeclr (ident "x") []),Nothing,Nothing)]
                   undefNode)]

test2 = print $ pretty $ cTopLevel
  [cFunction (ident "main") [] cInt
   (cCompound [CBlockStmt (cReturn (cIntConst 0))])]

test3 = print $ pretty $ cTopLevel
  [cFunction (ident "main") [] cInt
   (cCompound [CBlockStmt (cExpr (cAssign (cVar (ident "x")) (cIntConst 2)))])]

test4 = print $ pretty $ cTopLevel
  [cFunction (ident "main") [] cVoid
   (cCompound [CBlockStmt (cExpr (cCall (cVar (ident "f")) [cIntConst 0])),
               CBlockStmt (cExpr (cIndex (cVar (ident "a")) (cIntConst 1))),
               CBlockStmt (cExpr (cDot (cVar (ident "x")) (ident "foo"))),
               CBlockStmt cReturnVoid
               ])]

test5 = print $ pretty $ cTopLevel
  [
  ]

-- }}}
