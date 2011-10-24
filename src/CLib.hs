-- Wrappers for Language.C

module CLib where

import Language.C hiding (cChar)
import qualified Language.C

-- Miscellaneous {{{

cTopLevel ns = CTranslUnit ns undefNode
ident = internalIdent

-- }}}
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

-- Type qualifiers
cConst = CConstQual undefNode
cStatic = CStatic undefNode
cExtern = CExtern undefNode
-- CTypedef ??

-- }}}
-- Declarations {{{

-- Declarators (internal)
cDeclr id declrs = CDeclr (Just $ ident id) declrs Nothing [] undefNode
cDeclrFun params = CFunDeclr (Right (params,False)) [] undefNode

-- Declarators
cFunction id params return body = CFDefExt $ CFunDef
  [return] (cDeclr id [cDeclrFun params]) [] body undefNode
--cDeclExt  

-- CDecl [declaration specifier]
--       [(maybe declarator, maybe initializer, maybe expression)] undefNode

-- Initializers
cInitExpr expr = CInitExpr expr undefNode

{-
(CTranslUnit [CFDefExt (CFunDef [CTypeSpec (CIntType undefNode)] (CDeclr (Just
"main") [CFunDeclr (Right ([],False)) [] undefNode] Nothing [] undefNode) []
(CCompound [] [

-- int a[5];
CBlockDecl (CDecl [cInt] [(Just (CDeclr (Just "a") [CArrDeclr [] (CArrSize False
(cIntConst 5)) undefNode] Nothing [] undefNode),Nothing,Nothing)] undefNode),

-- int* b = NULL;
CBlockDecl (CDecl [cInt] [(Just (CDeclr (Just "b") [CPtrDeclr [] undefNode]
Nothing [] undefNode),Just (CInitExpr (CVar "NULL" undefNode)
undefNode),Nothing)] undefNode),

-- int c = 0;
CBlockDecl (CDecl [cInt] [(Just (CDeclr (Just "c") [] Nothing [] undefNode),Just
(cInitExpr (cIntConst 0)),Nothing)] undefNode)

] undefNode)
undefNode)] undefNode)
-}

-- }}}
-- Expressions {{{

cComma exprs = CComma exprs undefNode

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

cTernary test conseq alt = CCond test (Just conseq) alt undefNode

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
cDot expr id = CMember expr (ident id) False undefNode
cArrow expr id = CMember expr (ident id) True undefNode
cVar id = CVar (ident id) undefNode

-- Constants
cIntConst x = CConst $ CIntConst (cInteger x) undefNode
cCharConst x = CConst $ CCharConst (Language.C.cChar x) undefNode
cStrConst x = CConst $ CStrConst (cString x) undefNode

-- }}}
-- Statements {{{

-- Switch
cCase expr stmt = CCase expr stmt undefNode
cDefault stmt = CDefault stmt undefNode
cSwitch' expr stmt = CSwitch expr stmt undefNode
cBreak = CBreak undefNode
-- CExpr -> [(Maybe CExpression, [CStatement])] -> CStatement
cSwitch expr cases = cSwitch' expr (cCompound (map (Left . f) cases)) where
  f (Nothing,stmts) = cDefault $ cCompound $ map Left (stmts ++ [cBreak])
  f (Just expr,stmts) = cCase expr $ cCompound $ map Left (stmts ++ [cBreak])

cExpr expr = CExpr (Just expr) undefNode

-- Left = CStatement; Right = CDeclaration
cCompound stmts = CCompound [] stmts' undefNode where
  stmts' = map (either CBlockStmt CBlockDecl) stmts

cIfThen expr conseq = CIf expr conseq Nothing undefNode
cIfThenElse expr conseq alt = CIf expr conseq (Just alt) undefNode

-- Looping constructs
cWhile expr stmt = CWhile expr stmt False undefNode
cDoWhile expr stmt = CWhile expr stmt True undefNode
cFor expr1 expr2 expr3 stmt = CFor (Left expr1) expr2 expr3 stmt undefNode
cForDecl decl expr2 expr3 stmt = CFor (Right decl) expr2 expr3 stmt undefNode
cContinue = CCont undefNode

cReturn expr = CReturn (Just expr) undefNode
cReturnVoid = CReturn Nothing undefNode

-- }}}
-- Tests {{{

test1 = print $ pretty $ cTopLevel
  [CDeclExt (CDecl [cInt]
                   [(Just (cDeclr "x" []),Nothing,Nothing)]
                   undefNode)]

test2 = print $ pretty $ cTopLevel
  [cFunction "main" [] cInt
   (cCompound [Left (cReturn (cIntConst 0))])]

test3 = print $ pretty $ cTopLevel
  [cFunction "main" [] cInt
   (cCompound [Left (cExpr (cAssign (cVar "x") (cIntConst 2)))])]

test4 = print $ pretty $ cTopLevel
  [cFunction "main" [] cVoid
   (cCompound [Left (cExpr (cCall (cVar "f") [cIntConst 0])),
               Left (cExpr (cIndex (cVar "a") (cIntConst 1))),
               Left (cExpr (cDot (cVar "x") "foo")),
               Left (cExpr (cTernary (cVar "x") (cVar "x") (cVar "x"))),
               Left (cExpr (cComma [cVar "x",cVar "y"])),
               Left cReturnVoid
               ])]

test5 = print $ pretty $ cTopLevel
  [cFunction "main" [] cInt
   (cCompound [Left (cSwitch (cVar "f")
                     [(Just (cIntConst 1),[cExpr $ cVar "x"]),
                      (Nothing,[cExpr $ cVar "y"])]),
               Left (cWhile (cVar "x") (cExpr $ cVar "y")),
               Left (cFor (Just (cVar "x")) (Just (cVar "y")) (Just (cVar "z"))
                           (cExpr $ cIntConst 0)),
               Left (cReturn (cIntConst 0))
               ])]

test6 = print $ pretty $ cTopLevel
  [cFunction "main" [] cInt
   (cCompound [
               ])]

-- }}}
