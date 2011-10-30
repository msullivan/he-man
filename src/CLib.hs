-- Wrappers for Language.C
-- Only functions starting with 'c' should be used externally.

module CLib where

import Language.C hiding (cChar)
import qualified Language.C

-- Miscellaneous {{{

cFile ns = CTranslUnit ns undefNode
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

-- Structs and unions
cStruct id = structUnion CStructTag id Nothing
cUnion id = structUnion CUnionTag id Nothing

structUnion tag id decls = CTypeSpec $ CSUType
  (CStruct tag (Just $ ident id) decls [] undefNode) undefNode

-- Enums
cEnum id = enum id Nothing

enum id enumls = CTypeSpec $ CEnumType
  (CEnum (Just $ ident id) enumls [] undefNode) undefNode

-- }}}
-- Declarations {{{

cDecl typ = decl [typ]
cDeclStatic typ = decl [cStatic,typ]
cDeclExtern typ = decl [cExtern,typ]

cDeclExt = CDeclExt

decl typSpecs indirs id init = CDecl typSpecs
  [(Just $ cDeclr id indirs,initExpr,Nothing)] undefNode
  where initExpr = init >>= (Just . cInit)

-- Type name declarations
typeDecl indirs declr = CDeclExt $ CDecl [declr]
  [(Just $ cAbstractDeclr indirs,Nothing,Nothing)] undefNode

-- Typedefs
cTypedef indirs typ id = CDeclExt $ decl
  [CStorageSpec $ CTypedef undefNode,typ] indirs id Nothing

-- Structure declarations
cStructDecl id decls = typeDecl [] $ structUnion CStructTag id (Just decls)
cUnionDecl id decls = typeDecl [] $ structUnion CUnionTag id (Just decls)
cEnumDecl id idexps = typeDecl [] $ enum id (Just ls)
  where ls = map (\(id,expr) -> (ident id,expr)) idexps

-- Declarators
cDeclr id indirs = CDeclr (Just $ ident id) indirs Nothing [] undefNode
cAbstractDeclr indirs = CDeclr Nothing indirs Nothing [] undefNode

-- Indirections (derived declarators)
cPtr = CPtrDeclr [] undefNode
cArray Nothing = CArrDeclr [] (CNoArrSize True) undefNode
cArray (Just expr) = CArrDeclr [] (CArrSize False expr) undefNode

-- Storage specifiers
cStatic = CStorageSpec $ CStatic undefNode
cExtern = CStorageSpec $ CExtern undefNode

-- Declarators
cFunction id params retTyp body = CFDefExt $ CFunDef
  [retTyp] (cDeclr id [cFun $ map f params]) [] body undefNode where
  cFun params = CFunDeclr (Right (params,False)) [] undefNode
  f (typ,id) = decl [typ] [] id Nothing

-- Initializers
cInit expr = CInitExpr expr undefNode

-- }}}
-- Expressions {{{

-- Assignment operators
assignOp assop lhs rhs = CAssign assop lhs rhs undefNode 
cAssign = assignOp CAssignOp
cMulEq = assignOp CMulAssOp
cDivEq = assignOp CDivAssOp
cModEq = assignOp CRmdAssOp
cAddEq = assignOp CAddAssOp
cSubEq = assignOp CSubAssOp
cShlEq = assignOp CShlAssOp
cShrEq = assignOp CShrAssOp
cAndEq = assignOp CAndAssOp
cXorEq = assignOp CXorAssOp
cOrEq = assignOp COrAssOp

-- Unary operators
unaryOp op expr = CUnary op expr undefNode
cPreInc = unaryOp CPreIncOp
cPreDec = unaryOp CPreDecOp
cPostInc = unaryOp CPostIncOp
cPostDec = unaryOp CPostDecOp
cAddr = unaryOp CAdrOp
cDeref = unaryOp CIndOp
cNot = unaryOp CCompOp
cLnot = unaryOp CNegOp

-- Binary operators
binaryOp op expr1 expr2 = CBinary op expr1 expr2 undefNode
cMul = binaryOp CMulOp
cDiv = binaryOp CDivOp
cMod = binaryOp CRmdOp
cAdd = binaryOp CAddOp
cSub = binaryOp CSubOp
cShl = binaryOp CShlOp
cShr = binaryOp CShrOp
cLt = binaryOp CLeOp
cGt = binaryOp CGrOp
cLeq = binaryOp CLeqOp
cGeq = binaryOp CGeqOp
cEq = binaryOp CEqOp
cNeq = binaryOp CNeqOp
cAnd = binaryOp CAndOp
cXor = binaryOp CXorOp
cOr = binaryOp COrOp
cLand = binaryOp CLndOp
cLor = binaryOp CLorOp

cCast typ expr = CCast (CDecl [typ] [] undefNode) expr undefNode
cSizeofExpr expr = CSizeofExpr expr undefNode
cSizeofType typ = CSizeofType (CDecl [typ] [] undefNode) undefNode

cTernary test conseq alt = CCond test (Just conseq) alt undefNode
cComma exprs = CComma exprs undefNode
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
cSwitch expr stmt = CSwitch expr stmt undefNode
cBreak = CBreak undefNode
-- :: CExpr -> [(Maybe CExpression, [CStatement])] -> CStatement
cSwitchBlock expr cases = cSwitch expr (cCompound (map (Left . f) cases))
  where
  f (Nothing,stmts) = cDefault $ cCompound $ map Left (stmts ++ [cBreak])
  f (Just expr,stmts) = cCase expr $ cCompound $ map Left (stmts ++ [cBreak])

cExpr expr = CExpr (Just expr) undefNode

-- Left = CStatement; Right = CDeclaration
cCompound stmts = CCompound [] stmts' undefNode where
  stmts' = map (either CBlockStmt CBlockDecl) stmts

cIfThen expr conseq altM = CIf expr conseq altM undefNode

-- Looping constructs
cWhile expr stmt = CWhile expr stmt False undefNode
cDoWhile expr stmt = CWhile expr stmt True undefNode
cFor exprDeclE expr2 expr3 stmt = CFor exprDeclE expr2 expr3 stmt undefNode
cContinue = CCont undefNode

cReturn expr = CReturn (Just expr) undefNode
cReturnVoid = CReturn Nothing undefNode

-- }}}
-- Tests {{{

test1 = print $ pretty $ cFile
  [cDeclExt $ cDecl cInt [] "x" Nothing]

test2 = print $ pretty $ cFile
  [cFunction "main" [] cInt
   (cCompound [Left (cReturn (cIntConst 0))])]

test3 = print $ pretty $ cFile
  [cFunction "main" [] cInt
   (cCompound [Left (cExpr (cAssign (cVar "x") (cIntConst 2)))])]

test4 = print $ pretty $ cFile
  [cFunction "main" [] cVoid
   (cCompound [Left (cExpr (cCall (cVar "f") [cIntConst 0])),
               Left (cExpr (cIndex (cVar "a") (cIntConst 1))),
               Left (cExpr (cDot (cVar "x") "foo")),
               Left (cExpr (cTernary (cVar "x") (cVar "x") (cVar "x"))),
               Left (cExpr (cComma [cVar "x",cVar "y"])),
               Left cReturnVoid])]

test5 = print $ pretty $ cFile
  [cFunction "main" [] cInt
   (cCompound [Left (cSwitchBlock (cVar "f")
                     [(Just (cIntConst 1),[cExpr $ cVar "x"]),
                      (Nothing,[cExpr $ cVar "y"])]),
               Left (cWhile (cVar "x") (cExpr $ cVar "y")),
               Left (cFor (Left $ Just (cVar "x"))
                          (Just (cVar "y")) (Just (cVar "z"))
                           (cExpr $ cIntConst 0)),
               Left (cReturn (cIntConst 0))])]

test6 = print $ pretty $ cFile
  [cStructDecl "Node"
     [cDecl cInt [] "data" Nothing,
     cDecl (cStruct "Node") [cPtr] "next" Nothing],
   cEnumDecl "Color" [("red",Nothing),("green",Just $ cIntConst 5)],
   cTypedef [cPtr] (cStruct "Node") "nodeptr",
   cTypedef [] (cEnum "Color") "enumcolor",
   cDeclExt $ cDecl cChar [] "a" Nothing,
   cFunction "main" [(cInt,"x")] cInt
   (cCompound [Right $ cDecl cChar [] "a" Nothing,
               Right $ cDecl cInt [cArray $ Just $ cIntConst 5] "b" Nothing,
               Right $ cDecl cInt [cPtr] "c" (Just $ cIntConst 0),
               Right $ cDecl cInt [cPtr] "c" (Just $ cVar "null"),
               Left $ cExpr $ cCast cInt (cIntConst 0),
               Left $ cExpr $ cSizeofType cInt])]

-- }}}
