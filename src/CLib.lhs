\section{CLib}
This module contains wrappers for the \tt{Language.C} AST which expose a
sufficiently large subset of C99. Only functions whose names begin with \tt{c}
are meant for use outside this module.

\begin{code}
module CLib where

import Language.C hiding (cChar)
import qualified Language.C
\end{code}

\begin{code}
-- Miscellaneous {{{

cFile ns = CTranslUnit ns undefNode
ident = internalIdent

-- }}}
\end{code}

\subsection{Types} % {{{
C types (C99 6.2.5) are \tt{CDeclarationSpecifier}s. We first define some
built-in types.

\begin{code}
cVoid = CTypeSpec $ CVoidType undefNode
cChar = CTypeSpec $ CCharType undefNode
cShort = CTypeSpec $ CShortType undefNode
cInt = CTypeSpec $ CIntType undefNode
cLong = CTypeSpec $ CLongType undefNode
cSigned = CTypeSpec $ CSignedType undefNode
cUnsigned = CTypeSpec $ CUnsigType undefNode
cBool = CTypeSpec $ CBoolType undefNode
\end{code}

We also define \tt{struct}, \tt{union}, and \tt{enum} specifiers, e.g.,
\tt{cStruct "Node"} is the type \tt{struct Node}. 
\begin{code}
cStruct id = structUnion CStructTag id Nothing
cUnion id = structUnion CUnionTag id Nothing
cEnum id = enum id Nothing

structUnion tag id decls = CTypeSpec $ CSUType
  (CStruct tag (Just $ ident id) decls [] undefNode) undefNode
enum id enumls = CTypeSpec $ CEnumType
  (CEnum (Just $ ident id) enumls [] undefNode) undefNode
\end{code}

% }}}
\subsection{Declarations} % {{{

\begin{code}
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

\end{code}

% }}}
\subsection{Expressions} % {{{
C expressions (C99 6.5) are \tt{CExpression}s. They compute a value, designate
an object or function, generate side effects, or a combination of these.

We define assignment operators, e.g., \tt{cAssign (cVar "x") (cIntConst 0)} is
\tt{x = 0};
\begin{code}
cAssign = assignOp CAssignOp -- =
cMulEq = assignOp CMulAssOp  -- *=
cDivEq = assignOp CDivAssOp  -- /=
cModEq = assignOp CRmdAssOp  -- %=
cAddEq = assignOp CAddAssOp  -- +=
cSubEq = assignOp CSubAssOp  -- -=
cShlEq = assignOp CShlAssOp  -- <<=
cShrEq = assignOp CShrAssOp  -- >>=
cAndEq = assignOp CAndAssOp  -- &=
cXorEq = assignOp CXorAssOp  -- ^=
cOrEq = assignOp COrAssOp    -- |=

assignOp assop lhs rhs = CAssign assop lhs rhs undefNode 
\end{code}
%
unary operators, e.g., \tt{cDeref (cVar "p")} is \tt{*p};
\begin{code}
cPreInc = unaryOp CPreIncOp   -- ++expr
cPreDec = unaryOp CPreDecOp   -- --expr
cPostInc = unaryOp CPostIncOp -- expr++
cPostDec = unaryOp CPostDecOp -- expr--
cAddr = unaryOp CAdrOp        -- &expr
cDeref = unaryOp CIndOp       -- *expr
cNot = unaryOp CCompOp        -- ~expr
cLnot = unaryOp CNegOp        -- !expr

unaryOp op expr = CUnary op expr undefNode
\end{code}
%
binary operators, e.g., \tt{cAnd (cVar "x") (cIntConst 1)} is \tt{x \& 1};
\begin{code}
cMul = binaryOp CMulOp  -- *
cDiv = binaryOp CDivOp  -- /
cMod = binaryOp CRmdOp  -- %
cAdd = binaryOp CAddOp  -- +
cSub = binaryOp CSubOp  -- -
cShl = binaryOp CShlOp  -- <<
cShr = binaryOp CShrOp  -- >>
cLt = binaryOp CLeOp    -- <
cGt = binaryOp CGrOp    -- >
cLeq = binaryOp CLeqOp  -- <=
cGeq = binaryOp CGeqOp  -- >=
cEq = binaryOp CEqOp    -- ==
cNeq = binaryOp CNeqOp  -- !=
cAnd = binaryOp CAndOp  -- &
cXor = binaryOp CXorOp  -- ^
cOr = binaryOp COrOp    -- |
cLand = binaryOp CLndOp -- &&
cLor = binaryOp CLorOp  -- ||

binaryOp op expr1 expr2 = CBinary op expr1 expr2 undefNode
\end{code}
%
constants, e.g., \tt{cCharConst 'a'} is \tt{'a'};
\begin{code}
cIntConst x = CConst $ CIntConst (cInteger x) undefNode
cCharConst x = CConst $ CCharConst (Language.C.cChar x) undefNode
cStrConst x = CConst $ CStrConst (cString x) undefNode
\end{code}
%
and a variety of other constructs. TODO
\begin{code}
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
\end{code}

% }}}
\subsection{Statements} % {{{
\begin{code}
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
\end{code}
% }}}
\subsection{Tests} % {{{
\begin{code}
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
\end{code}
% }}}
