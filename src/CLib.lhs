\section{CLib}
This module contains wrappers for the \tt{Language.C} AST which expose a
sufficiently large subset of C99. Only functions whose names begin with \tt{c}
are meant for use outside this module.
\begin{code}
module CLib where

import Language.C hiding (cChar, pretty)
import qualified Language.C
\end{code}

Using \tt{Language.C.Pretty.pretty}, we can pretty-print any C entity, although
the main entity of interest is the file, or \tt{CTranslationUnit}, which is
comprised of a list of \tt{CExternalDeclaration}s.
\begin{code}
cFile edecls = CTranslUnit edecls undefNode
ident = internalIdent
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

Finally, arbitrary identifiers may also act as types if they have been
\tt{typedef}ed elsewhere.
\begin{code}
cType id = CTypeSpec $ CTypeDef (ident id) undefNode
\end{code}

% }}}
\subsection{Declarations} % {{{
C declarations (C99 6.7) are \tt{CDeclaration}s.

A basic C declaration takes a list of \tt{CDeclarationSpecifier}s, a list of
indirections (\tt{CDerivedDeclararator}s), an identifier, and optionally, an
initializing expression. It may occur either at top-level or inside other
constructs.
\begin{code}
cDecl typs indirs id expr = CDecl typs
  [(Just $ declr id indirs,init,Nothing)] undefNode
  where init = expr >>= (\init -> Just $ CInitExpr init undefNode)
cDeclExt typs indirs id expr = CDeclExt $ cDecl typs indirs id expr

declr id indirs = CDeclr (Just $ ident id) indirs Nothing [] undefNode
\end{code}
%
The list of \tt{CDeclarationSpecifier}s must end in a type, and may optionally
be proceeded by storage specifiers:
\begin{code}
cAuto = CStorageSpec $ CAuto undefNode
cRegister = CStorageSpec $ CRegister undefNode
cStatic = CStorageSpec $ CStatic undefNode
cExtern = CStorageSpec $ CExtern undefNode
\end{code}
%
The list of indirections may contain pointer or array declarators; the latter
optionally specifies a length expression.
\begin{code}
cPtr = CPtrDeclr [] undefNode
cArray Nothing = CArrDeclr [] (CNoArrSize False) undefNode
cArray (Just expr) = CArrDeclr [] (CArrSize False expr) undefNode
\end{code}

We will restrict other declarations to occur at top-level. We can \tt{typedef} a
type with indirections as another identifier. 
\begin{code}
cTypedef typ indirs id = CDeclExt $ cDecl
  [CStorageSpec $ CTypedef undefNode,typ] indirs id Nothing
\end{code}

Declaring a named \tt{struct} or \tt{union} type requires an identifier and a
list of declarations. Declaring a named \tt{enum} type requires an identifier
and a list of pairs of an identifier and an optional expression corresponding to
its enumeration constant.
\begin{code}
cStructDecl id decls = typeDecl [] $ structUnion CStructTag id (Just decls)
cUnionDecl id decls = typeDecl [] $ structUnion CUnionTag id (Just decls)
cEnumDecl id idExprMs = typeDecl [] $ enum id (Just ls)
  where ls = map (\(id,expr) -> (ident id,expr)) idExprMs

typeDecl indirs declr = CDeclExt $ CDecl [declr]
  [(Just $ abstractDeclr indirs,Nothing,Nothing)] undefNode
abstractDeclr indirs = CDeclr Nothing indirs Nothing [] undefNode
\end{code}

Function declarations require an identifier, a list of parameters, a return type
(list), and a body statement. Each parameter is a tuple of
\tt{CDeclarationSpecifier}s, indirections, and an identifier.
\begin{code}
cFunction id params typs body = CFDefExt $ CFunDef typs
  (declr id [cFun $ map f params]) [] body undefNode where
  cFun params = CFunDeclr (Right (params,False)) [] undefNode
  f (typs,indirs,id) = cDecl typs indirs id Nothing
\end{code}

% }}}
\subsection{Expressions} % {{{
C expressions (C99 6.5) are \tt{CExpression}s.

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
and a variety of other constructs. Below, \tt{typ} arguments are
\tt{CDeclarationSpecifier}s, and \tt{id}s are \tt{String}s.
\begin{code}
cCast typ expr = CCast (CDecl [typ] [] undefNode) expr undefNode -- (typ) expr
cSizeofExpr expr = CSizeofExpr expr undefNode                    -- sizeof(expr)
cSizeofType typ = CSizeofType(CDecl [typ] [] undefNode) undefNode -- sizeof(typ)

cTernary e1 e2 e3 = CCond e1 (Just e2) e3 undefNode     -- e1 ? e2 : e3
cComma exprs = CComma exprs undefNode                   -- (expr1, expr2 ...)
cIndex expr ind = CIndex expr ind undefNode             -- expr[ind]
cCall expr args = CCall expr args undefNode             -- expr(args ...)
cDot expr id = CMember expr (ident id) False undefNode  -- expr.id
cArrow expr id = CMember expr (ident id) True undefNode -- expr->id
cVar id = CVar (ident id) undefNode                     -- id
\end{code}

% }}}
\subsection{Statements} % {{{
C statements (C99 6.8) are \tt{CStatement}s.

Any expression is a statement, as is the null statement.
\begin{code}
cExpr expr = CExpr (Just expr) undefNode
cNull = CExpr Nothing undefNode
\end{code}

A list of statements (\tt{Left}) and/or declarations (\tt{Right}) comprise a
compound statement.
\begin{code}
cCompound stmts = CCompound [] stmts' undefNode where
  stmts' = map (either CBlockStmt CBlockDecl) stmts
\end{code}

\tt{if} takes an expression, a \tt{then} statement, and optionally, an \tt{else}
statement.
\begin{code}
cIfThen expr conseq altM = CIf expr conseq altM undefNode
\end{code}

\tt{while} and \tt{do while} have test expressions and statement bodies.
\begin{code}
cWhile expr stmt = CWhile expr stmt False undefNode
cDoWhile expr stmt = CWhile expr stmt True undefNode
\end{code}

A \tt{for} loop has a statement body and three optional fields; the first may be
either an expression (\tt{Left}) or a declaration (\tt{Right}), while the last
two must be expressions.
\begin{code}
cFor initME expr2M expr3M stmt = CFor init expr2M expr3M stmt undefNode
  where init = maybe (Left Nothing) (either (Left . Just) Right) initME
\end{code}

Other control-flow statements include \tt{break}, \tt{continue}, and
\tt{return}, with or without an expression.
\begin{code}
cBreak = CBreak undefNode
cContinue = CCont undefNode
cReturn expr = CReturn (Just expr) undefNode
cReturnVoid = CReturn Nothing undefNode
\end{code}

\tt{switch} blocks test an expression and have a statement body. Within the
body, we may have \tt{case} or \tt{default} statements, which test an expression
and have a statement body. 
\begin{code}
cSwitch expr stmt = CSwitch expr stmt undefNode
cCase expr stmt = CCase expr stmt undefNode
cDefault stmt = CDefault stmt undefNode

-- :: CExpr -> [(Maybe CExpression, [CStatement])] -> CStatement
cSwitchBlock expr cases = cSwitch expr (cCompound (map (Left . f) cases)) where
  f (Nothing,stmts) = cDefault $ cCompound $ map Left (stmts ++ [cBreak])
  f (Just expr,stmts) = cCase expr $ cCompound $ map Left (stmts ++ [cBreak])
\end{code}

Pretty prints a program
\begin{code}
pretty = Language.C.pretty

\end{code}
% }}}
\subsection{Tests} % {{{
\begin{code}
test1 = pretty $ cFile
  [cDeclExt [cStatic,cInt] [] "x" Nothing,
   cFunction "main" [] [cInt]
   (cCompound [Left (cReturn (cIntConst 0))])]

test2 = pretty $ cFile
  [cFunction "main" [([cInt],[],"argc"),([cChar],[cArray Nothing,cPtr],"argv")]
             [cInt]
   (cCompound [Left (cExpr (cAssign (cVar "x") (cIntConst 2)))])]

test3 = pretty $ cFile
  [cFunction "main" [] [cVoid]
   (cCompound [Left (cExpr (cCall (cVar "f") [cIntConst 0])),
               Left (cExpr (cIndex (cVar "a") (cIntConst 1))),
               Left (cExpr (cDot (cVar "x") "foo")),
               Left (cExpr (cTernary (cVar "x") (cVar "x") (cVar "x"))),
               Left (cExpr (cComma [cVar "x",cVar "y"])),
               Left cReturnVoid])]

test4 = pretty $ cFile
  [cFunction "main" [] [cInt]
   (cCompound [Left (cSwitchBlock (cVar "f")
                     [(Just (cIntConst 1),[cExpr $ cVar "x"]),
                      (Nothing,[cExpr $ cVar "y"])]),
               Left (cWhile (cVar "x") (cExpr $ cVar "y")),
               Left (cFor (Just $ Left (cVar "x"))
                          (Just (cVar "y")) (Just (cVar "z"))
                           (cExpr $ cIntConst 0)),
               Left (cReturn (cIntConst 0))])]

test5 = pretty $ cFile
  [cStructDecl "Node"
     [cDecl [cInt] [] "data" Nothing,
     cDecl [cStruct "Node"] [cPtr] "next" Nothing],
   cEnumDecl "Color" [("red",Nothing),("green",Just $ cIntConst 5)],
   cTypedef (cStruct "Node") [cPtr] "nodeptr",
   cTypedef (cEnum "Color") [] "enumcolor",
   cDeclExt [cType "nodeptr"] [] "node" (Just $ cVar "null"),
   cDeclExt [cChar] [] "a" Nothing,
   cFunction "main" [([cInt],[],"x")] [cInt]
   (cCompound [Right $ cDecl [cChar] [] "a" Nothing,
               Right $ cDecl [cInt] [cArray $ Just $ cIntConst 5] "b" Nothing,
               Right $ cDecl [cInt] [cPtr] "c" (Just $ cIntConst 0),
               Right $ cDecl [cInt] [cPtr] "c" (Just $ cVar "null"),
               Left $ cExpr $ cCast cInt (cIntConst 0),
               Left $ cExpr $ cSizeofType cInt])]
\end{code}
% }}}
