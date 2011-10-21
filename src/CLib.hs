-- Wrappers for Language.C

module CLib where

import Language.C
--import Language.C.Data.Ident

-- C types

cVoid = CTypeSpec $ CVoidType undefNode
cChar' = CTypeSpec $ CCharType undefNode
cShort = CTypeSpec $ CShortType undefNode
cInt = CTypeSpec $ CIntType undefNode
cLong = CTypeSpec $ CLongType undefNode
cSigned = CTypeSpec $ CSignedType undefNode
cUnsig = CTypeSpec $ CUnsigType undefNode
cBool = CTypeSpec $ CBoolType undefNode
-- TODO structs and unions
-- TODO enums
-- TODO typedefs

-- C constants

cIntConst x = CConst $ CIntConst (cInteger x) undefNode
cCharConst x = CConst $ CCharConst (cChar x) undefNode
cStrConst x = CConst $ CStrConst (cString x) undefNode

-- C identifiers

-- TODO better to use namesStartingFrom :: Int -> [Name]
noname = head $ newNameSupply

ident name = mkIdent nopos name noname

-- C declarations

cDeclr ident declrs = CDeclr (Just ident) declrs Nothing [] undefNode
cDeclrFun params = CFunDeclr (Right (params,False)) [] undefNode

cFunction ident params return body = CFDefExt $ CFunDef
  [return] (cDeclr ident [cDeclrFun params]) [] body undefNode

-- C statements

cReturnVoid = CReturn Nothing undefNode
cReturn expr = CReturn (Just expr) undefNode
cCompound exprs = CCompound [] exprs undefNode
-- TODO avoid wrapping block items with CBlockItem constructors
-- ...

-- noodling around:

{-
int main() {return 0;}
==>
cTopLevel [cFunction (ident "main") [] cInt
  (cCompound [CBlockStmt (cReturn (cIntConst 0))])]
-}

cTopLevel ns = CTranslUnit ns undefNode

-- Tests

-- int x;
x = print $ pretty $ cTopLevel
  [CDeclExt (CDecl [cInt]
                   [(Just (cDeclr (ident "x") []),Nothing,Nothing)]
                   undefNode)]

f = print $ pretty $ cTopLevel []


