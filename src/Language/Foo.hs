module Language.Foo
  (module Language.Foo.Syntax,
   module Language.Foo.Lib,
   compile,
   testFront, testBack, testAll)
  where

import Language.Foo.Syntax
import Language.Foo.Lib

import Language.Foo.Backend
import Language.Foo.Codegen
import Language.Foo.Pretty

compile file p = do writeFile file (codegen $ backend $ desugar p)

testFront = pretty . desugar
testBack = pretty . fst . backend . desugar
testAll = codegen . backend . desugar

