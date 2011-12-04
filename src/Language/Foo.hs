module Language.Foo
  (module Language.Foo.Lib,
   module Language.Foo.Sugar,
   module Language.Foo.Lang,
   compile,
   testFront, testBack, testAll)
  where

import Language.Foo.Lang
import Language.Foo.Lib
import Language.Foo.Sugar -- (desugar)
import Language.Foo.Back (backend)
import Language.Foo.Codegen (codegen)
import Language.Foo.Pretty

compile file p = do writeFile file (codegen $ backend $ desugar p)

testFront = pretty . desugar
testBack = pretty . fst . backend . desugar
testAll = codegen . backend . desugar

