module Language.HeMan
  (module Language.HeMan.Syntax,
   module Language.HeMan.Lib,
   compile,
   testFront, testBack, testAll)
  where

import Language.HeMan.Syntax
import Language.HeMan.Lib

import Language.HeMan.Backend
import Language.HeMan.Codegen
import Language.HeMan.Pretty

compile file p = do writeFile file (codegen $ backend $ desugar p)

testFront = pretty . desugar
testBack = pretty . fst . backend . desugar
testAll = codegen . backend . desugar

