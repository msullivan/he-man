module Test where

import Lang
import Back
import Sugar
import Lib
import Codegen
import Pretty

testFront = pretty . desugar
testBack = pretty . fst . backend . desugar
testAll = pretty . codegen . backend . desugar

main_loop0 = do
  fd <- var "fd" Int 12
  print_int fd
  exit

main_loop = do
  fd <- var "fd" Int 12
  ifE fd (
    do print_int 7) (
    do print_int fd)
  exit

main_loop_code = testFront main_loop
main_loop_back = testBack main_loop
main_loop_all = testAll main_loop
