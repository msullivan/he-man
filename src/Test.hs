module Test where

import Lang
import Back
import Sugar
import Lib

main_loop0 = do
  fd <- var "fd" Int 12
  print_int fd
  exit

main_loop = do
  fd <- var "fd" Int 12
  ifE fd (
    do print_int 7) (
    do print_int 4)
  exit
main_loop_code = compile main_loop
main_loop_back = runPasses $ compile main_loop
