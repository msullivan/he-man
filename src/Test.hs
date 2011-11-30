module Test where

import Lang
import Back
import Sugar
import Lib


main_loop = do
  fd <- var "fd" Int 12
  print_int fd
  exit
main_loop_code = compile main_loop
main_loop_back = runPasses $ compile main_loop
