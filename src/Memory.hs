module Memory where

-- Tests our overhead by spawning as many threads as possible.

import Language.Foo

child_code = declare_thread [] $
  \[] -> do x <- var "x" Int 1
            while 1 $ do
              y <- var "y" Int 1
              return ()

main_loop = do
  while 1 $ do
    spawn child_code []
