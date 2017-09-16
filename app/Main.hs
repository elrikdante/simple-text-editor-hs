module Main where

import qualified Interpreter.Read   (run)
import qualified Interpreter.Pretty (run)

main :: IO ()
main = do
  (_,program) <- Interpreter.Read.run
  putStrLn (Interpreter.Pretty.run program)
  return ()
