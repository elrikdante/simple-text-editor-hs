module Main where

import Types
import Common
import qualified Interpreter.Read   (run)
import qualified Interpreter.Pretty (run)
import qualified Interpreter.Execute(run,run')

main :: IO ()
main = do
  (_,program) <- Interpreter.Read.run
  putStrLn (Interpreter.Pretty.run program)
  evalStateT (Interpreter.Execute.run' program) defaultState'
  evalStateT (Interpreter.Execute.run program) defaultState
  return ()
