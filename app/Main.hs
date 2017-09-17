-- Copyright (C) 2017 - Present, ElrikGroup.com, Inc
-- Author: Dante Elrik
-- All rights reserved.
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Types -- http://lpaste.net/3752197452577374208
import Common -- http://lpaste.net/3029320831361613824 Common
import qualified Interpreter.Read   (run)
import qualified Interpreter.Pretty (run)
import qualified Interpreter.Execute(run)
import qualified Interpreter.Execute2(run)

main :: IO ()
main = do
  (_,program) <- Interpreter.Read.run
  putStrLn (Interpreter.Pretty.run program)
  evalStateT (Interpreter.Execute2.run program) defaultState2
  evalStateT (Interpreter.Execute.run program) defaultState
  return ()
