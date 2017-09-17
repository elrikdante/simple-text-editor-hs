-- Copyright (C) 2017 - Present, ElrikGroup.com, Inc
-- Author: Dante Elrik
-- All rights reserved.
{-# LANGUAGE OverloadedStrings #-}
module Main where -- http://lpaste.net/3465999502194769920

import Types -- http://lpaste.net/3752197452577374208 Types
import Common -- http://lpaste.net/3029320831361613824 Common
import qualified Interpreter.Read   (run)   -- http://lpaste.net/7583342056232189952
import qualified Interpreter.Pretty (run)   -- http://lpaste.net/edit/6717294233507594240
import qualified Interpreter.Execute(run)   -- http://lpaste.net/5169620089997099008
import qualified Interpreter.Execute2(run)  -- http://lpaste.net/1413778628252008448

main :: IO ()
main = do
  (_,program) <- Interpreter.Read.run
  putStrLn (Interpreter.Pretty.run program)
  evalStateT (Interpreter.Execute2.run program) defaultState2
  evalStateT (Interpreter.Execute.run program) defaultState
  return ()
