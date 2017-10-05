-- Copyright (C) 2017 - Present, ElrikGroup.com, Inc
-- Author: Dante Elrik
-- All rights reserved.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}
module Main where -- http://lpaste.net/3465999502194769920

import Types -- http://lpaste.net/3752197452577374208 Types
import Common -- http://lpaste.net/3029320831361613824 Common
import qualified Interpreter.Read   (run)   -- http://lpaste.net/7583342056232189952
import qualified Interpreter.Pretty (run)   -- http://lpaste.net/6717294233507594240
import qualified Interpreter.Build (run)   -- http://lpaste.net/6717294233507594240
import qualified Interpreter.Execute(run)   -- http://lpaste.net/5169620089997099008
import qualified Interpreter.Execute2(run)  -- http://lpaste.net/1413778628252008448
import Data.IORef
-- printing program
-- liftM Interpreter.Pretty.run (generate arbitraryProgram) >>= putStrLn
-- putStrLn =<< liftM Interpreter.Pretty.run (generate arbitrary :: IO (Free (Op ByteString Int) ()))

-- executing program
-- liftM Interpreter.Execute.run (generate arbitraryProgram) >>= flip evalStateT defaultState

-- executing program2
-- liftM Interpreter.Execute2.run (generate arbitraryProgram) >>= flip evalStateT defaultState2

-- 0.pg @ http://lpaste.net/358624
{-
stack exec app-exe < programs/0.pg
amountTaken(S)
APPEND "ABC"
ECHO 3
DEL 3
APPEND "XY"
ECHO 2
UNDO
UNDO
ECHO 1
RETURN ()

C
Y
A
-}
main :: IO ()
main = getLine >> (forever . loop =<< newIORef defaultState2)
    where parse     = Interpreter.Execute2.run . Interpreter.Build.run
          loop sRef = do
             chunk       <- getLine
             state       <- readIORef sRef
             writeIORef sRef . snd =<< runStateT (parse chunk) state
