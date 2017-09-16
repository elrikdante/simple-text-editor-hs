-- Copyright (C) 2017 - Present, ElrikGroup.com, Inc
-- Author: Dante Elrik
-- All rights reserved.
{-# LANGUAGE OverloadedStrings #-}
module Interpreter.Read (run) where -- TODO: ADD UNIVERSAL RESOURCE LOCATOR

import Types
import Common -- http://lpaste.net/3029320831361613824
import qualified Interpreter.Build (run)
import qualified Text.Read (readMaybe)

run :: IO (Int,(Free (Op ByteString Int) ()))
run =  do
  commands <- Text.Read.readMaybe <$> getLine :: IO (Maybe Int)
  case commands of
    Nothing             -> putStrLn "PLEASE SUPPLY (expectedNumberOfCommands: INTEGER) SO RESOURCES CAN BE ALLOCATED APPROPRIATELY." *> run
    Just amountExpected -> liftM (foldl (step amountExpected) zed . lines) getContents
  where
    step _N (0,_) s    = (1    ,                               Interpreter.Build.run s)
    step _N (n,trie) s
      | n <= _N       = (succ n,                       trie *> Interpreter.Build.run s)
      | otherwise     = (_N    ,begin' "unamed" _N  *> trie *> halt')
    zed               = (0     ,undefined)
    



