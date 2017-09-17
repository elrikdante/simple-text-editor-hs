-- Copyright (C) 2017 - Present, ElrikGroup.com, Inc
-- Author: Dante Elrik
-- All rights reserved.
{-# LANGUAGE OverloadedStrings #-}
module Interpreter.Read (run) where -- TODO: ADD UNIVERSAL RESOURCE LOCATOR

import Types -- http://lpaste.net/3752197452577374208 Types
import Common -- http://lpaste.net/3029320831361613824 Common
import qualified Interpreter.Build (run)
import qualified Text.Read (readMaybe)

run :: IO (Int,(Free (Op ByteString Int) ()))
run =  do
  commands <- Text.Read.readMaybe <$> getLine :: IO (Maybe Int)
  case commands of
    Nothing             -> do
      putStrLn "PLEASE SUPPLY (expectedNumberOfCommands: INTEGER) SO RESOURCES CAN BE ALLOCATED APPROPRIATELY."
      run
    Just amountExpected -> do
      (amountTaken,program) <- liftM (foldl (step amountExpected) zed . lines) getContents
      when (amountTaken > amountExpected) (putStrLn "amountTaken > amountExpected (L00)")
      when (amountTaken < amountExpected) (putStrLn "amountTaken < amountExpected (L01)")
-- {- DIAGNOSTIC CODE BEGIN     
      when (amountTaken > 100000000) (putStrLn "amountTaken(SSSSSS)")
      when (amountTaken > 10000000) (putStrLn "amountTaken(SSSSS)")
      when (amountTaken > 1000000) (putStrLn "amountTaken(SSSS)")
      when (amountTaken > 100000) (putStrLn "amountTaken(SSS)")
      when (amountTaken > 10000) (putStrLn "amountTaken(SSS)")
      when (amountTaken > 1000) (putStrLn "amountTaken(SS)")
      when (amountTaken <=1000) (putStrLn "amountTaken(S)")
-- -} DIAGNOSTIC CODE END

      return (amountTaken,program)

  where
    step _N (0,_) s    = (1    ,                               Interpreter.Build.run s)
    step _N (n,trie) s
      | n <= _N       = (succ n,                       trie *> Interpreter.Build.run s)
      | otherwise     = (_N    ,begin' "unamed" _N  *> trie )
    zed               = (0     ,undefined)
    



