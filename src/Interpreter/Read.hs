-- Copyright (C) 2017 - Present, ElrikGroup.com, Inc
-- Author: Dante Elrik
-- All rights reserved.
module Interpreter.Read (run) where

import Types
import Common -- http://lpaste.net/3029320831361613824
import qualified Interpreter.Build (run)

run :: IO (Int,(Free (Op ByteString Int) ()))
run =  do
  commands <- read <$> getLine :: IO Int
  liftM (foldl (step commands) zed . lines) getContents
  where
    step _N (0,_) s    = (1     ,Interpreter.Build.run s)
    step _N (n,trie) s
      | n < pred _N = (succ n,trie *> Interpreter.Build.run s)
      | otherwise   = (_N, trie *> halt')
    zed             = (0     ,undefined)
    



