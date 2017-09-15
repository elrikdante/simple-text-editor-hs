-- Copyright (C) 2017 - Present, ElrikGroup.com, Inc
-- Author: Dante Elrik
-- All rights reserved.
{-# LANGUAGE OverloadedStrings #-}
import Common
import Common (Builder,ByteString)
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Control.Monad.Free
tests = [
        testGroup "Sorting Group 1" [
                testProperty "prop1" prop1,
                testProperty "prop2" prop2
           ]
      ]

main :: IO ()
main = defaultMain tests

prop1 b = b == False
  where types = (b :: Bool)

prop2 i = i == 42
  where types = (i :: Int)

type Error      = ByteString

data Op b k next =
  Append b b next
  |
  Del k next
  |
  Echo k next
  |
  NoOp next
  |
  Throw Error
  |
  Halt

instance Functor (Op b k) where
  fmap f (Append b d next) = Append b d (f next)
  fmap f (Del k next)      = Del k (f next)
  fmap f (Echo k next)     = Echo k (f next)
  fmap f (NoOp next)       = NoOp (f next)
  fmap f (Throw e)         = Throw e
  fmap f Halt              = Halt

app'  b   = Free (Append mempty b (Pure ()))
del'  k   = Free (Del    k   (Pure ()))
echo' k   = Free (Echo   k   (Pure ()))
noop'     = Free (NoOp       (Pure ()))
halt'     = Free Halt
throw' e  = Free (Throw e)

prg :: Free (Op ByteString Integer) ()
prg = do
  app' "abc"
  echo' 3
  del' 3
  app' "xy"
