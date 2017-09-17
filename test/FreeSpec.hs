-- Copyright (C) 2017 - Present, ElrikGroup.com, Inc
-- Author: Dante Elrik
-- All rights reserved.
{-# LANGUAGE OverloadedStrings #-}
import Common -- http://lpaste.net/3029320831361613824 Common
import Common -- http://lpaste.net/3029320831361613824 Common
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Control.Monad.Free
import Control.Monad.State.Lazy

import qualified Data.ByteString.Char8    as CBS
import qualified Data.ByteString.Builder  as LBS
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
  Append b next
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
  fmap f (Append b next) = Append b (f next)
  fmap f (Del k next)      = Del k (f next)
  fmap f (Echo k next)     = Echo k (f next)
  fmap f (NoOp next)       = NoOp (f next)
  fmap f (Throw e)         = Throw e
  fmap f Halt              = Halt

app'  b   = Free (Append b   (Pure ()))
del'  k   = Free (Del    k   (Pure ()))
echo' k   = Free (Echo   k   (Pure ()))
noop'     = Free (NoOp       (Pure ()))
halt'     = Free Halt
throw' e  = Free (Throw e)

prg :: Free (Op ByteString Int) ()
prg = do
  app' "abc"
  echo' 3
  del' 3
  app' "xy"
  echo' 2
  del' 2
  app' "abc"
  echo' 1

showProgram :: (Show a
               , Show b
               , Show r) => Free (Op a b) r -> String
showProgram (Free (Append b r)) =  "APPEND " ++ show b   ++ "\n" ++ showProgram r
showProgram (Free (Del    k r))   =  "DEL "    ++ show k ++ "\n" ++ showProgram r
showProgram (Free (Echo   k r))   =  "ECHO "   ++ show k ++ "\n" ++ showProgram r
showProgram (Free (NoOp   r))     =  "NOOP "             ++ "\n" ++ showProgram r
showProgram (Free Halt)           =  "HALT "   ++ "\n"
showProgram (Pure r)              =  "RETURN " ++ show r ++ "\n"

runProgram :: Free (Op ByteString Int) r -> StateT ByteString IO ()
runProgram (Pure _) = pure ()
runProgram (Free Halt)         = pure ()
runProgram (Free (Append b r)) = modify (flip CBS.append b) *> runProgram r
runProgram (Free (Del k r))    = modify (\s -> CBS.take (CBS.length s - k) s) *>  runProgram r
runProgram (Free (Echo k r))   = do
  s <- flip CBS.index (pred k) <$> get
  liftIO (CBS.hPutStrLn stdout (CBS.singleton s))
  runProgram r
