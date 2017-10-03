
-- Copyright (C) 2017 - Present, ElrikGroup.com, Inc
-- Author: Dante Elrik
-- All rights reserved.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
-- INTERNAL
import Types  -- http://lpaste.net/3752197452577374208 Types
import Common -- http://lpaste.net/3029320831361613824 Common

import qualified Interpreter.Read    (run)   -- http://lpaste.net/7583342056232189952
import qualified Interpreter.Pretty  (run)   -- http://lpaste.net/6717294233507594240
import qualified Interpreter.Execute (run)   -- http://lpaste.net/5169620089997099008
import qualified Interpreter.Execute2(run)   -- http://lpaste.net/1413778628252008448

-- EXTERNAL
import Control.Monad.Free
import Control.Monad.State.Lazy

import Test.Framework                       as Spec --(defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 as Spec (testProperty)
import Test.QuickCheck                      as Spec

import qualified Data.ByteString.Char8      as CBS
import qualified Data.ByteString.Builder    as LBS

import qualified System.Random              as IO



data Scenario = forall a. ( Spec.Testable a
                          , Arbitrary a
                          , Show a)
             => Scene String String [(a -> a)] Scenario
              | Curtain

data Modifier = AppendOnly | None deriving Show

scene = Scene "Appends are consistent" "Oh no" [(\a -> a == True)] Curtain

runScenario :: Scenario -> [Test]
runScenario Curtain                             = mzero
runScenario (Scene group dbg inProps rest) = front : runScenario rest
  where front = testGroup
                   group
                   (snd (foldl (\ (cases,props') prop ->
                                (succ cases, testProperty ("Spec." ++ group ++ "." ++ show cases) prop:props'))
                              (0,[])
                              inProps))



runtimeCfg = Spec.RunnerOptions

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
