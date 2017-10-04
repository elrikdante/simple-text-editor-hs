-- Copyright (C) 2017 - Present, ElrikGroup.com, Inc
-- Author: Dante Elrik
-- All rights reserved.
{-# LANGUAGE OverloadedStrings,FlexibleInstances #-}

module Types where -- http://lpaste.net/3752197452577374208

import Common -- http://lpaste.net/3029320831361613824 Common
import qualified Data.ByteString.Char8 as CBS

type Error      = ByteString
data UndoOp2 b k next = NONE
                      | UDA k next
                      | UDD b next

instance Functor (UndoOp2 b k) where
  fmap f (UDA k next) = UDA k (f next)
  fmap f (UDD b next) = UDD b (f next)
  fmap f NONE         = NONE

data Env        = Env ByteString [UndoOp ByteString Int]
data Env2 r     = Env2 ByteString (Free (UndoOp2 ByteString Int) ())
data UndoOp b k   = UndoAppend k
                  | UndoDel    b

data Op b k next =
  Append b next
  |
  Del k next
  |
  Echo k next
  |
  Begin b k next
  |
  NoOp next
  |
  Undo next
  |
  Throw Error
  |
  Halt

instance Functor (Op b k) where
  fmap f (Append b next)   = Append b (f next)
  fmap f (Del k next)      = Del k (f next)
  fmap f (Echo k next)     = Echo k (f next)
  fmap f (NoOp next)       = NoOp (f next)
  fmap f (Throw e)         = Throw e
  fmap f (Undo next)       = Undo (f next)
  fmap f (Begin desc steps next) = Begin desc steps (f next)
  fmap f Halt              = Halt


instance Arbitrary ByteString                   where  arbitrary = liftM CBS.pack arbitrary
instance Arbitrary (Free (Op ByteString Int) r) where  arbitrary = arbitraryProgram

positiveInt       = arbitrary `suchThat` (>= (0::Int))
arbitraryProgram  = positiveInt >>= actions

actions n = liftM2 begin'
            (pure (CBS.concat ["RANDOM PROGRAM OF "
                              , CBS.pack (show n)
                              , " ACTIONS"
                              ]))
            (pure n) >>= \ prefix ->
  liftM2 (>>) (pure prefix) (foldr step zed (take n (repeat ())))

  where
    step _ program = liftM2 (>>)
                     (oneof [
                         liftM app' arbitrary
                       , liftM echo' positiveInt
                       , liftM del' positiveInt
                       , return undo'
                         ])
                     (program)
    zed            = (return halt')

app'  b   = Free (Append b   (Pure ()))
del'  k   = Free (Del    k   (Pure ()))
echo' k   = Free (Echo   k   (Pure ()))
noop'     = Free (NoOp       (Pure ()))
undo'     = Free (Undo       (Pure ()))
halt'     = Free Halt
throw' e  = Free (Throw e)
begin' desc s = Free (Begin desc s (Pure ()))
env' = Env2 "" (Free (UDA 10 (Pure ())))


prg :: Free (Op ByteString Int) ()
prg = do
   pure (Begin "the canonical test case" 8)
   app' "abc"
   echo' 3
   del' 3
   app' "xy"
   echo' 2
   undo'
   undo'
   echo' 1

defaultState = Env CBS.empty []
defaultState2 = Env2 CBS.empty (Free NONE)
-- evalStateT (Interpreter.Execute.run prg) defaultState
-- evalStateT (Interpreter.Exectur2.run prg) defaultState2
