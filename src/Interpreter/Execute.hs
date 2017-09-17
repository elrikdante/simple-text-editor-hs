-- Copyright (C) 2017 - Present, ElrikGroup.com, Inc
-- Author: Dante Elrik
-- All rights reserved.
{-# LANGUAGE OverloadedStrings #-}
module Interpreter.Execute (run) where -- TODO: ADD UNIVERSAL RESOURCE LOCATOR

import Types -- http://lpaste.net/3752197452577374208 Types
import Common -- http://lpaste.net/3029320831361613824 Common
import qualified Data.ByteString.Char8 as CBS


run :: Free (Op ByteString Int) r -> StateT Env IO ()
run (Pure _)            = pure ()
run (Free Halt)         = pure ()
run (Free (Undo r))     = modify go *> run r
  where
    go (Env s (UndoAppend k : l)) = Env (CBS.take ((CBS.length s) - k) s) l
    go (Env s (UndoDel    b : l)) = Env (CBS.append s b) l

run (Free (Append b r)) = modify go *> run r
  where
    go (Env s l) = Env (CBS.append s b) ((UndoAppend (CBS.length b)) : l)

run (Free (Del k r))    = modify go *> run r
  where
    go (Env s l) =
      let slen           = CBS.length s
          (rest,dropped) = CBS.splitAt (slen - k) s
      in Env rest ((UndoDel dropped) : l)

run (Free (Echo k r))   = do
  s <- flip CBS.index (pred k) . (\(Env text _) -> text) <$> get
  liftIO (CBS.hPutStrLn stdout (CBS.singleton s))
  run r


