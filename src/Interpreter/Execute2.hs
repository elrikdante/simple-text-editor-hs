-- Copyright (C) 2017 - Present, ElrikGroup.com, Inc
-- Author: Dante Elrik
-- All rights reserved.
{-# LANGUAGE OverloadedStrings #-}
module Interpreter.Execute2 (run) where -- TODO: ADD UNIVERSAL RESOURCE LOCATOR

import Types -- http://lpaste.net/3752197452577374208
import Common -- http://lpaste.net/3029320831361613824
import qualified Data.ByteString.Char8 as CBS

run :: Free (Op ByteString Int) r -> StateT (Env2 ()) IO ()
run (Pure _)            = pure ()
run (Free (Undo r)) = modify go *> run r
  where
    go (Env2 s l) = update s l
    update s (Free (UDA k r)) = Env2 (CBS.take ((CBS.length s) - k) s) r
    update s (Free (UDD b r)) = Env2 (CBS.append s b) r
    update s l                = Env2 s l
run (Free (Append b r)) = modify go *> run r
  where
    go (Env2 s l) = Env2 (CBS.append s b) (Free (UDA (CBS.length b) l))

run (Free (Del k r))    = modify go *> run r
  where
    go (Env2 s l) =
      let slen           = CBS.length s
          (rest,dropped) = CBS.splitAt (slen - k) s
      in Env2 rest (Free (UDD dropped l))
run (Free Halt)         = pure ()
run (Free (Echo k r))   = do
  s <- flip CBS.index (pred k) . (\(Env2 text _) -> text) <$> get
  liftIO (CBS.hPutStrLn stdout (CBS.singleton s))
  run r
