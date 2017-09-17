-- Copyright (C) 2017 - Present, ElrikGroup.com, Inc
-- Author: Dante Elrik
-- All rights reserved.
{-# LANGUAGE OverloadedStrings #-}
module Interpreter.Pretty (run) where

import Types -- http://lpaste.net/3752197452577374208
import Common -- http://lpaste.net/3029320831361613824 Common

run :: (Show a
               , Show b
               , Show r) => Free (Op a b) r -> String
run (Free (Append b r))   =  "APPEND " ++ show b ++ "\n" ++ run r
run (Free (Del    k r))   =  "DEL "    ++ show k ++ "\n" ++ run r
run (Free (Echo   k r))   =  "ECHO "   ++ show k ++ "\n" ++ run r
run (Free (Undo   r))     =  "UNDO "             ++ "\n" ++ run r
run (Free (NoOp   r))     =  "NOOP "             ++ "\n" ++ run r
run (Free Halt)           =  "HALT "   ++ "\n"
run (Pure r)              =  "RETURN " ++ show r ++ "\n"
