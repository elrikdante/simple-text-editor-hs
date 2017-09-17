-- Copyright (C) 2017 - Present, ElrikGroup.com, Inc
-- Author: Dante Elrik
-- All rights reserved.
{-# LANGUAGE OverloadedStrings #-}
module Interpreter.Build (run) where -- TODO: ADD UNIVERSAL RESOURCE LOCATOR

import Types -- http://lpaste.net/3752197452577374208
import Common -- http://lpaste.net/3029320831361613824 Common
import qualified Data.ByteString.Char8 as CBS (pack)

run :: String -> Free (Op ByteString Int) ()
run ""          = halt'
run ('1':' ':b) = app' (CBS.pack b)
run ('2':' ':k) = del' (read k :: Int)
run ('3':' ':k) = echo' (read k :: Int)
run ('4':_    ) = undo'
run x           = noop'
