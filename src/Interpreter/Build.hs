-- Copyright (C) 2017 - Present, ElrikGroup.com, Inc
-- Author: Dante Elrik
-- All rights reserved.
module Interpreter.Build (run) where

import Types
import Common -- http://lpaste.net/3029320831361613824
import qualified Data.ByteString.Char8 as CBS (pack)

run :: String -> Free (Op ByteString Int) ()
run ""          = pure ()
run ('1':' ':b) = app' (CBS.pack b)
run ('2':' ':k) = del' (read k :: Int)
run ('3':' ':k) = echo' (read k :: Int)
run ('4':_    ) = undo'
