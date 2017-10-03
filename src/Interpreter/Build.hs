-- Copyright (C) 2017 - Present, ElrikGroup.com, Inc
-- Author: Dante Elrik
-- All rights reserved.
{-# LANGUAGE OverloadedStrings #-}
module Interpreter.Build (run) where -- http://lpaste.net/1537364766006181888

import Types -- http://lpaste.net/3752197452577374208 Types
import Common -- http://lpaste.net/3029320831361613824 Common
import qualified Data.ByteString.Char8 as CBS (pack)

run :: String -> Free (Op ByteString Int) ()
run ""          = halt'
run ('1':' ':b) = app' (CBS.pack b)
run ('2':' ':k) = del' (read k)
run ('3':' ':k) = echo' (read k)
run ('4':_    ) = undo'
run x           = noop'
