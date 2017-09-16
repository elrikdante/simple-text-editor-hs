-- Copyright (C) 2017 - Present, ElrikGroup.com, Inc
-- Author: Dante Elrik
-- All rights reserved.
{-# LANGUAGE OverloadedStrings #-}
module Frontend (serialize) where
import Common -- http://lpaste.net/3029320831361613824
import Common (Builder,ByteString)
import Types
import qualified Data.ByteString.Char8 as CBS

serialize :: String -> Op ByteString Int (Free f ())
serialize line = parseLine instructionIDToken instructionArgToken
  where
    (instructionIDToken,instructionArgToken) = break (==' ') line -- <INSTRUCTION-ID><SPC><INSTRUCTION-ARG1><NULL>
    parseLine opCode opArg
      | isPrefixOf "C1" opCode = Append (CBS.pack opArg)  (Pure ())
      | isPrefixOf "C2" opCode = Del (read opArg :: Int)  (Pure ())
      | isPrefixOf "C3" opCode = Echo (read opArg :: Int) (Pure ())
      | isPrefixOf "C4" opCode = Undo                     (Pure ())

serializeIO :: IO ()
serializeIO = forever $ do
  line <- getLine
  let c  = serialize line
  return ()

main = serializeIO
  




