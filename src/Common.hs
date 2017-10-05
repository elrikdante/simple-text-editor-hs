-- Copyright (C) 2017 - Present, ElrikGroup.com, Inc
-- Author: Dante Elrik
-- All rights reserved.
module Common -- http://lpaste.net/3029320831361613824
       (
         module Data.Monoid,
         module Data.Text,
         module Data.Either,
         module Data.Maybe,
         module GHC.Generics,
         module Control.Applicative,
         module Control.Monad.Except,
         module Control.Monad.Free,
         module Control.Arrow,
         module Test.QuickCheck,
         LBS.Builder,
         CBS.ByteString,
         StateT,
         IO.stdout,
         isPrefixOf,
         modify,
         get,
         put,
         evalStateT,
         runStateT
       ) where

import qualified Data.Text
import Data.Maybe
import Data.Either
import GHC.Generics
import Control.Monad.Except
import Control.Monad.State.Lazy (modify,StateT,put,get,evalStateT,runStateT)
import Control.Monad.Free
import Control.Applicative
import Control.Arrow
import Data.Monoid (Monoid(..), mempty, mappend)
import Data.List(isPrefixOf)
import qualified Data.ByteString.Builder as LBS
import qualified Data.ByteString.Char8   as CBS
import qualified System.IO               as IO (stdout)
import Test.QuickCheck         
--import qualified Data.Time.Clock.POSIX    as Clock


