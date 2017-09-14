{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NPlusKPatterns #-}

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Data.Monoid (Monoid(..), mempty, mappend)
import qualified Data.Text
import Data.Text(Text)
import GHC.Generics
import qualified Data.Vector.Unboxed
import Control.Monad.Except
import Control.Monad.Writer.Lazy
import qualified Data.ByteString.Builder  as LBS       
import qualified Data.ByteString.Char8    as CBS
import qualified Data.Time.Clock.POSIX    as Clock
main :: IO ()
main = defaultMain tests


data State = ST { _W         :: Input,
                  headCursor :: !Int,
                  tailCursor :: !Int,
                  auditQueue :: forall a. [(IO a -> IO CommandAudit)],
                  output     :: LBS.Builder
                }

instance Monoid State where
  mempty = ST "" 0 0 [] ""
  mappend l r = undefined

type Error  = CBS.ByteString
-- ^ Runtime information describing failure
type Input  = CBS.ByteString
-- ^ Input to traverse
type Output = ((),State)
-- ^ Output accumulated in output buffer.
type Query = WriterT State (ExceptT Error IO)
-- ^ Means of asking this system a question and having the output or error sent as the response.


data Command =
  Append Input
-- ^ Append Input to W
  |
  Echo Int
-- ^ queue a command to Echo the Nth character in W
  |
  Del Int
-- ^ let: `x = min(length W, N)`; 
-- ^ Delete last x characters from W
  deriving (Show,Generic)

data CommandAudit = Event {
  at :: Clock.POSIXTime,
  debug :: CBS.ByteString,
  command :: Command
}

runCommands :: State -> Int -> Input -> [Command] -> Query Output

runCommands s ix w [] = return ((), s)

tests = [
        testGroup "Sorting Group 1" [
                testProperty "prop1" prop1,
                testProperty "prop2" prop2
           ]
      ]

prop1 b = b == False
  where types = (b :: Bool)

prop2 i = i == 42
  where types = (i :: Int)
