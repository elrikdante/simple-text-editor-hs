-- Copyright (C) 2017 - Present, ElrikGroup.com, Inc
-- Author: Dante Elrik
-- All rights reserved.
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NPlusKPatterns #-}
import Common
import Common (Builder,ByteString)
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Data.Text(Text)
import qualified Data.Vector.Unboxed
import System.IO                          as IO (stdout)
import qualified Data.ByteString.Char8    as CBS
import qualified Data.Time.Clock.POSIX    as Clock


main :: IO ()
main = defaultMain tests


data State = ST { _W         :: Input,
                  size       :: !Int,
                  tailCursor :: !Int,
                  undoQueue  :: [Command],
                  auditQueue :: forall a. [(IO a -> IO CommandAudit)],
                  output     :: Builder
                }

instance Monoid State where
  mempty = ST "" 0 0 [] [] ""
  mappend l r = undefined

type Error  = ByteString
-- ^ Runtime information describing failure
type Input  = ByteString
-- ^ Input to traverse
type Output = ((),State)
-- ^ Output accumulated in output buffer.
type Query = ExceptT Error IO Output
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
  |
  Undo Command
-- ^ Undo last modification to W (Append | Del)
  deriving (Show,Generic)

data CommandAudit = Event {
  at :: Clock.POSIXTime,
  debug :: ByteString,
  command :: Command
}

undo = Undo
runCommand :: State -> Command -> Maybe Command -> Query
runCommand s (Undo (Append t)) (Just q) = return ((), s')
  where s' = s {
          _W = CBS.drop tlen (_W s),
          size       = size',
          tailCursor = (tailCursor s) - tlen
          }
        size' = max 0 (size s - tlen)
        tlen  = CBS.length t

runCommand s (Undo (Del    k)) (Just q) = undefined
runCommand s (Undo c)          Nothing  = undefined
runCommand s (Append vs) q = go (CBS.uncons vs)
  where
    go Nothing         = return ((), s)
    go (Just (char,r)) = runCommand s' (Append r) q
      where s' = s {
              _W         = CBS.append (_W s) (CBS.singleton char),
              size       = size',
              tailCursor = min size' (tailCursor s)
              }
            size' = succ (size s)

runCommand s (Del 0)  q  = return ((), s)
runCommand s (Del kx) q  = runCommand s' (Del (pred kx)) q
                          where s' = s {
                                  tailCursor = succ (tailCursor s),
                                  _W = trim (CBS.unsnoc (_W s))
                                     }
                                trim (Just (w,_)) = w
                                trim Nothing      = _W s
runCommand s (Echo jx) q = liftIO (CBS.hPutStrLn stdout (CBS.singleton $ CBS.index (_W s) (pred jx))) *> return ((), s)
runCommands :: [Command] -> State -> IO (Either Error Output)
runCommands [] s     = return (Right ((),s'))
  where s' = s{_W    = CBS.take (tailCursor s) (_W s)}
runCommands (c:cs) s = (runExceptT $ runCommand s c (pure (undo c))) >>= runNext
  where
    runNext (Right ((),s'))= runCommands cs s'
    runNext (Left e)       = return (Left e)

prg = 
  [Append "abc",
   Echo 3,
   Del 3,
   Append "xy",
   Echo 2,
   Del 2,
   Append "abc",
   Echo 1
  ]

tryIt = runCommands prg mempty

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
