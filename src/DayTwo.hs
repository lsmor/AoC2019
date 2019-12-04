{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}

module DayTwo where

import           Data.ByteString.Lazy           ( ByteString )
import qualified Data.ByteString.Lazy.Char8    as B
import qualified Data.Vector                   as M
import           Data.Vector                    ( Vector
                                                , (//)
                                                , (!)
                                                )
import           System.Environment


data OpCode = Sum | Mult | Halt
  deriving (Show, Eq)

-- Fill a new vector from a file containing a list of numbers.
parse = M.unfoldr step
 where
  step !s = case B.readInt s of
    Nothing       -> Nothing
    Just (!k, !t) -> Just (k, B.tail t)

-- Transform a Int in an OpCode
readOpcode :: Int -> Maybe OpCode
readOpcode 1  = Just Sum
readOpcode 2  = Just Mult
readOpcode 99 = Just Halt
readOpcode _  = Nothing

runProgram :: Int -> Vector Int -> Maybe (Vector Int)
runProgram m v =
  let n   = 4 * m       -- Chunks are 4 bits
      h   = v ! n       -- the head of the chunk
      xId = v ! (n + 1) -- The first index in the chunk
      x   = v ! xId     -- The value to get from first index
      yId = v ! (n + 2) -- The second index in the chunk
      y   = v ! yId
      at  = v ! (n + 3) -- the index to place the value
  in  do
        opCode <- readOpcode h
        case opCode of
          Halt -> return v
          Sum  -> runProgram (m + 1) (v // [(at, x + y)])
          Mult -> runProgram (m + 1) (v // [(at, x * y)])

solve = runProgram 0 . (// [(1, 12), (2, 2)]) . parse
