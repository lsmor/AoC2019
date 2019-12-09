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
import qualified Control.Monad.State.Strict   as S

type Address = Int
data Transformation = Sum | Mult deriving (Show, Eq)
data OpCode         = Op Transformation | Halt deriving (Show, Eq)
data Instruction    = Ins {op :: Transformation, parameters :: Vector Address} deriving (Show, Eq)
data Intcode = Intcode {pointer :: Address, state :: Vector Address }
  deriving (Show, Eq)
type ProgrammState = S.StateT Intcode Maybe Int


-- Fill a new vector from a file containing a list of numbers.
parseIntcode = M.unfoldr step
 where
  step !s = case B.readInt s of
    Nothing       -> Nothing
    Just (!k, !t) -> Just (k, B.tail t)

-- Aux functions   
moves :: Int
moves = 3

transformToFunc :: Transformation -> (Int -> Int -> Int)
transformToFunc Sum  = (+)
transformToFunc Mult = (*)

-- Transfor an Int into a Transformation
readTransformation :: Int -> Maybe Transformation
readTransformation 1 = Just Sum
readTransformation 2 = Just Mult
readTransformation _ = Nothing 

-- Transform a Int in an OpCode
readOpcode :: Int -> Maybe OpCode
readOpcode 99 = Just Halt
readOpcode i  = Op <$> readTransformation i

-- Creates An instruction
createInstructionAt :: Vector Address -> Int -> Maybe Instruction
createInstructionAt v i = do 
  o <- readTransformation $ v ! i
  let par = M.slice (i+1) moves v
  return $ Ins o par

-- Reads An instruction evaluating the result.
evalInstruction :: Vector Address -> Instruction -> Vector Address
evalInstruction v (Ins op l) =
  let [xId, yId, at] = l 
      x = v ! xId
      y = v ! yId
      result = transformToFunc op x y
  in  v // [(at, result)]

runIntCode :: ProgrammState
runIntCode = do 
  Intcode currentPointer v <- S.get
  opCode <- S.lift $ readOpcode $ v ! currentPointer
  case opCode of 
    Halt -> return (v ! 0)
    Op t -> do 
      instruc  <- S.lift $ v `createInstructionAt` currentPointer
      let new_v       = v `evalInstruction` instruc
      let new_pointer = currentPointer + 1 + moves
      S.put $ Intcode new_pointer new_v
      runIntCode

execNoun :: Vector Int -> Int -> Vector Int
execNoun v n = v // [(1,n)]

execVerb :: Vector Int -> Int -> Vector Int
execVerb v n = v // [(2,n)]


solve s noun verb=
  let v = parseIntcode s `execNoun` noun `execVerb` verb
      initState = Intcode 0 v
  in  S.evalStateT runIntCode initState
   
