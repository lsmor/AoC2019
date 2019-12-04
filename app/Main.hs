module Main where

import DayTwo
import           Data.ByteString.Lazy           ( ByteString )
import qualified Data.ByteString.Lazy.Char8    as B
import qualified Data.Vector                   as M
import           Data.Vector                    ( Vector )
import System.Environment

main = do
  [f] <- getArgs 
  s  <- B.readFile f
  print $ solve s