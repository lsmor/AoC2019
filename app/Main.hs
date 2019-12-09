{-# LANGUAGE OverloadedStrings #-}

module Main where

import DayTwo
import           Data.ByteString.Lazy           ( ByteString )
import qualified Data.ByteString.Lazy.Char8    as B
import qualified Data.Vector                   as M
import           Data.Vector                    ( Vector )
import System.Environment

main = do
  [f, n, v] <- getArgs 
  s  <- B.readFile f
  let noun = read n
      verb = read v
  print $ solve s noun verb

  -- noun ~ + 388800
  -- verb ~ + 1