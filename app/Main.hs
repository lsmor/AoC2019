{-# LANGUAGE OverloadedStrings #-}

module Main where

import DayOne

inputFile :: FilePath
inputFile = "./inputs/dayOne.txt"


main :: IO ()
main = do 
  content <- readFile inputFile
  let readContent = allRequeriments $ read <$> lines content
  print readContent
