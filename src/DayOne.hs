{-# LANGUAGE OverloadedStrings #-}

module DayOne where

fuelRequeriments :: Integer -> Integer
fuelRequeriments n | n > 0     = r + fuelRequeriments r
                   | otherwise = 0
  where r = if floor (fromInteger n / 3) - 2 > 0 then floor (fromInteger n / 3) - 2 else 0

allRequeriments :: [Integer] -> Integer
allRequeriments = sum . fmap fuelRequeriments