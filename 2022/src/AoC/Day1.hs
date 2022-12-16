module AoC.Day1 where

import AoC.Util (readInputDay1)
import Data.List (sortBy)

calories :: IO [Integer]
calories = do
  input <- readInputDay1 "day1"
  return $ map sum input

maxCalories :: [Integer] -> Integer
maxCalories = maximum

-- 71506

topThree :: [Integer] -> [Integer]
topThree cals =
  take 3 (sortBy (flip compare) cals)

-- 209603

