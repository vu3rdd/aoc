module AoC.Day3
  ( gammaRate
  , epsilonRate
  , gammaRate'
  , epsilonRate'
  , leastCommonBit
  , mostCommonBit
  )
where

import Data.List ( transpose )

countBits :: Char -> String -> Int
countBits bit = foldr (\c acc -> if c == bit then acc + 1 else acc) 0

countZeros :: String -> Int
countZeros = countBits '0'

countOnes :: String -> Int
countOnes = countBits '1'

mostCommonBit :: String -> Char
mostCommonBit bits =
  let zs = countZeros bits
      os = countOnes bits
  in
    if zs > os then '0' else '1'

leastCommonBit :: String -> Char
leastCommonBit bits | mostCommonBit bits == '1' = '0'
                    | otherwise = '1'

binaryStringToInt :: String -> Int
binaryStringToInt bs =
  go (reverse bs) 0 0
  where
    go [] _ acc = acc
    go (x:xs) e acc | x == '1' = go xs (e + 1) (acc + 2^e)
                    | otherwise = go xs (e + 1) acc

gammaRate :: [String] -> Int
gammaRate = binaryStringToInt . gammaRate'

epsilonRate :: [String] -> Int
epsilonRate = binaryStringToInt . epsilonRate'

epsilonRate' :: [String] -> String
epsilonRate' = map leastCommonBit . transpose

gammaRate' :: [String] -> String
gammaRate' = map mostCommonBit . transpose
