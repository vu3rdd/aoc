module AoC.Day3
  ( gammaRate
  , epsilonRate
  , gammaRate'
  , epsilonRate'
  , leastCommonBit
  , mostCommonBit
  , filterStrings
  , repeatFilter
  , oxygenGenRating
  , co2scrubberRating
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

filterStrings :: Int -> Char -> [String] -> [String]
filterStrings _   _ []  = []
filterStrings pos c (x:xs) | (x !! pos) == c = x : filterStrings pos c xs
                           | otherwise = filterStrings pos c xs

repeatFilter :: Char -> ([String] -> String) -> [String] -> [String]
repeatFilter keep patF xs =
  go 0 xs
  where
    go i [xs] = [xs]
    go i xs =
      let pat = patF xs
          ys = filterStrings i (pat !! i) xs
      in
        if length ys == length xs `div` 2 && even (length xs)
        then
          -- keep the ones with 1
          let ys' = filterStrings i keep xs
          in
            go (i+1) ys
        else
          go (i+1) ys


oxygenGenRating :: [String] -> Int
oxygenGenRating xs =
  let pat = gammaRate' xs
  in
    binaryStringToInt $ head (repeatFilter '1' gammaRate' xs)

co2scrubberRating :: [String] -> Int
co2scrubberRating xs =
  let pat = epsilonRate' xs
  in
    binaryStringToInt $ head (repeatFilter '0' epsilonRate' xs)
