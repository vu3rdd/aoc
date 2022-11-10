module Day1 where

whichFloor :: String -> Integer
whichFloor parens = foldr f 0 parens
  where f p i | p == '(' = i+1
              | p == ')' = i-1
              | otherwise = 0

basementPos :: String -> Int
basementPos parens = length $ takeWhile (>= 0) $ scanl f 0 parens
  where f i p | p == '(' = i+1
              | p == ')' = i-1
              | otherwise = 0
