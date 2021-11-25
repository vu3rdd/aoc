{-# LANGUAGE TypeApplications #-}

module Main where

import System.Environment ( getArgs )

main :: IO ()
main = do
  inputFile:_ <- getArgs
  fileContent <- readFile inputFile
  let ms = map (\x -> read @Integer x) (lines fileContent)
  let totalMass = foldr (+) 0 (map (fuelForFuel . fuelRequired) ms)
  putStrLn (show totalMass)

type Mass = Integer
type Fuel = Integer
-- fuel required = mass/

fuelRequired :: Mass -> Fuel
fuelRequired m = (m `div` 3) - 2

fuelForFuel :: Fuel -> Fuel
fuelForFuel f = sum $ takeWhile (> 0) $ iterate fuelRequired f
