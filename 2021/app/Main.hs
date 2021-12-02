{-# LANGUAGE TypeApplications #-}
module Main where

import System.Environment (getArgs)

import AoC.Day1 ( getLargerMeasurements, slidingWindow3Sum )

runDay1 :: String -> IO ()
runDay1 f =  do
  (arg:args) <- getArgs
  f <- readFile arg
  let xs = map (\x -> read @Int x) (lines f)
  let n = getLargerMeasurements xs
  putStrLn (show n)
  let m = slidingWindow3Sum xs
  putStrLn (show m)

main :: IO ()
main = do
  (arg:args) <- getArgs
  f <- readFile arg
  runDay1 f
