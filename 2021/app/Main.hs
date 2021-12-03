{-# LANGUAGE TypeApplications #-}
module Main where

import System.Environment (getArgs)

import AoC.Day1 ( getLargerMeasurements, slidingWindow3Sum )
import AoC.Day2 ( parseCommand, Pos(..), runMoves )
import AoC.Day3 ( gammaRate', epsilonRate', gammaRate, epsilonRate, oxygenGenRating, co2scrubberRating )

runDay1 :: String -> IO ()
runDay1 f =  do
  (arg:args) <- getArgs
  f <- readFile arg
  let xs = map (read @Int) (lines f)
  let n = getLargerMeasurements xs
  print n
  let m = slidingWindow3Sum xs
  print m

runDay2 :: String -> IO ()
runDay2 fileStr = do
  let commands = lines fileStr
      cmds = traverse parseCommand commands
      init = Pos 0 0 0
      Just finalPos = runMoves init <$> cmds
  print (horiz finalPos * depth finalPos)

runDay3 :: String -> IO ()
runDay3 fileStr = do
  let bitStrings = lines fileStr
      er = epsilonRate bitStrings
      gr = gammaRate bitStrings
      or = oxygenGenRating bitStrings
      cr = co2scrubberRating bitStrings
  -- print er
  -- print gr
  print (or * cr)
  
main :: IO ()
main = do
  (arg:args) <- getArgs
  f <- readFile arg
  runDay3 f
