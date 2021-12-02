{-# LANGUAGE TypeApplications #-}
module Main where

import System.Environment (getArgs)

import AoC.Day1 ( getLargerMeasurements, slidingWindow3Sum )
import AoC.Day2 ( parseCommand, Pos(..), runMoves )

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

main :: IO ()
main = do
  (arg:args) <- getArgs
  f <- readFile arg
  runDay2 f
