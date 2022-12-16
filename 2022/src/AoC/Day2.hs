module AoC.Day2
  ( game
  , totalScore
  , Shape (..)
  , Outcome (..)
  , Move (..)
  , playDay2Game1
  )
where

import AoC.Util (readInputDay2)

data Move = Move !Shape !Shape
  deriving (Show, Eq)

data Shape = Rock
           | Paper
           | Scissors
           deriving (Show, Eq, Enum)

data Outcome = Lose
             | Draw
             | Win
             deriving (Show, Eq, Enum)

totalScore :: Shape -> Outcome -> Int
totalScore s o = 1 + fromEnum s + 3 * fromEnum o

game :: Shape -> Shape -> Outcome
game Scissors Rock       = Win
game Paper    Scissors   = Win
game Rock     Paper      = Win
game x        y | x == y = Draw
                | otherwise = Lose

toShape :: String -> Maybe Shape
toShape "A" = Just Rock
toShape "B" = Just Paper
toShape "C" = Just Scissors
toShape "X" = Just Rock
toShape "Y" = Just Paper
toShape "Z" = Just Scissors
toShape _   = Nothing

toMoves :: [(String, String)] -> [Maybe Move]
toMoves []         = []
toMoves ((a,b):ps) = (Move <$> toShape a <*> toShape b) : toMoves ps


playDay2Game1 :: FilePath -> IO Int
playDay2Game1 input = do
  ss <- readInputDay2 input
  let scores = map play (toMoves ss)
  return $ sum scores
        where play mv = case mv of
                          Nothing -> 0
                          Just (Move s1 s2) ->
                            let outcome = game s1 s2
                            in
                              totalScore s2 outcome
-- 14069
