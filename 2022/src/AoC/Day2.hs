module AoC.Day2
  ( game
  , totalScore
  , Shape (..)
  , Outcome (..)
  , Move (..)
  , playDay2Game1
  , playDay2Game2
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

toShape1 :: String -> Maybe Shape
toShape1 "A" = Just Rock
toShape1 "B" = Just Paper
toShape1 "C" = Just Scissors
toShape1 "X" = Just Rock
toShape1 "Y" = Just Paper
toShape1 "Z" = Just Scissors
toShape1 _   = Nothing

toMoves1 :: [(String, String)] -> [Maybe Move]
toMoves1 []         = []
toMoves1 ((a,b):ps) = (Move <$> toShape1 a <*> toShape1 b) : toMoves1 ps


playDay2Game1 :: FilePath -> IO Int
playDay2Game1 input = do
  ss <- readInputDay2 input
  let scores = map play (toMoves1 ss)
  return $ sum scores
        where play mv = case mv of
                          Nothing -> 0
                          Just (Move s1 s2) ->
                            let outcome = game s1 s2
                            in
                              totalScore s2 outcome
-- 14069

toOutcome :: Shape -> String -> (Shape, Outcome)
toOutcome Rock "X" = (Scissors, Lose)
toOutcome Rock "Y" = (Rock, Draw)
toOutcome Rock "Z" = (Paper, Win)
toOutcome Paper "X" = (Rock, Lose)
toOutcome Paper "Y" = (Paper, Draw)
toOutcome Paper "Z" = (Scissors, Win)
toOutcome Scissors "X" = (Paper, Lose)
toOutcome Scissors "Y" = (Scissors, Draw)
toOutcome Scissors "Z" = (Rock, Win)
toOutcome _        _   = undefined

toShape2 :: String -> Shape
toShape2 "A" = Rock
toShape2 "B" = Paper
toShape2 "C" = Scissors
toShape2 _   = undefined

playDay2Game2 :: FilePath -> IO Int
playDay2Game2 input = do
  ss <- readInputDay2 input
  -- take first element of each pair and convert it into a Shape
  let s1s = map (toShape2 . fst) ss
  -- take all second elements into a list, these will be mapped to
  -- desired outcomes later.
  let outcomes = map snd ss
  -- based on the desired outcome, get shape we need to play and the
  -- outcome for it output is a list of (Shape, Outcome)
  let sos = zipWith toOutcome s1s outcomes
  let total = sum $ (uncurry . zipWith) totalScore (unzip sos)
  return total

-- 12411

