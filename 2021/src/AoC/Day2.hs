{-# LANGUAGE TypeApplications #-}
module AoC.Day2
  ( parseCommand
  , runMoves
  , Pos(..)
  )
where

import Control.Monad.State.Lazy ( State, runState, StateT (StateT) )
import Control.Monad.State ( MonadState(get), put )
import Control.Monad.Identity (Identity(Identity))
import Control.Monad.State.Lazy (execState)

type Depth = Int
type Horiz = Int

data Integral a => Pos a = Pos
                           { horiz :: a
                           , depth :: a
                           } deriving (Eq, Show)

data Integral a => Command a = Forward a
                             | Down a
                             | Up a
                             deriving (Eq, Show)


move :: Integral a => Command a -> State (Pos a) ()
move cmd = do
  pos <- get
  let d = depth pos
      h = horiz pos
  case cmd of
    Forward x -> put (Pos { horiz = h + x, depth = d })
    Down y -> put (Pos { horiz = h, depth = d + y })
    Up y -> put (Pos { horiz = h, depth = d - y })

moves :: Integral a => [Command a] -> State (Pos a) ()
moves [] = StateT (\ pos -> Identity ((), pos))
moves (cmd:cmds) = do
  move cmd
  moves cmds

runMoves :: Integral a => Pos a -> [Command a] -> Pos a
runMoves initial cmds = execState (moves cmds) initial

parseCommand :: String -> Maybe (Command Int)
parseCommand cmdStr =
  let [c, v] = words cmdStr
  in
    case c of
      "forward" -> return $ Forward (read @Int  v)
      "up" -> return $ Up (read @Int v)
      "down" -> return $ Down (read @Int v)
      _ -> Nothing
