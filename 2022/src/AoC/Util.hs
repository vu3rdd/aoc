module AoC.Util
  ( readInputDay1
  , readInputDay2
  )
where

import qualified Data.List as DL (uncons)

-- | Given a file with integers in each line representing a bunch of
-- inputs, separated with an empty line to group values together, read
-- them and produce a list of list of strings.
readInputDay1 :: FilePath -> IO [[Integer]]
readInputDay1 input = do
  contents <- readFile input
  let ls = lines contents
  return $ (map . map) read $ group "" ls


group :: Eq a => a -> [a] -> [[a]]
group _ []     = []
group c (x:xs) = let (ys, zs) = span (/= c) xs
                 in
                   -- span leaves an empty string "" as the first
                   -- element of zs, we need to get rid of it.
                   case DL.uncons zs of
                     Nothing -> [x:ys]
                     Just (_,bs) -> (x:ys) : group c bs

readInputDay2 :: FilePath -> IO [(String, String)]
readInputDay2 input = do
  contents <- readFile input
  let ls = lines contents
  let ws = map words ls
  return $ zip (map (!! 0) ws) (map (!! 1) ws)
