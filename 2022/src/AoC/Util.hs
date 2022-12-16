module AoC.Util
( readInput
  )
where

import qualified Data.List as DL (uncons)

-- | Given a file with integers in each line representing a bunch of
-- inputs, separated with an empty line to group values together, read
-- them and produce a list of list of strings.
readInput :: FilePath -> IO [[Integer]]
readInput input = do
  contents <- readFile input
  let ls = lines contents
  return $ (map . map) read $ group "" ls


group :: Eq a => a -> [a] -> [[a]]
group _ []     = []
group c (x:xs) = let (ys, zs) = span (/= c) xs
                 in
                   case DL.uncons zs of
                     Nothing -> [x:ys]
                     Just (a,bs) -> (x:ys) : group c bs

