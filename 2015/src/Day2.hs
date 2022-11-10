module Day2 where

type Length = Int
type Width = Int
type Height = Int

data Box = Box {
  l :: Length
  ,w :: Width
  ,h :: Height
  } deriving (Show, Eq)

split :: String -> Char -> [String]
split s c = go s c []
  where go s c acc | s == "" = acc
                   | otherwise =
                     let x = takeWhile (/= c) s
                         rs = drop (length x + 1) s
                     in
                       go rs c (acc ++ [x])


strToDimension :: String -> Box
strToDimension s =
  let numList = map read $ split s 'x'
  in
    Box (numList !! 0) (numList !! 1) (numList !! 2)


area :: Int -> Int -> Int
area x y = x * y

paperArea :: Box -> Int
paperArea b =
  let area1 = area (l b) (w b)
      area2 = area (w b) (h b)
      area3 = area (h b) (l b)
      minArea = minimum [area1, area2, area3]
  in
    2*area1 + 2*area2 + 2*area3 + minArea


readTestFile :: FilePath -> IO [String]
readTestFile file = do
  fileContents <- readFile file
  return $ lines fileContents

totalPaperArea :: [String] -> Int
totalPaperArea ls =
  sum (map (\l -> (paperArea (strToDimension l))) ls)

