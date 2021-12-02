module AoC.Day1
  ( getLargerMeasurements
  , slidingWindow3Sum
  )
where

getLargerMeasurements :: [Int] -> Int
getLargerMeasurements [] = 0
getLargerMeasurements (x:xs) = go x xs 0
  where go prev [] acc = acc
        go prev (x:xs) acc | x > prev = go x xs (acc + 1)
                           | otherwise = go x xs acc

slidingWindow3Sum :: [Int] -> Int
slidingWindow3Sum [] = 0
slidingWindow3Sum l@(p:q:r:xs) = go (p+q+r) p (q:r:xs) 0
  where go prevSum _ [] acc = acc
        go prevSum x (p:q:r:[]) acc | prevSum - x + r > prevSum = acc + 1
                                    | otherwise = acc
        go prevSum p (q:r:s:xs) acc | (prevSum - p + s) > prevSum =
                                        go (prevSum - p + s) q (r:s:xs) (acc+1)
                                    | otherwise =
                                        go (prevSum - p + s) q (r:s:xs) acc

