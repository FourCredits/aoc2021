module Day1 where

parser :: String -> [Int]
parser = map read . lines

part1 :: [Int] -> Int
part1 ns = length $ filter id $ zipWith (<) ns (tail ns)

slidingWindow :: Int -> [a] -> [[a]]
slidingWindow n as
  | n <= length as = take n as : slidingWindow n (tail as)
  | otherwise = []

part2 :: [Int] -> Int
part2 = part1 . map sum . slidingWindow 3
