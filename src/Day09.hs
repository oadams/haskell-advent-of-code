module Day09 where

import Paths_aoc2023 (getDataFileName)

parseSeq :: String -> [Integer]
parseSeq line = map read (words line)

diffSeq :: [Integer] -> [Integer]
diffSeq seq = zipWith (-) (tail seq) seq

diffStack :: [[Integer]] -> [[Integer]]
diffStack (layer:layers) 
    | null (diffSeq layer) = layer:layers
    | all (== 0) (diffSeq layer) = layer:layers
    | otherwise = diffStack ((diffSeq layer):layer:layers)
diffStack [] = []

nextVal :: [Integer] -> Integer
nextVal history = foldr (\x y -> last x + y) 0 stack
  where
    stack = diffStack [history]

day09 :: IO ()
day09 = do
    inputLines <- lines <$> (getDataFileName "day09-input.txt" >>= readFile)
    let seqs = map parseSeq inputLines
    print seqs
    print $ sum $ map nextVal seqs

main :: IO ()
main = day09