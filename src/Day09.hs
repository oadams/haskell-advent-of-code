module Day09 where

import Paths_aoc2023 (getDataFileName)

day09 :: IO ()
day09 = do
    inputLines <- lines <$> (getDataFileName "day09-toy-input.txt" >>= readFile)

main :: IO ()
main = day09