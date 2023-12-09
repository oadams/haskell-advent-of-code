module Day04 where

import Paths_aoc2023 (getDataFileName)

import Data.List.Split 

data Card = Card { winNums :: [Integer], haveNums :: [Integer] } deriving Show

-- This is an unsafe function and assumes correct input formatting
parseCard :: String -> Card
parseCard cardStr = Card { winNums = [read x | x <- words winGroup],  haveNums = [read x | x <- words haveGroup]}
  where
    [_, winGroup, haveGroup] = splitOneOf ":|" cardStr

numWins :: Card -> Integer
numWins card = sum [1 | x <- haveNums card, x `elem` winNums card]

day04 :: IO ()
day04 = do
  inputLines <- lines <$> (getDataFileName "day04-input.txt" >>= readFile)
  let cards = map parseCard inputLines
  print $ sum $ map (\x -> if x == 0 then 0 else 2^(x-1)) $ map (numWins) cards