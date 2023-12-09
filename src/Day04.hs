module Day04 where

import Paths_aoc2023 (getDataFileName)

import Data.List.Split 

data Card = Card { cardId :: Int,  winNums :: [Int], haveNums :: [Int] } deriving Show

-- This is an unsafe function and assumes correct input formatting
parseCard :: String -> Card
parseCard cardStr = Card { cardId = read (words idGroup !! 1), winNums = [read x | x <- words winGroup],  haveNums = [read x | x <- words haveGroup]}
  where
    [idGroup, winGroup, haveGroup] = splitOneOf ":|" cardStr

numWins :: Card -> Int
numWins card = sum [1 | x <- haveNums card, x `elem` winNums card]

getWinCopies :: Card -> [Card] -> [Card]
getWinCopies card allCards = take (numWins card) (drop (cardId card) allCards)

getCardsWinCopies :: [Card] -> [Card] -> [Card]
getCardsWinCopies cards allCards = concat [getWinCopies card allCards | card <- cards]

getAllCopies :: [Card] -> [Card]
getAllCopies allCards = getAllCopies' allCards []
  where
    getAllCopies' :: [Card] -> [Card] -> [Card]
    getAllCopies' [] acc = acc
    getAllCopies' cards acc = getAllCopies' (getCardsWinCopies cards allCards) (acc ++ cards)

day04 :: IO ()
day04 = do
  inputLines <- lines <$> (getDataFileName "day04-input.txt" >>= readFile)
  let allCards = map parseCard inputLines
  print allCards
  print $ getCardsWinCopies allCards allCards
  print $ length $ getAllCopies allCards
  -- print $ sum $ map (\x -> if x == 0 then 0 else 2^(x-1)) $ map (numWins) cards