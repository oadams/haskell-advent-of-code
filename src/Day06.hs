module Day06 where

import Paths_aoc2023 (getDataFileName)

data Race = Race { time :: Int, distance :: Int } deriving (Show, Eq)

-- Note: This function assumes input strings are formatted correctly and could break otherwise
parseRace :: [String] -> Race
parseRace [timeStr, distanceStr] = Race (read t) (read d)
  where
    t = concat $ drop 1 $ words timeStr
    d = concat $ drop 1 $ words distanceStr
parseRace _ = error "invalid input"

numWaysToWin :: Race -> Int
numWaysToWin Race { time = t, distance = d } = length $ filter (> d) possibleDistances
  where
    possibleDistances = map getDistance [1..t]
    getDistance :: Int -> Int
    getDistance waitTime = (t - waitTime) * waitTime

day06 :: IO ()
day06 = do
    inputLines <- lines <$> (getDataFileName "day06-input.txt" >>= readFile)
    let race = parseRace inputLines
    print race
    print $ numWaysToWin race

main :: IO ()
main = day06