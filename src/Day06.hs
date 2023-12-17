module Day05 where

import Paths_aoc2023 (getDataFileName)

data Race = Race { time :: Int, distance :: Int } deriving (Show, Eq)

-- Note: This function assumes input strings are formatted correctly and could break otherwise
parseRaces :: [String] -> [Race]
parseRaces [timeStr, distanceStr] = [Race (read t) (read d) | (t, d) <- zip times distances]
  where
    times = drop 1 $ words timeStr
    distances = drop 1 $ words distanceStr
parseRaces _ = error "invalid input"

numWaysToWin :: Race -> Int
numWaysToWin Race { time = t, distance = d } = length $ filter (> d) possibleDistances
  where
    possibleDistances = map getDistance [1..t]
    getDistance :: Int -> Int
    getDistance waitTime = (t - waitTime) * waitTime

day06 :: IO ()
day06 = do
    inputLines <- lines <$> (getDataFileName "day06-input.txt" >>= readFile)
    let races = parseRaces inputLines
    print races
    print $ product $ map numWaysToWin races

main :: IO ()
main = day06