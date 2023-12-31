module Day02 where

import Paths_aoc2023 (getDataFileName)
import Data.List (isPrefixOf)
import Data.List.Split

data GameDraw = GameDraw
  { green :: Integer
  , blue :: Integer
  , red :: Integer
  } deriving Show

parseGame' :: [String] -> GameDraw
parseGame' [] = GameDraw {green=0, red=0, blue=0}
parseGame' [_] = GameDraw {green=0, red=0, blue=0}
parseGame' (x:y:xs)
  | "green" `isPrefixOf` y = let game = parseGame' xs in GameDraw {green = maximum (green game, read x), red = red game, blue = blue game}
  | "red" `isPrefixOf` y = let game = parseGame' xs in GameDraw {green = green game, red = maximum (red game, read x), blue = blue game}
  | "blue" `isPrefixOf` y = let game = parseGame' xs in GameDraw {green = green game, red = red game, blue = maximum (blue game, read x)}
  | otherwise = parseGame' (y:xs)

parseGame :: String -> GameDraw
parseGame str = parseGame' (words str)

parseGames :: String -> [GameDraw]
parseGames str = map parseGame (splitOn ";" str)

isValidGameDraw :: GameDraw -> Bool
isValidGameDraw game = (green game) <= 13 && (red game) <= 12 && (blue game) <= 14

isValidGame :: [GameDraw] -> Bool
isValidGame games = all isValidGameDraw games 

minDraw :: [GameDraw] -> GameDraw
minDraw [] = GameDraw {green=0, red=0, blue=0}
minDraw [x] = x
minDraw (x:xs) = let m = minDraw xs in GameDraw {green = max (green m) (green x), red = max (red m) (red x), blue = max (blue m) (blue x)}

power :: [GameDraw] -> Integer
power game = let m = minDraw game in (green m) * (red m) * (blue m)
  
day02 :: IO ()
day02 = do
  inputLines <- lines <$> (getDataFileName "day02-input.txt" >>= readFile)
  -- putStrLn $ show $ map parseGames inputLines
  let games = (map parseGames inputLines)
  -- Sum over the first element of the tuples in validGames
  let result = sum $ map power games 
  putStrLn $ show result
