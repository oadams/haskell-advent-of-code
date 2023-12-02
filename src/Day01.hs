module Day01 where

import Paths_aoc2023 (getDataFileName)
import Data.Char (isDigit)

lineNum :: String -> Integer
lineNum line = read $ [firstDigit line] <> [secondDigit line]

firstDigit :: String -> Char
firstDigit "" = error "no first char"
firstDigit (x:xs)
   | isDigit x = x
   | otherwise = firstDigit xs

secondDigit :: String -> Char
secondDigit = firstDigit . reverse

solution :: [String] -> Integer
solution ls = foldr (+) 0 (map lineNum ls)

day01 :: IO ()
day01 = do
  inputLines <- lines <$> (getDataFileName "day01-input.txt" >>= readFile)
  putStrLn "This is what I read from input:"
  putStrLn $ show $ solution inputLines
  putStrLn "TODO: implement Day 01"
