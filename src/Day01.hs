module Day01 where

import Paths_aoc2023 (getDataFileName)
import Data.Char (isDigit)
import Data.List (isPrefixOf)
import Data.Maybe (isJust, fromJust)

lineNum :: String -> Integer
lineNum line = read $ [firstDigit line] <> [secondDigit line]

digitString :: String -> Maybe Char
digitString xs
   | isPrefixOf "one" xs = Just '1'
   | isPrefixOf "two" xs = Just '2'
   | isPrefixOf "three" xs = Just '3'
   | isPrefixOf "four" xs = Just '4'
   | isPrefixOf "five" xs = Just '5'
   | isPrefixOf "six" xs = Just '6'
   | isPrefixOf "seven" xs = Just '7'
   | isPrefixOf "eight" xs = Just '8'
   | isPrefixOf "nine" xs = Just '9'
   | otherwise = Nothing

revDigitString :: String -> Maybe Char
revDigitString xs
   | isPrefixOf (reverse "one") xs = Just '1'
   | isPrefixOf (reverse "two") xs = Just '2'
   | isPrefixOf (reverse "three") xs = Just '3'
   | isPrefixOf (reverse "four") xs = Just '4'
   | isPrefixOf (reverse "five") xs = Just '5'
   | isPrefixOf (reverse "six") xs = Just '6'
   | isPrefixOf (reverse "seven") xs = Just '7'
   | isPrefixOf (reverse "eight") xs = Just '8'
   | isPrefixOf (reverse "nine") xs = Just '9'
   | otherwise = Nothing

firstDigit :: String -> Char
firstDigit "" = error "no first char"
firstDigit (x:xs)
   | isDigit x = x
   | isJust $ digitString (x:xs) = fromJust $ digitString (x:xs)
   | otherwise = firstDigit xs

secondDigit' :: String -> Char
secondDigit' "" = error "no second char"
secondDigit' (x:xs)
   | isDigit x = x
   | isJust $ revDigitString (x:xs) = fromJust $ revDigitString (x:xs)
   | otherwise = secondDigit' xs

secondDigit :: String -> Char
secondDigit = secondDigit' . reverse

solution :: [String] -> Integer
solution ls = foldr (+) 0 (map lineNum ls)

day01 :: IO ()
day01 = do
  inputLines <- lines <$> (getDataFileName "day01-input.txt" >>= readFile)
  putStrLn $ show $ solution inputLines
