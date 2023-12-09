module Day03 where

import Data.Char (intToDigit)
import Data.List (nub)
import Data.List.Split (splitOneOf)
import Text.Read (readMaybe)

import Paths_aoc2023 (getDataFileName)


getSymbols :: String -> String
getSymbols = filter (not . (`elem` [intToDigit y | y <- [0..9]] ++ ['.', '\n'])) . nub

data PartNumber = PartNumber { number :: Int, offset :: Int} deriving Show

isInteger :: String -> Bool
isInteger s = case readMaybe s :: Maybe Integer of
    Just _  -> True
    Nothing -> False

getPartNumbers :: String -> String -> [PartNumber]
getPartNumbers symbols row = getPartNumbers' 0 $ splitOneOf symbols row

getPartNumbers' :: Int -> [String] -> [PartNumber]
getPartNumbers' _ [] = []
getPartNumbers' acc [x]
    | isInteger x = [PartNumber {number = read x, offset = acc}]
    | otherwise = []
getPartNumbers' acc (x:xs)
    | isInteger x = PartNumber {number = read x, offset = acc} : getPartNumbers' (acc + 1 + length x) xs
    | otherwise = getPartNumbers' (acc + 1) xs

hasPartInRow :: PartNumber -> String -> String -> Bool
hasPartInRow partNumber symbols row = hasPart' (zip [0..] row)
  where
    hasPart' :: [(Int, Char)] -> Bool
    hasPart' [] = False
    hasPart' ((i, c):xs)
        | c `elem` symbols && i >= offset partNumber - 1 && i <= offset partNumber + (length . show . number) partNumber = True
        | otherwise = hasPart' xs

getRowGroups :: [a] -> [[a]]
getRowGroups rows = makeListFrom start mid end
  where
    mid = zip3 rows (drop 1 rows) (drop 2 rows)
    start = zip (take 1 rows) (drop 1 rows)
    end = zip (drop (length rows - 1) rows) (drop (length rows - 2) rows)

makeListFrom :: [(a, a)] -> [(a, a, a)] -> [(a, a)] -> [[a]]
makeListFrom start mid end = newStart ++ newMid ++ newEnd
    where
    newMid = map (\(x, y, z) -> [x, y, z]) mid
    newStart = map (\(x, y) -> [x, y]) start
    newEnd = map (\(x, y) -> [x, y]) end

hasPartInRowGroup :: String -> [String] -> PartNumber -> Bool
hasPartInRowGroup symbols rowGroup partNumber = any (hasPartInRow partNumber symbols) rowGroup

partsInRowGroup :: String -> [String] -> [PartNumber] -> [PartNumber]
partsInRowGroup symbols rowGroup partNumbers = filter (hasPartInRowGroup symbols rowGroup) partNumbers

gearRatio :: [PartNumber] -> Gear  -> Int
gearRatio partNumbers gear 
    | length adjacentPartNumbers == 2 = product [number x | x <- adjacentPartNumbers]
    | otherwise = 0
    where 
        adjacentPartNumbers = filter isAdjacent partNumbers
        isAdjacent :: PartNumber -> Bool
        isAdjacent partNumber = gearOffset gear >= offset partNumber - 1 && gearOffset gear <= offset partNumber + (length . show . number) partNumber

newtype Gear = Gear { gearOffset :: Int } deriving Show

getGears :: String -> [Gear]
getGears row = getGears' 0 row
  where
    getGears' :: Int -> String -> [Gear]
    getGears' _ [] = []
    getGears' acc (x:xs)
        | x == '*' = Gear {gearOffset = acc} : getGears' (acc + 1) xs
        | otherwise = getGears' (acc + 1) xs

-- gearRatios :: Gear -> [PartNumber] -> [Int]
-- gearRatios [] partNumbers = []
--gearRatios (x:xs) partNumbers = 

day03 :: IO ()
day03 = do
  inputLines <- lines <$> (getDataFileName "day03-input.txt" >>= readFile)
  let symbols = getSymbols $ unlines inputLines
  let partNumberGroups = map concat $ getRowGroups $ map (getPartNumbers (symbols ++ ".")) inputLines
  let gears = map getGears inputLines
  -- let rowGroups = getRowGroups inputLines
  print partNumberGroups
  print gears
  print $ sum $ concat [map (gearRatio partNumbers) rowGears | (rowGears, partNumbers) <- zip gears partNumberGroups]
  -- let validParts = concat $ [partsInRowGroup symbols rowGroup rowPartNumbers | (rowGroup, rowPartNumbers) <- zip rowGroups partNumbers]
  --print $ sum [number x | x <- validParts]