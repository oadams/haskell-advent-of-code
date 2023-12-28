module Day11 where

import Paths_aoc2023 (getDataFileName)
import Data.List (intercalate, elemIndices)
import Debug.Trace

newtype Universe = Universe { unUniverse :: [[Char]] }

instance Show Universe where
    show grid = intercalate "\n" $ unUniverse grid

-- Partial function: raises errors on bad input.
buildUniverse :: [String] -> Universe
buildUniverse inputLines
    | (not . allSameLength) inputLines = error $ "Bad input: " <> show inputLines
    | otherwise = Universe inputLines
      where
        allSameLength :: [[a]] -> Bool
        allSameLength [] = True
        allSameLength (x:xs) = all (\list -> length list == length x) xs

-- TODO Explore replacing this with a Data.Array because it's painful to work on columns when
-- dealing with lists of lists
expandUniverse :: Universe -> Universe
expandUniverse (Universe universe) = Universe (expandRows $ expandCols universe)
  where
    expandRows :: [[Char]] -> [[Char]]
    expandRows [] =  []
    expandRows (r:rs)
        | all (== '.') r = r : r : expandRows rs
        | otherwise = r : expandRows rs
    expandCols :: [[Char]] -> [[Char]]
    expandCols rows
        | all (== []) rows = trace ("Base case") rows
        | all (\r -> take 1 r == ['.']) rows = trace ("Empty col") map (\r -> ".." <> r) rest
        | otherwise = trace ("Otherwise") zipWith (\x y -> take 1 x <> y) rows rest
          where
            rest = expandCols (map (drop 1) rows)

galaxyPositions :: Universe -> [(Int, Int)]
galaxyPositions (Universe universe) = galaxyPositions' 0 universe 
  where
    galaxyPositions' :: Int -> [[Char]] -> [(Int, Int)]
    galaxyPositions' _ [] = []
    galaxyPositions' i (row:rows) = rowCoords <> galaxyPositions' (i+1) rows
      where
        js = elemIndices '#' row
        rowCoords = map (\j -> (i, j)) js


getUniquePairs :: [(Int, Int)] -> [((Int, Int), (Int, Int))]
getUniquePairs galaxyCoords = filter (uncurry (<)) [(x, y) | x <- galaxyCoords, y <- galaxyCoords]

manhattanDist :: ((Int, Int), (Int, Int)) -> Int
manhattanDist ((x1, y1), (x2, y2)) = abs (x2 - x1) + abs (y2 - y1)

day11 :: IO ()
day11 = do
    inputLines <- lines <$> (getDataFileName "day11-input.txt" >>= readFile)
    let universe = buildUniverse inputLines
    print universe
    putStrLn "----------"
    print $ expandUniverse universe
    print $ galaxyPositions $ expandUniverse universe
    print $ sum $ map manhattanDist $ getUniquePairs $ galaxyPositions $ expandUniverse universe

main :: IO ()
main = day11