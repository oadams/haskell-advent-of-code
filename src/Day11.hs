module Day11 where

import Paths_aoc2023 (getDataFileName)
import qualified Data.Array as A
import Data.Array (Array, (!))
-- import Data.List (intercalate, elemIndices, findIndices)

newtype Universe = Universe { unUniverse :: Array (Int, Int) Char } deriving Show

{-
instance Show Universe where
    show grid = intercalate "\n" $ unUniverse grid
-}

buildUniverse :: [[Char]] -> Universe
buildUniverse listOfLists = Universe $ A.listArray ((0, 0), (numRows - 1, numCols - 1)) (concat listOfLists)
  where
    numRows = length listOfLists
    numCols = if numRows > 0 then length (head listOfLists) else 0

getCol :: Int -> Array (Int, Int) a -> [a]
getCol col arr = [arr ! (i, col) | i <- [l..u]]
  where
    ((l, _), (u, _)) = A.bounds arr

findGaps :: Universe -> ([Int], [Int])
findGaps (Universe universe) = (rowGaps, colGaps)
  where
    colGaps = [j | j <- [l..u], all (== '.') (getCol j universe)]
      where
        ((_, l), (_, u)) = A.bounds universe


-- TODO Explore replacing this with a Data.Array because it's painful to work on columns when
-- dealing with lists of lists
{-
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
        | all (== []) rows = rows
        | all (\r -> take 1 r == ['.']) rows = map (".." <>) rest
        | otherwise = zipWith (\x y -> take 1 x <> y) rows rest
          where
            rest = expandCols (map (drop 1) rows)
-}

{-
galaxyPositions :: Universe -> [(Int, Int)]
galaxyPositions (Universe universe) = galaxyPositions' 0 universe 
  where
    galaxyPositions' :: Int -> [[Char]] -> [(Int, Int)]
    galaxyPositions' _ [] = []
    galaxyPositions' i (row:rows) = rowCoords <> galaxyPositions' (i+1) rows
      where
        js = elemIndices '#' row
        rowCoords = map (\j -> (i, j)) js
-}


-- The idea here is that, given two galaxies x and y, we only want one of (x, y) and (y, x),
-- and we don't want (x, x) or (y, y), so we use a less-than operator to ensure this.
{-
getUniquePairs :: [(Int, Int)] -> [((Int, Int), (Int, Int))]
getUniquePairs galaxyCoords = filter (uncurry (<)) [(x, y) | x <- galaxyCoords, y <- galaxyCoords]

manhattanDist :: ((Int, Int), (Int, Int)) -> Int
manhattanDist ((x1, y1), (x2, y2)) = abs (x2 - x1) + abs (y2 - y1)
-}

day11 :: IO ()
day11 = do
    inputLines <- lines <$> (getDataFileName "day11-toy-input.txt" >>= readFile)
    let universe = buildUniverse inputLines
    print universe
    print $ findGaps universe
    -- putStrLn "----------"
    -- print $ findGaps universe
    -- print $ expandUniverse universe
    -- print $ galaxyPositions $ expandUniverse universe
    -- print $ sum $ map manhattanDist $ getUniquePairs $ galaxyPositions $ expandUniverse universe

main :: IO ()
main = day11