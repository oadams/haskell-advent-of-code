module Day10 where

import Paths_aoc2023 (getDataFileName)
import Data.List (intercalate, elemIndex)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Maybe (fromMaybe)
import Debug.Trace

newtype Grid = Grid { unGrid :: [[Char]] }

instance Show Grid where
    show grid = intercalate "\n" $ unGrid grid

-- Partial function: raises errors on bad input.
buildGrid :: [String] -> Grid
buildGrid inputLines
    | (not . allSameLength) inputLines = error $ "Bad input: " <> show inputLines
    | otherwise = Grid inputLines
      where
        allSameLength :: [[a]] -> Bool
        allSameLength [] = True
        allSameLength (x:xs) = all (\list -> length list == length x) xs

-- Also partial, assuming there is a start
findStart :: Grid -> (Int, Int)
findStart (Grid grid) = findStart' 0 grid 
  where
    findStart' :: Int -> [[Char]] -> (Int, Int)
    findStart' _ [] = error "No start found in grid."
    findStart' i (row:rows) = let maybeJ = elemIndex 'S' row in
        case maybeJ of
            Nothing -> findStart' (i+1) rows
            Just j -> (i, j)

getElement :: Grid -> (Int, Int) -> Maybe Char
getElement (Grid grid) (i, j)
  | i < 0 || j < 0 = Nothing
  | i >= height || j >= width = Nothing
  | otherwise = Just ((grid !! i) !! j)
  where
    height = length grid
    width = case take 1 grid of 
        [] -> 0
        (r:_) -> length r

data Cardinal = North | South | East | West

{-
chooseNextTiles' :: Grid -> (Int, Int) -> Cardinal -> Maybe (Int, Int)
chooseNextTiles' grid curLoc North
    | c `elem` ['|', 'F', '7', 'S'] = Just (i-1, j)
    | otherwise = Nothing
      where c = getElement grid (i-1, j)
-}


-- getNextTiles' :: Grid -> (Int, Int) -> [(Int, Int)]
-- getNextTiles grid curLoc

-- Need to change these checks so that the character on the current tile is considered too!
chooseNextTiles :: Grid -> (Int, Int) -> [(Int, Int)]
chooseNextTiles grid (i, j) = concat [checkNorth, checkSouth, checkEast, checkWest]
  where
    cur = fromMaybe '.' $ getElement grid (i, j)
    checkNorth :: [(Int, Int)]
    checkNorth = let north = getElement grid (i-1, j) in
        case north of
            Just c -> if (c `elem` ['|', 'F', '7', 'S']) && (cur `elem` ['L', 'J', '|', 'S']) then [(i-1, j)] else []
            Nothing -> []
    checkSouth :: [(Int, Int)]
    checkSouth = let south = getElement grid (i+1, j) in
        case south of
            Just c -> if (c `elem` ['|', 'J', 'L', 'S']) && (cur `elem` ['|', 'F', '7', 'S']) then [(i+1, j)] else []
            Nothing -> []
    checkEast :: [(Int, Int)]
    checkEast = let east = getElement grid (i, j+1) in
        case east of
            Just c -> if (c `elem` ['-', 'J', '7', 'S']) && (cur `elem` ['-', 'F', 'L', 'S']) then [(i, j+1)] else []
            Nothing -> []
    checkWest :: [(Int, Int)]
    checkWest = let west = getElement grid (i, j-1) in
        case west of
            Just c -> if (c `elem` ['-', 'L', 'F', 'S']) && (cur `elem` ['-', 'J', '7', 'S']) then [(i, j-1)] else []
            Nothing -> []

-- There can only be one loop, so I don't even need to do BFS, I can just keep
-- track of cur and prev and iterate around the loop until I'm back at the Start,
-- keeping track of the distance and then halving it.
{-
loopDistance :: Grid -> Int
loopDistance grid = loopDistance' 0 start (head $ chooseNextTiles grid start)
  where
    start = findStart grid
    loopDistance' :: Int -> (Int, Int) -> (Int, Int) -> Int
    loopDistance' distance cur next
        | getElement grid next == Just 'S' = distance + 1
        | otherwise = loopDistance' (distance + 1) cur' next'
          where 
            cur' = next
            next' = head [tile | tile <- chooseNextTiles grid next, tile /= cur]
-}

-- Oh, no you do need to do BFS because you can still go down garden paths that don't become loops

bfsDistance :: Grid -> Int
bfsDistance grid = bfsDistance' 0 initPrevTiles [findStart grid] initVisited
  where
    initVisited = S.empty :: Set (Int, Int)
    initPrevTiles = []
    bfsDistance' :: Int -> [(Int, Int)] -> [(Int, Int)] -> Set (Int, Int) -> Int
    bfsDistance' distance prevTiles tiles visited
        | any (`elem` visited) tiles /= all (`elem` visited) tiles = error ("Wrong assumption.\n" <> show visited <> "\n" <> show tiles)
        | all (`elem` visited) tiles = distance
        | otherwise = trace ("------\nprevTiles: " <> show prevTiles <> "\ntiles: " <> show tiles <> "\nvisited: " <> show visited) bfsDistance' (distance + 1) prevTiles' nextTiles visited'
          where
            nextTiles = filter (`notElem` prevTiles) $ concat [chooseNextTiles grid tile | tile <- tiles]
            prevTiles' = tiles 
            visited' = visited `S.union` S.fromList tiles




{-
Idea:
- Parse in the map
- BFS until there's nothing left to explore
- When you find a node you've already visited, store the count at that loop juncture.
- Need to handle race conditions though.
-}

{-
bfsDistance :: Map String (String, String) -> Set String -> Integer -> String -> Maybe Integer
bfsDistance graph visited distance cur
    | size visited == size graph = Nothing
    | cur == ""
-}



day10 :: IO ()
day10 = do
    inputLines <- lines <$> (getDataFileName "day10-toy-input-2.txt" >>= readFile)
    let grid = buildGrid inputLines
    print grid
    -- print $ chooseNextTiles grid $ findStart grid
    print $ bfsDistance grid
    -- print $ (loopDistance grid) `div` 2

main :: IO ()
main = day10