module Day08 where

import Paths_aoc2023 (getDataFileName)
import Data.Map (Map)
import qualified Data.Map as M
import Data.List.Split (splitOneOf)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Maybe (fromMaybe)

parseLines :: [String] -> Map String (String, String)
parseLines ls = M.fromList $ map parseLine ls

parseLine :: String -> (String, (String, String))
parseLine line = (head nodes, (nodes !! 1, nodes !! 2))
  where
    toks = splitOneOf " (),=" line
    nodes = filter (/= "") toks

-- bfsDistance :: Map String (String, String) -> Set String -> Integer -> String -> Maybe Integer
-- bfsDistance graph visited distance cur
--    | size visited == size graph = Nothing
--    | cur == ""

countSteps :: Map String (String, String) -> String -> Integer
countSteps graph instructions = countSteps' graph instructions "AAA" 0

countSteps' :: Map String (String, String) -> String -> String -> Integer -> Integer
countSteps' _ [] _ _ = error "No instructions left."
countSteps' graph (instr:instructions) cur counter
    | cur == "ZZZ" = counter
    | instr == 'L' = countSteps' graph instructions left (counter + 1)
    | instr == 'R' = countSteps' graph instructions right (counter + 1)
    | otherwise = error "Unexpected instruction."
      where
        left = fst $ fromMaybe ("", "") $ M.lookup cur graph 
        right = snd $ fromMaybe ("", "") $ M.lookup cur graph

day08 :: IO ()
day08 = do
    inputLines <- lines <$> (getDataFileName "day08-input.txt" >>= readFile)
    let instructions = (cycle . head) inputLines
    print $ instructions !! 5
    let graph = parseLines $ filter (elem ',') inputLines
    print graph 
    print $ countSteps graph instructions
    print $ M.lookup "AAA" graph

main :: IO ()
main = day08