module Day05 where

import Paths_aoc2023 (getDataFileName)

import Data.List (isPrefixOf, sort)
import Data.List.Split (splitOn)
import Debug.Trace

parseSeeds :: [String] -> [Int]
parseSeeds [] = []
parseSeeds (l:ls)
    | "seeds:" `isPrefixOf` l = [read x | x <- concatMap words $ drop 1 (splitOn ":" l)]
    | otherwise = parseSeeds ls

parseSeedPairs :: [Int] -> [Int]
parseSeedPairs [] = []
parseSeedPairs [_] = error "odd number of seeds"
parseSeedPairs (x:y:xs) = [x..x+y-1] ++ parseSeedPairs xs

data MapType = SeedToSoil | SoilToFertilizer | FertililzerToWater | WaterToLight | LightToTemperature | TemperatureToHumidity | HumidityToLocation

instance Show MapType where
    show SeedToSoil = "seed-to-soil map:"
    show SoilToFertilizer = "soil-to-fertilizer map:"
    show FertililzerToWater = "fertilizer-to-water map:"
    show WaterToLight = "water-to-light map:"
    show LightToTemperature = "light-to-temperature map:"
    show TemperatureToHumidity = "temperature-to-humidity map:"
    show HumidityToLocation = "humidity-to-location map:"

parseMapLines :: MapType -> [String] -> [[Int]]
parseMapLines _ [] = []
parseMapLines mapType (l:ls)
    | show mapType `isPrefixOf` l = parseMapLines' ls
    | otherwise = parseMapLines mapType ls
    where
      parseMapLines' :: [String] -> [[Int]]
      parseMapLines' [] = []
      parseMapLines' (l':ls')
          | null l' = []
          | otherwise = [read x | x <- words l'] : parseMapLines' ls'

data MapRow = MapRow { sourceNum :: Int, destNum :: Int, rangeLen :: Int} deriving (Show, Eq, Ord)

parseMap :: MapType -> [String] -> [MapRow]
parseMap mapType ls = [MapRow { destNum = x !! 0, sourceNum = x !! 1, rangeLen = x !! 2} | x <- mapLines]
  where
    mapLines = parseMapLines mapType ls

runMap :: [MapRow] -> Int -> Int
runMap [] x = x
runMap (m:ms) x
  | x >= sourceNum m && x < sourceNum m + rangeLen m = destNum m + x - sourceNum m
  | otherwise = runMap ms x

data Beam = Beam { low :: Int, high :: Int} deriving (Show, Eq, Ord)

-- assuming sorted input beams and map rows
translateBeams :: [MapRow] -> [Beam] ->  [Beam]
translateBeams _ [] = []
translateBeams [] beams = beams
translateBeams (m:ms) (b:bs)
    | low b >= sourceNum m + rangeLen m = translateBeams ms (b:bs) -- Assuming input beams are sorted, then none will hit this map row
    | high b < sourceNum m = b : translateBeams (m:ms) bs -- Pass b through unchanged
    | otherwise = outBeams ++ translateBeams (m:ms) (residualBeams ++ bs)
      where
        (residualBeams, outBeams) = translateBeam b m
        translateBeam :: Beam -> MapRow -> ([Beam], [Beam])
        translateBeam b m
            | low b < sourceNum m && high b < sourceNum m + rangeLen m = 
                ([],
                 [Beam { low = low b, high = sourceNum m - 1}, -- TODO: Check for off-by-one issues with the LT/GT signs and rangeLen offsets
                  Beam { low = destNum m, high = destNum m + high b - sourceNum m}])
            | low b < sourceNum m && high b >= sourceNum m + rangeLen m = 
                ([Beam { low = sourceNum m + rangeLen m, high = high b}],
                 [Beam { low = low b, high = sourceNum m - 1},
                  Beam { low = destNum m, high = destNum m + rangeLen m - 1}])
            | low b >= sourceNum m && high b < sourceNum m + rangeLen m =
                ([],
                 [Beam { low = destNum m + low b - sourceNum m, high = destNum m + high b - sourceNum m}])
            | low b >= sourceNum m && high b >= sourceNum m + rangeLen m =
                ([Beam { low = sourceNum m + rangeLen m, high = high b}],
                 [Beam { low = destNum m + low b - sourceNum m + rangeLen m, high = destNum m + rangeLen m}])
            | otherwise = error "We shouldn't end up here. Alternative cases should have been handled above."

parseSeedBeams :: [Int] -> [Beam]
parseSeedBeams [] = []
parseSeedBeams [_] = error "odd number of seeds"
parseSeedBeams (x:y:xs) = Beam {low = x, high = x + y - 1} : parseSeedBeams xs

day05 :: IO ()
day05 = do
  inputLines <- lines <$> (getDataFileName "day05-toy-input.txt" >>= readFile)
  let seedToSoilMap = parseMap SeedToSoil inputLines
  let soilToFertilizerMap = parseMap SoilToFertilizer inputLines
  let fertilizerToWaterMap = parseMap FertililzerToWater inputLines
  let waterToLightMap = parseMap WaterToLight inputLines
  let lightToTemperatureMap = parseMap LightToTemperature inputLines
  let temperatureToHumidityMap = parseMap TemperatureToHumidity inputLines
  let humidityToLocationMap = parseMap HumidityToLocation inputLines
  -- let seedToLocationMap x = runMap humidityToLocationMap . runMap temperatureToHumidityMap . runMap lightToTemperatureMap . runMap waterToLightMap . runMap fertilizerToWaterMap . runMap soilToFertilizerMap . runMap seedToSoilMap $ trace (show x) x
  --print $ parseSeedPairs $ parseSeeds inputLines
  --print $ minimum $ map seedToLocationMap $ parseSeedPairs $ parseSeeds inputLines
  let seedBeams = parseSeedBeams $ parseSeeds inputLines
  print seedBeams
  print seedToSoilMap
  print $ (low . minimum) $ translateBeams (sort humidityToLocationMap) $ translateBeams (sort temperatureToHumidityMap) $ translateBeams (sort lightToTemperatureMap) $ translateBeams (sort waterToLightMap) $ translateBeams (sort fertilizerToWaterMap) $ translateBeams (sort soilToFertilizerMap) $ translateBeams (sort seedToSoilMap) (sort seedBeams)

