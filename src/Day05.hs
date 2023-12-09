module Day05 where

import Paths_aoc2023 (getDataFileName)

import Data.List (isPrefixOf)
import Data.List.Split (splitOn)

parseSeeds :: [String] -> [Int]
parseSeeds [] = []
parseSeeds (l:ls)
    | "seeds:" `isPrefixOf` l = [read x | x <- concatMap words $ drop 1 (splitOn ":" l)]
    | otherwise = parseSeeds ls

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

data MapRow = MapRow { destNum :: Int, sourceNum :: Int, rangeLen :: Int}

parseMap :: MapType -> [String] -> [MapRow]
parseMap mapType ls = [MapRow { destNum = x !! 0, sourceNum = x !! 1, rangeLen = x !! 2} | x <- mapLines]
  where
    mapLines = parseMapLines mapType ls

runMap :: [MapRow] -> Int -> Int
runMap [] x = x
runMap (m:ms) x
  | x >= sourceNum m && x < sourceNum m + rangeLen m = destNum m + x - sourceNum m
  | otherwise = runMap ms x

day05 :: IO ()
day05 = do
  inputLines <- lines <$> (getDataFileName "day05-input.txt" >>= readFile)
  let seedToSoilMap = parseMap SeedToSoil inputLines
  let soilToFertilizerMap = parseMap SoilToFertilizer inputLines
  let fertilizerToWaterMap = parseMap FertililzerToWater inputLines
  let waterToLightMap = parseMap WaterToLight inputLines
  let lightToTemperatureMap = parseMap LightToTemperature inputLines
  let temperatureToHumidityMap = parseMap TemperatureToHumidity inputLines
  let humidityToLocationMap = parseMap HumidityToLocation inputLines
  let seedToLocationMap = runMap humidityToLocationMap . runMap temperatureToHumidityMap . runMap lightToTemperatureMap . runMap waterToLightMap . runMap fertilizerToWaterMap . runMap soilToFertilizerMap . runMap seedToSoilMap
  print $ minimum $ map seedToLocationMap $ parseSeeds inputLines
