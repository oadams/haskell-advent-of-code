module Day05 where

import Paths_aoc2023 (getDataFileName)

import Data.List (isPrefixOf, sort)
import Data.List.Split (splitOn)

parseSeeds :: [String] -> [Int]
parseSeeds [] = []
parseSeeds (l:ls)
    | "seeds:" `isPrefixOf` l = [read x | x <- concatMap words $ drop 1 (splitOn ":" l)]
    | otherwise = parseSeeds ls

parseSeedPairs :: [Int] -> [Int]
parseSeedPairs [] = []
parseSeedPairs [_] = error "odd number of seeds"
parseSeedPairs (x:y:xs) = [x..x+y-1] ++ parseSeedPairs xs

data MapType = SeedToSoil | SoilToFertilizer | FertilizerToWater | WaterToLight | LightToTemperature | TemperatureToHumidity | HumidityToLocation

instance Show MapType where
    show SeedToSoil = "seed-to-soil map:"
    show SoilToFertilizer = "soil-to-fertilizer map:"
    show FertilizerToWater = "fertilizer-to-water map:"
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

data MapRow = MapRow { sourceLow :: Int, sourceHigh :: Int, destLow :: Int, destHigh :: Int, rangeLen :: Int} deriving (Show, Eq, Ord)

parseMap :: MapType -> [String] -> [MapRow]
parseMap mapType ls = [MapRow { destLow = x !! 0, destHigh = (x !! 0) + (x !! 2) - 1,
                                sourceLow = x !! 1, sourceHigh = (x !! 1) + (x !! 2) - 1,
                                rangeLen = x !! 2} | x <- mapLines]
  where
    mapLines = parseMapLines mapType ls

data Beam = Beam { low :: Int, high :: Int} deriving (Show, Eq, Ord)

-- assuming sorted input beams and map rows
translateBeams :: [MapRow] -> [Beam] ->  [Beam]
translateBeams _ [] = []
translateBeams [] beams = beams
translateBeams (m:ms) (b:bs)
    | low b > sourceHigh m = translateBeams ms (b:bs) -- Assuming input beams are sorted, then none will hit this map row
    | high b < sourceLow m = b : translateBeams (m:ms) bs -- Pass b through unchanged
    | otherwise = outBeams ++ translateBeams (m:ms) (residualBeams ++ bs)
      where
        (residualBeams, outBeams) = translateBeam b m

translateBeam :: Beam -> MapRow -> ([Beam], [Beam])
translateBeam b m
    | low b < sourceLow m && high b <= sourceHigh m = 
        ([],
          [Beam { low = low b, high = sourceLow m - 1}, -- TODO: Check for off-by-one issues with the LT/GT signs and rangeLen offsets
          Beam { low = destLow m, high = destLow m + high b - sourceLow m}])
    | low b < sourceLow m && high b > sourceHigh m = 
        ([Beam { low = sourceHigh m + 1, high = high b}],
          [Beam { low = low b, high = sourceLow m - 1},
          Beam { low = sourceLow m, high = destHigh m }])
    | low b >= sourceLow m && high b <= sourceHigh m =
        ([],
          [Beam { low = destLow m + low b - sourceLow m, high = destLow m + high b - sourceLow m}])
    | low b >= sourceLow m && high b > sourceHigh m =
        ([Beam { low = sourceHigh m + 1, high = high b}],
          [Beam { low = destLow m + low b - sourceLow m, high = destHigh m}])
    | otherwise = error "We shouldn't end up here. Alternative cases should have been handled above."

parseSeedBeams :: [Int] -> [Beam]
parseSeedBeams [] = []
parseSeedBeams [_] = error "odd number of seeds"
parseSeedBeams (x:y:xs) = Beam {low = x, high = x + y - 1} : parseSeedBeams xs

overlapMapRows :: [MapRow] -> Bool
overlapMapRows mapRows = overlapMapRows' $ sort mapRows
  where
    overlapMapRows' [] = False
    overlapMapRows' [_] = False
    overlapMapRows' (m1:m2:ms) = (sourceHigh m1 >= sourceLow m2) || overlapMapRows' (m2:ms)

overlapBeams :: [Beam] -> Bool
overlapBeams beams = overlapBeams' $ sort beams
  where
    overlapBeams' [] = False
    overlapBeams' [_] = False
    overlapBeams' (b1:b2:bs) = (high b1 >= low b2) || overlapBeams' (b2:bs)

day05 :: IO ()
day05 = do
  inputLines <- lines <$> (getDataFileName "day05-input.txt" >>= readFile)
  let seedToSoilMap = parseMap SeedToSoil inputLines
  let soilToFertilizerMap = parseMap SoilToFertilizer inputLines
  let fertilizerToWaterMap = parseMap FertilizerToWater inputLines
  let waterToLightMap = parseMap WaterToLight inputLines
  let lightToTemperatureMap = parseMap LightToTemperature inputLines
  let temperatureToHumidityMap = parseMap TemperatureToHumidity inputLines
  let humidityToLocationMap = parseMap HumidityToLocation inputLines
  -- let seedToLocationMap x = runMap humidityToLocationMap . runMap temperatureToHumidityMap . runMap lightToTemperatureMap . runMap waterToLightMap . runMap fertilizerToWaterMap . runMap soilToFertilizerMap . runMap seedToSoilMap $ trace (show x) x
  --print $ parseSeedPairs $ parseSeeds inputLines
  --print $ minimum $ map seedToLocationMap $ parseSeedPairs $ parseSeeds inputLines
  let seedBeams = sort $ parseSeedBeams $ parseSeeds inputLines
  print seedBeams
  print seedToSoilMap
  let soilBeams = sort $ translateBeams (sort seedToSoilMap) seedBeams
  print soilBeams
  print soilToFertilizerMap
  let fertilizerBeams = sort $ translateBeams (sort soilToFertilizerMap) soilBeams
  print fertilizerBeams
  print fertilizerToWaterMap
  let waterBeams = sort $ translateBeams (sort fertilizerToWaterMap) fertilizerBeams
  print waterBeams
  print waterToLightMap
  let lightBeams = sort $ translateBeams (sort waterToLightMap) waterBeams
  print lightBeams
  print lightToTemperatureMap
  let temperatureBeams = sort $ translateBeams (sort lightToTemperatureMap) lightBeams
  print temperatureBeams
  print temperatureToHumidityMap
  let humidityBeams = sort $ translateBeams (sort temperatureToHumidityMap) temperatureBeams
  print humidityBeams
  print humidityToLocationMap
  let locationBeams = sort $ translateBeams (sort humidityToLocationMap) humidityBeams
  print locationBeams
  print $ (low . minimum) locationBeams
  -- print $ (low . minimum) $ translateBeams (sort humidityToLocationMap) $ translateBeams (sort temperatureToHumidityMap) $ translateBeams (sort lightToTemperatureMap) $ translateBeams (sort waterToLightMap) $ translateBeams (sort fertilizerToWaterMap) $trace ("Beam: " ++ show seedBeams ++ "\nMap: " ++ show soilToFertilizerMap) translateBeams (sort soilToFertilizerMap) $ translateBeams (sort seedToSoilMap) (sort seedBeams)
  -- print $ map overlapMapRows [seedToSoilMap, soilToFertilizerMap, fertilizerToWaterMap, waterToLightMap, lightToTemperatureMap, temperatureToHumidityMap, humidityToLocationMap]

main :: IO ()
main = day05