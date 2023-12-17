module Day06 where

import Paths_aoc2023 (getDataFileName)
import Data.List (nub, group, sort)

data HandType = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind deriving (Show, Eq, Ord)

data Card = Two | Three | Four | Five | Six | Seven | Eight | Nine | T | J | Q | K | A deriving (Eq, Ord)

instance Show Card where
    show Two = "2"
    show Three = "3"
    show Four = "4"
    show Five = "5"
    show Six = "6"
    show Seven = "7"
    show Eight = "8"
    show Nine = "9"
    show T = "T"
    show J = "J"
    show Q = "Q"
    show K = "K"
    show A = "A" 

data Hand = Hand { handType :: HandType, handCards :: [Card] } deriving (Show, Eq, Ord)

parseCard :: Char -> Card
parseCard '2' = Two
parseCard '3' = Three
parseCard '4' = Four
parseCard '5' = Five
parseCard '6' = Six
parseCard '7' = Seven
parseCard '8' = Eight
parseCard '9' = Nine
parseCard 'T' = T
parseCard 'J' = J
parseCard 'Q' = Q
parseCard 'K' = K
parseCard 'A' = A
parseCard _ = error "Invalid card"

determineHandType :: [Card] -> HandType
determineHandType cards
    | length (nub cards) == 5 = HighCard
    | length (nub cards) == 4 = OnePair
    | (length (nub cards) == 3) && maximum (map length (group $ sort cards)) == 3 = ThreeOfAKind
    | (length (nub cards) == 3) && maximum (map length (group $ sort cards)) == 2 = TwoPair
    | (length (nub cards) == 2) && maximum (map length (group $ sort cards)) == 3 = FullHouse
    | (length (nub cards) == 2) && maximum (map length (group $ sort cards)) == 4 = FourOfAKind
    | length (nub cards) == 1 = FiveOfAKind
    | otherwise = error "Invalid hand"

parseHand :: String -> Hand
parseHand handStr
    | length cards /= 5 = error "Invalid hand"
    | otherwise = Hand (determineHandType cards) cards
    where cards = map parseCard handStr

parseLine :: String -> (Hand, Integer)
parseLine line
    | length parts /= 2 = error "Invalid line"
    | otherwise = (parseHand $ head parts, read $ last parts)
    where parts = words line

day07 :: IO ()
day07 = do
    inputLines <- lines <$> (getDataFileName "day07-input.txt" >>= readFile)
    print inputLines
    let hands = map parseLine inputLines
    print $ sum [bid*i | (i, (_, bid)) <- zip [1..] $ sort hands]

main :: IO ()
main = day07