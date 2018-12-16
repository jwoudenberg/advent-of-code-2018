{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}

module Lib where

import Data.Map.Strict (Map)
import Text.Parsec
import Text.Parsec.String (parseFromFile)
import Debug.Trace (traceShowId)
import Control.Monad (join)
import Data.List (nub, (\\))
import Data.Maybe (listToMaybe, catMaybes)

import qualified Data.Map.Strict as Map

-- Day 1

day1 :: IO ()
day1 = do
  ops' <- fmap read' . lines <$> readFile "data/day1-input.txt"
  let ops = cycle ops'
  let freq = scanl (+) 0 ops
  let dups = firstDup <$> scanl (flip (:)) [] freq
  putStrLn . show . listToMaybe $ catMaybes dups

read' :: String -> Int
read' = read . dropWhile (=='+')

firstDup :: Eq a => [a] -> Maybe a
firstDup [] = Nothing
firstDup (x : xs) = if x `elem` xs then Just x else Nothing

-- Day 2

day2part1 :: IO ()
day2part1 = do
  ids <- lines <$> readFile "data/day2-input.txt"
  let has2count = length $ filter (hasExactly 2) ids
  let has3count = length $ filter (hasExactly 3) ids
  putStrLn $ show $ has2count * has3count

day2part2 :: IO ()
day2part2 = do
  ids <- lines <$> readFile "data/day2-input.txt"
  let pairs = combinations ids
  let similar = catMaybes $ fmap evalPair pairs
  putStrLn $ show $ listToMaybe $ similar

evalPair :: Eq a => ([a], [a]) -> Maybe [a]
evalPair (xs, ys) = if length common == length xs - 1 then Just common else Nothing
  where common = commonLetters xs ys

combinations :: [a] -> [(a, a)]
combinations [] = []
combinations (x : xs) = fmap ((,) x) xs <> combinations xs

hasExactly :: Eq a => Int -> [a] -> Bool
hasExactly n xs = any (\x -> n == (length $ filter (==x) xs)) (nub xs)

commonLetters :: Eq a => [a] -> [a] -> [a]
commonLetters xs ys = catMaybes $ zipWith (\x y -> if x == y then Just x else Nothing) xs ys

day3part1 :: IO ()
day3part1 = do
  (Right claims') <- parseFromFile (claimParser `endBy` newline) "data/day3-input.txt"
  let claims = foldl addClaim mempty claims'
  let overlapCount = Map.size $ Map.filter ((> 1) . length) claims
  putStrLn (show overlapCount)

day3part2 :: IO ()
day3part2 = do
  (Right claims') <- parseFromFile (claimParser `endBy` newline) "data/day3-input.txt"
  let claims = foldl addClaim mempty claims'
  let overlappingClaims = foldMap id $ Map.filter ((> 1) . length) claims
  let nonOverlappingClaim = listToMaybe $ claims' \\ nub overlappingClaims
  putStrLn $ show $ id' <$> nonOverlappingClaim

type Claims = Map (Int, Int) [Claim]

addClaim :: Claims -> Claim -> Claims
addClaim claims c@Claim {leftOffset, topOffset, width, height} =
  foldl insert claims coords
  where
    coords :: [(Int, Int)]
    coords = [(x, y)
                | x <- [(leftOffset + 1)..(leftOffset + width)]
                , y <- [(topOffset + 1)..(topOffset + height)]
             ]
    insert :: Claims -> (Int, Int) ->  Claims
    insert claims' key = Map.insertWith (<>) key [c] claims'


claimParser :: Parsec String () Claim
claimParser = do
  char '#'
  id' <- intParser
  space
  char '@'
  space
  leftOffset <- intParser
  char ','
  topOffset <- intParser
  char ':'
  space
  width <- intParser
  char 'x'
  height <- intParser
  pure $ Claim { id', leftOffset, topOffset, width, height }

intParser :: Parsec String () Int
intParser = read <$> many digit

data Claim = Claim
  { id' :: Int
  , leftOffset :: Int
  , topOffset :: Int
  , width :: Int
  , height :: Int
  } deriving (Eq, Show)
