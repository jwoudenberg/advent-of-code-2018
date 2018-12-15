{-# LANGUAGE ScopedTypeVariables #-}
module Lib where

import Debug.Trace (traceShowId)
import Control.Monad (join)
import Data.List (nub, (\\))
import Data.Maybe (listToMaybe, catMaybes)

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
