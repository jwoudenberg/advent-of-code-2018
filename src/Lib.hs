{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}

module Lib where

import Data.Char (toUpper, isUpper, isAlpha, toLower)
import Data.List (sort, sortOn)
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

day4part1 :: IO ()
day4part1 = do
  (Right records) <- parseFromFile (patrolRecParser `endBy` newline) "data/day4-input.txt"
  let (Right naps) = parse guardAsleepParser "" (sort records)
  let napTimesPerGuard = foldl (\reg nap -> Map.insertWith (+) (guardId nap) (duration nap) reg) mempty naps
  let chiefNapper = fst $ last $ sortOn snd $ Map.toList napTimesPerGuard
  let chiefNaps = filter ((==) chiefNapper . guardId) naps
  let napHistogram = foldl insertNaps mempty chiefNaps
  let mostNappedMinute = fst $ last $ sortOn snd $ Map.toList $ fmap length napHistogram
  putStrLn $ show $ chiefNapper * mostNappedMinute

day4part2 :: IO ()
day4part2 = do
  (Right records) <- parseFromFile (patrolRecParser `endBy` newline) "data/day4-input.txt"
  let (Right naps) = parse guardAsleepParser "" (sort records)
  let napTimesPerGuard = foldl insertNapsPerGuard mempty naps
  let (GuardNapped { guardId', nappedMinute }) = fst $ last $ sortOn snd $ Map.toList napTimesPerGuard
  putStrLn $ show $ guardId' * nappedMinute

insertNaps :: Map Int [Nap] -> Nap -> Map Int [Nap]
insertNaps histogram nap =
  foldl
    (\h t -> Map.insertWith (<>) t [nap] h)
    histogram
    [(minutes $ from nap)..((minutes $ to nap) - 1)]

insertNapsPerGuard :: Map GuardNapped Int -> Nap -> Map GuardNapped Int
insertNapsPerGuard histogram nap =
  foldl
    (\h t -> Map.insertWith (+) (GuardNapped (guardId nap) t) 1 h)
    histogram
    [(minutes $ from nap)..((minutes $ to nap) - 1)]

data GuardNapped =
  GuardNapped
    { guardId' :: Int
    , nappedMinute :: Int
    } deriving (Eq, Ord, Show)

patrolRecParser :: Parsec String () PatrolRec
patrolRecParser = do
  string "[1518-"
  months <- intParser
  char '-'
  days <- intParser
  space
  hours <- intParser
  char ':'
  minutes <- intParser
  char ']'
  space
  action <- guardActionParser
  let timestamp = Timestamp { months, days, hours, minutes }
  pure PatrolRec { timestamp, action }

guardActionParser :: Parsec String () GuardAction
guardActionParser =
  choice
    [ fmap BeginShift $ string "Guard #" *> intParser <* string " begins shift"
    , pure FallsAsleep <* string "falls asleep"
    , pure WakesUp <* string "wakes up"
    ]

guardAsleepParser :: Parsec [PatrolRec] () [Nap]
guardAsleepParser = fmap join $ many $ do
  id' <- beginShift
  many $ Nap id' <$> fallsAsleep <*> wakesUp


beginShift :: Parsec [PatrolRec] () Int
beginShift =
  tokenPrim show incSrcLine $ \rec ->
    case action rec of
      BeginShift id' -> Just id'
      _ -> Nothing

fallsAsleep :: Parsec [PatrolRec] () Timestamp
fallsAsleep =
  tokenPrim show incSrcLine $ \rec ->
    case action rec of
      FallsAsleep -> Just (timestamp rec)
      _ -> Nothing

wakesUp :: Parsec [PatrolRec] () Timestamp
wakesUp =
  tokenPrim show incSrcLine $ \rec ->
    case action rec of
      WakesUp -> Just (timestamp rec)
      _ -> Nothing

incSrcLine :: SourcePos -> t -> s -> SourcePos
incSrcLine pos _ _ = setSourceLine pos (1 + sourceLine pos)

data PatrolRec =
  PatrolRec
    { timestamp :: Timestamp
    , action :: GuardAction
    } deriving (Eq, Ord, Show)

data Timestamp = Timestamp
    { months :: Int
    , days :: Int
    , hours :: Int
    , minutes :: Int
    } deriving (Eq, Ord, Show)

printTimestamp :: Timestamp -> String
printTimestamp Timestamp { months, days, hours, minutes } =
  mconcat
    [ "["
    , show months
    , "-"
    , show days
    , "] "
    , show hours
    , ":"
    , show minutes
    ]

data GuardAction
  = BeginShift Int
  | FallsAsleep
  | WakesUp
  deriving (Eq, Ord, Show)

data Nap = Nap
  { guardId :: Int
  , from :: Timestamp
  , to :: Timestamp
  } deriving (Show)

duration :: Nap -> Int
duration Nap { to, from } =
  toMinutes to - toMinutes from

toMinutes :: Timestamp -> Int
toMinutes Timestamp { months, days, hours, minutes } = minutes + 60 * (hours + 24 * (days + 30 * months))

day5part1 :: IO ()
day5part1 = do
  polymer <- filter isAlpha <$> readFile "data/day5-input.txt"
  putStrLn $ show $ length $ react [] polymer

day5part2 :: IO ()
day5part2 = do
  polymer <- filter isAlpha <$> readFile "data/day5-input.txt"
  let shortened = zipWith shortenWith ['a'..'z'] (repeat polymer)
  putStrLn $ show $ snd $ head $ sortOn snd shortened

shortenWith :: Char -> String -> (Char, Int)
shortenWith letter = (,) letter . length . react [] . filter (not . (== letter) . toLower)

react :: String -> String -> String
react xs       []       = reverse xs
react []       (y : ys) = react [y] ys
react (x : xs) (y : ys) = if x == switchCase y
                            then react xs ys
                            else react (y : x : xs) ys

switchCase :: Char -> Char
switchCase c =
  if isUpper c then toLower c else toUpper c
