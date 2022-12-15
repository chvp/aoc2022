module Main (main) where

import Control.Monad ((<=<))
import Data.List (sort)
import Data.Maybe (mapMaybe)
import Lib (parseInput, readIntP)
import System.Environment (getArgs, getExecutablePath)
import Text.ParserCombinators.ReadP (ReadP, char, many, string)

type Distance = ((Int, Int), (Int, Int))

readDistanceP :: ReadP Distance
readDistanceP = do
  _ <- string "Sensor at x="
  x1 <- readIntP
  _ <- string ", y="
  y1 <- readIntP
  _ <- string ": closest beacon is at x="
  x2 <- readIntP
  _ <- string ", y="
  y2 <- readIntP
  _ <- char '\n'
  return ((x1, y1), (x2, y2))

readDistancesP :: ReadP [Distance]
readDistancesP = many readDistanceP

rangeOnLine :: Int -> Distance -> Maybe (Int, Int)
rangeOnLine l ((x1, y1), (x2, y2))
  | overlap = Just (x1 - vertToGo, x1 + vertToGo)
  | otherwise = Nothing
  where
    distance = abs (x1 - x2) + abs (y1 - y2)
    overlap = abs (l - y1) <= distance
    vertToGo = distance - abs (l - y1)

mergeOverlappingRanges :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
mergeOverlappingRanges acc [] = acc
mergeOverlappingRanges [] (x : xs) = mergeOverlappingRanges [x] xs
mergeOverlappingRanges ((sa, ea) : as) ((sx, ex) : xs)
  | ea >= sx - 1 = mergeOverlappingRanges ((sa, max ex ea) : as) xs
  | otherwise = mergeOverlappingRanges ((sx, ex) : (sa, ea) : as) xs

sumDifferences :: [(Int, Int)] -> Int
sumDifferences [] = 0
sumDifferences ((s, e) : xs) = e - s + sumDifferences xs

part1 :: String -> IO ()
part1 = print . sumDifferences . mergeOverlappingRanges [] . sort . mapMaybe (rangeOnLine 2000000) . parseInput readDistancesP <=< readFile

part2 :: String -> IO ()
part2 fn = do
  content <- readFile fn
  let distances = parseInput readDistancesP content
  let lineRanges = map (\y -> (y, mergeOverlappingRanges [] $ sort $ mapMaybe (rangeOnLine y) distances)) [0 .. 4000000]
  let (y, ranges) = head $ dropWhile (\(_, r) -> length r == 1 && fst (head r) <= 0 && snd (head r) >= 4000000) lineRanges
  let x
        | length ranges > 1 = fst (head ranges) - 1
        | fst (head ranges) == 1 = 0
        | otherwise = 4000000
  print $ x * 4000000 + y

_main :: [String] -> IO ()
_main [_, "1", fn] = part1 fn
_main [_, "2", fn] = part2 fn
_main _ = do
  path <- getExecutablePath
  putStrLn $ "usage: " ++ path ++ " _ 1|2 <inputfile>"

main :: IO ()
main = getArgs >>= _main
