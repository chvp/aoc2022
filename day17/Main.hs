module Main (main) where

import Data.Functor (($>))
import Data.Map (Map)
import qualified Data.Map.Lazy as Map
import Data.Maybe (fromJust, isJust)
import Data.Sequence (Seq, adjust, fromList, index, insertAt)
import qualified Data.Sequence as Seq
import Lib (parseInput)
import System.Environment (getArgs, getExecutablePath)
import Text.ParserCombinators.ReadP (ReadP, char, many, (+++))

data Move = L | R deriving (Show, Eq)

readLP :: ReadP Move
readLP = char '<' $> L

readRP :: ReadP Move
readRP = char '>' $> R

readMoveP :: ReadP Move
readMoveP = readLP +++ readRP

readMovesP :: ReadP (Seq Move)
readMovesP = do
  moves <- many readMoveP
  _ <- char '\n'
  return $ fromList moves

type Rock = [(Int, Int)]

rock1 :: Rock
rock1 = [(0, 0), (1, 0), (2, 0), (3, 0)]

rock2 :: Rock
rock2 = [(1, 0), (0, 1), (1, 1), (2, 1), (1, 2)]

rock3 :: Rock
rock3 = [(0, 0), (1, 0), (2, 0), (2, 1), (2, 2)]

rock4 :: Rock
rock4 = [(0, 0), (0, 1), (0, 2), (0, 3)]

rock5 :: Rock
rock5 = [(0, 0), (1, 0), (0, 1), (1, 1)]

rocks :: Seq Rock
rocks = fromList [rock1, rock2, rock3, rock4, rock5]

adjustRock :: Int -> Int -> Rock -> Rock
adjustRock xd yd = map (\(x, y) -> (x + xd, y + yd))

validPosition :: Seq (Seq Bool) -> Rock -> Bool
validPosition well rock = all ((\x -> x >= 0 && x < 7) . fst) rock && noOverlap
  where
    noOverlap = not $ any valueInWell $ filter ((< 0) . snd) rock
    valueInWell (x, y) = index (index well y') x
      where
        y' = negate $ y + 1

addToWell :: Seq (Seq Bool) -> Rock -> Seq (Seq Bool)
addToWell well r = well''
  where
    stickOut = maximum (0 : map ((+) 1 . snd) r)
    well' = foldl (\w _ -> insertAt 0 (fromList $ replicate 7 False) w) well [1 .. stickOut]
    well'' = foldr (\(x, y) -> adjust (adjust (const True) x) (negate y + stickOut - 1)) well' r

simulateRock :: Seq (Seq Bool) -> Rock -> Int -> Int -> Seq Move -> Int -> (Seq (Seq Bool), Int)
simulateRock well r xd yd moves mi
  | validPosition well mr' && validPosition well mr'' = simulateRock well r xd' yd' moves (mi + 1)
  | validPosition well mr' = (addToWell well mr', mi + 1)
  | validPosition well r'' = simulateRock well r xd yd' moves (mi + 1)
  | otherwise = (addToWell well r', mi + 1)
  where
    yd' = yd - 1
    xd'
      | m == L = xd - 1
      | otherwise = xd + 1
    r' = adjustRock xd yd r
    r'' = adjustRock xd yd' r
    mr' = adjustRock xd' yd r
    mr'' = adjustRock xd' yd' r
    m = index moves $ mi `mod` Seq.length moves

simulateRocks :: Int -> Map (Int, Int, Seq (Seq Bool)) (Int, Int) -> Seq (Seq Bool) -> Int -> Seq Move -> Int -> (Seq (Seq Bool), Int)
simulateRocks turns seen well ri moves mi
  | ri >= turns = (well, 0)
  | inMap && skipped > 0 = (fst $ simulateRocks turns seen well (ri + cycles * cycleLength) moves mi, skipped)
  | otherwise = simulateRocks turns seen' well' (ri + 1) moves mi'
  where
    (well', mi') = simulateRock well r 2 3 moves mi
    r = index rocks (ri `mod` Seq.length rocks)
    seen' = Map.insert key value seen
    key = (ri `mod` Seq.length rocks, mi `mod` Seq.length moves, Seq.take 10 well)
    value = (ri, Seq.length well)
    inMap = isJust $ Map.lookup key seen
    (previousTurn, previousHeight) = fromJust $ Map.lookup key seen
    cycleLength = ri - previousTurn
    cycles = (turns - ri) `div` cycleLength
    cycleHeight = Seq.length well - previousHeight
    skipped = cycles * cycleHeight

part :: Int -> String -> IO ()
part n fn = do
  content <- readFile fn
  let moves = parseInput readMovesP content
  let (well, skipped) = simulateRocks n Map.empty (fromList [fromList $ replicate 7 True]) 0 moves 0
  print $ skipped + Seq.length well - 1

part1 :: String -> IO ()
part1 = part 2022

part2 :: String -> IO ()
part2 = part 1000000000000

_main :: [String] -> IO ()
_main [_, "1", fn] = part1 fn
_main [_, "2", fn] = part2 fn
_main _ = do
  path <- getExecutablePath
  putStrLn $ "usage: " ++ path ++ " _ 1|2 <inputfile>"

main :: IO ()
main = getArgs >>= _main
