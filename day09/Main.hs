module Main (main) where

import Control.Monad ((<=<))
import Data.Functor (($>))
import qualified Data.Set as Set
import Lib (parseInput, readIntP)
import System.Environment (getArgs, getExecutablePath)
import Text.ParserCombinators.ReadP (ReadP, char, endBy, (+++))

data Direction = L | U | R | D deriving (Eq, Show)

readLeftP :: ReadP Direction
readLeftP = char 'L' $> L

readUpP :: ReadP Direction
readUpP = char 'U' $> U

readRightP :: ReadP Direction
readRightP = char 'R' $> R

readDownP :: ReadP Direction
readDownP = char 'D' $> D

readDirectionP :: ReadP Direction
readDirectionP = readLeftP +++ readUpP +++ readRightP +++ readDownP

readInstructionP :: ReadP (Direction, Int)
readInstructionP = do
  d <- readDirectionP
  _ <- char ' '
  n <- readIntP
  return (d, n)

readInstructionsP :: ReadP [(Direction, Int)]
readInstructionsP = readInstructionP `endBy` char '\n'

touching :: (Int, Int) -> (Int, Int) -> Bool
touching (hx, hy) (tx, ty) = abs (hx - tx) <= 1 && abs (hy - ty) <= 1

move :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
move (hx, hy) ((x, y) : ps)
  | touching (hx, hy) (x, y) = (x, y) : ps
  | otherwise = (x', y') : move (x', y') ps
  where
    x'
      | x < hx = x + 1
      | x > hx = x - 1
      | otherwise = x
    y'
      | y < hy = y + 1
      | y > hy = y - 1
      | otherwise = y
move _ [] = []

simulateRope :: Set.Set (Int, Int) -> [(Int, Int)] -> [(Direction, Int)] -> Set.Set (Int, Int)
simulateRope acc _ [] = acc
simulateRope acc positions ((_, 0) : xs) = simulateRope acc positions xs
simulateRope acc ((hx, hy) : ps) ((d, n) : xs) = simulateRope acc' positions' ((d, n - 1) : xs)
  where
    acc' = Set.insert (last positions') acc
    positions' = (hx', hy') : move (hx', hy') ps
    hx'
      | d == L = hx - 1
      | d == R = hx + 1
      | otherwise = hx
    hy'
      | d == U = hy - 1
      | d == D = hy + 1
      | otherwise = hy
simulateRope _ _ _ = error "rope has to have at least an element"

part1 :: String -> IO ()
part1 = print . Set.size . simulateRope Set.empty [(0, 0), (0, 0)] . parseInput readInstructionsP <=< readFile

part2 :: String -> IO ()
part2 = print . Set.size . simulateRope Set.empty (replicate 10 (0, 0)) . parseInput readInstructionsP <=< readFile

_main :: [String] -> IO ()
_main [_, "1", fn] = part1 fn
_main [_, "2", fn] = part2 fn
_main _ = do
  path <- getExecutablePath
  putStrLn $ "usage: " ++ path ++ " _ 1|2 <inputfile>"

main :: IO ()
main = getArgs >>= _main
