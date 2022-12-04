module Main (main) where

import Control.Monad ((<=<))
import Lib (readIntP)
import System.Environment (getArgs, getExecutablePath)
import Text.ParserCombinators.ReadP (ReadP, char, endBy, eof, readP_to_S)

type Range = (Int, Int)

type RangePair = (Range, Range)

readRangeP :: ReadP Range
readRangeP = do
  n1 <- readIntP
  _ <- char '-'
  n2 <- readIntP
  return (n1, n2)

readRangePairP :: ReadP RangePair
readRangePairP = do
  p1 <- readRangeP
  _ <- char ','
  p2 <- readRangeP
  return (p1, p2)

readRangePairsP :: ReadP [RangePair]
readRangePairsP = readRangePairP `endBy` char '\n'

parseRangePairs :: String -> [RangePair]
parseRangePairs = fst . head . readP_to_S (readRangePairsP <* eof)

isContainedIn :: Range -> Range -> Bool
isContainedIn (x1, x2) (y1, y2) = x1 <= y1 && x2 >= y2

undirectedContains :: RangePair -> Bool
undirectedContains (p1, p2) = isContainedIn p1 p2 || isContainedIn p2 p1

overlap :: RangePair -> Bool
overlap ((x1, x2), (y1, y2)) = not (y1 > x2 || y2 < x1)

part1 :: String -> IO ()
part1 = print . length . filter undirectedContains . parseRangePairs <=< readFile

part2 :: String -> IO ()
part2 = print . length . filter overlap . parseRangePairs <=< readFile

_main :: [String] -> IO ()
_main [_, "1", fn] = part1 fn
_main [_, "2", fn] = part2 fn
_main _ = do
  path <- getExecutablePath
  putStrLn $ "usage: " ++ path ++ " _ 1|2 <inputfile>"

main :: IO ()
main = getArgs >>= _main
