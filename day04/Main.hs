module Main (main) where

import Control.Monad ((<=<))
import Data.Char (isDigit)
import System.Environment (getArgs, getExecutablePath)
import Text.ParserCombinators.ReadP (ReadP, char, eof, many1, readP_to_S, satisfy)

readIntP :: ReadP Int
readIntP = read <$> many1 (satisfy isDigit)

readRangeP :: ReadP (Int, Int)
readRangeP = do
  n1 <- readIntP
  _ <- char '-'
  n2 <- readIntP
  return (n1, n2)

readRangePairP :: ReadP ((Int, Int), (Int, Int))
readRangePairP = do
  p1 <- readRangeP
  _ <- char ','
  p2 <- readRangeP
  eof
  return (p1, p2)

parseRangePair :: String -> ((Int, Int), (Int, Int))
parseRangePair = fst . head . readP_to_S readRangePairP

parseRangePairs :: String -> [((Int, Int), (Int, Int))]
parseRangePairs = map parseRangePair . lines

isContainedIn :: (Int, Int) -> (Int, Int) -> Bool
isContainedIn (x1, x2) (y1, y2) = x1 <= y1 && x2 >= y2

undirectedContains :: ((Int, Int), (Int, Int)) -> Bool
undirectedContains (p1, p2) = isContainedIn p1 p2 || isContainedIn p2 p1

overlap :: ((Int, Int), (Int, Int)) -> Bool
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
