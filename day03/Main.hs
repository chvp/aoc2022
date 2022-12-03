module Main (main) where

import Data.Char (isUpper, ord)
import qualified Data.IntSet as IntSet
import System.Environment (getArgs, getExecutablePath)

value :: Char -> Int
value c
  | isUpper c = ord c - ord 'A' + 27
  | otherwise = ord c - ord 'a' + 1

commonElement :: String -> Int
commonElement s = IntSet.findMin $ IntSet.intersection (IntSet.fromList (take len items)) (IntSet.fromList (drop len items))
  where
    items = map value s
    len = length items `div` 2

commonBadgeSum :: [String] -> Int
commonBadgeSum (x : y : z : xs) = IntSet.findMin (intersect3 (toSet x) (toSet y) (toSet z)) + commonBadgeSum xs
  where
    toSet s = IntSet.fromList $ map value s
    intersect3 s1 s2 s3 = IntSet.intersection s1 $ IntSet.intersection s2 s3
commonBadgeSum _ = 0

part1 :: String -> IO ()
part1 filename = do
  elements <- map commonElement . lines <$> readFile filename
  print $ sum elements

part2 :: String -> IO ()
part2 filename = do
  bags <- lines <$> readFile filename
  print $ commonBadgeSum bags

_main :: [String] -> IO ()
_main [_, "1", fn] = part1 fn
_main [_, "2", fn] = part2 fn
_main _ = do
  path <- getExecutablePath
  putStrLn $ "usage: " ++ path ++ " _ 1|2 <inputfile>"

main :: IO ()
main = getArgs >>= _main
