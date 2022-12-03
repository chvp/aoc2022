module Main (main) where

import Control.Monad ((<=<))
import Lib (splitOn)
import System.Environment (getArgs, getExecutablePath)

parseElves :: String -> [Int]
parseElves = map (sum . map read) . splitOn "" . lines

max3Sum :: (Num a, Ord a) => a -> a -> a -> [a] -> a
max3Sum a b c (x : xs)
  | x > a = max3Sum x a b xs
  | x > b = max3Sum a x b xs
  | x > c = max3Sum a b x xs
  | otherwise = max3Sum a b c xs
max3Sum a b c _ = a + b + c

part1 :: String -> IO ()
part1 = print . maximum . parseElves <=< readFile

part2 :: String -> IO ()
part2 = print . max3Sum 0 0 0 . parseElves <=< readFile

_main :: [String] -> IO ()
_main [_, "1", fn] = part1 fn
_main [_, "2", fn] = part2 fn
_main _ = do
  path <- getExecutablePath
  putStrLn $ "usage: " ++ path ++ " _ 1|2 <inputfile>"

main :: IO ()
main = getArgs >>= _main
