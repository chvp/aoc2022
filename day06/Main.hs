module Main (main) where

import Control.Monad ((<=<))
import qualified Data.Set as Set
import System.Environment (getArgs, getExecutablePath)

allDifferent :: [Char] -> Bool
allDifferent xs = length xs == Set.size (Set.fromList xs)

firstNDifferentIndex :: Int -> Int -> [Char] -> Int
firstNDifferentIndex n i cs
  | allDifferent (take n cs) = i + n
  | otherwise = firstNDifferentIndex n (i + 1) (drop 1 cs)

part1 :: String -> IO ()
part1 = print . firstNDifferentIndex 4 0 <=< readFile

part2 :: String -> IO ()
part2 = print . firstNDifferentIndex 14 0 <=< readFile

_main :: [String] -> IO ()
_main [_, "1", fn] = part1 fn
_main [_, "2", fn] = part2 fn
_main _ = do
  path <- getExecutablePath
  putStrLn $ "usage: " ++ path ++ " _ 1|2 <inputfile>"

main :: IO ()
main = getArgs >>= _main
