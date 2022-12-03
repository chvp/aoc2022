module Main (main) where

import Control.Monad ((<=<))
import Data.Char (isUpper, ord)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import System.Environment (getArgs, getExecutablePath)

value :: Char -> Int
value c
  | isUpper c = ord c - ord 'A' + 27
  | otherwise = ord c - ord 'a' + 1

stringToSet :: String -> IntSet
stringToSet = IntSet.fromList . map value

commonElement :: String -> Int
commonElement s = IntSet.findMin $ IntSet.intersection (stringToSet s1) (stringToSet s2)
  where
    (s1, s2) = splitAt (length s `div` 2) s

commonBadgeSum :: [IntSet] -> Int
commonBadgeSum (x : y : z : xs) = intersection + commonBadgeSum xs
  where
    intersection = IntSet.findMin $ IntSet.intersection x $ IntSet.intersection y z
commonBadgeSum _ = 0

part1 :: String -> IO ()
part1 = print . sum . map commonElement . lines <=< readFile

part2 :: String -> IO ()
part2 = print . commonBadgeSum . map stringToSet . lines <=< readFile

_main :: [String] -> IO ()
_main [_, "1", fn] = part1 fn
_main [_, "2", fn] = part2 fn
_main _ = do
  path <- getExecutablePath
  putStrLn $ "usage: " ++ path ++ " _ 1|2 <inputfile>"

main :: IO ()
main = getArgs >>= _main
