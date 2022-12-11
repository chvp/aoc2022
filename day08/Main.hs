module Main (main) where

import Control.Monad ((<=<))
import Data.Char (digitToInt)
import Data.Ix (range)
import Data.Sequence (Seq, fromList, index, mapWithIndex, update)
import qualified Data.Sequence as Sequence
import System.Environment (getArgs, getExecutablePath)

readGrid :: [[Char]] -> Seq (Seq Int)
readGrid = fromList . map (fromList . map digitToInt)

trace :: Seq (Seq Int) -> Seq (Seq Bool) -> (Int, Int) -> (Int, Int) -> Int -> Seq (Seq Bool)
trace trees visibility (x, y) (xd, yd) h =
  ( if notAtEnd
      then
        ( if higher
            then trace trees updatedVisibility (x + xd, y + yd) (xd, yd) currentTree
            else trace trees visibility (x + xd, y + yd) (xd, yd) h
        )
      else
        ( if higher
            then updatedVisibility
            else visibility
        )
  )
  where
    notAtEnd = x + xd >= 0 && x + xd < Sequence.length (index trees y) && y + yd >= 0 && y + yd < Sequence.length trees
    currentTree = index (index trees y) x
    higher = index (index trees y) x > h
    updatedVisibility = update y (update x True (index visibility y)) visibility

countVisible :: Seq (Seq Int) -> Int
countVisible trees = sum $ fmap (Sequence.length . Sequence.filter id) visibility''''
  where
    visibility = fromList $ replicate (Sequence.length trees) (fromList $ replicate (Sequence.length $ index trees 0) False)
    visibility' = foldl (\v idx -> trace trees v (0, idx) (1, 0) (-1)) visibility (range (0, Sequence.length trees - 1))
    visibility'' = foldl (\v idx -> trace trees v (Sequence.length (index trees 0) - 1, idx) (-1, 0) (-1)) visibility' (range (0, Sequence.length trees - 1))
    visibility''' = foldl (\v idx -> trace trees v (idx, 0) (0, 1) (-1)) visibility'' (range (0, Sequence.length (index trees 0) - 1))
    visibility'''' = foldl (\v idx -> trace trees v (idx, Sequence.length trees - 1) (0, -1) (-1)) visibility''' (range (0, Sequence.length (index trees 0) - 1))

scenicScore :: Seq (Seq Int) -> Int -> Int -> Int
scenicScore trees x y = up * left * down * right
  where
    up = length (takeWhile (\idx -> idx == y || index (index trees idx) x < currentTree) (reverse (range (1, y))))
    down = length (takeWhile (\idx -> idx == y || index (index trees idx) x < currentTree) (range (y, Sequence.length trees - 2)))
    left = length (takeWhile (\idx -> idx == x || index (index trees y) idx < currentTree) (reverse (range (1, x))))
    right = length (takeWhile (\idx -> idx == x || index (index trees y) idx < currentTree) (range (x, Sequence.length (index trees 0) - 2)))
    currentTree = index (index trees y) x

part1 :: String -> IO ()
part1 = print . countVisible . readGrid . lines <=< readFile

part2 :: String -> IO ()
part2 fn = do
  content <- readFile fn
  let trees = readGrid $ lines content
  let scores = mapWithIndex (\x -> mapWithIndex (\y _ -> scenicScore trees x y)) trees
  let m = maximum $ fmap maximum scores
  print m

_main :: [String] -> IO ()
_main [_, "1", fn] = part1 fn
_main [_, "2", fn] = part2 fn
_main _ = do
  path <- getExecutablePath
  putStrLn $ "usage: " ++ path ++ " _ 1|2 <inputfile>"

main :: IO ()
main = getArgs >>= _main
