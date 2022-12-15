module Main (main) where

import Control.Monad ((<=<))
import Data.Sequence (Seq, fromList, index, insertAt, update)
import qualified Data.Sequence as Sequence
import Lib (parseInput, readIntP)
import System.Environment (getArgs, getExecutablePath)
import Text.ParserCombinators.ReadP (ReadP, char, many, sepBy, string)

readCoordinateP :: ReadP (Int, Int)
readCoordinateP = do
  x <- readIntP
  _ <- char ','
  y <- readIntP
  return (x, y)

readPathP :: ReadP [(Int, Int)]
readPathP = readCoordinateP `sepBy` string " -> " <* char '\n'

readPathsP :: ReadP [[(Int, Int)]]
readPathsP = many readPathP

bounds :: [[(Int, Int)]] -> (Int, Int, Int, Int)
bounds [] = (500, 500, 0, 0) -- Sand is dropped from (500,0) so these coordinates need to be present
bounds (p : ps) = (min minXPs minXP - max maxYPs maxYP, max maxXP maxXPs + max maxYPs maxYP, min minYPs minYP, max maxYPs maxYP)
  where
    (minXPs, maxXPs, minYPs, maxYPs) = bounds ps
    minXP = minimum $ map fst p
    maxXP = maximum $ map fst p
    minYP = minimum $ map snd p
    maxYP = maximum $ map snd p

addHorizontalSection :: Seq (Seq Bool) -> Int -> (Int, Int) -> Seq (Seq Bool)
addHorizontalSection grid y (from, to) = update y (foldl (\s i -> update i True s) (index grid y) [min from to .. max from to]) grid

addVerticalSection :: Seq (Seq Bool) -> Int -> (Int, Int) -> Seq (Seq Bool)
addVerticalSection grid x (from, to) = foldl (\g i -> update i (update x True (index g i)) g) grid [min from to .. max from to]

addPathToGrid :: Int -> Int -> Seq (Seq Bool) -> [(Int, Int)] -> Seq (Seq Bool)
addPathToGrid xOffset yOffset grid ((x1, y1) : (x2, y2) : cs) = addPathToGrid xOffset yOffset updatedGrid ((x2, y2) : cs)
  where
    updatedGrid
      | x1 == x2 = addVerticalSection grid (x1 - xOffset) (y1 - yOffset, y2 - yOffset)
      | y1 == y2 = addHorizontalSection grid (y1 - yOffset) (x1 - xOffset, x2 - xOffset)
      | otherwise = error "diagonal path section encountered"
addPathToGrid _ _ grid _ = grid

makeGrid :: [[(Int, Int)]] -> (Int, Int, Seq (Seq Bool))
makeGrid ps = (minX, minY, foldl (addPathToGrid minX minY) initialGrid ps)
  where
    (minX, maxX, minY, maxY) = bounds ps
    initialGrid = fromList $ replicate (maxY - minY + 1) $ fromList $ replicate (maxX - minX + 1) False

addBottom :: (Int, Int, Seq (Seq Bool)) -> (Int, Int, Seq (Seq Bool))
addBottom (minX, minY, grid) = (minX, minY, grid'')
  where
    grid' = insertAt (Sequence.length grid) (fromList (replicate (Sequence.length (index grid 0)) False)) grid
    grid'' = insertAt (Sequence.length grid') (fromList (replicate (Sequence.length (index grid 0)) True)) grid'

dropSand :: Int -> Int -> Int -> Int -> (Bool, Seq (Seq Bool)) -> (Bool, Seq (Seq Bool))
dropSand baseX baseY x y (_, grid)
  | outOfBounds = (False, grid)
  | onTop = (False, grid)
  | downFree = dropSand baseX baseY x (y + 1) (True, grid)
  | leftFree = dropSand baseX baseY (x - 1) (y + 1) (True, grid)
  | rightFree = dropSand baseX baseY (x + 1) (y + 1) (True, grid)
  | otherwise = (True, update y (update x True (index grid y)) grid)
  where
    outOfBounds = y + 1 >= Sequence.length grid || (not downFree && x == 0) || (not downFree && not leftFree && x + 1 >= Sequence.length (index grid y))
    onTop = not downFree && not leftFree && not rightFree && x == baseX && y == baseY
    downFree = not (index (index grid (y + 1)) x)
    leftFree = not (index (index grid (y + 1)) (x - 1))
    rightFree = not (index (index grid (y + 1)) (x + 1))

part1 :: String -> IO ()
part1 = print . length . tail . takeWhile fst . (\(minX, minY, grid) -> iterate (dropSand (500 - minX) (0 - minY) (500 - minX) (0 - minY)) (True, grid)) . makeGrid . parseInput readPathsP <=< readFile

part2 :: String -> IO ()
part2 = print . length . takeWhile fst . (\(minX, minY, grid) -> iterate (dropSand (500 - minX) (0 - minY) (500 - minX) (0 - minY)) (True, grid)) . addBottom . makeGrid . parseInput readPathsP <=< readFile

_main :: [String] -> IO ()
_main [_, "1", fn] = part1 fn
_main [_, "2", fn] = part2 fn
_main _ = do
  path <- getExecutablePath
  putStrLn $ "usage: " ++ path ++ " _ 1|2 <inputfile>"

main :: IO ()
main = getArgs >>= _main
