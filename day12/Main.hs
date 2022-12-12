{-# LANGUAGE TupleSections #-}

module Main (main) where

import Control.Monad ((<=<))
import Data.Char (ord)
import Data.Maybe (catMaybes, fromJust, isNothing)
import Data.Sequence (Seq, elemIndexL, fromList, index, mapWithIndex, update)
import qualified Data.Sequence as Sequence
import Data.Tuple (swap)
import System.Environment (getArgs, getExecutablePath)

traversable :: Char -> Char -> Bool
traversable x y
  | x == 'S' = True
  | y == 'E' = ord x + 1 >= ord 'z'
  | otherwise = ord x + 1 >= ord y

updateDistance :: Seq (Seq Char) -> (Int, Int) -> (Char -> Char -> Int -> Int) -> (Seq (Seq (Maybe Int)), [(Int, Int)]) -> (Int, Int) -> (Seq (Seq (Maybe Int)), [(Int, Int)])
updateDistance grid (fx, fy) distF (dists, queue) (tx, ty)
  | canMove && isNothing curDist = (dists', queue')
  | otherwise = (dists, queue)
  where
    fromDist = fromJust $ index (index dists fy) fx
    curDist = index (index dists ty) tx
    from = index (index grid fy) fx
    to = index (index grid ty) tx
    newDist = distF from to fromDist
    inBounds = ty >= 0 && ty < Sequence.length grid && tx >= 0 && tx < Sequence.length (index grid ty)
    canMove = inBounds && traversable from to
    dists' = update ty (update tx (Just newDist) (index dists ty)) dists
    queue' = if newDist == 0 then (tx, ty) : queue else queue ++ [(tx, ty)]

dijkstra :: (Char -> Char -> Int -> Int) -> (Int, Int) -> Seq (Seq Char) -> Seq (Seq (Maybe Int)) -> [(Int, Int)] -> Int
dijkstra distF goal grid dists ((x, y) : qs)
  | goal == (x, y) = curDist
  | otherwise = dijkstra distF goal grid dists' queue'
  where
    curDist = fromJust $ index (index dists y) x
    (dists', queue') = foldl (updateDistance grid (x, y) distF) (dists, qs) [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
dijkstra _ _ _ _ _ = error "no path found"

pathLength :: (Char -> Char -> Int -> Int) -> Seq (Seq Char) -> Int
pathLength distF grid = dijkstra distF e grid (fmap (fmap (\c -> if c == 'S' then Just 0 else Nothing)) grid) [s]
  where
    s = swap $ findPos 'S'
    e = swap $ findPos 'E'
    findPos el = head $ catMaybes $ foldr (:) [] $ mapWithIndex (\i -> fmap (i,) . elemIndexL el) grid

simpleDist :: Char -> Char -> Int -> Int
simpleDist _ _ i = i + 1

flatDist :: Char -> Char -> Int -> Int
flatDist x y i
  | (x == 'a' || x == 'S') && y == 'a' && i == 0 = 0
  | otherwise = i + 1

part1 :: String -> IO ()
part1 = print . pathLength simpleDist . fromList . map fromList . lines <=< readFile

part2 :: String -> IO ()
part2 = print . pathLength flatDist . fromList . map fromList . lines <=< readFile

_main :: [String] -> IO ()
_main [_, "1", fn] = part1 fn
_main [_, "2", fn] = part2 fn
_main _ = do
  path <- getExecutablePath
  putStrLn $ "usage: " ++ path ++ " _ 1|2 <inputfile>"

main :: IO ()
main = getArgs >>= _main
