module Main (main) where

import Data.Set (Set)
import qualified Data.Set as Set
import Lib (parseInput, readIntP)
import System.Environment (getArgs, getExecutablePath)
import Text.ParserCombinators.ReadP (ReadP, char, many)

type Cube = (Int, Int, Int)

readCubeP :: ReadP Cube
readCubeP = do
  x <- readIntP
  _ <- char ','
  y <- readIntP
  _ <- char ','
  z <- readIntP
  _ <- char '\n'
  return (x, y, z)

readCubesP :: ReadP [Cube]
readCubesP = many readCubeP

expandCube :: Cube -> [Cube]
expandCube (x, y, z) = [(x + 1, y, z), (x - 1, y, z), (x, y + 1, z), (x, y - 1, z), (x, y, z + 1), (x, y, z - 1)]

bounds :: [Cube] -> ((Int, Int), (Int, Int), (Int, Int))
bounds [(x, y, z)] = ((x - 1, x + 1), (y - 1, y + 1), (z - 1, z + 1))
bounds ((x, y, z) : cs) = ((min (x - 1) minx, max (x + 1) maxx), (min (y - 1) miny, max (y + 1) maxy), (min (z - 1) minz, max (z + 1) maxz))
  where
    ((minx, maxx), (miny, maxy), (minz, maxz)) = bounds cs
bounds [] = error "can't determine bounds if there are no elements"

fill :: Cube -> ((Int, Int), (Int, Int), (Int, Int)) -> Set Cube -> Set Cube -> Set Cube
fill curr bs lavaSet airSet = airSet'
  where
    ((minx, maxx), (miny, maxy), (minz, maxz)) = bs
    airSet'
      | Set.member curr airSet = airSet
      | otherwise = foldl (\acc nw -> fill nw bs lavaSet acc) (Set.insert curr airSet) toAdd
    toAdd = filter valid $ expandCube curr
    valid (x, y, z) = Set.notMember (x, y, z) lavaSet && x >= minx && x <= maxx && y >= miny && y <= maxy && z >= minz && z <= maxz

part1 :: String -> IO ()
part1 fn = do
  content <- readFile fn
  let cubes = parseInput readCubesP content
  let cubeSet = Set.fromList cubes
  let expanded = cubes >>= expandCube
  let filtered = filter (`Set.notMember` cubeSet) expanded
  print $ length filtered

part2 :: String -> IO ()
part2 fn = do
  content <- readFile fn
  let cubes = parseInput readCubesP content
  let cubeSet = Set.fromList cubes
  let bs = bounds cubes
  let airSet = fill (1, 1, 1) bs cubeSet Set.empty
  let expanded = cubes >>= expandCube
  let filtered = filter (`Set.member` airSet) expanded
  print $ length filtered

_main :: [String] -> IO ()
_main [_, "1", fn] = part1 fn
_main [_, "2", fn] = part2 fn
_main _ = do
  path <- getExecutablePath
  putStrLn $ "usage: " ++ path ++ " _ 1|2 <inputfile>"

main :: IO ()
main = getArgs >>= _main
