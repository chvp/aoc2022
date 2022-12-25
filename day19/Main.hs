module Main (main) where

import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Lib (parseInput, readIntP)
import System.Environment (getArgs, getExecutablePath)
import Text.ParserCombinators.ReadP (ReadP, char, endBy, string)

data Blueprint = Blueprint {getId :: Int, getOre :: Int, getClay :: Int, getObsidian :: (Int, Int), getGeode :: (Int, Int)} deriving (Show)

readBlueprintP :: ReadP Blueprint
readBlueprintP = do
  _ <- string "Blueprint "
  bid <- readIntP
  _ <- string ": "
  _ <- string "Each ore robot costs "
  oreOre <- readIntP
  _ <- string " ore. Each clay robot costs "
  clayOre <- readIntP
  _ <- string " ore. Each obsidian robot costs "
  obsOre <- readIntP
  _ <- string " ore and "
  obsClay <- readIntP
  _ <- string " clay. Each geode robot costs "
  geoOre <- readIntP
  _ <- string " ore and "
  geoObs <- readIntP
  _ <- string " obsidian."
  return $ Blueprint {getId = bid, getOre = oreOre, getClay = clayOre, getObsidian = (obsOre, obsClay), getGeode = (geoOre, geoObs)}

readBlueprintsP :: ReadP [Blueprint]
readBlueprintsP = readBlueprintP `endBy` char '\n'

buildOre :: Blueprint -> (Int, Int, Int, Int) -> (Int, Int, Int, Int) -> Maybe ((Int, Int, Int, Int), (Int, Int, Int, Int))
buildOre bp (rOre, rClay, rObs, rGeo) (ore, clay, obs, geo)
  | rOre >= maximum [getOre bp, getClay bp, fst $ getObsidian bp, fst $ getGeode bp] = Nothing
  | ore >= getOre bp = Just ((rOre + 1, rClay, rObs, rGeo), (ore - getOre bp, clay, obs, geo))
  | otherwise = Nothing

buildClay :: Blueprint -> (Int, Int, Int, Int) -> (Int, Int, Int, Int) -> Maybe ((Int, Int, Int, Int), (Int, Int, Int, Int))
buildClay bp (rOre, rClay, rObs, rGeo) (ore, clay, obs, geo)
  | rClay >= snd (getObsidian bp) = Nothing
  | ore >= getClay bp = Just ((rOre, rClay + 1, rObs, rGeo), (ore - getClay bp, clay, obs, geo))
  | otherwise = Nothing

buildObs :: Blueprint -> (Int, Int, Int, Int) -> (Int, Int, Int, Int) -> Maybe ((Int, Int, Int, Int), (Int, Int, Int, Int))
buildObs bp (rOre, rClay, rObs, rGeo) (ore, clay, obs, geo)
  | rObs >= snd (getGeode bp) = Nothing
  | ore >= fst (getObsidian bp) && clay >= snd (getObsidian bp) = Just ((rOre, rClay, rObs + 1, rGeo), (ore - fst (getObsidian bp), clay - snd (getObsidian bp), obs, geo))
  | otherwise = Nothing

buildGeo :: Blueprint -> (Int, Int, Int, Int) -> (Int, Int, Int, Int) -> Maybe ((Int, Int, Int, Int), (Int, Int, Int, Int))
buildGeo bp (rOre, rClay, rObs, rGeo) (ore, clay, obs, geo)
  | ore >= fst (getGeode bp) && obs >= snd (getGeode bp) = Just ((rOre, rClay, rObs, rGeo + 1), (ore - fst (getGeode bp), clay, obs - snd (getGeode bp), geo))
  | otherwise = Nothing

findMax :: Blueprint -> Int -> Int -> Set (Int, (Int, Int, Int, Int), (Int, Int, Int, Int)) -> (Int, Int, Int, Int) -> (Int, Int, Int, Int) -> (Int, Set (Int, (Int, Int, Int, Int), (Int, Int, Int, Int)))
findMax _ 0 _ seen _ (_, _, _, geo) = (geo, seen)
findMax bp t currMax seen rs@(rOre, rClay, rObs, rGeo) os@(ore, _, obs, geo)
  | Set.member (t, rs, os) seen = (currMax, seen)
  | impossible = (currMax, seen)
  | onlyGeo = (geo + t * rGeo + ((t * (t - 1)) `div` 2), seen)
  | otherwise = (currMax'', Set.insert (t, rs, os) seen'')
  where
    validBuys = mapMaybe (\f -> f bp rs os) [buildGeo, buildObs, buildClay, buildOre, \_ rs' os' -> Just (rs', os')]
    (currMax'', seen'') = foldl step (currMax, seen) validBuys
    step (currMax', seen') (rs', (ore', clay', obs', geo')) = (max currMax' newMax, newSeen)
      where
        (newMax, newSeen) = findMax bp (t - 1) currMax' seen' rs' (ore' + rOre, clay' + rClay, obs' + rObs, geo' + rGeo)
    impossible = t * rGeo + ((t * (t - 1)) `div` 2) + geo < currMax
    onlyGeo = rOre > fst (getGeode bp) && rObs > snd (getGeode bp) && ore > fst (getGeode bp) && obs > snd (getGeode bp)

qualityLevel :: Blueprint -> Int
qualityLevel bp = getId bp * m
  where
    m = fst $ findMax bp 24 0 Set.empty (1, 0, 0, 0) (0, 0, 0, 0)

part1 :: String -> IO ()
part1 fn = do
  content <- readFile fn
  let blueprints = parseInput readBlueprintsP content
  print $ sum $ map qualityLevel blueprints

part2 :: String -> IO ()
part2 fn = do
  content <- readFile fn
  let blueprints = take 3 $ parseInput readBlueprintsP content
  print $ product $ map (\bp -> fst $ findMax bp 32 0 Set.empty (1, 0, 0, 0) (0, 0, 0, 0)) blueprints

_main :: [String] -> IO ()
_main [_, "1", fn] = part1 fn
_main [_, "2", fn] = part2 fn
_main _ = do
  path <- getExecutablePath
  putStrLn $ "usage: " ++ path ++ " _ 1|2 <inputfile>"

main :: IO ()
main = getArgs >>= _main
