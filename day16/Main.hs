module Main (main) where

import Data.Foldable (foldl', toList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Lib (parseInput, readIntP)
import System.Environment (getArgs, getExecutablePath)
import Text.ParserCombinators.ReadP (ReadP, char, endBy, many, satisfy, sepBy, string, (+++))

data Valve = Valve {getFlow :: Int, getRoutes :: [String]} deriving (Show)

readValveP :: ReadP (String, Valve)
readValveP = do
  _ <- string "Valve "
  name <- many $ satisfy (`elem` ['A' .. 'Z'])
  _ <- string " has flow rate="
  flow <- readIntP
  _ <- string "; tunnels lead to valves " +++ string "; tunnel leads to valve "
  routes <- many (satisfy (`elem` ['A' .. 'Z'])) `sepBy` string ", "
  return (name, Valve {getFlow = flow, getRoutes = routes})

readValvesP :: ReadP (Map String Valve)
readValvesP = do
  valves <- readValveP `endBy` char '\n'
  return $ Map.fromList valves

openValves :: Int -> Set String -> Map (String, String) Int -> String -> Int -> Set String -> Map String Valve -> Int
openValves time useful dists from released opened valves
  | useful == opened = released + (30 - time) * currentRelease
  | null options = released + (30 - time) * currentRelease
  | otherwise = maximum options
  where
    options = map step $ filter (\to -> time + fromJust (Map.lookup (from, to) dists) + 1 < 30) $ toList $ Set.difference useful opened
    step to = openValves (time + dist + 1) useful dists to (released + currentRelease * (dist + 1)) (Set.insert to opened) valves
      where
        dist = fromJust $ Map.lookup (from, to) dists
    currentRelease = foldl' (\acc v -> acc + getFlow (fromJust $ Map.lookup v valves)) 0 opened

openValvesTogether :: Int -> Set String -> Map (String, String) Int -> (String, String) -> (Int, Int) -> Int -> Int -> Set String -> Map String Valve -> Int
openValvesTogether time useful dists (iFrom, eFrom) (iWait, eWait) released currentMax opened valves
  | useful == opened = released + (26 - time) * currentRelease
  | highestPossible <= currentMax = currentMax
  | otherwise = newMaximum
  where
    toOpen = Set.difference useful opened
    newMaximum = foldl' (\m -> max m . step m) currentMax toOpen
    step m to
      | iDist < 0 && eDist < 0 = error "this should never happen"
      | time + iDist >= 26 && time + eDist >= 26 = released + (26 - time) * currentRelease
      | iDist < 0 = eresult
      | eDist < 0 = iresult
      | otherwise = max iresult eresult
      where
        iDist = fromJust (Map.lookup (iFrom, to) dists) - iWait + 1
        eDist = fromJust (Map.lookup (eFrom, to) dists) - eWait + 1
        iresult = openValvesTogether (time + iDist) useful dists (to, eFrom) (0, iDist + eWait) (released + currentRelease * iDist) m (Set.insert to opened) valves
        eresult = openValvesTogether (time + eDist) useful dists (iFrom, to) (eDist + iWait, 0) (released + currentRelease * eDist) m (Set.insert to opened) valves
    currentRelease = foldl' (\acc v -> acc + getFlow (fromJust $ Map.lookup v valves)) 0 opened
    highestPossible = released + (26 - time) * currentRelease + foldl' (\s v -> s + highestForValve v) 0 toOpen
    highestForValve v = (26 - time) * getFlow (fromJust $ Map.lookup v valves)

usefulValves :: Map String Valve -> Set String
usefulValves = Set.fromList . map fst . filter (\(_, v) -> getFlow v > 0) . Map.toList

distance :: Map String Valve -> [String] -> String -> String -> Maybe Int
distance valves path from to
  | from == to = Just 0
  | null options = Nothing
  | otherwise = Just $ minimum options
  where
    currentValve = fromJust $ Map.lookup from valves
    options = mapMaybe (\s -> fmap (+ 1) (distance valves (from : path) s to)) $ filter (`notElem` path) $ getRoutes currentValve

distances :: Map String Valve -> Map (String, String) Int
distances valves = Map.fromList $ map (\(from, to) -> ((from, to), fromJust $ distance valves [] from to)) $ toList $ Set.cartesianProduct (Set.insert "AA" useful) useful
  where
    useful = usefulValves valves

part1 :: String -> IO ()
part1 fn = do
  content <- readFile fn
  let valves = parseInput readValvesP content
  let dists = distances valves
  print $ openValves 0 (usefulValves valves) dists "AA" 0 Set.empty valves

part2 :: String -> IO ()
part2 fn = do
  content <- readFile fn
  let valves = parseInput readValvesP content
  let dists = distances valves
  print $ openValvesTogether 0 (usefulValves valves) dists ("AA", "AA") (0, 0) 0 0 Set.empty valves

_main :: [String] -> IO ()
_main [_, "1", fn] = part1 fn
_main [_, "2", fn] = part2 fn
_main _ = do
  path <- getExecutablePath
  putStrLn $ "usage: " ++ path ++ " _ 1|2 <inputfile>"

main :: IO ()
main = getArgs >>= _main
