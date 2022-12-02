module Main (main) where

import System.Environment (getArgs, getExecutablePath)

data RPS = Rock | Paper | Scissors deriving (Eq)

instance Read RPS where
  readsPrec _ ('A' : rest) = [(Rock, rest)]
  readsPrec _ ('B' : rest) = [(Paper, rest)]
  readsPrec _ ('C' : rest) = [(Scissors, rest)]
  readsPrec _ ('X' : rest) = [(Rock, rest)]
  readsPrec _ ('Y' : rest) = [(Paper, rest)]
  readsPrec _ ('Z' : rest) = [(Scissors, rest)]
  readsPrec _ _ = []

data Result = Lose | Draw | Win

instance Read Result where
  readsPrec _ ('X' : rest) = [(Lose, rest)]
  readsPrec _ ('Y' : rest) = [(Draw, rest)]
  readsPrec _ ('Z' : rest) = [(Win, rest)]
  readsPrec _ _ = []

parseRound :: (Read a) => [String] -> (RPS, a)
parseRound [x, y] = (read x, read y)
parseRound _ = error "invalid input"

parseRounds :: (Read a) => String -> [(RPS, a)]
parseRounds = map (parseRound . words) . lines

forLoss :: RPS -> RPS
forLoss Rock = Scissors
forLoss Paper = Rock
forLoss Scissors = Paper

forWin :: RPS -> RPS
forWin = forLoss . forLoss

value :: RPS -> Int
value Rock = 1
value Paper = 2
value Scissors = 3

playRound1 :: (RPS, RPS) -> Int
playRound1 (x, y)
  | x == y = value y + 3
  | forWin x == y = value y + 6
  | otherwise = value y

playRound2 :: (RPS, Result) -> Int
playRound2 (x, Draw) = playRound1 (x, x)
playRound2 (x, Win) = playRound1 (x, forWin x)
playRound2 (x, Lose) = playRound1 (x, forLoss x)

part1 :: String -> IO ()
part1 filename = do
  rounds <- parseRounds <$> readFile filename
  print $ (sum . map playRound1) rounds

part2 :: String -> IO ()
part2 filename = do
  rounds <- parseRounds <$> readFile filename
  print $ (sum . map playRound2) rounds

_main :: [String] -> IO ()
_main [_, "1", fn] = part1 fn
_main [_, "2", fn] = part2 fn
_main _ = do
  path <- getExecutablePath
  putStrLn $ "usage: " ++ path ++ " _ 1|2 <inputfile>"

main :: IO ()
main = getArgs >>= _main
