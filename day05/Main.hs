module Main (main) where

import Control.Monad ((<=<))
import Data.Foldable (toList)
import Data.List (transpose)
import Data.Maybe (catMaybes)
import Data.Sequence (fromList, index, update, Seq)
import Lib (readIntP)
import System.Environment (getArgs, getExecutablePath)
import Text.ParserCombinators.ReadP (ReadP, char, count, endBy, eof, get, many, readP_to_S, satisfy, sepBy, string)

data Move = Move { amount :: Int, from :: Int, to :: Int } deriving Show

readMoveP :: ReadP Move
readMoveP = do
  _ <- string "move "
  am <- readIntP
  _ <- string " from "
  f <- readIntP
  _ <- string " to "
  t <- readIntP
  -- Subtract 1 from "from" and "to", to have normal indices
  return $ Move { amount = am, from = f - 1, to = t - 1 }

readContainerP :: ReadP (Maybe Char)
readContainerP = do
  _ <- get -- Either [ or a space
  item <- get
  _ <- get -- Either ] or a space
  return $ if item /= ' ' then Just item else Nothing

readLevelP :: ReadP [Maybe Char]
readLevelP = readContainerP `sepBy` char ' '

readLevelsP :: ReadP [[Maybe Char]]
readLevelsP = readLevelP `endBy` char '\n'

readInputP :: ReadP (Seq [Char], [Move])
readInputP = do
  stacks <- map catMaybes . transpose <$> readLevelsP
  _ <- many $ satisfy (/= '\n') -- Skip a line
  _ <- count 2 $ char '\n' -- 2 newlines at the end of the stacks
  moves <- readMoveP `endBy` char '\n'
  return (fromList stacks, moves)

parseInput :: String -> (Seq [Char], [Move])
parseInput = fst . head . readP_to_S (readInputP <* eof)

doMove :: Seq [Char] -> Move -> Seq [Char]
doMove state Move { amount = am, from = f, to = t } = state''
  where
    state' = update f (drop am $ index state f) state
    state'' = update t (reverse (take am (index state f)) ++ (index state t)) state'

doMove' :: Seq [Char] -> Move -> Seq [Char]
doMove' state Move { amount = am, from = f, to = t } = state''
  where
    state' = update f (drop am $ index state f) state
    state'' = update t (take am (index state f) ++ (index state t)) state'

doMoves :: (Seq [Char], [Move]) -> Seq [Char]
doMoves (state, moves) = foldl doMove state moves

doMoves' :: (Seq [Char], [Move]) -> Seq [Char]
doMoves' (state, moves) = foldl doMove' state moves

part1 :: String -> IO ()
part1 = print . map head . toList . doMoves . parseInput <=< readFile

part2 :: String -> IO ()
part2 = print . map head . toList . doMoves' . parseInput <=< readFile

_main :: [String] -> IO ()
_main [_, "1", fn] = part1 fn
_main [_, "2", fn] = part2 fn
_main _ = do
  path <- getExecutablePath
  putStrLn $ "usage: " ++ path ++ " _ 1|2 <inputfile>"

main :: IO ()
main = getArgs >>= _main
