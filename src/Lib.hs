module Lib (splitOn, readIntP, parseInput) where

import Data.Char (isDigit)
import Text.ParserCombinators.ReadP (ReadP, char, choice, eof, many1, readP_to_S, satisfy)

_splitOn :: (Eq a) => a -> [a] -> [a] -> [[a]]
_splitOn el (x : xs) acc
  | x == el = reverse acc : _splitOn el xs []
  | otherwise = _splitOn el xs (x : acc)
_splitOn _ _ acc = [reverse acc]

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn el xs = _splitOn el xs []

readNegIntP :: ReadP Int
readNegIntP = negate . read <$> (char '-' *> many1 (satisfy isDigit))

readPosIntP :: ReadP Int
readPosIntP = read <$> many1 (satisfy isDigit)

readIntP :: ReadP Int
readIntP = choice [readNegIntP, readPosIntP]

parseInput :: ReadP a -> String -> a
parseInput parser = justOne . readP_to_S (parser <* eof)
  where
    justOne [(x, _)] = x
    justOne (_ : _) = error "ambiguous parse"
    justOne [] = error "no parse"
