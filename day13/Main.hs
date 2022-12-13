module Main (main) where

import Control.Monad ((<=<))
import Data.List (elemIndex, sort)
import Data.Maybe (fromJust)
import Lib (parseInput, readIntP)
import System.Environment (getArgs, getExecutablePath)
import Text.ParserCombinators.ReadP (ReadP, char, endBy, sepBy, string, (+++))

data Node = SimpleNode Int | ListNode [Node] deriving (Eq, Show)

instance Ord Node where
  compare (SimpleNode n1) (SimpleNode n2) = compare n1 n2
  compare (ListNode l1) (ListNode l2) = compare l1 l2
  compare n1 (ListNode l2) = compare [n1] l2
  compare (ListNode l1) n2 = compare l1 [n2]

readSimpleNodeP :: ReadP Node
readSimpleNodeP = SimpleNode <$> readIntP

readListNodeP :: ReadP Node
readListNodeP = do
  _ <- char '['
  items <- readNodeP `sepBy` char ','
  _ <- char ']'
  return $ ListNode items

readNodeP :: ReadP Node
readNodeP = readSimpleNodeP +++ readListNodeP

readPacketPairP :: ReadP (Node, Node)
readPacketPairP = do
  p1 <- readNodeP <* char '\n'
  p2 <- readNodeP <* char '\n'
  return (p1, p2)

readPacketPairsP :: ReadP [(Node, Node)]
readPacketPairsP = readPacketPairP `sepBy` char '\n'

readPacketsP :: ReadP [Node]
readPacketsP = readNodeP `endBy` (string "\n" +++ string "\n\n")

sumTrueIndices :: Int -> [Bool] -> Int
sumTrueIndices i (True : xs) = i + sumTrueIndices (i + 1) xs
sumTrueIndices i (_ : xs) = sumTrueIndices (i + 1) xs
sumTrueIndices _ _ = 0

divider1 :: Node
divider1 = ListNode [ListNode [SimpleNode 2]]

divider2 :: Node
divider2 = ListNode [ListNode [SimpleNode 6]]

findDividerIndices :: [Node] -> (Int, Int)
findDividerIndices l = (fromJust (elemIndex divider1 l) + 1, fromJust (elemIndex divider2 l) + 1)

part1 :: String -> IO ()
part1 = print . sumTrueIndices 1 . map (uncurry (<)) . parseInput readPacketPairsP <=< readFile

part2 :: String -> IO ()
part2 = print . uncurry (*) . findDividerIndices . sort . ([divider1, divider2] ++) . parseInput readPacketsP <=< readFile

_main :: [String] -> IO ()
_main [_, "1", fn] = part1 fn
_main [_, "2", fn] = part2 fn
_main _ = do
  path <- getExecutablePath
  putStrLn $ "usage: " ++ path ++ " _ 1|2 <inputfile>"

main :: IO ()
main = getArgs >>= _main
