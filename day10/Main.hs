module Main (main) where

import Control.Monad ((<=<))
import Data.Functor (($>))
import Lib (parseInput, readIntP)
import System.Environment (getArgs, getExecutablePath)
import Text.ParserCombinators.ReadP (ReadP, char, endBy, string, (+++))

data Instruction = Noop | AddX Int deriving (Show)

readNoopP :: ReadP Instruction
readNoopP = string "noop" $> Noop

readAddXP :: ReadP Instruction
readAddXP = string "addx " *> (AddX <$> readIntP)

readInstructionP :: ReadP Instruction
readInstructionP = readNoopP +++ readAddXP

readInstructionsP :: ReadP [Instruction]
readInstructionsP = readInstructionP `endBy` char '\n'

doInstructions :: Int -> [Instruction] -> [Int]
doInstructions x (Noop : is) = x : doInstructions x is
doInstructions x (AddX v : is) = x : x : doInstructions (x + v) is
doInstructions _ [] = []

keepInteresting :: Int -> [Int] -> [Int]
keepInteresting i (x : xs)
  | i `mod` 40 == 20 = x * i : keepInteresting (i + 1) xs
  | otherwise = keepInteresting (i + 1) xs
keepInteresting _ _ = []

drawScreen :: Int -> [Int] -> String
drawScreen i (x : xs)
  | i `mod` 40 == 39 = pixel : '\n' : drawScreen (i + 1) xs
  | otherwise = pixel : drawScreen (i + 1) xs
  where
    pixel
      | x >= i `mod` 40 - 1 && x <= i `mod` 40 + 1 = '#'
      | otherwise = '.'
drawScreen _ _ = ""

part1 :: String -> IO ()
part1 = print . sum . keepInteresting 1 . doInstructions 1 . parseInput readInstructionsP <=< readFile

part2 :: String -> IO ()
part2 = putStr . drawScreen 0 . doInstructions 1 . parseInput readInstructionsP <=< readFile

_main :: [String] -> IO ()
_main [_, "1", fn] = part1 fn
_main [_, "2", fn] = part2 fn
_main _ = do
  path <- getExecutablePath
  putStrLn $ "usage: " ++ path ++ " _ 1|2 <inputfile>"

main :: IO ()
main = getArgs >>= _main
