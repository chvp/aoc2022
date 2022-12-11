module Main (main) where

import Control.Monad ((<=<))
import Data.Functor (($>))
import Data.Ix (range)
import Data.Sequence (Seq, fromList, index, update)
import qualified Data.Sequence as Sequence
import Lib (parseInput, readIntP)
import System.Environment (getArgs, getExecutablePath)
import Text.ParserCombinators.ReadP (ReadP, char, eof, many, sepBy, string, (+++))

data Monkey = Monkey {items :: [Int], op :: Int -> Int, test :: Int, trueTarget :: Int, falseTarget :: Int, nThrown :: Int}

readItemListP :: ReadP [Int]
readItemListP = do
  _ <- string "  Starting items: "
  list <- readIntP `sepBy` string ", "
  _ <- char '\n'
  return list

readTimesOpP :: ReadP (Int -> Int)
readTimesOpP = do
  _ <- string "old * "
  n <- readIntP
  return (* n)

readPlusOpP :: ReadP (Int -> Int)
readPlusOpP = do
  _ <- string "old + "
  n <- readIntP
  return (+ n)

readSquareOpP :: ReadP (Int -> Int)
readSquareOpP = string "old * old" $> (\x -> x * x)

readOpP :: ReadP (Int -> Int)
readOpP = do
  _ <- string "  Operation: new = "
  o <- readTimesOpP +++ readPlusOpP +++ readSquareOpP
  _ <- char '\n'
  return o

readTestP :: ReadP Int
readTestP = do
  _ <- string "  Test: divisible by "
  n <- readIntP
  _ <- char '\n'
  return n

readTrueTargetP :: ReadP Int
readTrueTargetP = do
  _ <- string "    If true: throw to monkey "
  n <- readIntP
  _ <- char '\n'
  return n

readFalseTargetP :: ReadP Int
readFalseTargetP = do
  _ <- string "    If false: throw to monkey "
  n <- readIntP
  _ <- char '\n'
  return n

readMonkeyP :: ReadP Monkey
readMonkeyP = do
  _ <- string "Monkey "
  _ <- readIntP
  _ <- string ":\n"
  is <- readItemListP
  o <- readOpP
  t <- readTestP
  tt <- readTrueTargetP
  ft <- readFalseTargetP
  (char '\n' $> ()) +++ eof
  return $ Monkey {items = is, op = o, test = t, trueTarget = tt, falseTarget = ft, nThrown = 0}

readMonkeysP :: ReadP (Seq Monkey)
readMonkeysP = fromList <$> many readMonkeyP

doItem :: Int -> Int -> Monkey -> Int -> Seq Monkey -> Int -> Seq Monkey
doItem divisor modulo (Monkey {items = _, op = o, test = t, trueTarget = tt, falseTarget = ft, nThrown = nt}) idx ms item =
  update
    targetIdx
    (Monkey {items = targetItems', op = targetOp, test = targetTest, trueTarget = targetTrueTarget, falseTarget = targetFalseTarget, nThrown = targetNThrown})
    (update idx (Monkey {items = [], op = o, test = t, trueTarget = tt, falseTarget = ft, nThrown = nt + 1}) ms)
  where
    newItem = (o item `div` divisor) `mod` modulo
    testResult = newItem `mod` t == 0
    targetIdx = if testResult then tt else ft
    Monkey {items = targetItems, op = targetOp, test = targetTest, trueTarget = targetTrueTarget, falseTarget = targetFalseTarget, nThrown = targetNThrown} = index ms targetIdx
    targetItems' = targetItems ++ [newItem]

doMonkey :: Int -> Int -> Seq Monkey -> Int -> Seq Monkey
doMonkey divisor modulo monkeys cur = monkeys'
  where
    monkeys' = foldl (\ms -> doItem divisor modulo (index ms cur) cur ms) monkeys $ items (index monkeys cur)

doRound :: Int -> Int -> Seq Monkey -> Seq Monkey
doRound divisor modulo monkeys = foldl (doMonkey divisor modulo) monkeys (range (0, Sequence.length monkeys - 1))

max2 :: Int -> Int -> [Int] -> (Int, Int)
max2 a b (x : xs)
  | x > a = max2 x a xs
  | x > b = max2 a x xs
  | otherwise = max2 a b xs
max2 a b [] = (a, b)

part1 :: String -> IO ()
part1 = print . uncurry (*) . max2 0 0 . foldr (:) [] . fmap nThrown . (!! 20) . (\ms -> iterate (doRound 3 (product (fmap test ms))) ms) . parseInput readMonkeysP <=< readFile

part2 :: String -> IO ()
part2 = print . uncurry (*) . max2 0 0 . foldr (:) [] . fmap nThrown . (!! 10000) . (\ms -> iterate (doRound 1 (product (fmap test ms))) ms) . parseInput readMonkeysP <=< readFile

_main :: [String] -> IO ()
_main [_, "1", fn] = part1 fn
_main [_, "2", fn] = part2 fn
_main _ = do
  path <- getExecutablePath
  putStrLn $ "usage: " ++ path ++ " _ 1|2 <inputfile>"

main :: IO ()
main = getArgs >>= _main
