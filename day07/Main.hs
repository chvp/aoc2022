module Main (main) where

import Control.Monad ((<=<))
import Data.Functor (void)
import Data.Maybe (catMaybes)
import Lib (parseInput, readIntP)
import System.Environment (getArgs, getExecutablePath)
import Text.ParserCombinators.ReadP (ReadP, char, eof, many, satisfy, string, (+++))

data FSTree = Directory String [FSTree] | File String Int deriving (Show)

readFileP :: ReadP (Maybe FSTree)
readFileP = do
  size <- readIntP
  _ <- char ' '
  fn <- many $ satisfy (/= '\n')
  _ <- char '\n'
  return $ Just (File fn size)

readDirEntryP :: ReadP (Maybe FSTree)
readDirEntryP = do
  _ <- string "dir "
  _ <- many $ satisfy (/= '\n')
  _ <- char '\n'
  return Nothing

readDirectoryP :: ReadP FSTree
readDirectoryP = do
  _ <- string "$ cd "
  dn <- many $ satisfy (/= '\n')
  _ <- string "\n$ ls\n"
  files <- many (readFileP +++ readDirEntryP)
  subdirs <- many readDirectoryP
  void (string "$ cd ..\n") +++ eof
  return $ Directory dn (catMaybes files ++ subdirs)

fileSize :: FSTree -> Int
fileSize (File _ size) = size
fileSize _ = 0

directorySizes :: FSTree -> [Int]
directorySizes (Directory _ xs) = (sum (map fileSize xs) + sum childrenSizes) : allChildrenSizes
  where
    childrenSizes = map head subSizes
    subSizes = map directorySizes xs
    allChildrenSizes = concat subSizes
directorySizes _ = [0] -- hack

part1 :: String -> IO ()
part1 = print . sum . filter (<= 100000) . directorySizes . parseInput readDirectoryP <=< readFile

part2 :: String -> IO ()
part2 fn = do
  content <- readFile fn
  let sizes = directorySizes $ parseInput readDirectoryP content
  let sizeRequired = 30000000 - (70000000 - head sizes)
  print $ minimum (filter (>= sizeRequired) sizes)

_main :: [String] -> IO ()
_main [_, "1", fn] = part1 fn
_main [_, "2", fn] = part2 fn
_main _ = do
  path <- getExecutablePath
  putStrLn $ "usage: " ++ path ++ " _ 1|2 <inputfile>"

main :: IO ()
main = getArgs >>= _main
