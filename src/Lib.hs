module Lib (splitOn) where

_splitOn :: (Eq a) => a -> [a] -> [a] -> [[a]]
_splitOn el (x : xs) acc
  | x == el = reverse acc : _splitOn el xs []
  | otherwise = _splitOn el xs (x : acc)
_splitOn _ _ acc = [reverse acc]

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn el xs = _splitOn el xs []
