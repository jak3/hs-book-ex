module Chp9 where

import Data.Char

myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) = x && myAnd xs

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

-- No function coercion nor optimization
myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = f x || myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem x [] = False
myElem x (y:ys) = x == y || myElem x ys

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f [] = []
squishMap f (x:xs) = f x ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain xs = squishMap id xs

myMaxMin :: Ordering -> (a -> a -> Ordering) -> [a] -> a
myMaxMin _ _ [] = undefined
myMaxMin _ _ [x] = x
myMaxMin o f (x:y:xs)
  | f x y == o = myMaxMin o f ([x] ++ xs)
  | otherwise  = myMaxMin o f ([y] ++ xs)

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy = myMaxMin GT

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy = myMaxMin LT

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
