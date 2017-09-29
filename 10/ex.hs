module Chp10 where

import Data.Time

data DatabaseItem = DbString String | DbNumber Integer | DbDate UTCTime
  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
           (fromGregorian 1911 5 1)
           (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime
           (fromGregorian 1921 5 1)
           (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr f []
  where f (DbDate a) b = a : b
        f         _  b = b

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr f []
  where f (DbNumber a) b = a : b
        f           _  b = b

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = (foldr max
                (UTCTime (fromGregorian 0 0 0) (secondsToDiffTime 34123))
             ) . filterDbDate
-- mostRecent = maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = (foldl (+) 0) . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb xs = fromIntegral (sumDb xs) / fromIntegral (length $ filterDbNumber xs)

stops  = "pbtdkg"
vowels = "aeiou"

svs stops vowels  = [ (s,v,s2) | s <- stops, v <- vowels, s2 <- stops ]
psvs stops vowels = [ (s,v,s2) | s <- stops, v <- vowels, s2 <- stops, s == 'p' ]

-- Get avg charactes per word
seekritFunc :: String -> Int
seekritFunc x = div (sum (map length (words x))) (length (words x))

seekritFunc2 :: Fractional a => String -> a
seekritFunc2 x = fromIntegral (sum (map length (words x))) / fromIntegral (length (words x))

-- direct recursion, not using (&&)
myAnddrB :: [Bool] -> Bool
myAnddrB [] = True
myAnddrB (x:xs) =
  if x == False
  then False
  else myAnddrB xs

-- direct recursion, using (&&)
myAnddr :: [Bool] -> Bool
myAnddr [] = True
myAnddr (x:xs) = x && myAnddr xs

-- fold, not point-free
-- in the folding function
myAndnpf :: [Bool] -> Bool
myAndnpf = foldr
  (\a b ->
  if a == False
  then False
  else b) True

-- fold, both myAnd and the folding
-- function are point-free now
myAnd :: [Bool] -> Bool
myAnd = foldr (&&) True

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\a b -> f a || b) False

myElem :: Eq a => a -> [a] -> Bool
myElem a = foldr (\x y -> a == x || y) False

myElemWithAny :: Eq a => a -> [a] -> Bool
myElemWithAny a = myAny (== a)

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x y -> f x : y) []

-- Another more elegant solution
myMap2 f = foldr ((:) . f) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter pr = foldr f []
  where
    f a b
      | pr a      = a : b
      | otherwise = b

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaxMin :: Ordering -> (a -> a -> Ordering) -> [a] -> a
myMaxMin o pr (x:xs) = foldr f x xs
  where
    f a b
      | pr a b == o = a
      | otherwise   = b

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy = myMaxMin GT

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy = myMaxMin LT

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
