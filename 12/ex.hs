module Chp12 where

import Data.List.Split

vowels     = "aeiou"
consonants = filter (\c -> not $ isVowel c) ['a'..'z']

dropLastSpace :: String -> String
dropLastSpace s = reverse $ drop 1 $ reverse s

notThe :: String -> String
notThe s = case s of
  "the" -> "a"
  _     -> s

replaceThe :: String -> String
replaceThe s = dropLastSpace $ concatMap ((++ " ") . notThe) (splitOnSpace s)

splitOnSpace :: String -> [String]
splitOnSpace = splitOn " "

countVowels :: String -> Int
countVowels = length . filter isVowel

countConsonants :: String -> Int
countConsonants = length . filter isConsonant

isVowel :: Char -> Bool
isVowel c = c `elem` vowels

isConsonant :: Char -> Bool
isConsonant c = c `elem` consonants

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel s = beforeVowel (splitOnSpace s) 0
  where
    beforeVowel :: [String] -> Integer -> Integer
    beforeVowel []  c = c
    beforeVowel [x] c = c
    beforeVowel (x:y:xz) c
      | x == "the" && (isVowel $ head y) = beforeVowel (y:xz) (c+1)
      | otherwise                        = beforeVowel (y:xz) c

newtype Word' =
  Word' String
  deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord s
  | countVowels s > countConsonants s = Nothing
  | otherwise                         = Just (Word' s)

data Nat =
    Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger = natToInteger' 0
  where
    natToInteger' c Zero      = c
    natToInteger' c (Succ x)  = natToInteger' (c+1) x

integerToNat :: Integer -> Maybe Nat
integerToNat = integerToNat' 0 Zero
  where
    integerToNat' c n i
      | i >  0    = integerToNat' (c+1) (Succ n) (i-1)
      | i == 0    = Just n
      | otherwise = Nothing
