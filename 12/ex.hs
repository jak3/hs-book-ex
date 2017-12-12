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

myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x = (fst $ head tp) ++ myUnfoldr f (snd $ head tp)
  where
    tp = case f x of
          Just (a, b) -> [([a], b)]
          Nothing     -> []

-- [Data.List implementation] --------------------------------------------------
--
--  unfoldr f b0 = build (\c n ->
--    let go b = case f b of
--                 Just (a, new_b) -> a `c` go new_b
--                 Nothing         -> n
--    in go b0)
--
-- -----------------------------------------------------------------------------

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\x -> Just (x, f x))

data BinaryTree a = Leaf
                  | Node (BinaryTree a) a (BinaryTree a)
                  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f x =
    case f x of
      Just (la, b, ra)  -> Node (unfold f la) b (unfold f ra)
      Nothing           -> Leaf

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = treeBuild' n 0
  where
    treeBuild' t b =
      case t == b of
        True  -> Leaf
        False -> Node dig b dig
      where dig = treeBuild' t (b+1)
