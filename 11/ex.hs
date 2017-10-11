{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}

module Chp11 where

import Data.List
import Data.Char
import Data.Maybe

data Price = Price Integer
  deriving (Eq, Show)

data Manufacturer = Mini | Mazda | Tata
  deriving (Eq, Show)

data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited
  deriving (Eq, Show)

data Size = Size Integer
  deriving (Eq, Show)

data Vehicle = Car Manufacturer Price | Plane Airline Size
  deriving (Eq, Show)

data Example = MakeExample Int deriving Show

class TooMany a where
  tooMany :: a -> Bool
  guessChars :: a -> Bool
  countGoats :: a -> Int

instance TooMany (Int, a) where
  tooMany (n, _) = n > 42

instance TooMany (Int, String) where
  guessChars (n, s) = n == (length s)

instance TooMany (Int, Int) where
  countGoats (n, m) = n + m

instance TooMany (Num a, TooMany a) => (a, a) where

newtype Goats = Goats Int
  deriving (Eq, Show)

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _         = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _ ) = True
isPlane _            = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Manufacturer
getManu (Plane _ _) = undefined
getManu (Car m _)   = m

-- HOw Does your Garden Grow
