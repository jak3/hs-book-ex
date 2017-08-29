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
