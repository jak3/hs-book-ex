module Chp11 where

import Data.List
import Data.Char
import Data.Maybe

vigenere :: String -> String -> String
vigenere plain pass = vigenere' pass plain pass
  where
    vigenere' _ [] _ = []
    vigenere' pass plain [] = vigenere' pass plain pass
    vigenere' _ plain partialPass = undefined

sanitize :: String -> String -> [Int]
sanitize insane allowedChars = map (\x -> getSumValue x allowedChars) $
                                filter (\x -> elem x allowedChars) (map toLower insane)
  where
    getSumValue :: Char -> String -> Int
    getSumValue = (fromMaybe 0) . elemIndex
