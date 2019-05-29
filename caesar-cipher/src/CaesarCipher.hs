module CaesarCipher (
    cipherString,
    cipherChar
) where

import Data.Char

cipherString :: Int -> String -> String
cipherString s = map (cipherChar s)

cipherChar :: Int -> Char -> Char
cipherChar s c
    | (c >= 'a') && (c <= 'z') = shiftInRange s 'a' 'z' c
    | (c >= 'A') && (c <= 'Z') = shiftInRange s 'A' 'Z' c
    | otherwise = c

shiftInRange :: Int -> Char -> Char -> Char -> Char
shiftInRange s a z c = chr (oa + mod ((oc - oa) + s) rangeSize)
    where oa = ord a
          oz = ord z
          oc = ord c
          rangeSize = oz - oa + 1