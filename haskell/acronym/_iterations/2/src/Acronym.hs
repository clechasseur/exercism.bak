module Acronym (abbreviate) where

import Data.Char (isAlpha, isAlphaNum, isUpper, isLower, toUpper)

abbreviate :: String -> String
abbreviate xs = map toUpper . abbreviate' $ '\0':xs
    where abbreviate' [] = []
          abbreviate' [_] = []
          abbreviate' (p:x:xs) | (isAlpha x) && (not . isWordChar $ p) = x:abbreviate' (x:xs)
                               | (isUpper x) && (isLower p) = x:abbreviate' (x:xs)
                               | otherwise = abbreviate' (x:xs)
                                    where isWordChar c = (isAlphaNum c) || (c == '\'')
