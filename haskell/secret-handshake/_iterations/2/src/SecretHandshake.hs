module SecretHandshake (handshake) where

import Data.Bits

handshake :: Int -> [String]
handshake n = reverseAction n . handshake' n actions $ []
  where
    actions :: [String]
    actions = ["wink", "double blink", "close your eyes", "jump"]

    handshake' :: Int -> [String] -> [String] -> [String]
    handshake' _ [] out = out
    handshake' 0 _ out = out
    handshake' n (a:as) out
             | testBit n 0 = handshake' n' as (a:out)
             | otherwise = handshake' n' as out
                 where n' = shiftR n 1

    reverseAction :: Int -> [String] -> [String]
    reverseAction n out
                | testBit n 4 = out
                | otherwise = reverse out
