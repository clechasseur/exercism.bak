module SecretHandshake (handshake) where

import Data.Bits

actions :: [String]
actions = ["wink", "double blink", "close your eyes", "jump"]

handshake :: Int -> [String]
handshake n = reverseAction n . handshake' n actions $ []
  where
    handshake' :: Int -> [String] -> [String] -> [String]
    handshake' _ [] out = out
    handshake' n (a:as) out
             | n .&. 1 /= 0 = handshake' n' as (a:out)
             | otherwise = handshake' n' as out
                 where n' = shiftR n 1

    reverseAction :: Int -> [String] -> [String]
    reverseAction n out
                | n .&. 16 /= 0 = out
                | otherwise = reverse out
