module FoodChain (song) where

import Data.List (intercalate)

song :: String
song = intercalate "\n" [verse n | n <- [1..8]]
  where
    verse :: Int -> String
    verse n = (firstPart n) <> (secondPart n)

    animal :: Int -> String
    animal 1 = "fly"
    animal 2 = "spider"
    animal 3 = "bird"
    animal 4 = "cat"
    animal 5 = "dog"
    animal 6 = "goat"
    animal 7 = "cow"
    animal 8 = "horse"
    animal _ = error "Invalid animal"

    firstPart :: Int -> String
    firstPart n = "I know an old lady who swallowed a " <> curAnimal <> ".\n"
      where
        curAnimal = animal n

    secondPart :: Int -> String
    secondPart n = (secondLine n) <> (recursive n)
      where
        secondLine :: Int -> String
        secondLine 1 = ""
        secondLine 2 = "It wriggled and jiggled and tickled inside her.\n"
        secondLine 3 = "How absurd to swallow a bird!\n"
        secondLine 4 = "Imagine that, to swallow a cat!\n"
        secondLine 5 = "What a hog, to swallow a dog!\n"
        secondLine 6 = "Just opened her throat and swallowed a goat!\n"
        secondLine 7 = "I don't know how she swallowed a cow!\n"
        secondLine 8 = "She's dead, of course!\n"
        secondLine _ = error "Invalid second line"

        recursive :: Int -> String
        recursive 1 = "I don't know why she swallowed the fly. Perhaps she'll die.\n"
        recursive 8 = ""
        recursive n = "She swallowed the " <> biggerAnimal <> " to catch the " <> smallerAnimal <> addon <> ".\n" <> next
          where
            next = recursive (n - 1)
            biggerAnimal = animal n
            smallerAnimal = animal (n - 1)
            addon
                | smallerAnimal == "spider" = " that wriggled and jiggled and tickled inside her"
                | otherwise = ""
