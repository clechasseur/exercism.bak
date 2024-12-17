module FoodChain (song) where

import Data.Char (toLower)
import Data.List (intercalate)

data Animal = Fly
            | Spider
            | Bird
            | Cat
            | Dog
            | Goat
            | Cow
            | Horse deriving (Show, Eq, Enum)

song :: String
song = intercalate "\n" [verse animal | animal <- [Fly .. Horse]]
  where
    animalName :: Animal -> String
    animalName animal = [lowerFirst] <> rest
      where
        lowerFirst = toLower first
        (first:rest) = show animal
  
    verse :: Animal -> String
    verse animal = (firstPart animal) <> (secondPart animal)

    firstPart :: Animal -> String
    firstPart animal = "I know an old lady who swallowed a " <> name <> ".\n"
      where
        name = animalName animal

    secondPart :: Animal -> String
    secondPart animal = (secondLine animal) <> (recursive animal)
      where
        spiderSense = "wriggled and jiggled and tickled inside her"
      
        secondLine :: Animal -> String
        secondLine Fly = ""
        secondLine Spider = "It " <> spiderSense <> ".\n"
        secondLine Horse = "She's dead, of course!\n"
        secondLine animal = prefix <> " a " <> name <> "!\n"
          where
            prefix = animalPrefix animal
            name = animalName animal
          
            animalPrefix :: Animal -> String
            animalPrefix Bird = "How absurd to swallow"
            animalPrefix Cat = "Imagine that, to swallow"
            animalPrefix Dog = "What a hog, to swallow"
            animalPrefix Goat = "Just opened her throat and swallowed"
            animalPrefix Cow = "I don't know how she swallowed"
            animalPrefix _ = error "Unreachable code"

        recursive :: Animal -> String
        recursive Fly = "I don't know why she swallowed the fly. Perhaps she'll die.\n"
        recursive Horse = ""
        recursive animal = "She swallowed the " <> biggerAnimal <> " to catch the " <> smallerAnimal <> addon <> ".\n" <> next
          where
            nextAnimal = pred animal
            biggerAnimal = animalName animal
            smallerAnimal = animalName nextAnimal
            addon = if nextAnimal == Spider
                      then " that " <> spiderSense
                      else ""
            next = recursive nextAnimal
