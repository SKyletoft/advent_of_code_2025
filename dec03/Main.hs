{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

import Data.ByteString.Char8 (unpack)
import Data.FileEmbed (embedFile)
import Data.Maybe (fromJust)

input :: String
input = unpack $(embedFile "input.txt")

both f g x = (f x, g x)

part1 = sum . map (findMax 2)

part2 = sum . map (findMax 12)

findMax :: Int -> String -> Int
findMax c l = read' . head $ memoMap !! c
 where
  memoMap = [[findMaxInner x y | y <- [0 ..]] | x <- [0 ..]]
  read' = \case
    "" -> 0
    s -> read s
  findMaxInner :: Int -> Int -> String
  findMaxInner charsLeft suffix
    | charsLeft <= 0 = ""
    | suffix == length l = ""
    | otherwise =
        let a = (l !! suffix) : memoMap !! (charsLeft - 1) !! (suffix + 1)
            b = memoMap !! charsLeft !! (suffix + 1)
         in if read' a > read' b then a else b

main = print . both part1 part2 . lines $ input
