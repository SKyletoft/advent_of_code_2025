{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE TemplateHaskell #-}

import Data.FileEmbed (embedFile)

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as C8

input :: String
input = C8.unpack $(embedFile "input.txt")

parse :: String -> Int
parse = \case
  ('R' : xs) -> read xs
  ('L' : xs) -> -(read xs)
  _ -> error "Invalid"

wrapAdd x y = (x + y) `mod` 100

part1 :: Int
part1 =
  length
    . filter (== 0)
    . scanl wrapAdd 50
    . map parse
    . lines
    $ input

flatten x
  | x < 0 = replicate (abs x) (-1)
  | otherwise = replicate x 1

part2 :: Int =
  length
    . filter (== 0)
    . scanl wrapAdd 50
    . concatMap (flatten . parse)
    . lines
    $ input

main :: IO ()
main = print part2
