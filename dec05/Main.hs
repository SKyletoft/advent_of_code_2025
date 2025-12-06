{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

import Data.ByteString.Char8 (unpack)
import Data.FileEmbed (embedFile)
import Data.List (sort)
import Data.List.Split (splitOn)

input :: String
input = unpack $(embedFile "input.txt")

parse s =
  let [ranges, available] = splitOn "\n\n" s
      r = map ((\[x, y] -> (read x, read y)) . splitOn "-") . lines $ ranges
      a = map read . lines $ available
   in (r, a)

part1 ranges = length . filter (\id -> any (\(lo, hi) -> lo <= id && id <= hi) ranges)

part2 =
  sum
    . map (\(x, y) -> y - x + 1)
    . merge1
    . sort

merge1 ((x, y) : xs) = merge (x, y) xs

merge (x, y) [] = [(x, y)]
merge (x, y) ((z, w) : xs)
  | z <= y || x == z || y + 1 == z = merge (min x z, max y w) xs
  | otherwise = (x, y) : merge (z, w) xs

main = do
  let (ranges, ids) = parse input
  print $ part1 ranges ids
  print $ part2 ranges
