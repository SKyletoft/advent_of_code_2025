{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

import Data.ByteString.Char8 (unpack)
import Data.FileEmbed (embedFile)
import Data.List (nub, sort, sortBy, sortOn)
import Data.List.Split (splitOn)
import Data.Ord (Down (..), comparing)
import Data.Set (Set)

import Data.Set qualified as S

input :: String
input = unpack $(embedFile "input.txt")

type Box = (Int, Int, Int)

pos :: [Int] -> Box
pos [x, y, z] = (x, y, z)

dist :: (Box, Box) -> Int
dist ((a, b, c), (x, y, z)) =
  floor . sqrt
    $ fromIntegral (a - x) ** 2
      + fromIntegral (b - y) ** 2
      + fromIntegral (c - z) ** 2

connections :: [Box] -> [(Box, Box)]
connections boxes =
  sortOn dist
    $ [(min x y, max x y) | (idx, x) <- zip [0 ..] boxes, y <- drop idx boxes, x /= y]

with :: (a -> Bool) -> [a] -> a
with f = head . filter f

unify :: (Eq a, Ord a) => [Set a] -> (a, a) -> [Set a]
unify sets (x, y)
  | withX == withY = sets
  | otherwise = S.union withX withY : without
 where
  withX = with (x `S.member`) sets
  withY = with (y `S.member`) sets
  without = filter (\c -> not (x `S.member` c || y `S.member` c)) sets

main = do
  let boxes = map (pos . map read . splitOn ",") . lines $ input
  let sortedConnections = connections boxes
  let stream =
        scanl unify (map S.singleton boxes)
          sortedConnections
  let lastConnection =
        S.findMin
          . (!! 1)
          . last
          . takeWhile (\case [_] -> False ; _ -> True)
          $ stream
  let part1 =
        product
          . take 3
          . sortBy (comparing Down)
          . map length
          . (!! 1000)
          $ stream
  let part2 =
        (\((x, _, _), (a, _, _)) -> x * a)
          . with (\(x, y) -> x == lastConnection || y == lastConnection)
          $ sortedConnections
  print (part1, part2)
