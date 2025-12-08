{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

import Data.ByteString.Char8 (unpack)
import Data.FileEmbed (embedFile)
import Data.List (nub, sort, sortOn)
import Data.List.Split (splitOn)

input :: String
input = unpack $(embedFile "input.txt")

type Box = (Int, Int, Int)

pos :: [Int] -> Box
pos [x, y, z] = (x, y, z)

dist :: Box -> Box -> Int
dist (a, b, c) (x, y, z) =
  floor . sqrt
    $ fromIntegral (a - x) ** 2
      + fromIntegral (b - y) ** 2
      + fromIntegral (c - z) ** 2

connections boxes =
  nub . sortOn (uncurry dist)
    $ [(min x y, max x y) | x <- boxes, y <- boxes, x /= y]

with f = head . filter f

unify :: Eq a => [[a]] -> (a, a) -> [[a]]
unify sets (x, y) =
  let withX = with (x `elem`) sets
      withY = with (y `elem`) sets
      without = filter (\c -> not (x `elem` c || y `elem` c)) sets
   in if withX == withY
        then sets
        else (withX ++ withY) : without

g :: Eq a => [[a]] -> a -> Int
g (c : cs) x | x `elem` c = 0
g (c : cs) x = 1 + g cs x
g [] _ = -1

part1 = do
  let boxes = map (pos . map read . splitOn ",") . lines $ input
  let sortedConnections = connections boxes
  print
    . product
    . take 3
    . reverse
    . sort
    . map length
    . (!! 1000)
    . scanl unify (map (: []) boxes)
    $ sortedConnections

main = do
  let boxes = map (pos . map read . splitOn ",") . lines $ input
  let sortedConnections = connections boxes
  let lastConnection =
        head
          . (!! 1)
          . last
          . takeWhile (\c -> length c > 1)
          . scanl unify (map (: []) boxes)
          $ sortedConnections
  print
    . (\((x, _, _), (a, _, _)) -> x * a)
    . with (\(x, y) -> x == lastConnection || y == lastConnection)
    $ sortedConnections
