{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

import Data.ByteString.Char8 (unpack)
import Data.FileEmbed (embedFile)

input :: String
input = unpack $(embedFile "input.txt")

display :: Show a => [[a]] -> IO ()
display = putStrLn . unlines . map show

shiftRight = map ((0 :) . init)

shiftLeft = map ((++ [0]) . tail)

shiftDown xs = replicate (length (head xs)) 0 : init xs

shiftUp xs = tail xs ++ [replicate (length (head xs)) 0]

l .+ r = [[x + y | (x, y) <- zip xs ys] | (xs, ys) <- zip l r]

l .- r = [[x - y | (x, y) <- zip xs ys] | (xs, ys) <- zip l r]

l .* r = [[x * y | (x, y) <- zip xs ys] | (xs, ys) <- zip l r]

removable xss =
  map (map $ \x -> if 0 < x && x <= 4 then 1 else 0)
    $ xss
      .* ( foldl1 (.+) . map ($ xss)
              $ [ shiftRight . shiftDown
                , shiftLeft . shiftDown
                , shiftRight . shiftUp
                , shiftLeft . shiftUp
                , shiftRight
                , shiftLeft
                , shiftUp
                , shiftDown
                , id
                ]
          )

part1 =
  print
    . sum
    . concat
    . removable
    . map (map (\case '@' -> 1; _ -> 0))
    . lines
    $ input

fix f x
  | f x == x = x
  | otherwise = fix f (f x)

part2 = do
  let parsed = map (map (\case '@' -> 1; _ -> 0)) . lines $ input
  let totalRemovable = sum . concat . fix (\x -> x .- removable x) $ parsed
  let totalStart = sum . concat $ parsed
  print $ totalStart - totalRemovable

main = do
  part1
  part2
