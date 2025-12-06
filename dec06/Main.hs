{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE TemplateHaskell #-}

import Data.ByteString.Char8 (unpack)
import Data.FileEmbed (embedFile)
import Data.List
import Data.List.Split

input :: String
input = unpack $(embedFile "input.txt")

part1 =
  print
    . sum
    . map
      ( (\(op : nums) -> foldl1' (case op of "*" -> (*); "+" -> (+)) $ map read nums)
          . reverse
      )
    . transpose
    . map words
    . lines
    $ input

padTo n s = replicate (n - length s) ' ' ++ s

trim (' ' : xs) = trim xs
trim x = x

part2 =
  let nums =
        map (map (read :: String -> Int))
          . splitOn [[]]
          . map trim
          . transpose
          . init
          . lines
          $ input
      ops = map (\case "*" -> (*); "+" -> (+)) . words . last . lines $ input
   in print $ sum . zipWith foldl1' ops $ nums

main = do
  part1
  part2
