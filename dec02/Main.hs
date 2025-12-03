{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GHC2024 #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

import Data.FileEmbed (embedFile)
import Data.List.Split
import Debug.Trace

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8

input :: String
input = C8.unpack $(embedFile "input.txt")

both f g x = (f x, g x)

isInvalid by s =
  let len = length s
      chunks = chunksOf (len `div` by) s
  in len `mod` by == 0
      && all (== head chunks) chunks

isInvalidAny s =
  any (`isInvalid` s) [2..length s]

part1
  = sum
  . map fst
  . filter (isInvalid 2 . snd)

part2
  = sum
  . map fst
  . filter (isInvalidAny . snd)

main
  = print
  . both part1 part2
  . map (\x -> (x, show x))
  . concatMap ( (\[lo, hi] -> [read lo :: Int .. read hi])
              . splitOn "-"
              )
  . splitOn ","
  $ input
