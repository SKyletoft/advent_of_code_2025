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

isInvalid by s =
  let len = length s
      chunks = chunksOf (len `div` by) s
  in len `mod` by == 0
      && (all (== head chunks) $ chunks)

isInvalidAny s =
  or . map (`isInvalid` s) $ [2..length s]

part1
  = print
  . sum
  . map (read :: String -> Int)
  . filter (isInvalid 2)
  . map show
  . concatMap ( (\[lo, hi] -> [read lo :: Int .. read hi])
              . splitOn "-"
              )
  . splitOn ","
  $ input

main
  = print
  . sum
  . map (read :: String -> Int)
  . filter (isInvalidAny)
  . map show
  . concatMap ( (\[lo, hi] -> [read lo :: Int .. read hi])
              . splitOn "-"
              )
  . splitOn ","
  $ input
