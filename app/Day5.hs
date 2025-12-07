
module Main where

import Data.List.Split (splitOn)
import Data.List (sort)

sample = "3-5\n10-14\n16-20\n12-18\n\n1\n5\n8\n11\n17\n32"


solve1 :: String -> Int
solve1 inp = 
  let
    [rn, vals] = splitOn "\n\n" inp
    ids = map read $ lines vals :: [Int]
    dRanges = [
      (read a, read b)
      | [a, b] <- map (splitOn "-") $ lines rn] :: [(Int, Int)]
  in
    length [
      x
      | x <- ids
      , any (\(u, v) -> u <= x && x <= v) dRanges]

rec :: [(Int, Int)] -> Int -> Int
rec [] _ = 0
rec ((a, b) : rest) k
  | k >= b = rec rest k
  | otherwise = (b - max a (k + 1)) + 1 + rec rest b
  
solve2 :: String -> Int
solve2 inp = 
  let 
    [rn, _] = splitOn "\n\n" inp
    dRanges = [
      (read a, read b)
      | [a, b] <- map (splitOn "-") $ lines rn] :: [(Int, Int)]
  in
    rec (sort dRanges) 0
  
main :: IO ()
main = do
  content <- readFile "day5.in"
  let result = solve1 content
  print result
  let result2 = solve2 content
  print result2
