module Main where

import Data.List.Split (splitOn)
import Data.List.Split (chunksOf)

ranges :: String -> [(Int, Int)]
ranges inp = map parseOne (splitOn "," inp)
  where
    parseOne str = 
      let [a, b] = splitOn "-" str
      in (read a, read b)
 
checkDiv :: String -> Int -> Bool
checkDiv n split = 
  ((==0) $ length n `mod` split) && all (== first) chunks
  where
    chunks = chunksOf split n
    first = head chunks

solve1 :: String -> Int
solve1 content =
  let
    rs = ranges content
  in
    sum $ map (uncurry xcount) rs
      where
        xcount a b = sum [x | x <- [a..b], let s = show x, let len = length s, even len, checkDiv s (len `div` 2)]

solve2 content =
  let
    rs = ranges content
  in
    sum $ map (uncurry xcount) rs
      where
        xcount a b = sum [x | x <- [a..b], let s = show x, let len = length s, any (checkDiv s) [1..(len `div` 2)]]
    
 
sample = "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"

main :: IO ()
main = do
  content <- readFile "day2.in"
  let result = solve1 content
  print result
  let result2 = solve2 content
  print result2
