module Main where
import Data.List (delete)
sample = "987654321111111\n811111111111119\n234234234234278\n818181911112111"

solve1 :: String -> Int
solve1 content = 
  let
    inp = lines content
    toN (c1, c2) = read [c1, c2]
    stepfn a b
        | u >= v = (u, max v b)
        | otherwise = (v, b)
      where
        u = fst a
        v = snd a
  in
    sum $ map (toN . (foldl stepfn ('0', '0'))) inp
    
removeOne :: String -> String
removeOne []  = []
removeOne [x] = []
removeOne (x:y:rest)
    | x < y     = y : rest 
    | otherwise = x : removeOne (y:rest) 


solve2 content = 
  let
    inp = lines content

    stepfn a b = max a $ (removeOne a) ++ [b]
      where
        mn = minimum a
  in
    sum $ map (read . (foldl stepfn (replicate 12 '0'))) inp

main :: IO ()
main = do
  content <- readFile "day3.in"
  let result = solve1 content
  print result
  let result2 = solve2 content
  print result2
