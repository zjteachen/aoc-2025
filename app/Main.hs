
module Main where

parseMove :: String -> Int
parseMove[] = 0
parseMove (pre : num)
  | pre == 'L' = negate (read num)
  | pre == 'R' = read num
  | otherwise = 0

parseInp :: String -> [Int]
parseInp content = map parseMove (lines content)

solve:: String -> Int
solve content = 
  let
    moves = parseInp content
    stepfn a b = (a + b) `mod` 100
    history = scanl stepfn 50 moves
  in
    length (filter (==0) history)

passes :: Int -> Int -> Int
passes pos move = 
  let
    eflip k = (-k) `mod` 100
  in
    if move > 0 then 
      div (pos + move) 100
    else
      div ((eflip pos) - move) 100

   
solve2 :: String -> Int
solve2 content =
  let
    moves = parseInp content
    stepfn a b = (a + b) `mod` 100
    history = scanl stepfn 50 moves
    nhist = init history
  in
    sum $ zipWith passes nhist moves
    

main :: IO ()
main = do
  content <- readFile "day1.in"
  let result = solve content
  print result
  let result2 = solve2 content
  print result2
