module Main where
import Data.List (transpose)
import Data.List.Split (splitWhen)
import Data.Char (isSpace)

sample = "123 328  51 64 \n 45 64  387 23 \n  6 98  215 314\n*   +   *   +  "

getAns :: [String] -> String -> Int
getAns nums op
  | op == "*" = product $ numsInt
  | otherwise = sum $ numsInt
    where
      numsInt = map read nums :: [Int]

solve1 :: String -> Int
solve1 inp =
    sum . map (\(op : ns) -> getAns ns op) $ map reverse $ transpose $ map words $ lines inp 

solve2 :: String -> Int
solve2 inp =
  let
    (ops : nums) = reverse $ lines inp 
    newnums = splitWhen (all isSpace) (transpose (reverse nums))
  in
    sum $ zipWith getAns newnums (words ops)
    

main :: IO ()
main = do
  content <- readFile "day6.in"
  let result = solve1 content
  print result
  let result2 = solve2 content
  print result2

