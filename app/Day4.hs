module Main where
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (mapMaybe)

sample = "..@@.@@@@.\n@@@.@.@.@@\n@@@@@.@.@@\n@.@@@@..@.\n@@.@@@@.@@\n.@@@@@@@.@\n.@.@.@.@@@\n@.@@@.@@@@\n.@@@@@@@@.\n@.@.@@@.@.\n"

type Coord = (Int, Int)
type Grid = M.Map Coord Char

parseGrid :: String -> Grid
parseGrid inp = M.fromList
  [
    ((r, c), char)
    | (r, line) <- zip [0..] $ lines inp
    , (c, char) <- zip [0..] $ line
  ]

getRem :: Grid -> [Coord] -> [Coord]
getRem grid coords = 
  [
    c 
    | c <- coords
    , M.lookup c grid == Just '@'
    , (length $ neighbours grid c) <= 4
  ]
  where
    neighbours gr (row, col) = [
      () 
      | nr <- [row-1..row+1]
      , nc <- [col-1..col+1]
      , M.lookup (nr, nc) gr == Just '@']

spread :: [Coord] -> Grid -> Int
spread [] grid = 0
spread coords grid =
  let
    cand = getRem grid coords
    grepl = foldl (\g c -> M.insert c '.' g) grid cand
    expand coords = S.toList . S.fromList $ [
      (nr, nc)
      | (r, c) <- coords
      , nr <- [r-1..r+1]
      , nc <- [c-1..c+1]
      , (nr, nc) /= (r, c)]
  in
    (length cand) + spread (expand cand) grepl
    

solve1 :: String -> Int
solve1 inp = 
  let
    grid = parseGrid inp
  in
    length $ getRem grid (M.keys $ M.filter (=='@') grid)

solve2 :: String -> Int
solve2 inp = 
  let
    grid = parseGrid inp
    start = getRem grid (M.keys $ M.filter (=='@') grid) 
  in
    spread start grid

main :: IO ()
main = do
  content <- readFile "day4.in"
  let result = solve1 content
  print result
  let result2 = solve2 content
  print result2
