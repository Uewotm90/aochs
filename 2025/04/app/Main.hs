module Main (main) where

import Data.Either (fromRight)
import Data.List
import Text.Parsec

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  print $ parser test


test = "..@@.@@@@.\n@@@.@.@.@@\n@@@@@.@.@@\n@.@@@@..@.\n@@.@@@@.@@\n.@@@@@@@.@\n.@.@.@.@@@\n@.@@@.@@@@\n.@@@@@@@@.\n@.@.@@@.@."
testp = fromRight undefined (parser test)

-- part 1 logic
leftAdj :: String -> Int -> Bool
leftAdj _ 0 = False
leftAdj (x : y : xs) 1 = x == '@'
leftAdj (x : xs) n = leftAdj xs $ n - 1

-- count rolls adjacent to a 2d index
countAdjacencies :: [String] -> (Int, Int) -> Int
countAdjacencies xss p = length $ filter (\(c, i) -> (i `elem` a) && c == '@') xi
  where
    xi = withIndex xss
    a = adjacents p 9

adjacents (a, b) n = filter (\(f, s) -> any (< 0) [f, s] && any (> n) [f, s]) [(a - 1, b), (a + 1, b), (a, b + 1), (a, b - 1), (a + 1, b + 1), (a - 1, b + 1), (a + 1, b - 1), (a - 1, b - 1)]

withIndex xss = concat $ zipWith (\a x -> map (\(v, w) -> (w, (v, a))) x) [0 ..] $ map (\xs -> zip [0 ..] xs) xss

-- part 2 logic

-- parsing

parser = parse input ""

input = (line `sepBy` endOfLine) <* eof

line = many roll

roll = char '.' <|> char '@'