module Main where

import System.Directory (getCurrentDirectory)
import Text.Parsec

type Input = [(Char, Int)]

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  pwd <- getCurrentDirectory
  print pwd
  input <- readFile "./input/input.txt"
  --   input <- test
  print $ solve $ parse Main.lines "input" input
  print $ solve2 $ parse Main.lines "input" input

test = "L68\nL30\nR48\nL5\nR60\nL55\nL1\nL99\nR14\nL82\n"

-- solve :: Either a Input -> Int
solve (Left a) = undefined
solve (Right b) = length . filter (== 0) $ foldr (\c acc -> rotateDial (head acc) c : acc) [50] $ reverse b

solve2 (Left _) = undefined
solve2 (Right b) = foldr (\(a, b) acc -> acc + b + if a == 0 then 0 else 0) 0 $ foldr (\c acc -> rotateDial2 (fst $ head acc) c : acc) [(50, 0)] $ reverse b

rotateDial :: Int -> (Char, Int) -> Int
rotateDial d ('L', n)
  | d >= n = d - n
  | otherwise = (d - n) `mod` 100
rotateDial d ('R', n) = (d + n) `rem` 100

rotateDial2 :: Int -> (Char, Int) -> (Int, Int)
rotateDial2 d r@('L', n) = (rotateDial d r, abs $ (d - n) `div` 100)
rotateDial2 d r@('R', n) = (rotateDial d r, (d + n) `div` 100)

-- test = print ( parse line "test" "R1")
-- parsing
lines = many line

line = do
  c <- char 'L' <|> char 'R'
  dist <- many digit
  endOfLine
  return (c, read dist :: Int)