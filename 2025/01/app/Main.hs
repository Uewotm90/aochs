module Main where

import Text.Parsec
import System.Directory (getCurrentDirectory)

type Input = [(Char, Int)]

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  pwd <- getCurrentDirectory
  print pwd
  input <- readFile "./input/input.txt"
  --   input <- test
  print $ solve $ parse Main.lines "input" test

test = "L68\nL30\nR48\nL5\nR60\nL55\nL1\nL99\nR14\nL82\n"

-- solve :: Either a Input -> Int
solve (Left a) = undefined
solve (Right b) = length . filter (==0) $ reverse $ foldr (\c acc -> rotateDial (head acc) c : acc) [50] $ reverse b

rotateDial :: Int -> (Char, Int) -> Int
rotateDial d ('L', n) = if d >= n then d - n else 100 - (n - d)
rotateDial d ('R', n) = (d + n) `rem` 100

-- test = print ( parse line "test" "R1")
-- parsing
lines = many line

line = do
  c <- char 'L' <|> char 'R'
  dist <- many digit
  endOfLine
  return (c, read dist :: Int)