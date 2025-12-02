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
  print $ solve2 $ parse Main.lines "input" test
  print $ solve2 $ parse Main.lines "input" input

test = "L68\nL30\nR48\nL5\nR60\nL55\nL1\nL99\nR14\nL82\n"

-- solve :: Either a Input -> Int
solve (Left a) = undefined
solve (Right b) = length . filter (== 0) $ foldr (\c acc -> rotateDial (head acc) c : acc) [50] $ reverse b

solve2 (Left _) = undefined
-- solve2 (Right b) = take 50 $ drop 100 $ reverse$ zip (reverse b) $ {- foldr (\(a, b) acc -> acc + b + if a == 0 then 1 else 0) 0 $ -} foldr (\c acc -> rotateDial2 (fst $ head acc) c : acc) [(50, 0)] $ reverse b
solve2 (Right b) ={-  take 40 $ reverse$ zip (reverse b) $ -} foldr (\(a, b) acc -> acc + b + if a == 0 then 1 else 0) 0 $ foldr (\c acc -> rotateDial2 (fst $ head acc) c : acc) [(50, 0)] $ reverse b
--FIXME
rotateDial :: Int -> (Char, Int) -> Int
rotateDial d ('L', n)
  | d >= n = d - n
  | otherwise = (d - n) `mod` 100
rotateDial d ('R', n) = (d + n) `rem` 100

rotateDial2 :: Int -> (Char, Int) -> (Int, Int)
rotateDial2 d r@('L', n) = (rotateDial d r, countoverflow d n - if d == 0 && n `mod` 100 /= 0 then 1 else 0)
  where
    countoverflow d n = abs $ (d - n) `div` 100 -- 7026
    -- countoverflow d n = (abs $ d - n) `div` 100 -- 5805
rotateDial2 d r@('R', n) = (rotateDial d r, countoverflow d n)
  where
    countoverflow d n = if d + n == 100 then 0 else (d + n) `div` 100

rotateDial2' d r@('L', n) = (rotateDial d r, countoverflow d n)
  where
    countoverflow d n = undefined
rotateDial2' d r@('R', n) = (rotateDial d r, countoverflow d n)
  where
    countoverflow d n = undefined

-- test = print ( parse line "test" "R1")
-- parsing
lines = many line

line = do
  c <- char 'L' <|> char 'R'
  dist <- many digit
  endOfLine
  return (c, read dist :: Int)