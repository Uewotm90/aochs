module Main (main) where

import Data.Either
import Text.Parsec

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  print $ solve1 $ parser test
  inp <- readFile "./input/input.txt"
  print $ solve1 $ parser inp

test = "987654321111111\n811111111111119\n234234234234278\n818181911112111"

testInput = fromRight [] $ parser test

input = do 
    inp <- readFile "./input/input.txt"
    return $ zip [1..] $ map maxBank $ fromRight [] $ parser inp
-- logic1

-- firstTry (x : xs) = (x, maximum xs)
solve1 (Right xss) = sums xss

sums xss = sum $ map maxBank xss

maxBank [] = 0 -- bit of a hack, but 0 is identity element :)))
maxBank xs = pairToInt $ maximum $ allCombs xs
pairToInt (a, b) = (a * 10) + b

allCombs [] = []
allCombs [_] = []
allCombs (x : xs) = (x, maximum xs) : allCombs xs

-- logic2

-- parsing
parser :: String -> Either ParseError [[Int]]
parser s = parse allBanks "input" s

allBanks = do
  a <- bank `sepBy` (endOfLine)
  eof
  return a

bank = do
  js <- many jolt
  --   _ <- endOfLine
  return js

jolt = do
  d <- digit
  return (read [d] :: Int)