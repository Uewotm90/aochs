module Main (main) where

import Data.Either (fromRight, isRight)
import Data.List
import Data.List (isPrefixOf, isSuffixOf)
import Text.Parsec

-- import Data.List (subsequences)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  problemInput <- readFile "./input/input.txt"
  print $ solve1 $ parse input "input" problemInput
  print $ solve2 $ parse input "input" test

test = "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124\n"

-- logic

-- palindromes :: (Integer, Integer) -> [Integer]
-- palindromes (a, b) = [x | x <- [a .. b], isPalindrome $ show x]

-- isPalindrome :: String -> Bool
-- isPalindrome [] = True
-- isPalindrome [_] = True
-- isPalindrome s@(x : y : xs) = x == last (y : xs) && isPalindrome (init $ y : xs)

{- part 2 -}
solve2 (Right rs) = sum $ allInvalids2 rs

allInvalids2 xs = concat $ map getInvalids2 xs

getInvalids2 :: (Integer, Integer) -> [Integer]
getInvalids2 (a, b) = filter (patt2 . show) [a .. b]

-- TODO: fix

-- String -> String -> Bool
-- patt2 :: String -> a
patt2 s = isRight $ parse (parsers $ reverse subs) "combinations" s -- ? no idea why reversing the order is necessary, choice no worky
  where
    subs = init $ filter (\ss -> length s `rem` length ss == 0) $ drop 1 $ inits s
    p x = do
      _ <- many $ string x
      eof
      --
      --   notFollowedBy digit
      return ()
    parsers xs = choice $ map p xs -- try all combinations, short ciruit if one succeeds


patt2' s = isRight $ parse (parsers $ reverse subs) "combinations" s -- ? no idea why reversing the order is necessary, choice no worky
  where
    subs = init $ filter (\ss -> length s `rem` length ss == 0) $ drop 1 $ inits s
    p x = do
      _ <- many $ string x
      eof
      --
      --   notFollowedBy digit
      return ()
    parsers xs = try . choice  $ map p xs -- try all combinations, short ciruit if one succeeds
    -- parsers xs = foldr (<|>) $ map p xs

-- idea: parse subsequence repeatedly until consumed
subs s = filter (\ss -> length s `rem` length ss == 0 && length ss <= (length s `div` 2)) $ drop 1 $ inits s
subs' s = init $ filter (\ss -> length s `rem` length ss == 0) $ drop 1 $ inits s
-- ! 565656,2121212121
p x = do
  a <- try $ many $ string x
  eof
  return a

parsers xs = choice $ map p xs -- try all combinations, short ciruit if one succeeds

{- part 1 -}
solve1 (Right rs) = sum $ allInvalids rs

allInvalids xs = concat $ map getInvalids xs

getInvalids :: (Integer, Integer) -> [Integer]
getInvalids (a, b) = filter (patt . show) [a .. b]

patt :: String -> Bool
patt s
  | odd (length s) = False
  | otherwise = fhalf `isPrefixOf` s && fhalf `isSuffixOf` s
  where
    fhalf = take (length s `div` 2) s

-- subsequences [1]
-- inits

--   | even (length s) = True
--   | otherwise = False

-- parsing
parser = parse input "input"

input = do
  fr <- range
  --   many $ char ','
  frs <- rest
  return $ fr : frs
  where
    rest =
      do
        _ <- char ','
        input
        <|> do
          _ <- endOfLine
          return []

range = do
  f <- many digit
  char '-'
  s <- many digit
  return (read f :: Integer, read s :: Integer)