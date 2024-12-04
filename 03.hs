{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use uncurry" #-}
import Data.Maybe
import Parsing

type Mul = (Int, Int)

getmuls =
  do
    string "mul("
    left <- int
    char ','
    right <- int
    char ')'
    rest <- getmuls
    return $ Just (left, right) : rest
    <|> do
      string "mul("
      left <- int
      char ','
      right <- int
      char ')'
      -- rest <- getmuls
      return [Just (left, right)]
    <|> do
      item
      getmuls
    <|> do
      empty
      return []

summuls :: [Maybe Mul] -> Int
summuls xs = sum $ map (\(l, r) -> l * r) (muls xs)
  where
    muls = catMaybes

firstsol = do
  input <- readFile "./input/03.txt"
  print $ summuls $ muls input
  return ()
  where
    muls xs = concat $ map (fst) $ parse getmuls xs

-- second part

getmuls' =
  do
    string "don't()"
    dont
    <|> do
      string "mul("
      left <- int
      char ','
      right <- int
      char ')'
      rest <- getmuls'
      return $ Just (left, right) : rest
    <|> do
      string "mul("
      left <- int
      char ','
      right <- int
      char ')'
      return [Just (left, right)]
    <|> do
      item
      getmuls'
    <|> do
      empty
      return []

do' =
  do
    string "don't()"
    dont
    <|> getmuls'

dont =
  do
    string "do()"
    muls <- do'
    return (muls)
    <|> do
      item
      dont
    <|> do
      empty
      return []

secondsol = do
  input <- readFile "./input/03.txt"
  print $ summuls $ muls input
  return ()
  where
    muls xs = concat $ map (fst) $ parse do' xs