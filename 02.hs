import Data.List
import Parsing

type Line = [Int]

type Lines = [Line]

line =
  do
    level <- token int
    space
    levels <- token line
    return (level : levels)
    <|> do
      level <- token int
      return [level]

issafe :: Lines -> [(Bool, Line)]
issafe xs = map (\x -> (allincordec x && adjacentlevel x, x)) xs

allincordec xs = xs == sort xs || xs == reverse (sort xs)

adjacentlevel [] = True
adjacentlevel [x] = True
adjacentlevel (x : xx : xs) =
  (abs (x - xx) >= 1 && abs (x - xx) <= 3)
    && adjacentlevel (xx : xs)

firstsol = do
  input <- readFile "./input/02.txt"
  print $ length $ filter (\(x, y) -> x) (issafe $ tolines input)
  return ()
  where
    tolines xs = map (fst . head . parse line) $ (lines xs)

-- !!second half¡¡

powerset [] = [[]]
powerset (x : xs) = [x : ps | ps <- powerset xs] ++ powerset xs

-- linesminus xs = [a|a<-xs, s<-xs,a/=s]
linesminus :: Int -> [[a]] -> [[a]]
linesminus p = filter (\x -> length x >= p)

issafe''' :: Lines -> [Bool]
issafe''' xs = do
    x <- xs 
    return (sat $ combinations x)
        where
            combinations ys = linesminus (length ys-1) $ powerset ys
            sat xss = any (\y -> allincordec y && adjacentlevel y) xss

secondsol = do
  input <- readFile "./input/02.txt"
  print $ length $ filter id (issafe''' $ tolines input)
  return ()
  where
    tolines xs = map (fst . head . parse line) $ (lines xs)