module Main (main) where

import Data.Containers.ListUtils (nubOrd)
import Data.Either (fromRight)
import Data.Foldable (minimumBy)
import Data.Ix (range)
import Data.List (groupBy, maximumBy, nub, sort, sortBy)
import Data.Ord (comparing)
import Data.Set (fromList, size, unions)
import Text.Parsec

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  i <- readFile "./input/input.txt"
  print $ parser' i
  print $ solve1 $ fromRight ([], []) $ parser' i
  print $ solve2'''' $ fromRight ([], []) $ parser' i -- 353554946341142,353554946341253 too big
  -- print $ allRangesAsc $ fromRight ([],[]) $ parser' i
  --   where
  --     allRangesAsc (rs,_) = all (uncurry (<=)) rs

test = "3-5\n10-14\n16-20\n12-18\n\n1\n5\n8\n11\n17\n32"

testp = fromRight ([], []) $ parser test

solve1 :: ([(Integer, Integer)], [Integer]) -> Int
solve1 (rs, ps) = length $ filter (`inRanges` rs) ps

-- solve2 :: ([(Integer, Integer)], [Integer]) -> Int
-- solve2 (rs, _) = length $ nubOrd $ concatMap Data.Ix.range rs -- slow

-- -- solve2' :: ([(Integer, Integer)], [Integer]) -> Int
-- solve2' (rs, _) = size $ unions $ map (fromList . Data.Ix.range) rs -- also slow :)))

-- -- solve2'' (rs,_) = foldl1 (\f s -> f `rangeOverlapsWith` s then  else  ) sort rs
-- -- solve2''' :: ([(Integer, Integer)], [Integer]) -> Int
-- solve2''' (rs, _) = sum $ map sumRange $ mergeGroups $ groupRanges rs -- fast, but incorrect

solve2'''' :: ([(Integer, Integer)], [Integer]) -> Integer
solve2'''' (rs, _) = sumRanges $ mergeConverge rs -- no worky either

rangeOverlapsWith (f1, s1) (f2, s2) = f2 - f1 >= 0 && f2 - s1 <= 0

-- \|| (s2-f1)

mergeOverlaps (f1, s1) (f2, s2) = (min f1 f2, max s1 s2)

multiMergeRange rs = (fst $ minimumBy (comparing fst) rs, snd $ maximumBy (comparing snd) rs)

-- mergeRanges :: [(Integer,Integer)] -> [(Integer,Integer)]
mergeRanges (r@(f, s) : rs) = undefined

groupRanges rs = groupBy rangeOverlapsWith $ sortBy (comparing fst) rs

mergeGroups = map multiMergeRange

sumRange (f, s) = (s - f) + 1

sumRanges rs = sum $ map sumRange rs

inRange q (a, b) = a - q <= 0 && b - q >= 0

inRanges q = any (\x -> q `inRange` x)

window n xs
  | length xs >= n = take n xs : window n (drop 1 xs)
  | otherwise = [] -- no worky

window2 xs = map (\x -> (head x, x !! 1)) $ window 2 xs -- no worky

window2' xs = zip xs $ drop 1 xs

reMergeRanges = iterate (mergeGroups . groupRanges) -- continuously try to merge ranges

mergeConverge rs = fst $ head $ dropWhile (uncurry (/=)) (window2' $ reMergeRanges rs) -- merge until no longer possible

-- parsing
parser = parse input ""

parser' = parse input' ""

input = do
  ranges <- many (Main.range <* endOfLine)
  endOfLine
  products <- Main.id `sepBy` endOfLine
  -- eof
  return (ranges, products)

input' = do
  ranges <- many (Main.range <* endOfLine)
  endOfLine
  products <- manyTill id' eof
  return (ranges, products)

ranges = many $ Main.range `sepBy` endOfLine

range = do
  f <- many1 digit
  char '-'
  s <- many1 digit
  return (read f :: Integer, read s :: Integer)

ids = many1 $ Main.id `sepBy` endOfLine

id = do
  p <- many1 digit
  return (read p :: Integer)

id' = do
  p <- many1 digit
  endOfLine
  return (read p :: Integer)