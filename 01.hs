import Parsing
import Data.List

-- data Line = L Int Int deriving Show
type Line = (Int, Int)

line :: Parser (Int, Int)
line = do
    left <- int
    space
    right <- int
    return (left,right)

lines' :: [String] -> [Line]
lines' xs = map (fst. head . parse line ) xs

leftandrights :: [(a, b)] -> ([a], [b])
leftandrights xs = (map fst xs, map snd xs)

sortpairs :: (Ord a1, Ord a2) => ([a1], [a2]) -> ([a1], [a2])
sortpairs (ls,rs) = (sort ls, sort rs)

mindist :: Num a => ([a], [a]) -> [a]
mindist ([],[]) = []
mindist (x:xs,y:ys) =  abs (x-y) : mindist (xs,ys)


firstsolution :: IO ()
firstsolution = do
    input <- readFile "./input/01.txt"
    -- map (parse line (lines))
    print $ sum $ mindist $ sortpairs $ leftandrights $ lines' $ lines $ input
    return ()

-- tihi :)))
similarityscore :: ([Int], [Int]) -> Int 
similarityscore (ls,rs) = sum $ map (\y -> (fst y) * (snd y)) $ filter (\p -> snd p/=0) (map (\x -> (x,length $ filter (==x) rs) ) ls)

secondsolution = do
    input <- readFile "./input/01.txt"
    print $ similarityscore $ sortpairs $ leftandrights $ lines' $ lines $ input
    return ()