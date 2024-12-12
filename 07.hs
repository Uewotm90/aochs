import Parsing
import Debug.Trace

data Line = Equation Int [Int] deriving (Show)

testinput = ["190: 10 19", "3267: 81 40 27", "83: 17 5", "156: 15 6", "7290: 6 8 6 15", "161011: 16 10 13", "192: 17 8 14", "21037: 9 7 18 13", "292: 11 6 16 20"]

target = do
  val <- int
  symbol ":"
  return val

rhs = do
  num <- integer
  rest <- rhs
  return (num : rest)
  <|> do
    num <- integer
    return [num]

line = do
  lhs <- target
  right <- rhs
  return $ Equation lhs right

-- combinations:: [int] -> [[Integer]]
combinations [_] = []
combinations (x:xs) = do
    y <- xs
    -- return [x+y,x*y]:(concat (combinations xs))
    return $ concat ([x+y,x*y]:combinations xs)

bigIntIze :: Line -> (Int, [Integer])
bigIntIze (Equation n xs) = (n, map toInteger xs)

satisfiable (Equation t xs) =  t `elem` concat (combinations xs)

parseLines = do
    input <- readFile "./input/07.txt"
    return $ map (fst .head . parse line) (lines input)

firstSol = do
    lines <- parseLines
    return $ sum $ map (\(Equation s _)-> s) $ filter satisfiable lines
    --a <- mapM (\x ->print x; return x) $ map (\(Equation s _)-> s) $ filter satisfiable lines

main = do
    print "solving first problem"
    first <- firstSol
    print first
    return ()


