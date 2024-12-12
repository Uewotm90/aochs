import Parsing

p =
  do
    num <- int
    space
    rest <- p
    return $ toInteger num : rest
    <|> do
      num <- int
      space
      return [toInteger num]
splitInteger :: Integer -> [Integer]
splitInteger x = pair (splitAt (length s `div` 2) s)
    where
        s = show x
        pair (a,b) = [read a,read b]
evenDigit x = even (length $ show x)

transform :: Integer -> [Integer]
transform n | n == 0 = [1]
            | evenDigit n = splitInteger n
            | otherwise = [n * 2024]

blink xs = concatMap transform xs
testInput = [0,1,10,99,999]
testinput' = [125,17]


blinks 0 xs = xs
blinks n xs = blinks (n-1) $ blink xs

firstSol = do
    input <- readFile "./input/11.txt"
    return $ length $ blinks 25 $ fst $ head $ parse p input