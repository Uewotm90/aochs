import Parsing

data Direction = Left | Right | Up | Down deriving (Show)

data Point = Obstruction | Empty Bool | Guard Direction deriving (Show)

type Grid = [[Point]]

parseEmpty = do
  empty <- symbol "."
  return $ Empty False

parseObstruction = do
  obstruction <- symbol "#"
  return Obstruction

parseGuard = do
  guard <- symbol "^"
  return $ Guard Up

parseMap' =
  do
    empty <- parseEmpty
    rest <- parseMap'
    return $ empty : rest
    <|> do
      obs <- parseObstruction
      rest <- parseMap'
      return $ obs:rest
    <|> do
      guard <- parseGuard
      rest <- parseMap'
      return $ guard:rest
      <|> do
        item
        parseMap'
      <|> do
        empty
        return []

parseMap =
  do
    empty <- symbol "."
    rest <- parseMap
    return $ Empty False : rest
    <|> do
      obstruction <- symbol "#"
      rest <- parseMap
      return $ Obstruction : rest
    <|> do
      guard <- symbol "^"
      rest <- parseMap
      return $ Guard Up : rest
    <|> do
      empty
      return []

testinput = ["....#.....", ".........#", "..........", "..#.......", ".......#..", "..........", ".#..^.....", "........#.", "#.........", "......#..."]
