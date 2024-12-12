import Parsing

xmas = "XMAS"

-- should have 18 occurences total 
test = ["MMMSXXMASM", "MSAMXMSMSA", "AMXSXMAAMM", "MSAMASMSMX", "XMASAMXAMM", "XXAMMXXAMA", "SMSMSASXSS", "SAXAMASAAA", "MAMMMXMMMM", "MXMXAXMASX"]

firstsol = do
  input <- readFile "./input/04.txt"
  print $ lines input
  return ()