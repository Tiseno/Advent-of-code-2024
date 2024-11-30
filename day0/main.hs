{-# OPTIONS_GHC -Wall #-}

parseInput = lines

part1 = id

main :: IO ()
main = do
  input <- readFile "input.txt"
  let parsed = parseInput input
  putStrLn "Part 1"
  print $ part1 parsed
