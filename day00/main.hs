{-# OPTIONS_GHC -Wall #-}

parseInput = lines

part1 = id

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.txt"
  putStrLn "Part 1"
  print $ part1 input
