{-# OPTIONS_GHC -Wall #-}
parseInput :: String -> [Int]
parseInput = fmap read . words . head . lines

transform :: Int -> [Int]
transform 0 = [1]
transform stone | even $ length $ show stone =
  let stoneString = show stone
      splitLength = length stoneString `div` 2
   in [read $ take splitLength stoneString, read $ drop splitLength stoneString]
transform stone = [stone * 2024]

part1 :: [Int] -> Int
part1 (input :: [Int]) = length (iterate (concatMap transform) input !! 25)

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.txt"
  putStrLn "Part 1"
  print $ part1 input
