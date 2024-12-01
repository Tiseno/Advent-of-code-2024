{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
import qualified Data.List as List

parseInput :: String -> ([Int], [Int])
parseInput = unzip . fmap ((\[a, b] -> (read a, read b)) . words) . lines

part1 :: ([Int], [Int]) -> Int
part1 (left, right) = sum $ (\(l, r) -> abs (r - l)) <$> zip (List.sort left) (List.sort right)

part2 :: ([Int], [Int]) -> Int
part2 (left, right) = sum $ (\n -> n * length (filter (== n) right)) <$> left

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.txt"
  putStrLn "Part 1"
  print $ part1 input
  putStrLn "Part 2"
  print $ part2 input
