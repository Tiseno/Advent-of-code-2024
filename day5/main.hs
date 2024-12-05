-- {-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
import qualified Data.List.Split as Split

parseInput :: String -> ([(Int, Int)], [[Int]])
parseInput input =
  let [rules, updates] = (lines <$> Split.splitOn "\n\n" input)
      parseRule = (\[a, b] -> (read a :: Int, read b :: Int)) . Split.splitOn "|"
      parseUpdate = fmap (\u -> read u :: Int) . Split.splitOn ","
   in (fmap parseRule rules, fmap parseUpdate updates)

part1 (rules, updates) =
  let validUpdates = filter noViolation updates
   in sum $ fmap takeMiddleElement validUpdates
  where
    noViolation update = not $ hasViolation [] update
    hasViolation _ [] = False
    hasViolation previous (next:future) =
      let prohibitedPreviousForNext = fmap snd $ filter (\(a, _) -> a == next) rules
          hasPreviousViolation = any (`elem` prohibitedPreviousForNext) previous
          prohibitedFutureForNext = fmap fst $ filter (\(_, b) -> b == next) rules
          hasFutureViolation = any (`elem` prohibitedFutureForNext) future
       in hasPreviousViolation || hasFutureViolation || hasViolation (next:previous) future
    takeMiddleElement update = update !! (length update `div` 2)

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.txt"
  putStrLn "Part 1"
  print $ part1 input
