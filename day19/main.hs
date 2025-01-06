{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
import qualified Data.List       as List
import qualified Data.List.Split as Split
import qualified Data.Map        as Map
import qualified Data.Set        as Set

parseInput :: String -> ([String], [String])
parseInput input =
  let [towels, patterns] = Split.splitOn "\n\n" input
   in (Split.splitOn ", " towels, lines patterns)

part1 :: ([String], [String]) -> Int
part1 (towels, patterns) = length $ filter id $ fmap possiblePattern patterns
  where
    possiblePattern :: String -> Bool
    possiblePattern pattern = possiblePatterns [pattern]
    possiblePatterns :: [String] -> Bool
    possiblePatterns p
      | null p           = False
      | "" `List.elem` p = True
      | otherwise        = possiblePatterns $ List.nub $ concatMap possibleTails p
    possibleTails :: String -> [String]
    possibleTails p = (\prefix -> drop (length prefix) p) <$> filter (`List.isPrefixOf` p) towels

sumWithMemo :: (Foldable t, Num b) => a1 -> (a1 -> a2 -> (a1, b)) -> t a2 -> (a1, b)
sumWithMemo m fn = foldr (\a (m0, s) ->
  let (m1, r) = fn m0 a
   in (m1, s + r)) (m, 0)

part2 :: ([String], [String]) -> Int
part2 (towels, patterns) = sumOfAllPossibleCombinations
  where
    (_, sumOfAllPossibleCombinations) = sumWithMemo Map.empty numberOfPossibleCombinationsToCreatePattern patterns
    towelSet = Set.fromList towels
    numberOfPossibleCombinationsToCreatePattern :: Map.Map String Int -> String -> (Map.Map String Int, Int)
    numberOfPossibleCombinationsToCreatePattern m p | p `Map.member` m = (m, m Map.! p)
    numberOfPossibleCombinationsToCreatePattern m0 p =
      let tails = possibleTails p
          (m1, tailsResult) = sumWithMemo m0 numberOfPossibleCombinationsToCreatePattern tails
          result = (if p `Set.member` towelSet then 1 else 0) + tailsResult
          m2 = Map.insert p result m1
       in (m2, result)
    possibleTails :: String -> [String]
    possibleTails p = (\prefix -> drop (length prefix) p) <$> filter (`List.isPrefixOf` p) towels

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.txt"
  putStrLn "Part 1"
  print $ part1 input
  putStrLn "Part 2"
  print $ part2 input
