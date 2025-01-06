{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
import qualified Data.List       as List
import qualified Data.List.Split as Split

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

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.txt"
  putStrLn "Part 1"
  print $ part1 input
