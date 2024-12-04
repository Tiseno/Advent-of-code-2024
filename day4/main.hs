{-# OPTIONS_GHC -Wall #-}
import qualified Data.List       as List
import           Text.Regex.TDFA

diagonalTranspose :: [String] -> [[Char]]
diagonalTranspose input = List.transpose ((\(i, l) -> List.replicate i '.' ++ l ++ List.replicate (length input - i) '.' ) <$> zip [0..] input)

diagonalTranspose2 :: [String] -> [[Char]]
diagonalTranspose2 input = List.transpose ((\(i, l) -> List.replicate (length input - i) '.' ++ l ++ List.replicate i '.') <$> zip [0..] input)

count :: (Foldable t, RegexContext Regex source1 [[String]]) => String -> t source1 -> Int
count (word::String) input = length $ concatMap (\l -> concat ((l =~ word)::[[String]])) input

part1 :: [String] -> Int
part1 input =
  count "XMAS" input
  + count "XMAS" (List.transpose input)
  + count "XMAS" (diagonalTranspose input)
  + count "XMAS" (diagonalTranspose2 input)
  + count "SAMX" input
  + count "SAMX" (List.transpose input)
  + count "SAMX" (diagonalTranspose input)
  + count "SAMX" (diagonalTranspose2 input)

main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  putStrLn "Part 1"
  print $ part1 input
