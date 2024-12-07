{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
import qualified Control.Monad   as Monad
import qualified Data.List.Extra as Split
import qualified Data.Maybe      as Maybe

parseInput :: String -> [(Int, [Int])]
parseInput input = parseLine <$> lines input
  where
    parseLine l =
      let [test, ns] = Split.splitOn ":" l
       in (read test, fmap read (words ns))

operators :: [Int -> Int -> Int]
operators = [(+), (*)]

allResults :: Int -> Int -> [Int]
allResults acc (n::Int) = do
  op <- operators
  case acc of
    (-1) -> [n]
    prev -> [op prev n]

validEquation :: (Int, [Int]) -> Maybe Int
validEquation (testValue, numbers) =
  if testValue `elem` Monad.foldM allResults (-1) numbers
     then Just testValue
     else Nothing

part1 :: [(Int, [Int])] -> Int
part1 (equations :: [(Int, [Int])]) = sum $ Maybe.mapMaybe validEquation equations

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.txt"
  putStrLn "Part 1"
  print $ part1 input

