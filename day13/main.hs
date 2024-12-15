{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Functor law" #-}
{-# HLINT ignore "Evaluate" #-}
import qualified Data.List       as List
import qualified Data.List.Split as Split
import qualified Data.Maybe      as Maybe

newtype Vec = Vec (Int, Int) deriving (Show, Eq)

data Machine = Machine { a :: Vec, b :: Vec, prize :: Vec } deriving (Show)

parseInput :: String -> [Machine]
parseInput = fmap parseMachine . fmap lines . Split.splitOn "\n\n"
  where
    toTuple [a,b] = (a,b)
    parseButton = Vec . toTuple . fmap read . Split.splitOn ", Y+" . drop 12
    parsePrize = Vec . toTuple . fmap read . Split.splitOn ", Y=" . drop 9
    parseMachine ([a, b, prize] :: [String]) =
      Machine (parseButton a) (parseButton b) (parsePrize prize)

add :: Vec -> Vec -> Vec
add (Vec (a, b)) (Vec (a', b')) = Vec (a + a', b + b')

scale :: Int -> Vec -> Vec
scale scalar (Vec (a, b)) = Vec (a * scalar, b * scalar)

part1 :: [Machine] -> Int
part1 machines = sum $ Maybe.mapMaybe solveMachine machines
  where
    solveMachine :: Machine -> Maybe Int
    solveMachine m =
      let allCombinations = [(i,j) | i <- [1..100], j <- [1..100]]
          jjj = List.sort $ Maybe.mapMaybe (calculateOne m) allCombinations
       in if null jjj then Nothing else Just $ head jjj
    calculateOne m (aTimes, bTimes) =
      if add (scale aTimes $ a m) (scale bTimes $ b m) == prize m
         then Just $ aTimes * 3 + bTimes * 1
         else Nothing

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.txt"
  putStrLn "Part 1"
  print $ part1 input
