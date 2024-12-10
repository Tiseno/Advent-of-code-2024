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

data Op = Add | Mul | Cat

perform :: (Num a, Read a, Show a) => Op -> a -> a -> a
perform Add a b = a + b
perform Mul a b = a * b
perform Cat a b = read (show a ++ show b)

allResults :: [Op] -> Int -> Int -> [Int]
allResults operators acc n = do
  op <- operators
  case acc of
    (-1) -> [n]
    prev -> [perform op prev n]

validEquation :: [Op] -> (Int, [Int]) -> Maybe Int
validEquation operators (testValue, numbers) =
  if testValue `elem` Monad.foldM (allResults operators) (-1) numbers
     then Just testValue
     else Nothing

solve :: [Op] -> [(Int, [Int])] -> Int
solve operators equations = sum $ Maybe.mapMaybe (validEquation operators) equations

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.txt"
  putStrLn "Part 1"
  print $ solve [Add, Mul] input
  putStrLn "Part 2"
  print $ solve [Add, Mul, Cat] input

