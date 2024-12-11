import qualified Data.List.Extra as List
import qualified Data.Map        as Map
import qualified Data.Ord        as Ord

parseInput :: String -> [Int]
parseInput = fmap read . words . head . lines

transform :: Int -> [Int]
transform 0 = [1]
transform stone | even $ length $ show stone =
  let stoneString = show stone
      splitLength = length stoneString `div` 2
   in [read $ take splitLength stoneString, read $ drop splitLength stoneString]
transform stone = [stone * 2024]

transformCounted :: Map.Map Int Int -> Map.Map Int Int
transformCounted stonesWithCount =
  let newStonesWithCounts = concatMap (\(stone, count) -> (,count) <$> transform stone) (Map.assocs stonesWithCount)
      grouped = List.groupSortBy (Ord.comparing fst) newStonesWithCounts
      sumCounts = foldr (\(stone, count) (_, s) -> (stone, s + count)) (0, 0)
   in Map.fromAscList $ fmap sumCounts grouped

solve :: Int -> [Int] -> Int
solve i input = foldr (\(_, c) s -> s + c) 0 $ Map.assocs $ iterate transformCounted (Map.fromAscList (fmap (,1) input)) !! i

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.txt"
  putStrLn "Part 1"
  print $ solve 25 input
  putStrLn "Part 2"
  print $ solve 75 input
