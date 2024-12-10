{-# OPTIONS_GHC -Wall #-}
import qualified Data.Array as Array
import qualified Data.Set   as Set
import           Prelude    hiding (map)

type Pos = (Int, Int)
type Map = Array.Array Pos Int
parseInput :: String -> Map
parseInput input = Array.array ((0, 0), (length (head $ lines input) - 1, length (lines input) - 1)) [((x, y), read [h]) | (y, row) <- zip [0..] $ lines input, (x, h) <- zip [0..] row]

allDirections :: Map -> Pos -> [Pos]
allDirections map (x, y) =
  let bounds = Array.bounds map
   in filter (Array.inRange bounds) [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

walkToEnds :: (Set.Set Pos, Map) -> Pos -> (Set.Set Pos, [Pos])
walkToEnds (visited, _) pos | pos `Set.member` visited = (visited, [])
walkToEnds (visited, map) pos | map Array.! pos == 9 = (Set.insert pos visited, [pos])
walkToEnds (visited, map) pos =
  let currentHeight = map Array.! pos
      newVisited = Set.insert pos visited
      newPositions = filter (\p -> map Array.! p == (currentHeight + 1)) $ allDirections map pos
   in foldr folder (newVisited, []) newPositions
    where
      folder p (vis, acc) = let (nVis, nPos) = walkToEnds (vis, map) p in (nVis, acc ++ nPos)

findTrailheads :: Map -> [Pos]
findTrailheads map = fmap fst $ filter ((== 0) . snd) $ Array.assocs map

part1 :: Map -> Int
part1 map = length $ concatMap (snd . walkToEnds (Set.empty, map)) $ findTrailheads map

countAllPaths :: Map -> Pos -> Int
countAllPaths map pos | map Array.! pos == 9 = 1
countAllPaths map pos =
  let currentHeight = map Array.! pos
      newPositions = filter (\p -> map Array.! p == (currentHeight + 1)) $ allDirections map pos
   in foldr (\p acc -> acc + countAllPaths map p) 0 newPositions

part2 :: Map -> Int
part2 map = sum $ countAllPaths map <$> findTrailheads map

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.txt"
  putStrLn "Part 1"
  print $ part1 input
  putStrLn "Part 2"
  print $ part2 input
