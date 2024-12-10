{-# OPTIONS_GHC -Wall #-}
import qualified Data.Array as Array
import qualified Data.Set   as Set
import           Prelude    hiding (map)

type Pos = (Int, Int)
type Map = Array.Array Pos Int
parseInput :: String -> Map
parseInput input = Array.array ((0, 0), (length (head $ lines input) - 1, length (lines input) - 1)) [((x, y), read [h]) | (y, row) <- zip [0..] $ lines input, (x, h) <- zip [0..] row]

allDirections :: Map -> Pos -> [Pos]
allDirections (map :: Map) ((x, y) :: Pos) =
  let bounds = Array.bounds map
   in filter (Array.inRange bounds) [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

walkToEnd :: (Set.Set Pos, Map) -> Pos -> (Set.Set Pos, [Pos])
walkToEnd (visited, _) pos | pos `Set.member` visited = (visited, [])
walkToEnd (visited, map) pos | map Array.! pos == 9 = (Set.insert pos visited, [pos])
walkToEnd (visited, map) pos =
  let currentHeight = map Array.! pos
      newVisited = Set.insert pos visited
      newPositions = filter (\p -> map Array.! p == (currentHeight + 1)) $ allDirections map pos
   in foldr folder (newVisited, []) newPositions
    where
      folder p (vis, acc) = let (nVis, nPos) = walkToEnd (vis, map) p in (nVis, acc ++ nPos)

part1 :: Map -> Int
part1 (map :: Map) =
  let trailHeads = fmap fst $ filter ((== 0) . snd) $ Array.assocs map
   in length $ concatMap (snd . walkToEnd (Set.empty, map)) trailHeads

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.txt"
  putStrLn "Part 1"
  print $ part1 input
