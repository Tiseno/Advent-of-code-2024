{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
import qualified Data.List  as List
import qualified Data.Map   as Map
import qualified Data.Maybe as Maybe
import qualified Data.Ord   as Ord
import qualified Data.Set   as Set

data Direction = North | West | South | East deriving (Eq, Ord, Show, Enum, Bounded)
data Rotation = CounterClockwise | Clockwise

allDirections :: [Direction]
allDirections = [minBound..maxBound]

type Position = (Int, Int)
type Node = (Position, Direction)

forldr :: Foldable t => t a -> b -> (a -> b -> b) -> b
forldr t b fn = foldr fn b t

parseInput :: [Char] -> (Set.Set Position, Position, Position)
parseInput input =
  let (walls, Just start, Just end) =
        forldr (zip [0..] $ lines input) (Set.empty, Nothing, Nothing) (\(y, row) (m, s, e) ->
          forldr (zip [0..] row) (m, s, e) (\(x, a) (m', s', e') ->
            ( if a == '#' then Set.insert (x, y) m' else m'
            , if a == 'S' then Just (x, y) else s'
            , if a == 'E' then Just (x, y) else e')
            )
          )
   in (walls, start, end)

move :: Position -> Direction -> Position
move (x, y) North = (x, y - 1)
move (x, y) West  = (x - 1, y)
move (x, y) South = (x, y + 1)
move (x, y) East  = (x + 1, y)

opposite :: Direction -> Direction
opposite North = South
opposite West  = East
opposite South = North
opposite East  = West

turn :: Direction -> Rotation -> Direction
turn North Clockwise = East
turn West  Clockwise = North
turn South Clockwise = West
turn East  Clockwise = South
turn dir _           = opposite dir `turn` Clockwise

type Distance = Int
type Distances = Map.Map Node Distance

moveCost :: Distance
moveCost = 1

turnCost :: Distance
turnCost = 1000

findAllDistances :: Set.Set Position -> Node -> Distances
findAllDistances walls start = dijkstra Map.empty [(start, 0)]
  where
    insertSmallerDistance (node, cost) distances0 =
      let previousCost = Maybe.fromMaybe maxBound $ Map.lookup node distances0
       in Map.insert node (min cost previousCost) distances0
    insertTentativeNode n [] = [n]
    insertTentativeNode n@(pos, cost) (t@(tPos, tCost):ts)
      | pos == tPos = t : ts
      | cost < tCost = n : t : ts
      | otherwise = t : insertTentativeNode n ts
    movesFromNode ((pos, dir), cost) =
      [ ((pos `move` dir, dir), cost + moveCost)
      , ((pos, dir `turn` CounterClockwise), cost + turnCost)
      , ((pos, dir `turn` Clockwise), cost + turnCost)
      ]
    allAllowedMovesFromNode distances0 nodeWithCost =
      filter (\(newNode@(pos, _), _) -> pos `Set.notMember` walls && newNode `Map.notMember` distances0)
        $ movesFromNode nodeWithCost
    dijkstra distances0 [] = distances0
    dijkstra distances0 (current:tentative0) =
      let distances1 = insertSmallerDistance current distances0
          allMoves = allAllowedMovesFromNode distances1 current
          tentative1 = foldr insertTentativeNode tentative0 allMoves
       in dijkstra distances1 tentative1

part1 :: Distances -> Position -> Distance
part1 allDistances pos = minimum $ Maybe.mapMaybe ((`Map.lookup` allDistances) . (pos ,)) allDirections

part2 :: Map.Map Node Distance -> Position -> Int
part2 allDistances end = length allPositionsInAllShortestPaths
  where
    allPositionsInAllShortestPaths = Set.fromList $ fst <$> Set.toList allNodesInAllShortestPaths
    allNodesInAllShortestPaths = walkAllPathsInReverse Set.empty [endWithDistance]
    endWithDistance = List.minimumBy (Ord.comparing snd)
      $ Maybe.mapMaybe ((\n -> (n ,) <$> Map.lookup n allDistances) . (end ,)) allDirections
    reverseMovesFromNode ((pos, dir), cost) =
      [ ((pos `move` opposite dir, dir), cost - moveCost)
      , ((pos, dir `turn` CounterClockwise), cost - turnCost)
      , ((pos, dir `turn` Clockwise), cost - turnCost)
      ]
    walkAllPathsInReverse visited [] = visited
    walkAllPathsInReverse visited0 (current@(node, _):tentative) =
      let visited1 = Set.insert node visited0
          allReverseMoves =
            filter (\(newNode, d) -> Just d == Map.lookup newNode allDistances)
            $ reverseMovesFromNode current
          tentative1 = tentative ++ allReverseMoves
       in walkAllPathsInReverse visited1 tentative1

main :: IO ()
main = do
  (walls, start, end) <- parseInput <$> readFile "input.txt"
  putStrLn "Part 1"
  let dists = findAllDistances walls (start, East)
  print $ part1 dists end
  putStrLn "Part 2"
  print $ part2 dists end
