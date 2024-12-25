{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
import qualified Data.Map   as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set   as Set

type Pos = (Int, Int)
data Dir = North | West | South | East deriving (Eq, Ord, Show)
type Node = (Pos, Dir)

forldr :: Foldable t => t a -> b -> (a -> b -> b) -> b
forldr t b fn = foldr fn b t

parseInput :: [Char] -> (Set.Set Pos, Node, Pos)
parseInput input =
  let (walls, Just start, Just end) =
        forldr (zip [0..] $ lines input) (Set.empty, Nothing, Nothing) (\(y, row) (m, s, e) ->
          forldr (zip [0..] row) (m, s, e) (\(x, a) (m', s', e') ->
            ( if a == '#' then Set.insert (x, y) m' else m'
            , if a == 'S' then Just ((x, y), East) else s'
            , if a == 'E' then Just (x, y) else e')
            )
          )
   in (walls, start, end)

type Cost = Int

dirDelta :: (Num a, Num b) => Dir -> (a, b)
dirDelta North = (0, -1)
dirDelta West  = (-1, 0)
dirDelta South = (0, 1)
dirDelta East  = (1, 0)

add :: Pos -> Pos -> Pos
add (ax, ay) (bx, by) = (ax + bx, ay + by)

stepOne :: Pos -> Dir -> Pos
stepOne pos dir = pos `add` dirDelta dir

turnLeft :: Dir -> Dir
turnLeft North = West
turnLeft West  = South
turnLeft South = East
turnLeft East  = North

turnRight :: Dir -> Dir
turnRight North = East
turnRight West  = North
turnRight South = West
turnRight East  = South

part1 :: (Set.Set Pos, Node, Pos) -> Cost
part1 (walls, start, end) =
  minimum $ Maybe.catMaybes
    [ Map.lookup (end, North) allDistances
    , Map.lookup (end, West) allDistances
    , Map.lookup (end, South) allDistances
    , Map.lookup (end, East) allDistances
    ]
  where
    allDistances = dijkstra Map.empty [(start, 0)]
    insertSmallerDistance (node, cost) distances0 =
      let previousCost = Maybe.fromMaybe maxBound $ Map.lookup node distances0
       in Map.insert node (min cost previousCost) distances0
    insertTentativeNode (n :: (Node, Cost)) [] = [n]
    insertTentativeNode n@(pos, cost) (t@(tPos, tCost):ts)
      | pos == tPos = t : ts
      | cost < tCost = n : t : ts
      | otherwise = t : insertTentativeNode n ts
    moveCost = 1
    turnCost = 1000
    movesFromNode ((pos, dir), cost) =
      [ ((pos `stepOne` dir, dir), cost + moveCost)
      , ((pos, turnLeft dir), cost + turnCost)
      , ((pos, turnRight dir), cost + turnCost)
      ]
    allAllowedMovesFromNode (distances0 :: Map.Map Node Cost) (nodeWithCost :: (Node, Cost)) =
      filter (\(newNode@(pos, _), _) -> pos `Set.notMember` walls && newNode `Map.notMember` distances0) $ movesFromNode nodeWithCost
    dijkstra (distances0 :: Map.Map Node Cost) ([] :: [(Node, Cost)]) = distances0
    dijkstra distances0 (current:tentative0) =
      let distances1 = insertSmallerDistance current distances0
          allMoves = allAllowedMovesFromNode distances1 current
          tentative1 = foldr insertTentativeNode tentative0 allMoves
       in dijkstra distances1 tentative1

main :: IO ()
main = do
  putStrLn "Part 1"
  input <- parseInput <$> readFile "input.txt"
  print $ part1 input
