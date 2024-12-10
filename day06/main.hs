import qualified Data.Set as Set

type Pos = (Int, Int)
data Dir = L | R | U | D deriving (Show, Eq, Ord)

parseInput :: String -> ((Int, Int), Pos, Dir, Set.Set Pos)
parseInput input =
  let rows = lines input
      bounds = (length $ head rows, length rows)
      objects = [((x, y), c) | (y, row) <- zip [0..] rows, (x, c) <- filter ((/= '.') . snd) $ zip [0..] row]
      [guard] = fst <$> filter ((== '^') . snd) objects
      walls = Set.fromList $ fst <$> filter ((== '#') . snd) objects
   in (bounds, guard, U, walls)

takeOneStep :: Pos -> Dir -> Pos
takeOneStep (x, y) L = (x - 1, y)
takeOneStep (x, y) R = (x + 1, y)
takeOneStep (x, y) U = (x, y - 1)
takeOneStep (x, y) D = (x, y + 1)

turnRight :: Dir -> Dir
turnRight L = U
turnRight U = R
turnRight R = D
turnRight D = L

walk :: (Int, Int) -> Pos -> Dir -> Set.Set Pos -> (Set.Set Pos, Bool)
walk (width, height) guardPos guardDir walls = go Set.empty Set.empty guardPos guardDir
  where
    leftBounds (x, y) = x < 0 || x >= width || y < 0 || y >= height
    insideWall pos = Set.member pos walls
    inLoop = Set.member
    step pos dir =
      let newPos = takeOneStep pos dir
       in if insideWall newPos then step pos (turnRight dir) else (newPos, dir)
    go visited path pos dir =
      let newVisited = Set.insert pos visited
          newPath = Set.insert (pos, dir) path
          (newPos, newDir) = step pos dir
       in if leftBounds newPos then (newVisited, False)
          else if inLoop (newPos, newDir) newPath then (newVisited, True)
          else go newVisited newPath newPos newDir

part1 :: ((Int, Int), Pos, Dir, Set.Set Pos) -> Int
part1 (bounds, guardPos, dir, walls) = length $ fst $ walk bounds guardPos dir walls

part2 :: ((Int, Int), Pos, Dir, Set.Set Pos) -> Int
part2 (bounds@(width, height), guardPos, dir, walls) = length walksWhereGuardGetsStuck
  where
    extraWalls = filter (/= guardPos) $ filter (`Set.notMember` walls) $ [(x, y) | x <- [0..width], y <- [0..height]]
    walks = fmap (\extraWall -> walk bounds guardPos dir (Set.insert extraWall walls)) extraWalls
    gotStuckInLoop (_, stuck) = stuck
    walksWhereGuardGetsStuck = filter gotStuckInLoop walks

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.txt"
  putStrLn "Part 1"
  print $ part1 input
  putStrLn "Part 2"
  print $ part2 input
