{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
import qualified Data.List.Split as Split
import qualified Data.Map        as Map

type Vec = (Int, Int)
type Robot = (Vec, Vec)
type Robots = Map.Map Int Robot

parseInput :: String -> Robots
parseInput = Map.fromAscList . zip [0..] . fmap parseLine . lines
  where
    parseLine l =
      let [_:_:p, _:_:v] = Split.splitOn " " l
       in (read ("(" ++ p ++ ")"), read ("(" ++ v ++ ")"))

teleport :: Int -> Int -> Int
teleport bound p
  | p < 0 = p + bound
  | p >= bound = p - bound
  | otherwise = p

robotMovement :: Int -> Int -> Robot -> Robot
robotMovement (width :: Int) (height :: Int) (((px, py), (vx, vy)) :: Robot) =
  ((teleport width (px + vx), teleport height (py + vy)), (vx, vy))

moveAllRobots :: Int -> Int -> Robots -> Robots
moveAllRobots (width :: Int) (height :: Int) (robots :: Robots) =
  fmap (robotMovement width height) robots

part1 :: Int -> Int -> Int -> Robots -> Int
part1 (width :: Int) (height :: Int) (steps :: Int) (robots :: Robots) =
  let finalRobots = iterate (moveAllRobots width height) robots !! steps
      (tl, tr, bl, br) = foldr robotsInQuads (0, 0, 0, 0) finalRobots
   in product [tl,tr,bl,br]
  where
    top = (0, height `div` 2 - 1)
    bot = (height `div` 2 + 1, height - 1)
    left = (0, width `div` 2 - 1)
    right = (width `div` 2 + 1, width - 1)
    within i (mmin, mmax) = i >= mmin && i <= mmax
    withinQuad (px, py) qy qx = within py qy && within px qx
    robotsInQuads ((p, _) :: Robot) (tl, tr, bl, br) =
      ( tl + if withinQuad p top left then 1 else 0
      , tr + if withinQuad p top right then 1 else 0
      , bl + if withinQuad p bot left then 1 else 0
      , br + if withinQuad p bot right then 1 else 0
      )

main :: IO ()
main = do
  exampleInput <- parseInput <$> readFile "input.example.txt"
  putStrLn "Part 1 - example"
  print $ part1 11 7 100 exampleInput
  input <- parseInput <$> readFile "input.txt"
  putStrLn "Part 1"
  print $ part1 101 103 100 input
