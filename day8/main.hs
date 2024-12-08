{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
import qualified Data.MultiMap as MultiMap
import qualified Data.Set      as Set

parseInput :: String -> ((Int, Int), [((Int, Int), Char)])
parseInput input =
  let l = lines input
      height = length l
      width = length $ head l
      antennas = [((x, y), c) | (y, row) <- zip [0..] $ lines input, (x, c) <- zip [0..] row, c /= '.']
   in ((width, height), antennas)

createAntiNodesForPair :: ((Int, Int), (Int, Int)) -> [(Int, Int)]
createAntiNodesForPair ((xA, yA), (xB, yB)) =
  let xD = xA - xB
      yD = yA - yB
   in [(xA + xD, yA + yD), (xB - xD, yB - yD)]

allPairs :: Eq a => [a] -> [(a, a)]
allPairs list = [(a, b) | (i, a) <- zip [(0 :: Int)..] list, (j, b) <- zip [(0 :: Int)..] list, j > i]

createAntiNodesFromAntennas :: [(Int, Int)] -> [(Int, Int)]
createAntiNodesFromAntennas antennaPositions = concatMap createAntiNodesForPair $ allPairs antennaPositions

inBounds :: (Int, Int) -> (Int, Int) -> Bool
inBounds (width, height) (x, y) = x >= 0 && x < width && y >= 0 && y < height

part1 :: ((Int, Int), [((Int, Int), Char)]) -> Int
part1 ((bounds, antennas) :: ((Int, Int), [((Int, Int), Char)])) = length $ Set.fromList $ concatMap (filter (inBounds bounds) . createAntiNodesFromAntennas) (MultiMap.elems (foldr (\(pos, c) m -> MultiMap.insert c pos m) MultiMap.empty antennas))

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.txt"
  putStrLn "Part 1"
  print $ part1 input
