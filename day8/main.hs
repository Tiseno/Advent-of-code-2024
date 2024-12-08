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

createBasicAntiNodesForPair :: (Int, Int) -> ((Int, Int), (Int, Int)) -> [(Int, Int)]
createBasicAntiNodesForPair _ ((xA, yA), (xB, yB)) =
  let xD = xA - xB
      yD = yA - yB
   in [(xA + xD, yA + yD), (xB - xD, yB - yD)]

allPairs :: Eq a => [a] -> [(a, a)]
allPairs list = [(a, b) | (i, a) <- zip [(0 :: Int)..] list, (j, b) <- zip [(0 :: Int)..] list, j > i]

createAntiNodesFromAntennas :: (Int, Int) -> ((Int, Int) -> ((Int, Int), (Int, Int)) -> [(Int, Int)]) -> [(Int, Int)] -> [(Int, Int)]
createAntiNodesFromAntennas bounds createAntiNodeFunction antennaPositions = concatMap (createAntiNodeFunction bounds) $ allPairs antennaPositions

inBounds :: (Int, Int) -> (Int, Int) -> Bool
inBounds (width, height) (x, y) = x >= 0 && x < width && y >= 0 && y < height

solve :: ((Int, Int) -> ((Int, Int), (Int, Int)) -> [(Int, Int)]) -> ((Int, Int), [((Int, Int), Char)]) -> Int
solve createAntiNodeFunction (bounds, antennas) =
  let groupedAntennas = MultiMap.elems (foldr (\(pos, c) m -> MultiMap.insert c pos m) MultiMap.empty antennas)
      antiNodes = concatMap (createAntiNodesFromAntennas bounds createAntiNodeFunction) groupedAntennas
   in length $ Set.fromList $ filter (inBounds bounds) antiNodes

rangeUntilOutOfBounds :: (Int, Int) -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
rangeUntilOutOfBounds bounds pos@(x, y) step@(dx, dy) =
  if inBounds bounds pos then pos : rangeUntilOutOfBounds bounds (x + dx, y + dy) step else []

createAntiNodesForPairWithResonantHarmonics :: (Int, Int) -> ((Int, Int), (Int, Int)) -> [(Int, Int)]
createAntiNodesForPairWithResonantHarmonics bounds (a@(xA, yA), b@(xB, yB)) =
  let xD = xA - xB
      yD = yA - yB
   in rangeUntilOutOfBounds bounds a (xD, yD) ++ rangeUntilOutOfBounds bounds b (-xD, -yD)

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.txt"
  putStrLn "Part 1"
  print $ solve createBasicAntiNodesForPair input
  putStrLn "Part 2"
  print $ solve createAntiNodesForPairWithResonantHarmonics input
