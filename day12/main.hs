{-# OPTIONS_GHC -Wall #-}
import qualified Data.Array as Array
import qualified Data.List  as List

type Pos = (Int, Int)
type Plot = (Char, Bool)
type Plots = Array.Array Pos Plot

parseInput :: String -> Plots
parseInput input =
  let bounds = ((0,0), (length (head $ lines input) - 1, length (lines input) - 1))
   in Array.array bounds [((x, y), (c, False)) | (y, row) <- zip [0..] $ lines input, (x, c) <- zip [0..] row]

findUnfilled :: Plots -> Maybe (Pos, Plot)
findUnfilled plots = foldr findIt Nothing $ Array.assocs plots
    where
      findIt _ r@(Just _)              = r
      findIt posPlot@(_, (_, False)) _ = Just posPlot
      findIt _ _                       = Nothing

allDirectionsInBounds :: (Array.Ix a, Array.Ix b, Num a, Num b) => ((a, b), (a, b)) -> (a, b) -> [(a, b)]
allDirectionsInBounds bounds (x, y) = filter (Array.inRange bounds) [(x + 1, y),(x - 1, y),(x, y + 1),(x, y - 1)]

fillRegion :: Plots -> Maybe (Int, Int, Plots)
fillRegion (ps :: Plots) =
  case findUnfilled ps of
    Nothing            -> Nothing
    Just (pos, (c, _)) -> Just $ fillRegion' c [pos] 0 0 ps
  where
    bounds = Array.bounds ps
    fillRegion' :: Char -> [Pos] -> Int -> Int -> Array.Array Pos Plot -> (Int, Int, Plots)
    fillRegion' _ [] area perimeter plots = (area, perimeter, plots)
    fillRegion' c (next:stack) area perimeter plots =
      let dirs = allDirectionsInBounds bounds next
          ofSamePlantNotVisited = List.filter (\pos -> let (c', filled) = plots Array.! pos in c' == c && not filled && pos `List.notElem` stack) dirs
          ofNotSamePlant = List.filter (\pos -> let (c', _) = plots Array.! pos in c' /= c) dirs
          otherPlantsPerim = length ofNotSamePlant
          outerBoundsPerim = boundTouches bounds next
          perim = otherPlantsPerim + outerBoundsPerim
          plots' = plots Array.// [(next, (c, True))]
       in fillRegion' c (stack ++ ofSamePlantNotVisited) (area + 1) (perimeter + perim) (perim `seq` plots')

boundTouches :: (Pos, Pos) -> Pos -> Int
boundTouches ((_, (width, height)) :: (Pos, Pos)) ((x, y) :: Pos) = (if x == 0 then 1 else 0) + (if y == 0 then 1 else 0) + (if x == width then 1 else 0) + (if y == height then 1 else 0)


allRegions :: Plots -> [(Int, Int)]
allRegions plots =
  case fillRegion plots of
    Nothing -> []
    Just (area, perimeter, plots') ->
      (area, perimeter) : allRegions plots'

calculateCost :: (Int, Int) -> Int
calculateCost (area, perimeter) = area * perimeter

part1 :: Plots -> Int
part1 (plots :: Plots) = sum $ calculateCost <$> allRegions plots

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.txt"
  putStrLn "Part 1"
  print $ part1 input
