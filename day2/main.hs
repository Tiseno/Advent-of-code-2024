import qualified Data.List as List

type Report = [Int]

parseInput :: String -> [Report]
parseInput = fmap (fmap read . words) . lines

isSafe :: Report -> Bool
isSafe report = allChangeByAtMost3 id report || allChangeByAtMost3 negate report
  where
    allChangeByAtMost3 fn (x:y:xs) =
      let diff = fn (y - x) in diff > 0 && diff <= 3 && allChangeByAtMost3 fn (y:xs)
    allChangeByAtMost3 _ _       = True

isSafeWithDampener :: Report -> Bool
isSafeWithDampener report = any (isSafe . (\i -> take i report ++ drop (i + 1) report)) [0..(length report - 1)]

countSafeReports :: (Report -> Bool) -> [Report] -> Int
countSafeReports isSafeFn = length . List.filter isSafeFn

main :: IO ()
main = do
  reports <- parseInput <$> readFile "input.txt"
  putStrLn "Part 1"
  print $ countSafeReports isSafe reports
  putStrLn "Part 2"
  print $ countSafeReports isSafeWithDampener reports
