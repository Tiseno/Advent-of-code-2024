import qualified Control.Monad   as Monad
import qualified Data.List       as List
import qualified Data.List.Split as Split

parseInput :: String -> ([(Int, Int)], [[Int]])
parseInput input =
  let [rules, updates] = (lines <$> Split.splitOn "\n\n" input)
      parseRule = (\[a, b] -> (read a :: Int, read b :: Int)) . Split.splitOn "|"
      parseUpdate = fmap (\u -> read u :: Int) . Split.splitOn ","
   in (fmap parseRule rules, fmap parseUpdate updates)

atValidPosition :: [(Int,Int)] -> [Int] -> Int -> [Int] -> Bool
atValidPosition rules previous next future =
  let prohibitedPreviousForNext = snd <$> filter (\(a, _) -> a == next) rules
      previousViolation = any (`elem` prohibitedPreviousForNext) previous
      prohibitedFutureForNext = fst <$> filter (\(_, b) -> b == next) rules
      futureViolation = any (`elem` prohibitedFutureForNext) future
   in not previousViolation && not futureViolation

validUpdate :: [(Int, Int)] -> [Int] -> Bool
validUpdate rules = hasNoViolation []
  where
    hasNoViolation _ [] = True
    hasNoViolation previous (next:future) = atValidPosition rules previous next future && hasNoViolation (next:previous) future

sumMiddles :: [[Int]] -> Int
sumMiddles = sum . fmap (\update -> update !! (length update `div` 2))

part1 :: [[Int]] -> Int
part1 = sumMiddles

insertAt :: Int -> a -> [a] -> [a]
insertAt index e = insertAt' 0
  where
    insertAt' _ []     = [e]
    insertAt' i (x:xs) = if i == index then e:x:xs else x : insertAt' (i+1) xs

part2 :: [(Int, Int)] -> [[Int]] -> Int
part2 rules invalidUpdates = sumMiddles $ concatMap buildValidUpdate invalidUpdates -- Assumes only one valid ordering per update is produced
  where
    buildValidUpdate = Monad.foldM buildAllValid []
    buildAllValid :: [Int] -> Int -> [[Int]]
    buildAllValid prev x = do
      insertionPoint <- findValidInsertionPoints prev x
      pure $ insertAt insertionPoint x prev
    findValidInsertionPoints :: [Int] -> Int -> [Int]
    findValidInsertionPoints update x = f 0 [] x update
      where
        f :: Int -> [Int] -> Int -> [Int] -> [Int]
        f i prev x [] = [i | atValidPosition rules prev x []]
        f i prev x next@(n:ext) = ([i | atValidPosition rules prev x next]) ++ f (i+1) (prev ++ [n]) x ext

main :: IO ()
main = do
  (rules, updates) <- parseInput <$> readFile "input.txt"
  let (validUpdates, invalidUpdates) = List.partition (validUpdate rules) updates
  putStrLn "Part 1"
  print $ part1 validUpdates
  putStrLn "Part 2"
  print $ part2 rules invalidUpdates
