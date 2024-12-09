{-# OPTIONS_GHC -Wall #-}
import qualified Data.Array as Array

parseInput :: String -> Array.Array Int Int
parseInput input =
  let list :: [Int] = fmap (\c -> read [c]) $ head $ lines input
   in Array.array (0 :: Int, length list - 1) $ zip [(0 :: Int)..] list

checkSumPart :: (Num b, Enum b) => b -> b -> b -> b
checkSumPart pos size i = foldl (\s n -> s + n * i) 0 [pos..(pos + size - 1)]

part1 :: Array.Array Int Int -> Int
part1 (arr :: Array.Array Int Int) = go (0, 0) 0 0 (length arr - 1)
  where
    go (_, bufferFiles) (position :: Int) (leftIndex :: Int) (rightIndex :: Int)
      | even leftIndex && bufferFiles /= 0 && leftIndex > rightIndex = checkSumPart position bufferFiles (leftIndex `div` 2)
    go buffer (position :: Int) (leftIndex :: Int) (rightIndex :: Int) | even leftIndex =
      let files = arr Array.! leftIndex
          checkSum = checkSumPart position files (leftIndex `div` 2)
       in checkSum + go buffer (position + files) (leftIndex + 1) rightIndex
    go buffer position leftIndex rightIndex =
      let emptyBlock = arr Array.! leftIndex
       in fillFromBack buffer emptyBlock position leftIndex rightIndex
    fillFromBack (_, 0) _ _ leftIndex rightIndex | leftIndex > rightIndex = 0
    fillFromBack (bufferIndex, bufferFiles) (emptyBlock :: Int) (position :: Int) (leftIndex :: Int) (rightIndex :: Int) =
      let (files, newRightIndex, checksumIndex) =
            if bufferFiles == 0
               then (arr Array.! rightIndex, rightIndex - 2, rightIndex)
               else (bufferFiles, rightIndex, bufferIndex)
          filesToUse = min files emptyBlock
          checkSum = checkSumPart position filesToUse (checksumIndex `div` 2)
       in checkSum + if files >= emptyBlock
          then go (checksumIndex, files - emptyBlock) (position + emptyBlock) (leftIndex + 1) newRightIndex
          else fillFromBack (0, 0) (emptyBlock - files) (position + files) leftIndex newRightIndex

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.txt"
  putStrLn "Part 1"
  print $ part1 input
