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

type FileSystem = Array.Array Int Int

parseInput2 :: String -> FileSystem
parseInput2 input =
  let sectors :: [Int] = fmap (\c -> read [c]) $ head $ lines input
      fsSize = sum sectors
      emptyFs = Array.listArray (0 :: Int, fsSize - 1) $ repeat (negate (1 :: Int))
      indexedSectors = zip [(0 :: Int)..] sectors
   in snd $ foldl foldSector (0, emptyFs) indexedSectors
    where
      foldSector (fsPosition :: Int, fs :: FileSystem) (index :: Int, size :: Int) | odd index = (fsPosition + size, fs)
      foldSector (fsPosition, fs) (index, size) =
        let fileId :: Int = fromIntegral $ index `div` 2
         in (fsPosition + size, fs Array.// fmap (, fileId) [fsPosition..(fsPosition + size - 1)])

findFile :: (Int, FileSystem) -> Maybe (Int, Int)
findFile (index, _) | index < 0 = Nothing
findFile (index, fs) | (fs Array.! index) == -1 = findFile (index - 1, fs)
findFile (end, fs) = Just (findFileStart end, end)
  where
    fileId = fs Array.! end
    findFileStart i =
      if i <= 0 || (fs Array.! (i - 1)) /= fileId
         then i
         else findFileStart (i - 1)

firstEmptySpaceOfSize :: Int -> FileSystem -> Int -> Maybe (Int, Int)
firstEmptySpaceOfSize size fs beforeIndex = firstEmptySpaceOfSizeFrom 0
  where
    firstEmptySpaceOfSizeFrom from = case findSpaceSpan from of
        Nothing -> Nothing
        Just spaceSpan@(spaceStart, spaceEnd) ->
          if spaceEnd - spaceStart + 1 >= size
             then Just spaceSpan
             else firstEmptySpaceOfSizeFrom (spaceEnd + 1)
    findSpaceEnd i | (i + 1) >= beforeIndex = i
    findSpaceEnd i | fs Array.! (i + 1) /= -1 = i
    findSpaceEnd i = findSpaceEnd (i + 1)
    findSpaceSpan i | i >= beforeIndex = Nothing
    findSpaceSpan i | fs Array.! i /= -1 = findSpaceSpan (i + 1)
    findSpaceSpan spaceStart = Just (spaceStart, findSpaceEnd spaceStart)

swapData :: FileSystem -> Int -> Int -> Int -> FileSystem
swapData fs len a b =
  let aElem = fs Array.! a
      bElem = fs Array.! b
      newAData = [(i, bElem) | i <- [a..(a + len - 1)]]
      newBData = [(i, aElem) | i <- [b..(b + len - 1)]]
   in aElem `seq` bElem `seq` fs Array.// newAData Array.// newBData

moveFileFromEndToFront :: (Int, FileSystem) -> (Int, FileSystem)
moveFileFromEndToFront (index, fs) =
  case findFile (index, fs) of
    Nothing -> (-1, fs)
    Just (fileStart, fileEnd) ->
      let fileLength = (fileEnd - fileStart) + 1
       in case firstEmptySpaceOfSize fileLength fs fileStart of
         Nothing -> (fileStart - 1, fs)
         Just (emptySpaceStart, _) -> (fileStart - 1, swapData fs fileLength fileStart emptySpaceStart)

moveAllFiles :: (Int, FileSystem) -> (Int, FileSystem)
moveAllFiles (index, fs) = until (\(i, _) -> i < 0) moveFileFromEndToFront (index, fs)

calculateCheckSum :: FileSystem -> Int
calculateCheckSum fs = calculateCheckSumIt 0 0
  where
    (_, end) = Array.bounds fs
    calculateCheckSumIt acc index | index == end = acc
    calculateCheckSumIt acc index = calculateCheckSumIt (acc + checkFunction (index, fs Array.! index)) (index + 1)
    checkFunction (_, -1)         = 0
    checkFunction (index, fileId) = fromIntegral index * fromIntegral fileId

part2 :: FileSystem -> Int
part2 (fs :: FileSystem) = calculateCheckSum $ snd $ moveAllFiles (snd $ Array.bounds fs, fs)

main :: IO ()
main = do
  input <- readFile "input.txt"
  putStrLn "Part 1"
  print $ part1 $ parseInput input
  putStrLn "Part 2"
  print $ part2 $ parseInput2 input
