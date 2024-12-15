{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import qualified Control.Monad               as Monad
import qualified Control.Monad.ST            as ST
import qualified Data.List.Split             as Split
import qualified Data.Vector.Unboxed         as UV
import qualified Data.Vector.Unboxed.Mutable as MUV

type VecMatrix a = ((Int, Int), UV.Vector a)

vecMatrixFromListMatrix :: UV.Unbox a => [[a]] -> VecMatrix a
vecMatrixFromListMatrix listMatrix =
  let height = length listMatrix
      width = length $ head listMatrix
      size = height * width
      vec = ST.runST $ do
        v <- MUV.new size
        Monad.forM_ (zip [0..] listMatrix) $ \(y, row) -> do
          Monad.forM_ (zip [0..] row) $ \(x, e) -> do
            MUV.write v (x + y * width) e
        UV.freeze v
   in ((width, height), vec)

vecFromList :: UV.Unbox a => [a] -> UV.Vector a
vecFromList list = ST.runST $ do
  v <- MUV.new $ length list
  Monad.forM_ (zip [0..] list) $ \(i, e) -> do
    MUV.write v i e
  UV.freeze v

parseInput :: [Char] -> (VecMatrix Char, [Char])
parseInput input =
  let [warehouseMap, robotInstructions] = Split.splitOn "\n\n" input
   in (vecMatrixFromListMatrix $ lines warehouseMap, concat $ lines robotInstructions)

showVecMatrix :: VecMatrix Char -> String
showVecMatrix ((width, height), vec) =
  foldr (\y acc -> showRow y ++ acc ) "" [0..(height - 1)]
  where
    showRow y = foldr (\x acc -> vec UV.! (x + y * width) : acc) "\n" [0..(width - 1)]

type Pos = (Int, Int)
posToIndex :: Int -> Pos -> Int
posToIndex width (x, y) = x + y * width

move :: Char -> Pos -> Pos
move '^' (x, y) = (x, y - 1)
move 'v' (x, y) = (x, y + 1)
move '<' (x, y) = (x - 1, y)
move '>' (x, y) = (x + 1, y)

tryMoveDirection :: Char -> (Int, Int) -> Pos -> MUV.MVector s Char -> ST.ST s Pos
tryMoveDirection dir (width, _) pos vec = do
  let nextPos = move dir pos
  current <- MUV.read vec $ posToIndex width pos
  if current == '.'
     then pure nextPos -- The last block can move here, the '.' "moved away"
     else if current == '#'
        then pure pos -- The last block can not move here
        else do -- Current is 'O' or '@' and we can move only if the next block can also move
          nextBlockNewPos <- tryMoveDirection dir (width, undefined) nextPos vec
          if nextBlockNewPos == nextPos
             then pure pos -- The next block stayed put, we cannot move to nextPos, thus we stay at pos
             else do -- The other block did move, move this block as well and leave a new '.'
               MUV.write vec (posToIndex width nextPos) current
               MUV.write vec (posToIndex width pos) '.'
               pure nextPos

findRobot :: VecMatrix Char -> Pos
findRobot ((width, _), vec) = let Just r = UV.elemIndex '@' vec in (r `mod` width, r `div` width)

findAllBoxes :: VecMatrix Char -> [Pos]
findAllBoxes ((width, _), vec) = fmap (\i -> (i `mod` width, i `div` width)) $ UV.toList $ UV.elemIndices 'O' vec

part1 :: (VecMatrix Char, [Char]) -> IO ()
part1 (warehouse :: VecMatrix Char, instructions :: [Char]) =
  let (bounds, immVec) = warehouse
      robotPos = findRobot warehouse
      finalVec = ST.runST $ do
        v <- UV.thaw immVec
        Monad.foldM_ (\pos dir -> tryMoveDirection dir bounds pos v) robotPos instructions
        UV.freeze v
   in do
     putStrLn $ showVecMatrix (bounds, finalVec)
     print $ sum $ (\(x, y) -> y * 100 + x) <$> findAllBoxes (bounds, finalVec)

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.txt"
  putStrLn "Part 1"
  part1 input
