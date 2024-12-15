{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
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

findAll :: Char -> VecMatrix Char -> [Pos]
findAll c ((width, _), vec) = fmap (\i -> (i `mod` width, i `div` width)) $ UV.toList $ UV.elemIndices c vec

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
     print $ sum $ (\(x, y) -> y * 100 + x) <$> findAll 'O' (bounds, finalVec)

wideMatrixFromListMatrix :: [[Char]] -> VecMatrix Char
wideMatrixFromListMatrix listMatrix =
  let height = length listMatrix
      width = length (head listMatrix) * 2
      size = height * width
      vec = ST.runST $ do
        v <- MUV.new size
        Monad.forM_ (zip [0..] listMatrix) $ \(y, row) -> do
          Monad.forM_ (zip [0,2..] row) $ \(x, e) -> do
            Monad.when (e == '.' || e == '#') $ do
                MUV.write v (x + y * width) e
                MUV.write v (1 + x + y * width) e
            Monad.when (e == '@') $ do
                MUV.write v (x + y * width) '@'
                MUV.write v (1 + x + y * width) '.'
            Monad.when (e == 'O') $ do
                MUV.write v (x + y * width) '['
                MUV.write v (1 + x + y * width) ']'
        UV.freeze v
   in ((width, height), vec)

parseInput2 :: [Char] -> (VecMatrix Char, [Char])
parseInput2 input =
  let [warehouseMap, robotInstructions] = Split.splitOn "\n\n" input
   in (wideMatrixFromListMatrix $ lines warehouseMap, concat $ lines robotInstructions)

canMoveDirectionWide :: Char -> (Int, Int) -> Pos -> MUV.MVector s Char -> ST.ST s Bool
canMoveDirectionWide dir bounds@(width, _) pos@(x, y) vec = do
  let nextPos = move dir pos
  current <- MUV.read vec $ posToIndex width pos
  if current == '@'
    then canMoveDirectionWide dir bounds nextPos vec
    else if current == '.'
      then pure True
      else if current == '#'
        then pure False
        else if dir == '^' || dir == 'v'
          then if current == '['
            then do
              this <- canMoveDirectionWide dir bounds nextPos vec
              right <- canMoveDirectionWide dir bounds (move dir (x + 1, y)) vec
              pure $ this && right -- We can move if both can move
            else do -- == ']'
              left <- canMoveDirectionWide dir bounds (move dir (x - 1, y)) vec
              this <- canMoveDirectionWide dir bounds nextPos vec
              pure $ left && this -- We can move if both can move
          else do -- == '<' || == '>'
            let nextNextPos = move dir nextPos
            canMoveDirectionWide dir bounds nextNextPos vec -- We can move if next next can move

moveDirectionWide :: Char -> (Int, Int) -> Pos -> MUV.MVector s Char -> ST.ST s ()
moveDirectionWide dir bounds@(width, _) pos@(x, y) vec = do
  let nextPos = move dir pos
  current <- MUV.read vec $ posToIndex width pos
  if current == '.'
     then pure ()
     else if current == '#'
      then error "We cannot move a wall"
      else if current == '@'
        then do
          moveDirectionWide dir bounds nextPos vec
          MUV.write vec (posToIndex width nextPos) current
          MUV.write vec (posToIndex width pos) '.'
        else if dir == '^' || dir == 'v'
          then if current == '['
            then do
              let rightPos = (x + 1, y)
              let nextRightPos = move dir rightPos
              moveDirectionWide dir bounds nextPos vec
              moveDirectionWide dir bounds nextRightPos vec
              MUV.write vec (posToIndex width nextPos) '['
              MUV.write vec (posToIndex width nextRightPos) ']'
              MUV.write vec (posToIndex width pos) '.'
              MUV.write vec (posToIndex width rightPos) '.'
            else do
              let leftPos = (x - 1, y)
              let nextLeftPos = move dir leftPos
              moveDirectionWide dir bounds nextLeftPos vec
              moveDirectionWide dir bounds nextPos vec
              MUV.write vec (posToIndex width nextLeftPos) '['
              MUV.write vec (posToIndex width nextPos) ']'
              MUV.write vec (posToIndex width leftPos) '.'
              MUV.write vec (posToIndex width pos) '.'
          else do
            let nextNextPos = move dir nextPos
            next <- MUV.read vec $ posToIndex width nextPos
            moveDirectionWide dir bounds nextNextPos vec
            MUV.write vec (posToIndex width nextNextPos) next
            MUV.write vec (posToIndex width nextPos) current
            MUV.write vec (posToIndex width pos) '.'

tryMoveDirectionWide :: Char -> (Int, Int) -> Pos -> MUV.MVector s Char -> ST.ST s Pos
tryMoveDirectionWide dir bounds pos vec = do
  canMove <- canMoveDirectionWide dir bounds pos vec
  if canMove
    then do
      moveDirectionWide dir bounds pos vec
      pure (move dir pos)
    else pure pos

part2 :: (VecMatrix Char, [Char]) -> IO ()
part2 (warehouse :: VecMatrix Char, instructions :: [Char]) =
  let (bounds, immVec) = warehouse
      robotPos = findRobot warehouse
      finalVec = ST.runST $ do
        v <- UV.thaw immVec
        Monad.foldM_ (\pos dir -> tryMoveDirectionWide dir bounds pos v) robotPos instructions
        UV.freeze v
   in do
     putStrLn $ showVecMatrix (bounds, finalVec)
     print $ sum $ (\(x, y) -> y * 100 + x) <$> findAll '[' (bounds, finalVec)

main :: IO ()
main = do
  file <- readFile "input.txt"
  putStrLn "Part 1"
  part1 $ parseInput file
  putStrLn "Part 2"
  part2 $ parseInput2 file
