{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
import           Data.Bits       (Bits (xor))
import           Data.Function   ((&))
import qualified Data.List.Split as Split

data Machine = Machine { lengthOfProgram :: Int, program :: [Int], ip :: Int, a :: Int, b :: Int, c :: Int, halted :: Bool, out :: [Int] }

instance Show Machine where
  show m =
    "Register A: " ++ show (a m) ++ "\n"
    ++ "Register B: " ++ show (b m) ++ "\n"
    ++ "Register C: " ++ show (c m) ++ "\n"
    ++ "\n"
    ++ "Program: " ++ show (out m) ++ "\n"

parseInput :: [Char] -> Machine
parseInput input =
  let [registers, program] = Split.splitOn "\n\n" input
      [a, b, c] = (read . drop 12 <$> lines registers)
      prg = read $ "[" ++ drop 9 program ++ "]"
   in Machine (length prg) prg 0 a b c False []


comboOperand :: Machine -> Int
comboOperand m = combo (program m !! ip m)
  where
    combo 0 = 1
    combo 1 = 1
    combo 2 = 2
    combo 3 = 3
    combo 4 = a m
    combo 5 = b m
    combo 6 = c m
    combo c = error $ "Encountered invalid combo " ++ show c

literalOperand :: Machine -> Int
literalOperand m = program m !! ip m

truncatedDivision :: Machine -> Int
truncatedDivision m =
  let numerator = toInteger $ a m
      denominator = (2 :: Integer) ^ toInteger (comboOperand m)
      factor = numerator `div` denominator
   in fromIntegral factor

operate :: Int -> Machine -> Machine
operate 0 m = m { ip = ip m + 1, a = truncatedDivision m }
operate 1 m = m { ip = ip m + 1, b = xor (b m) (literalOperand m) }
operate 2 m = m { ip = ip m + 1, b = comboOperand m `mod` 8 }
operate 3 m = m { ip = if a m == 0 then ip m + 1 else literalOperand m }
operate 4 m = m { ip = ip m + 1, b = xor (b m) (c m) }
operate 5 m = m { ip = ip m + 1, out = out m ++ [comboOperand m `mod` 8]}
operate 6 m = m { ip = ip m + 1, b = truncatedDivision m }
operate 7 m = m { ip = ip m + 1, c = truncatedDivision m }
operate o _ = error $ "Undefined operation: " ++ show o

step :: Machine -> Machine
step m | halted m = m
step m | ip m >= lengthOfProgram m = m { halted = True }
step m = operate (program m !! ip m) (m { ip = ip m + 1 })

eval :: Machine -> Machine
eval m | halted m = m
eval m = step m & eval

part1 :: Machine -> Machine
part1 = eval

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.txt"
  putStrLn "Part 1"
  print $ part1 input
