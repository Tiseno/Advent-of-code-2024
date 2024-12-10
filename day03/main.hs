import qualified Data.Char       as Char
import qualified Data.List       as List
import           Text.Regex.TDFA

mulOne ('m':'u':'l':'(':xs) =
  let (a, ',':xs') = List.span Char.isDigit xs
      (b, _) = List.span Char.isDigit xs'
   in read a * read b

part1 :: String -> Int
part1 input =
  let muls = concat ((input =~ "mul[(][0-9]*,[0-9]*[)]")::[[String]])
   in sum $ fmap mulOne muls

part2 :: String -> Int
part2 input =
  let instructions = concat ((input =~ "do[(][)]|don't[(][)]|mul[(][0-9]*,[0-9]*[)]")::[[String]])
   in snd $ foldl eval (True, 0) instructions
  where
    eval (_, sum) instr | instr == "do()" = (True, sum)
    eval (_, sum) instr | instr == "don't()" = (False, sum)
    eval (enabled, sum) instr = (enabled, sum + if enabled then mulOne instr else 0)

main :: IO ()
main = do
  input <- readFile "input.txt"
  putStrLn "Part 1"
  print $ part1 input
  putStrLn "Part 2"
  print $ part2 input
