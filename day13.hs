import Intcode
import Data.Array
import Data.List.Split

boardSize = 100
gameBoard = listArray ((0,0), (boardSize,boardSize))
            [0 | x <- [0..] , y<- [0..]]

main = do
  s <- readFile "day13-input.txt"
  let inputProgram = listArray (0,4000) $ (map read $ splitOn "," s) ++ (repeat 0)

      out = runProgram $ defaultComputerState {program=inputProgram}
      outBuf = outputBuffer out
      outputInstructions = chunksOf 3 outBuf

      newBoard = applyOutputInstructionsToGameBoard gameBoard outputInstructions

  putStrLn $ show newBoard
  putStrLn $ show $ length $ filter (\e -> e == 2) (elems newBoard)

applyOutputInstructionsToGameBoard board instructions =
  foldl (\accBoard instruction ->

           accBoard // [((instruction !! 0, instruction !! 1), instruction !! 2)]

           ) board instructions
