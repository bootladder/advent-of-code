import Intcode
import Data.Array

inputTester = listArray (0,10) $ ([3,5,4,5,99] ++ [8..])


runProgramWithInputBuffer :: Program -> [Int] -> ComputerState
runProgramWithInputBuffer prog inputBuf =
  let cs = defaultComputerState{program = prog, inputBuffer = inputBuf}
  in
    runProgram cs

main = do
  putStrLn "Testing Intcode"
  let cs = runProgramWithInputBuffer inputTester [999]
  putStrLn $ show cs
  putStrLn $ show $ program cs
