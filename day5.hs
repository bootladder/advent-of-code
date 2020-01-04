
main = do
  s <- readFile "day5-input.txt"
  putStrLn "Running Short Test Programs"
  let testProgram1 = [1002,4,3,4,33]
  let testOut1 = runProgram testProgram1
  putStrLn $ show testOut1
  putStrLn "hello"

type Program = [Int]
type Counter = Int 
type Status  = Int
data ComputerState =
  ComputerState { program :: Program 
                , counter :: Counter 
                , status :: Status 
                }deriving Show

runProgram :: Program -> ComputerState
runProgram p =
  let cs = ComputerState p 0 0
  in
    runProgram' cs

runProgram' :: ComputerState -> ComputerState
runProgram' cs =
  if (program cs !! counter cs) == 99
  then
    ComputerState (program cs) (counter cs) 888
  else
    runProgram' $ ComputerState (executeAtPosition (counter cs) (program cs)) (counter cs +4) 999


executeAtPosition :: Int -> [Int] -> [Int]
executeAtPosition pos program =
  let instruction = getIntcodeAt pos program
      intcode = head instruction
  in case intcode of
    1 ->
      let newProgram = add (instruction !! 1) (instruction !! 2) (instruction !! 3) program
      in
        newProgram
    2 ->
      let newProgram = multiply (instruction !! 1) (instruction !! 2) (instruction !! 3) program
      in
        newProgram

    _ -> program
    

getIntcodeAt pos program =
  take 4 $ drop pos program

add x_pos y_pos result_pos program =
  let x_val = program !! x_pos
      y_val = program !! y_pos
      sum = x_val + y_val
  in
    (take result_pos program) ++ [sum] ++ (drop (result_pos + 1) program)

multiply x_pos y_pos result_pos program =
  let x_val = program !! x_pos
      y_val = program !! y_pos
      product = x_val * y_val
  in
    (take result_pos program) ++ [product] ++ (drop (result_pos + 1) program)
