type Program = [Int]
type Counter = Int 
data Status  = OK|HALTED|BADINSTRUCTION deriving Show
data ComputerState =
  ComputerState { program :: Program 
                , counter :: Counter 
                , status :: Status 
                }deriving Show

testProgram1 = [2,4,5,4,33,3]
testProgram2 = [1002,4,3,4,33]

main = do
  s <- readFile "day5-input.txt"
  putStrLn "Running Short Test Programs"
  
  let testOut1 = runProgram testProgram1
  putStrLn $ show testOut1
  putStrLn "hello"


runProgram :: Program -> ComputerState
runProgram p =
  let cs = ComputerState p 0 OK
  in
    runProgram' cs

runProgram' :: ComputerState -> ComputerState
runProgram' cs =
  case (status cs) of
    OK ->
                       
      if (program cs !! counter cs) == 99 --halt
      then
        cs {status = HALTED}
      else
        let afterExecute = executeAtPosition cs
            afterUpdate = updateCounter afterExecute
        in
            runProgram' afterUpdate

    _ -> cs
  
updateCounter :: ComputerState -> ComputerState
updateCounter cs = cs {counter = (counter cs) + 4}

executeAtPosition :: ComputerState -> ComputerState
executeAtPosition cs =
  let instruction = getIntcodeAt (counter cs) (program cs)
      intcode = head instruction
  in
    case intcode of
    1 ->
      let newProgram = add (instruction !! 1) (instruction !! 2) (instruction !! 3) $ program cs
      in
        ComputerState newProgram (counter cs) OK
    2 ->
      let newProgram = multiply (instruction !! 1) (instruction !! 2) (instruction !! 3) $ program cs
      in
        ComputerState newProgram (counter cs) OK

    _ -> ComputerState (program cs) (counter cs) BADINSTRUCTION
        
    

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
