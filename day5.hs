type Program = [Int]
type Counter = Int 
data Status  = OK|HALTED|BADINSTRUCTION deriving Show
data ComputerState =
  ComputerState { program :: Program 
                , counter :: Counter 
                , status :: Status 
                }deriving Show

data Opcode = ADD|MULT|INPUT|OUTPUT|HALT|INVALID deriving Show

data ParamType = Positional|Immediate deriving Show
data Param = Param Int ParamType deriving Show

data Instruction =
  Instruction { opcode :: Opcode
              , param1 :: Param
              , param2 :: Param
              , param3 :: Param
              } deriving Show

testProgram1 = [2,4,5,5,99,3]  :: [Int]
testProgram2 = [1002,4,3,4,33] :: [Int]
testProgram3 = [1101,100,-1,4,0] :: [Int]

main = do
  s <- readFile "day5-input.txt"
  putStrLn "Running Short Test Programs"
  
  let testOut1 = runProgram testProgram3
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
  let instruction = getNextInstruction cs
  in
    case opcode instruction of
    ADD ->
      let p1val = param2value (param1 instruction) (program cs)
          p2val = param2value (param2 instruction) (program cs)
          p3pos = getParamValue (param3 instruction)
          newProgram = add p1val p2val p3pos $ program cs
      in
        ComputerState newProgram (counter cs) OK

    MULT ->
      let p1val = param2value (param1 instruction) (program cs)
          p2val = param2value (param2 instruction) (program cs)
          p3pos = getParamValue (param3 instruction)
          newProgram = multiply p1val p2val p3pos $ program cs
      in
        ComputerState newProgram (counter cs) OK

    _ -> ComputerState (program cs) (counter cs) BADINSTRUCTION
        
    
getNextInstruction :: ComputerState -> Instruction
getNextInstruction cs = 
  let rawInstruction = drop (counter cs) (program cs)
      firstValue = rawInstruction !! 0
      rawOpcode  = firstValue `mod` 100
      param1type = case (firstValue `mod` 1000) `div` 100 of
        1 -> Immediate
        0 -> Positional
      param2type = case (firstValue `mod` 10000) `div` 1000 of
        1 -> Immediate
        0 -> Positional
  in
      
  Instruction { opcode = parseOpcode rawOpcode
              , param1 = Param (rawInstruction !! 1) Immediate
              , param2 = Param (rawInstruction !! 2) Immediate
              , param3 = Param (rawInstruction !! 3) Positional
              }

parseOpcode :: Int -> Opcode
parseOpcode i = case i of
  1 -> ADD
  2 -> MULT
  3 -> INPUT
  4 -> OUTPUT
  99 -> HALT
  _ -> INVALID


param2value :: Param -> Program -> Int
param2value param program =
  case param of
    Param i Immediate -> i
    Param i Positional -> program !! i

getParamValue (Param i _) = i
  
  
getIntcodeAt pos program =
  take 4 $ drop pos program

add x_val y_val result_pos program =
  let 
      sum = x_val + y_val
  in
    (take result_pos program) ++ [sum] ++ (drop (result_pos + 1) program)

multiply x_val y_val result_pos program =
  let 
      product = x_val * y_val
  in
    (take result_pos program) ++ [product] ++ (drop (result_pos + 1) program)
