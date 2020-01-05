type Program = [Int]
type Counter = Int 
data Status  = OK|WAITFORINPUT Int|OUTPUTTING Int|HALTED|BADINSTRUCTION deriving Show
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
testProgram4 = [3,0,4,0,99] :: [Int]

main = do
  s <- readFile "day5-input.txt"
  putStrLn "Running Short Test Programs"
  
  testOut1 <- runProgram testProgram4
  putStrLn $ show testOut1
  putStrLn "hello"


runProgram :: Program -> IO ComputerState
runProgram p =
  let cs = ComputerState p 0 OK
  in
    runProgram' $ return cs

runProgram' :: IO ComputerState -> IO ComputerState
runProgram' iocs =
  do
  cs <- iocs
  case (status cs) of
    WAITFORINPUT loc ->
      do
        putStrLn "YESSSSSSS WAITING FOR INPUT..."
        input <- getLine
        let inputValue = read input :: Int
        putStrLn $ "Storing value : " ++ (show inputValue) ++ " to "
          ++ "location : " ++ (show loc)
  
        let newProgram =
              (take loc (program cs)) ++ [inputValue]
              ++ (drop (loc+1) (program cs))
        
        runProgram' $ return cs {program=newProgram, status=OK}
  
    OUTPUTTING loc ->
      let output = show $ (program cs) !! loc
      in
        do
          putStrLn $ "YESSSSSSS THE OUTPUT IS  :   " ++ (show output)
          runProgram' $ return cs {status=OK}

    OK ->
                       
      if (program cs !! counter cs) == 99 --halt
      then
        return $ cs {status = HALTED}
      else
        let afterExecute = executeAtPosition cs
            afterUpdate = updateCounter afterExecute
        in
            runProgram' $ return afterUpdate

    _ -> return cs
  
updateCounter :: ComputerState -> ComputerState
updateCounter cs =
  let instruction = getNextInstruction cs
      length = case opcode instruction of
        ADD -> 4
        MULT -> 4
        INPUT -> 2
        OUTPUT -> 2
  in
    cs {counter = (counter cs) + length}

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

    INPUT ->
      let loc = getParamValue (param1 instruction)
      in
        ComputerState (program cs) (counter cs) (WAITFORINPUT loc)

    OUTPUT ->
      let loc = getParamValue (param1 instruction)
      in
        ComputerState (program cs) (counter cs) (OUTPUTTING loc)
      

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
              , param1 = Param (rawInstruction !! 1) param1type
              , param2 = Param (rawInstruction !! 2) param2type
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
