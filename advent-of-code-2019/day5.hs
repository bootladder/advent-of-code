import Data.List.Split

type Program = [Int]
type Counter = Int 
data Status  = OK|WAITFORINPUT Int|OUTPUTTING Int|HALTED|BADINSTRUCTION deriving Show
data ComputerState =
  ComputerState { program :: Program 
                , counter :: Counter 
                , status :: Status 
                }deriving Show

data Opcode = ADD|MULT|INPUT|OUTPUT|JUMPIFTRUE|JUMPIFFALSE|LESSTHAN|EQUALS|HALT|INVALID deriving Show

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
testProgram5 = [3,9,8,9,10,9,4,9,99,-1,8] :: [Int]
testProgram6 = [3,9,7,9,10,9,4,9,99,-1,8] :: [Int]
testProgram7 = [3,3,1108,-1,8,3,4,3,99] :: [Int]
testProgram8 = [3,3,1107,-1,8,3,4,3,99] :: [Int]
testProgram9 = [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9] :: [Int]
  --                                    9       1213   15
testProgram10 = [3,3,1105,-1,9,1101,0,0,12,4,12,99,1] :: [Int]
testProgram11 = [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99] :: [Int]

main = do
  s <- readFile "day5-input.txt"
  putStrLn "Running Short Test Programs"
  let program = map read $ splitOn "," s :: [Int]
  --putStrLn $ show program

  testOut1 <- runProgram program
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
  putStrLn $ "Derp : " ++ (show $ getNextInstruction cs)
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
  
    OUTPUTTING value ->
      do
        putStrLn $ "YESSSSSSS THE OUTPUT IS : " ++ (show value)
        runProgram' $ return cs {status=OK}

    OK ->
                       
      if (program cs !! counter cs) == 99 --halt
      then
        return $ cs {status = HALTED}
      else
        let afterExecute = executeAtPosition cs
            inc = getCounterIncrement $ getNextInstruction cs
            newState = afterExecute {counter = (counter afterExecute) + inc}
        in
            runProgram' $ return newState

    _ -> return cs
  
getCounterIncrement :: Instruction -> Int
getCounterIncrement instruction =
  case opcode instruction of
    ADD -> 4
    MULT -> 4
    INPUT -> 2
    OUTPUT -> 2
    JUMPIFTRUE -> 0
    JUMPIFFALSE -> 0
    LESSTHAN -> 4
    EQUALS -> 4

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
      let value = param2value (param1 instruction) (program cs)
      in
        ComputerState (program cs) (counter cs) (OUTPUTTING value)
      
    JUMPIFTRUE -> 
      let p1val = param2value (param1 instruction) (program cs) 
          p2val = param2value (param2 instruction) (program cs)
      in
        if p1val /= 0
        then
          cs {counter = p2val}
        else
          cs {counter = (counter cs) + 3}
      
    JUMPIFFALSE -> 
      let p1val = param2value (param1 instruction) (program cs) 
          p2val = param2value (param2 instruction) (program cs)
      in
        if p1val == 0
        then
          cs {counter = p2val}
        else
          cs {counter = (counter cs) + 3}
      
    LESSTHAN -> 
      let p1val = param2value (param1 instruction) (program cs)
          p2val = param2value (param2 instruction) (program cs)
          p3pos = getParamValue (param3 instruction)
      in
        if p1val < p2val
        then
          cs{program = writeValueToProgram (program cs) 1 p3pos}
        else
          cs{program = writeValueToProgram  (program cs) 0 p3pos}
      
    EQUALS -> 
      let p1val = param2value (param1 instruction) (program cs)
          p2val = param2value (param2 instruction) (program cs)
          p3pos = getParamValue (param3 instruction)
      in
        if p1val == p2val
        then
          cs{program = writeValueToProgram (program cs) 1 p3pos}
        else
          cs{program = writeValueToProgram  (program cs) 0 p3pos}

    _ -> ComputerState (program cs) (counter cs) BADINSTRUCTION
        

writeValueToProgram :: Program -> Int -> Int -> Program
writeValueToProgram program val loc =
  (take loc program) ++ [val] ++ (drop (loc + 1) program)
    
getNextInstruction :: ComputerState -> Instruction
getNextInstruction cs = 
  let rawInstruction = drop (counter cs) (program cs)
      firstValue = rawInstruction !! 0
      rawOpcode  = firstValue `mod` 100
      param1type = case (firstValue `mod` 1000) `div` 100 of
        1 -> Immediate
        0 -> Positional
      param1Value = if length rawInstruction > 1
                    then rawInstruction !! 1
                    else -1
      param2type = case (firstValue `mod` 10000) `div` 1000 of
        1 -> Immediate
        0 -> Positional
      param2Value = if length rawInstruction > 2
                    then rawInstruction !! 2
                    else -1
      param3Value = if length rawInstruction > 3
                    then rawInstruction !! 3
                    else -1
  in
      
  Instruction { opcode = parseOpcode rawOpcode
              , param1 = Param param1Value param1type
              , param2 = Param param2Value param2type
              , param3 = Param param3Value Positional
              }

parseOpcode :: Int -> Opcode
parseOpcode i = case i of
  1 -> ADD
  2 -> MULT
  3 -> INPUT
  4 -> OUTPUT
  5 -> JUMPIFTRUE
  6 -> JUMPIFFALSE
  7 -> LESSTHAN
  8 -> EQUALS
  99 -> HALT
  _ -> INVALID


param2value :: Param -> Program -> Int
param2value param program =
  case param of
    Param i Immediate -> i
    Param i Positional -> program !! i

getParamValue (Param i _) = i
  

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
