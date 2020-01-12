import Data.List.Split
import Data.List

testProgram1 = [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0] :: [Int]
testProgram2 = [3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0] :: [Int]
testProgram3 = [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0] :: [Int]

testProgram4 = [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5] :: [Int]

main :: IO ()
main = do
  putStrLn "Hello"

  let cs = runProgramWithPhaseSequence testProgram1 [4,3,2,1,0]
    in putStrLn $ "\n\nFirst Output : " ++ (show $ outputBuffer cs)

  let cs = runProgramWithPhaseSequence testProgram2 [0,1,2,3,4]
    in putStrLn $ "\n\nFirst Output : " ++ (show $ outputBuffer cs)

  let cs = runProgramWithPhaseSequence testProgram3 [1,0,4,3,2]
    in putStrLn $ "\n\nFirst Output : " ++ (show $ outputBuffer cs)

  s <- readFile "day7-input.txt"

  let inputProgram = map read $ splitOn "," s :: [Int]

      allPossibleOutputs = map
                           (runProgramWithPhaseSequence inputProgram)
                           (permutations [0,1,2,3,4])

      outputBuffers = map outputBuffer allPossibleOutputs

      lengthCheck = filter (\b -> length b > 1) outputBuffers

  putStrLn $ "Length check: there should be nothing with more than 1 result : "
    ++ (show lengthCheck)

  putStrLn $ (++) "Length of result : " $ show $ length outputBuffers
  putStrLn $ (++) "The answer is: " $ show $ maximum outputBuffers

  putStrLn "\n\nPART 2\n\n"
  let out = runProgramWithPhaseSequenceInFeedbackMode testProgram4 [9,8,7,6,5]
  putStrLn $ (++) "The answer  is: " $ show out
  putStrLn "Done"

runProgramWithPhaseSequenceInFeedbackMode :: Program -> [Int] -> Int
runProgramWithPhaseSequenceInFeedbackMode prog phaseSeq =
    --prime the amps with the phase sequence
    let
      ampStateA = runProgramWithInputBuffer prog [phaseSeq !! 0]
      ampStateB = runProgramWithInputBuffer prog [phaseSeq !! 1]
      ampStateC = runProgramWithInputBuffer prog [phaseSeq !! 2]
      ampStateD = runProgramWithInputBuffer prog [phaseSeq !! 3]
      ampStateE = runProgramWithInputBuffer prog [phaseSeq !! 4]

    --let programA = program ampStateA
    --    programB = program ampStateB
    --    programC = program ampStateC
    --    programD = program ampStateD
    --    programE = program ampStateE

      in 7

runProgramWithPhaseSequence :: Program -> [Int] -> ComputerState
runProgramWithPhaseSequence prog seq =
  let
    cs1 = runProgramWithInputBuffer prog [seq !! 0,0]

    output1 = head $ outputBuffer cs1
    cs2 = runProgramWithInputBuffer prog [seq !! 1,output1]

    output2 = head $ outputBuffer cs2
    cs3 = runProgramWithInputBuffer prog [seq !! 2,output2]

    output3 = head $ outputBuffer cs3
    cs4 = runProgramWithInputBuffer prog [seq !! 3,output3]

    output4 = head $ outputBuffer cs4
    cs5 = runProgramWithInputBuffer prog [seq !! 4,output4]

    in cs5


type Program = [Int]
type Counter = Int
data Status  = OK|WAITFORINPUT Int|OUTPUTTING Int|HALTED|BADINSTRUCTION deriving Show
data ComputerState =
  ComputerState { program :: Program
                , counter :: Counter
                , status :: Status
                , inputBuffer :: [Int]
                , outputBuffer :: [Int]
                }deriving Show

data Opcode = ADD|MULT|INPUT|OUTPUT|JUMPIFTRUE|JUMPIFFALSE|LESSTHAN|EQUALS|HALT|INVALID deriving Show

data ParamType = Positional|Immediate|Invalid deriving Show
data Param = Param Int ParamType deriving Show

data Instruction =
  Instruction { opcode :: Opcode
              , param1 :: Param
              , param2 :: Param
              , param3 :: Param
              } deriving Show


executeComputerUntilHaltOrInput :: ComputerState -> ComputerState
executeComputerUntilHaltOrInput cs =
  do
  case (status cs) of
    WAITFORINPUT _ ->
        cs

    OUTPUTTING value ->
        runProgram' cs {status=OK, outputBuffer = (outputBuffer cs) ++ [value]}

    OK ->
      if (program cs !! counter cs) == 99 --halt
      then
        cs {status = HALTED}
      else
        let afterExecute = executeAtPosition cs
            inc = getCounterIncrement $ getNextInstruction cs
            newState = afterExecute {counter = (counter afterExecute) + inc}
        in
            executeComputerUntilHaltOrInput $ newState

    _ -> cs


runProgramWithInputBuffer :: Program -> [Int] -> ComputerState
runProgramWithInputBuffer prog inputBuf =
  let cs = ComputerState prog 0 OK inputBuf []
  in
    runProgram' cs

runProgram' :: ComputerState -> ComputerState
runProgram' cs =
  do
  case (status cs) of
    WAITFORINPUT loc ->
      do
        let inputValue = head $ inputBuffer cs
            newProgram =
              (take loc (program cs)) ++ [inputValue]
              ++ (drop (loc+1) (program cs))

        runProgram' $ cs {program=newProgram, status=OK, inputBuffer = tail $ inputBuffer cs}

    OUTPUTTING value ->
      do
        runProgram' $ cs {status=OK, outputBuffer = (outputBuffer cs) ++ [value]}

    OK ->
      if (program cs !! counter cs) == 99 --halt
      then
        cs {status = HALTED}
      else
        let afterExecute = executeAtPosition cs
            inc = getCounterIncrement $ getNextInstruction cs
            newState = afterExecute {counter = (counter afterExecute) + inc}
        in
            runProgram' newState

    _ -> cs

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
    _ -> 999999

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
        cs {program=newProgram}

    MULT ->
      let p1val = param2value (param1 instruction) (program cs)
          p2val = param2value (param2 instruction) (program cs)
          p3pos = getParamValue (param3 instruction)
          newProgram = multiply p1val p2val p3pos $ program cs
      in
        cs {program=newProgram}

    INPUT ->
      let loc = getParamValue (param1 instruction)
      in
        cs {status=WAITFORINPUT loc}

    OUTPUT ->
      let value = param2value (param1 instruction) (program cs)
      in
        cs {status=OUTPUTTING value}

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

    _ -> cs {status=BADINSTRUCTION}

writeValueToProgram :: Program -> Int -> Int -> Program
writeValueToProgram prog val loc =
  (take loc prog) ++ [val] ++ (drop (loc + 1) prog)

getNextInstruction :: ComputerState -> Instruction
getNextInstruction cs =
  let rawInstruction = drop (counter cs) (program cs)
      firstValue = rawInstruction !! 0
      rawOpcode  = firstValue `mod` 100
      param1type = case (firstValue `mod` 1000) `div` 100 of
                     1 -> Immediate
                     0 -> Positional
                     _ -> Invalid

      param1Value = if length rawInstruction > 1
                    then rawInstruction !! 1
                    else -1

      param2type = case (firstValue `mod` 10000) `div` 1000 of
                     1 -> Immediate
                     0 -> Positional
                     _ -> Invalid

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
param2value param prog =
  case param of
    Param i Immediate -> i
    Param i Positional -> prog !! i
    Param _ Invalid -> 0

getParamValue :: Param -> Int
getParamValue (Param i _) = i


add :: Int -> Int -> Int -> Program -> Program
add x_val y_val result_pos prog =
  let
      sum_ = x_val + y_val
  in
    (take result_pos prog) ++ [sum_] ++ (drop (result_pos + 1) prog)

multiply :: Int -> Int -> Int -> Program -> Program
multiply x_val y_val result_pos prog =
  let
      product_ = x_val * y_val
  in
    (take result_pos prog) ++ [product_] ++ (drop (result_pos + 1) prog)
