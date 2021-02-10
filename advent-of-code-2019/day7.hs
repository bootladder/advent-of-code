import Data.List.Split
import Data.List

import Debug.Trace

myTrace :: (Show a) => String -> a -> a
myTrace comment t = trace (comment ++ show t) t

testProgram1 = [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0] :: [Int]
testProgram2 = [3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0] :: [Int]
testProgram3 = [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0] :: [Int]

testProgram4 = [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5] :: [Int]
testProgram5 = [3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10] :: [Int]

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
    in putStrLn $ (++) "Test Program 4 : The answer  is: " $ show out

  let out = runProgramWithPhaseSequenceInFeedbackMode testProgram5 [9,7,8,5,6]
    in putStrLn $ (++) "Test Program 5 : The answer  is: " $ show out


  let allPossibleOutputsFeedbackMode = map
                                       (runProgramWithPhaseSequenceInFeedbackMode inputProgram)
                                       (permutations [5,6,7,8,9])


    in putStrLn $ (++) "THE FINAL ANSWER IS::: " $ show $ maximum allPossibleOutputsFeedbackMode

  putStrLn "Done"

runProgramWithPhaseSequenceInFeedbackMode :: Program -> [Int] -> Int
runProgramWithPhaseSequenceInFeedbackMode prog phaseSeq =
  -- create initial computer states
  let
    initialComputerStates' = map
                            (\phase -> (ComputerState (show phase) prog 0 OK [phase] []))
                            phaseSeq
    initialComputerStates = trace ("Initial Computer States: " ++ (showChainOfComputerStates initialComputerStates')) initialComputerStates'

    finalComputerStates = loopExecuteChain initialComputerStates [0]
  in
    head $ outputBuffer $ last finalComputerStates

loopExecuteChain :: [ComputerState] -> [Int] -> [ComputerState]
loopExecuteChain states initialInput =
  let
    iterationResult = executeChainOfComputerStates states initialInput
  in
    if isHalted $ last iterationResult
    then iterationResult
    else
      let
        output = (outputBuffer $ last iterationResult)
        iterationResultWithOutputConsumed = (init iterationResult) ++ [ (last iterationResult) {outputBuffer = []} ]
      in
        loopExecuteChain iterationResultWithOutputConsumed output

  where isHalted cs =
          (status cs) == HALTED

executeChainOfComputerStates :: [ComputerState] -> [Int] -> [ComputerState]
executeChainOfComputerStates states initialInput =
  let newstates =
        foldl
        (\l cs ->
           let
             -- Consume the output buffer of the last node
             previousOutput = trace ("Previous Output and This Input" ++ show (outputBuffer $ last l)) (outputBuffer $ last l)    --myTrace comment t = trace (comment ++ show t) t

             newlist = (init l) ++ [(last l) {outputBuffer = []}]
             newState = runProgram $ cs {inputBuffer = (inputBuffer cs) ++ previousOutput}
           in
             --trace ("Progress so far: " ++ (showChainOfComputerStates $ newlist ++ [newState]))
             (newlist ++ [newState])
        )

        [defaultComputerState {outputBuffer = initialInput}]
        --(trace ("\n\nInput States: \n" ++ (showChainOfComputerStates states))
         states
        --)
  in
    trace ("\n\nChain Complete: " ++ (showChainOfComputerStates (tail newstates))) (tail newstates)

runProgramWithPhaseSequence :: Program -> [Int] -> ComputerState
runProgramWithPhaseSequence prog phaseSeq =
  let states = foldl
        (\l phase ->
           let
             previousOutput = head $ outputBuffer $ last l
             cs = runProgramWithInputBuffer prog [phase, previousOutput]
           in
             l ++ [cs]
        )

        [defaultComputerState {outputBuffer = [0]}]
        phaseSeq

  in last states


type Program = [Int]
type Counter = Int
data Status  = OK|WAITFORINPUT Int|OUTPUTTING Int|HALTED|BADINSTRUCTION deriving (Show, Eq)
data ComputerState =
  ComputerState { computerId :: String
                , program :: Program
                , counter :: Counter
                , status :: Status
                , inputBuffer :: [Int]
                , outputBuffer :: [Int]
                }


showComputerState :: ComputerState -> String
showComputerState cs = "ComputerState: ID : " ++ (computerId cs) ++ " Counter = " ++ (show $ counter cs) ++ " Status = " ++ (show $ status cs) ++ " inputbuffer = " ++ (show $ inputBuffer cs) ++ " outputBuffer = " ++ (show $ outputBuffer cs)

showChainOfComputerStates :: [ComputerState] -> String
showChainOfComputerStates states = "\n Chain: \n" ++ (unlines $ map show states)

instance Show ComputerState where show = showComputerState

defaultComputerState :: ComputerState
defaultComputerState =
  ComputerState { computerId = "default"
                ,program=[]
                , counter = 0
                , status = OK
                , inputBuffer = []
                , outputBuffer = []
                }

data Opcode = ADD|MULT|INPUT|OUTPUT|JUMPIFTRUE|JUMPIFFALSE|LESSTHAN|EQUALS|HALT|INVALID deriving Show

data ParamType = Positional|Immediate|Invalid deriving Show
data Param = Param Int ParamType deriving Show

data Instruction =
  Instruction { opcode :: Opcode
              , param1 :: Param
              , param2 :: Param
              , param3 :: Param
              } deriving Show


runProgramWithInputBuffer :: Program -> [Int] -> ComputerState
runProgramWithInputBuffer prog inputBuf =
  let cs = ComputerState "ID" prog 0 OK inputBuf []
  in
    runProgram cs

runProgram :: ComputerState -> ComputerState
runProgram cs =
  case (status cs) of
    WAITFORINPUT loc ->
      if null $ inputBuffer cs
      then cs
      else
        let inputValue = head $ inputBuffer cs
            newProgram =
              (take loc (program cs)) ++ [inputValue]
              ++ (drop (loc+1) (program cs))

        in runProgram $ cs {program=newProgram
                            , status=OK
                            , inputBuffer = tail $ inputBuffer cs
                            }

    OUTPUTTING value ->
      runProgram $ cs {status=OK
                       , outputBuffer = (outputBuffer cs) ++ [value]
                       }

    OK ->
      if (program cs !! counter cs) == 99 --halt
      then
        cs {status = HALTED}
      else
        let afterExecute = executeAtPosition cs
            inc = getCounterIncrement $ getNextInstruction cs
            newState = afterExecute {counter = (counter afterExecute) + inc}
        in
            runProgram newState

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
