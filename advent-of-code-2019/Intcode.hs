module Intcode where
import Data.List.Split
import Data.Array
import Debug.Trace

type Program = Array Int Int
type Counter = Int
data Status  = OK|WAITFORINPUT Int|OUTPUTTING Int|HALTED|BADINSTRUCTION deriving (Show, Eq)
data ComputerState =
  ComputerState { computerId :: String
                , program :: Program
                , counter :: Counter
                , relativeBase :: Int
                , status :: Status
                , inputBuffer :: [Int]
                , outputBuffer :: [Int]
                }

parseProgram :: String -> Program
parseProgram str = listArray (0,4000) $ (map read $ splitOn "," str) ++ (repeat 0)

showComputerState :: ComputerState -> String
showComputerState cs = "ComputerState: ID : " ++ (computerId cs) ++ " Counter = " ++ (show $ counter cs) ++ " Status = " ++ (show $ status cs) ++ " relativeBase = " ++ (show $ relativeBase cs) ++ " inputbuffer = " ++ (show $ inputBuffer cs) ++ " outputBuffer = " ++ (show $ outputBuffer cs)

instance Show ComputerState where show = showComputerState

showCurrentInstruction :: ComputerState -> String
showCurrentInstruction cs =
  show $ getNextInstruction cs

defaultComputerState :: ComputerState
defaultComputerState =
  ComputerState { computerId = "default"
                , program=listArray (0,10000) [1..]
                , counter = 0
                , relativeBase = 0
                , status = OK
                , inputBuffer = []
                , outputBuffer = []
                }

data Opcode = ADD|MULT|INPUT|OUTPUT|JUMPIFTRUE|JUMPIFFALSE|LESSTHAN|EQUALS|ADJUSTRELATIVEBASE|HALT|INVALID deriving Show

data ParamType = Relative|Positional|Immediate|Invalid deriving Show

data Param = Param Int ParamType deriving Show

data Instruction =
  Instruction { opcode :: Opcode
              , param1 :: Param
              , param2 :: Param
              , param3 :: Param
              } deriving Show

runProgram :: ComputerState -> ComputerState
runProgram cs =
  case (status cs) of
    WAITFORINPUT loc ->
      --terminate.
      --allow the outside to fill the inputBuffer and run again
      if null $ inputBuffer cs
      then cs
      else
        let inputValue = head $ inputBuffer cs
            newProgram = (program cs) // [(loc, inputValue)]

        in runProgram $ cs {program=newProgram
                            , status=OK
                            , inputBuffer = tail $ inputBuffer cs
                            }

    OUTPUTTING value ->
      runProgram $ cs {status=OK
                       , outputBuffer = (outputBuffer cs) ++ [value]
                       }

    OK ->
      if (program cs ! counter cs) == 99 --halt
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
    ADJUSTRELATIVEBASE -> 2
    _ -> 999999

executeAtPosition :: ComputerState -> ComputerState
executeAtPosition cs =
  let instruction = getNextInstruction cs
  in
    case opcode instruction of
    ADD ->
      let p1val = param2value (param1 instruction) cs
          p2val = param2value (param2 instruction) cs
          p3pos = param2position (param3 instruction) cs
          newProgram = add p1val p2val p3pos $ program cs
      in
        cs {program=newProgram}

    MULT ->
      let p1val = param2value (param1 instruction) cs
          p2val = param2value (param2 instruction) cs
          p3pos = param2position (param3 instruction) cs
          newProgram = multiply p1val p2val p3pos $ program cs
      in
        cs {program=newProgram}

    INPUT ->
      let loc = case (param1 instruction) of
                  Param i Immediate -> i
                  Param i Positional -> i
                  Param i Relative -> ((relativeBase cs) + i)
                  _ -> -999
      in
        cs {status=WAITFORINPUT loc}

    OUTPUT ->
      let value = param2value (param1 instruction) cs
      in
        cs {status=OUTPUTTING value}

    JUMPIFTRUE ->
      let p1val = param2value (param1 instruction) cs
          p2val = param2value (param2 instruction) cs
      in
        if p1val /= 0
        then
          cs {counter = p2val}
        else
          cs {counter = (counter cs) + 3}

    JUMPIFFALSE ->
      let p1val = param2value (param1 instruction) cs
          p2val = param2value (param2 instruction) cs
      in
        if p1val == 0
        then
          cs {counter = p2val}
        else
          cs {counter = (counter cs) + 3}

    LESSTHAN ->
      let p1val = param2value (param1 instruction) cs
          p2val = param2value (param2 instruction) cs
          p3pos = param2position (param3 instruction) cs
      in
        if p1val < p2val
        then
          cs{program = writeValueToProgram (program cs) 1 p3pos}
        else
          cs{program = writeValueToProgram  (program cs) 0 p3pos}

    EQUALS ->
      let p1val = param2value (param1 instruction) cs
          p2val = param2value (param2 instruction) cs
          p3pos = param2position (param3 instruction) cs
      in
        if p1val == p2val
        then
          cs{program = writeValueToProgram (program cs) 1 p3pos}
        else
          cs{program = writeValueToProgram  (program cs) 0 p3pos}

    ADJUSTRELATIVEBASE ->
      let
        value = param2value (param1 instruction) cs
      in
        cs {relativeBase = (relativeBase cs) + value}

    _ -> cs {status=BADINSTRUCTION}

writeValueToProgram :: Program -> Int -> Int -> Program
writeValueToProgram prog val loc =
  prog // [(loc, val)]
  --(take loc prog) ++ [val] ++ (drop (loc + 1) prog)

getNextInstruction :: ComputerState -> Instruction
getNextInstruction cs =
  let --rawInstruction = drop (counter cs) (program cs)
      --firstValue = rawInstruction !! 0
      firstValue = (program cs) ! (counter cs)
      rawOpcode  = firstValue `mod` 100
      param1type = case (firstValue `mod` 1000) `div` 100 of
                     2 -> Relative
                     1 -> Immediate
                     0 -> Positional
                     _ -> Invalid

      param1Value =
        (program cs) ! ((counter cs) + 1)

      param2type = case (firstValue `mod` 10000) `div` 1000 of
                     2 -> Relative
                     1 -> Immediate
                     0 -> Positional
                     _ -> Invalid

      param2Value =
        (program cs) ! ((counter cs) + 2)

      param3type = case (firstValue `mod` 100000) `div` 10000 of
                     2 -> Relative
                     1 -> Immediate
                     0 -> Positional
                     _ -> Invalid

      param3Value =
        (program cs) ! ((counter cs) + 3)
  in

  Instruction { opcode = parseOpcode rawOpcode
              , param1 = Param param1Value param1type
              , param2 = Param param2Value param2type
              , param3 = Param param3Value param3type
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
  9 -> ADJUSTRELATIVEBASE
  99 -> HALT
  _ -> INVALID


param2value :: Param -> ComputerState -> Int
param2value param cs =
  case param of
    Param i Immediate -> i
    Param i Positional -> (program cs) ! i
    Param i Relative -> (program cs) ! ((relativeBase cs) + i)
    Param _ Invalid -> 0

param2position :: Param -> ComputerState -> Int
param2position param cs =
  case param of
    Param i Positional -> i
    Param i Relative -> ((relativeBase cs) + i)
    Param _ _ -> -999


getParamValue :: Param -> Int
getParamValue (Param i _) = i


add :: Int -> Int -> Int -> Program -> Program
add x_val y_val result_pos prog =
  let
      sum_ = x_val + y_val
  in
    prog // [(result_pos, sum_)]

multiply :: Int -> Int -> Int -> Program -> Program
multiply x_val y_val result_pos prog =
  let
      product_ = x_val * y_val
  in
    prog // [(result_pos, product_)]
