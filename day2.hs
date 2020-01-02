import Data.List.Split
main = do
  s <- readFile "day2-input.txt"
  putStrLn s
  let program = map read $ splitOn "," s :: [Int]
  putStrLn $ show program

  -- Restore the program state
  -- Replace position 1 with value 12, position 2 with value 2

  let restoredProgram = 12:2:(drop 2 program)
  mapM_ (\chunk -> putStrLn $ (show chunk) ++ "\n" ) $ chunksOf 4 restoredProgram

  let finishedProgramState = runProgram restoredProgram
  putStrLn "The program is>......"
  putStrLn $ show finishedProgramState
  putStrLn $ show $ head finishedProgramState

runProgram program =
  let instructions = chunksOf 4 program
  in
    foldl (\p i -> executeInstruction i p) program instructions

  

executeInstruction instruction program =
  let intcode = head instruction
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
