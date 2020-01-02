import Data.List.Split
main = do
  s <- readFile "day2-input.txt"
  putStrLn s
  let program = map read $ splitOn "," s :: [Int]
  putStrLn $ show program

  -- Restore the program state
  -- Replace position 1 with value 12, position 2 with value 2

  let restoredProgram = (head program):12:2:(drop 3 program)
  putStrLn $ show restoredProgram

  let finishedProgramState = runProgram 0 restoredProgram
  putStrLn "\nTHE RESULT IS...."
  putStrLn $ show $ finishedProgramState
  putStrLn "\nDONE"

runProgram counter program =
  if (program !! counter) == 99
  then
    program
  else
    runProgram (counter+4) $ executeAtPosition counter program


  

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
