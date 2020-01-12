import Data.List.Split

main =
  (readFile "day2-input.txt") >>=
  (\input   -> pure (parseProgram input) >>=
  (\program -> (putStrLn $ show program :: IO ()) >>
  (pure (restoreProgram program) >>=
  (\restoredProgram -> pure (runProgram 0 restoredProgram) >>=
  (\finishedProgramState -> (putStrLn $ "The answer is: " ++ (show $ finishedProgramState !! 0))) >>

  putStrLn "yay" >>
  putStrLn "PART 2" >>

  (pure $ part2 program) >>=
  (\answer -> putStrLn ("The answer is " ++ (show answer)))
  ))))

parseProgram :: String -> [Int]
parseProgram s = map read $ splitOn "," s

-- Restore the program state
-- Replace position 1 with value 12, position 2 with value 2
restoreProgram :: [Int] -> [Int]
restoreProgram p = (head p):12:2:(drop 3 p)

setupProgram :: [Int] -> Int -> Int -> [Int]
setupProgram program noun verb =
   (head program):noun:verb:(drop 3 program)


part2 :: [Int] -> Int
part2 program =
  let allpossibilties =
        [(noun,verb,runProgram 0 (setupProgram program noun verb)) |
         noun <- [1..99] ,verb <- [1..99]]

      matches = filter (\(n,v,p) -> p !! 0 == 19690720) allpossibilties

      match = head matches
      noun_ (n,_,_) = n
      verb_ (_,v,_) = v
      answer = (100*(noun_ match)) + (verb_ match)
  in
    answer



runProgram :: Int -> [Int] -> [Int]
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
