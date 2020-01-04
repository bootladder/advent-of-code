import Data.List.Split

main = do
  s <- readFile "day4-input.txt"
  let l:u:[] = splitOn "-" s
  let lowerRange = read l :: Int
      upperRange = read u :: Int
  let range = [lowerRange..upperRange]

  let possibilities = filter isPossiblePassword range
  putStrLn $ show $ length possibilities

  putStrLn "Part 2"
  let possibilities = filter isPossiblePasswordPart2 range
  putStrLn $ show $ length possibilities

isPossiblePassword i =
  (hasTwoAdjacentDigits i) &&
  (digitsNeverDecrease i) 

isPossiblePasswordPart2 i = 
  (hasExactlyTwoAdjacentDigits i) &&
  (digitsNeverDecrease i) 


hasTwoAdjacentDigits i = 
    listHasTwoAdjacentMembers $ digitsOfInt i

digitsOfInt i = map (\c -> read [c]) $ show i :: [Int]

listHasTwoAdjacentMembers (x:[]) = False
listHasTwoAdjacentMembers (x:xs) =
  if x == (head xs)
  then True
  else listHasTwoAdjacentMembers xs
  
digitsNeverDecrease i = digitsNeverDecrease' $ digitsOfInt i
  where
    digitsNeverDecrease' (x:[]) = True
    digitsNeverDecrease' (x:xs) =
      if (head xs) < x
      then False
      else digitsNeverDecrease' xs


hasExactlyTwoAdjacentDigits i =
  let paddedDigits = [-1] ++ (digitsOfInt i) ++ [-1]
  in
    listHasExactlyTwoAdjacentMembers paddedDigits

listHasExactlyTwoAdjacentMembers (x1:x2:x3:[]) = False
listHasExactlyTwoAdjacentMembers (x1:x2:[]) = False
listHasExactlyTwoAdjacentMembers (x1:x2:x3:xs) =
  if x1 /= x2 && x2 == x3 && x3 /= (head xs)
  then True
  else listHasExactlyTwoAdjacentMembers (x2:x3:xs)
