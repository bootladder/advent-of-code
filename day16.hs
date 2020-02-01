testInputString1 :: String
testInputString1 = "12345678"

testInputString2 :: String
testInputString2 = "80871224585914546619083218645595"

testInputString3 :: String
testInputString3 = "19617804207202209144916044189917"

testInputString4 :: String
testInputString4 = "69317163492948606335995924319873"

stringToDigits :: String -> [Int]
stringToDigits str =
  map (\c -> read [c]) str

theBasePattern :: [Int]
theBasePattern = [0,1,0,(-1)]

getBasePatternForDigit :: Int -> [Int] -> [Int]
getBasePatternForDigit digitNum basePattern =
    let
      thePattern = concat $ map (replicate digitNum) basePattern
      patternForever = concat $ repeat thePattern
    in
      tail patternForever


calculateDigit :: Int -> [Int] -> [Int] -> Int
calculateDigit digitNum basePattern digits =
  let
    phasePattern = getBasePatternForDigit digitNum basePattern
    zipped = zip (phasePattern) digits
    multiplied = map (\(a,b) -> a*b) zipped
    summed = sum multiplied
    onesDigit = mod (abs summed) 10
  in
    onesDigit

calculatePhase :: [Int] -> [Int] -> [Int]
calculatePhase digits basePattern =
  let indexes = [1..(length digits)]
  in
    map (\i -> calculateDigit i basePattern digits ) indexes


main = do
  s <- readFile "day16.txt" >>= (\c -> return $ (filter (/= '\n')) c)

  putStrLn ("The Length is " ++ (show $ length s))

  let digits = stringToDigits testInputString1
  putStrLn $ (show $ calculateDigit 7 theBasePattern digits)

  let phaser = (\digits -> calculatePhase digits theBasePattern)

  let phase1 = (phaser . phaser . phaser . phaser) digits

      digitsFromFile = stringToDigits s
      phase100 = foldl (\acc _ -> phaser acc) digitsFromFile [1..100]

  --putStrLn $ (show $ take 8 phase100)

  putStrLn "PART 2"

  let
      answer :: [Int]
      answer = foldl (\acc _ -> phaser acc) testInputDigits5 [1..100]

  putStrLn $ show $ take 8 answer
  putStrLn "wtf"
  putStrLn "Hello"

----- PART 2
testInputString5 :: String
testInputString5 = "03036732577212944063491565474664"

testInputDigits5 = concat $ replicate 10000 $ stringToDigits testInputString5
