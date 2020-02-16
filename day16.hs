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
    summed = zipMultiplyAdd phasePattern digits
    onesDigit = mod (abs summed) 10
  in
    onesDigit

zipMultiplyAdd :: [Int] -> [Int] -> Int
zipMultiplyAdd as bs =
  sum $ map (\(a,b)->a*b) $ zip as bs

calculatePhase :: [Int] -> [Int] -> [Int]
calculatePhase digits basePattern =
  let indexes = [1..(length digits)]
  in
    map (\i -> calculateDigit i basePattern digits ) indexes


main1 = do
  s <- readFile "day16.txt" >>= (\c -> return $ (filter (/= '\n')) c)

  putStrLn ("The Length is " ++ (show $ length s))

  let testDigits1 = stringToDigits testInputString1
  putStrLn $ (show $ calculateDigit 7 theBasePattern testDigits1)

  let phaser = (\digits -> calculatePhase digits theBasePattern)

  let phase1 = (phaser . phaser . phaser . phaser) testDigits1

      digitsFromFile = stringToDigits s
      phase100 = foldl (\acc _ -> phaser acc) digitsFromFile [1..100]

  putStrLn $ (show $ take 8 phase100)

----- PART 2

repeat10000times :: [Int] -> [Int]
repeat10000times digits = concat $ replicate 10000 $ digits


getOffset :: [Int] -> Int
getOffset ints =
  let digits = take 7 ints
      digitString = map (\i -> head $ show i) digits :: String
  in
    read digitString

sumNDigits :: Int -> [Int] -> Int
sumNDigits numDigits digits =
  sum $ take numDigits digits

testInputString5 :: String
testInputString5 = "03036732577212944063491565474664"

testInputDigits5 :: [Int]
testInputDigits5 = repeat10000times $ stringToDigits testInputString5

onesDigit :: Int -> Int
onesDigit n = (mod (abs n) 10)

phaseOperation digits length = map (\d ->  onesDigit $ sumNDigits d digits) [1..length]

main = do
  putStrLn "PART 2"
  let
      offset = 4
      lengthOfInput = 8
      numDigitsToCalculate = lengthOfInput - offset
      transformedInput = take numDigitsToCalculate $ reverse $ stringToDigits testInputString1

      phaseOutputDigits1 = phaseOperation transformedInput numDigitsToCalculate
      phaseOutputDigits2 = phaseOperation phaseOutputDigits1 numDigitsToCalculate
      phaseOutputDigits3 = phaseOperation phaseOutputDigits2 numDigitsToCalculate
      phaseOutputDigits4 = phaseOperation phaseOutputDigits3 numDigitsToCalculate

      phase100 = foldl (\acc _ -> phaseOperation acc numDigitsToCalculate) transformedInput [1..4]

  putStrLn $ "Phase output 1 " ++ (show phase100)
  putStrLn "Hello"
