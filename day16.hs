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


main = do
  s <- readFile "day16.txt" >>= (\c -> return $ (filter (/= '\n')) c)

  putStrLn ("The Length is " ++ (show $ length s))

  let testDigits1 = stringToDigits testInputString1
  putStrLn $ (show $ calculateDigit 7 theBasePattern testDigits1)

  let phaser = (\digits -> calculatePhase digits theBasePattern)

  let phase1 = (phaser . phaser . phaser . phaser) testDigits1

      digitsFromFile = stringToDigits s
      phase100 = foldl (\acc _ -> phaser acc) digitsFromFile [1..100]

  --putStrLn $ (show $ take 8 phase100)

  putStrLn "PART 2"

  let
      offset = getOffset testInputDigits5
      answerP1 = phaserFromOffset offset testInputDigits5
      answerP1_fast = phaserFromOffsetUsingFirstPeriod offset (stringToDigits testInputString5)
      answerP2_fast = phaserFromOffsetUsingFirstPeriod offset answerP1_fast
      --answerP100 = foldl (\acc _ -> phaserFromOffsetUsingFirstPeriod offset acc) answerP1_fast [1..2]

  putStrLn ("Length of Input: " ++ (show $ length testInputDigits5))
  putStrLn ("Length of P1: " ++ (show $ length answerP1))
  putStrLn ("P1: " ++ (show $ answerP1))
  putStrLn ("Length of P100 Faster: " ++ (show $ length $ answerP2_fast))
  putStrLn "wtf"
  putStrLn "Hello"

----- PART 2
testInputString5 :: String
testInputString5 = "03036732577212944063491565474664"

testInputDigits5 :: [Int]
testInputDigits5 = concat $ replicate 10000 $ stringToDigits testInputString5


showBaseDigit :: Int -> String
showBaseDigit 0    = "0"
showBaseDigit (-1) = "-"
showBaseDigit 1    = "1"

showBasePatternForDigit :: Int -> Int -> String
showBasePatternForDigit digit length =
  concat $ map (\i -> (showBaseDigit i) ++ " ") (take length $ getBasePatternForDigit digit theBasePattern)

getOffset :: [Int] -> Int
getOffset ints =
  let digits = take 7 ints
      digitString = map (\i -> head $ show i) digits :: String
  in
    read digitString


phaserFromOffset :: Int -> [Int] -> [Int]
phaserFromOffset offset digits = map (\i -> calculateDigit (offset+i) theBasePattern digits) [0..7]


phaserFromOffsetUsingFirstPeriod :: Int -> [Int] -> [Int]
phaserFromOffsetUsingFirstPeriod offset onePeriodOfDigits =
  let
    reversedDigits = concat $ replicate 10000 (reverse onePeriodOfDigits)
    inputLength = 10000 * (length onePeriodOfDigits)
    distanceOfOffsetFromEnd = inputLength - offset
    sums = map (\i -> sum $ take (distanceOfOffsetFromEnd - i + 1) reversedDigits) [0..distanceOfOffsetFromEnd]
  in
    map (\i -> mod (abs i) 10) sums
