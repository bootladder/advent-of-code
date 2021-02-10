{-# LANGUAGE BangPatterns #-}
import Data.List (foldl')


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

testInputString5 = "03036732577212944063491565474664"
testInputString6 = "02935109699940807407585447034323"
testInputString7 = "03081770884921959731165446850517"

onesDigit :: Int -> Int
onesDigit n = (mod (abs n) 10)

phaseOperation digits length = map (\d ->  onesDigit $ sumNDigits d digits) [1..length]
phaseOperationFaster !digits = reverse $ foldl' (\acc d -> [onesDigit $ ((head acc) + d)] ++ acc) [0] digits
--phaseOperationFaster digits = foldl (\acc d -> acc ++ [onesDigit $ ((head acc) + d)] ) [0] digits

main = do
  putStrLn "PART 2"
  inputFromFile <- (readFile "day16.txt") >>= (\s -> pure $ init s)
  let
    inputDigits = stringToDigits inputFromFile
    offset = getOffset $ inputDigits
    lengthOfInput = (10000 * length inputDigits)
    numDigitsToCalculate = lengthOfInput - offset

    transformedInput = take numDigitsToCalculate $ reverse $ repeat10000times inputDigits

    phase1 = phaseOperationFaster transformedInput
    phase2 = phaseOperationFaster phase1
    phase3 = phaseOperationFaster phase2
    phase4 = phaseOperationFaster phase3
    phase5 = phaseOperationFaster phase4
    phase6 = phaseOperationFaster phase5
    phase7 = phaseOperationFaster phase6
    phase8 = phaseOperationFaster phase7
    phase9 = phaseOperationFaster phase8

    phase100 = foldl' (\acc _ -> phaseOperationFaster acc) transformedInput [1..100]


  putStrLn $ "Offset " ++ (show $ offset)
  putStrLn $ "Length of Input " ++ (show $ lengthOfInput)
  putStrLn $ "numDigitsToCalculate " ++ (show $ numDigitsToCalculate)

  --putStrLn $ "Phase output 1 " ++ (show $ take 8 $ reverse phase9)
  putStrLn $ "Phase output 1 " ++ (show $ take 8 $ reverse phase100)
  --putStrLn $ "Phase output 1 " ++ (show $ take 8 $ reverse phase100)
  putStrLn "Hello"
