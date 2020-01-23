import Data.List
import Data.List.Split
import Data.Char
import Debug.Trace

testFormula = "7 A, 1 B => 1 C"
testFormula2 = "7 A, 1 E => 1 FUEL"
testFormula3 = "10 ORE => 10 A"

testFormulaListString =
   "10 ORE => 10 A"
  ++ "\n1 ORE => 1 B"
  ++ "\n7 A, 1 B => 1 C"
  ++ "\n7 A, 1 C => 1 D"
  ++ "\n7 A, 1 D => 1 E"
  ++ "\n7 A, 1 E => 1 FUEL"

testFormulaListString2 =
   "9 ORE => 2 A"
  ++ "\n8 ORE => 3 B"
  ++ "\n7 ORE => 5 C"
  ++ "\n3 A, 4 B => 1 AB"
  ++ "\n5 B, 7 C => 1 BC"
  ++ "\n4 C, 1 A => 1 CA"
  ++ "\n2 AB, 3 BC, 4 CA => 1 FUEL"


data Formula = Formula { outputChem :: String
                       , outputAmount :: Int
                       , inputs :: [(String,Int)]
                       } deriving Show

parseFormula :: String -> Formula
parseFormula str =
  let
    stripped = filter (/= ' ') str
    outputStr = last $ splitOn "=>" stripped

    inputStrings = splitOn "," $ head $ splitOn "=>" stripped

  in
    Formula { outputChem = fst $ parseChemAmount outputStr
            , outputAmount = snd $ parseChemAmount outputStr
            , inputs = map parseChemAmount inputStrings}

parseChemAmount :: String -> (String,Int)
parseChemAmount str =
    let amountStr = takeWhile isDigit str
        amount = read amountStr :: Int
        chemStr = dropWhile isDigit str
    in
      (chemStr, amount)

chemMadeFromOre :: String -> [Formula] -> Bool
chemMadeFromOre chemStr formulas =
  let formula = head $ filter (\f -> chemStr == outputChem f) formulas
  in
    "ORE" == fst (head $ inputs formula)

findFuelFormula :: [Formula] -> Formula
findFuelFormula formulas =
  head $ filter (\f -> (outputChem f)  == "FUEL") formulas

getFormulaFor :: String -> [Formula] -> Formula
getFormulaFor chemStr formulas =
  head $ filter (\f -> chemStr == outputChem f) formulas

getListOfRawChemsToMakeChem :: Formula -> [Formula] -> [(String, Int)]
getListOfRawChemsToMakeChem formula formulas =
  foldl
  (\acc (inputChemStr, inputChemAmount) ->
     if chemMadeFromOre (inputChemStr) formulas
     then acc ++ [(inputChemStr, inputChemAmount)]
     else (++) acc $ getListOfRawChemsToMakeChem (getFormulaFor inputChemStr formulas) formulas
  )
  []
  (inputs formula)

consolidateListOfRawChems :: [(String,Int)] -> [(String,Int)]
consolidateListOfRawChems l =
  let
    groups = groupBy (\(s1,_) (s2,_) -> s1==s2) l
  in
    map (\group -> (fst $ head group, consolidate group)) groups
  where
    consolidate = foldl (\acc (_,i) -> acc+i) 0

numOresToMakeNumChems :: Formula -> Int -> Int
numOresToMakeNumChems formula numChems =
  let numReactions =
        ceiling $
        (fromIntegral numChems)
        /
        (fromIntegral $ outputAmount formula)

      numOresPerReaction = snd $ head $ (inputs formula)
  in
    numOresPerReaction * numReactions

testFormulaList = map parseFormula $ lines testFormulaListString

main = do

  let
    formulas = map parseFormula $ lines testFormulaListString2
    fuelFormula = getFormulaFor "FUEL" formulas
    listOfRawChems = getListOfRawChemsToMakeChem fuelFormula formulas
    consolidated = consolidateListOfRawChems listOfRawChems

    oresPerChem = map (\(chemStr,i) -> (chemStr, numOresToMakeNumChems (getFormulaFor chemStr formulas) i)) consolidated

    totalOres = sum $ map snd oresPerChem

  mapM_ (putStrLn . show) formulas
  putStrLn $ show fuelFormula
  putStrLn $ show listOfRawChems
  putStrLn $ show consolidated

  putStrLn $ show oresPerChem
  putStrLn $ show totalOres
  putStrLn "hello"
