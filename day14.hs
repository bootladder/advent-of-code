import Data.List
import Data.List.Split
import Data.Char
import Debug.Trace

data Formula = Formula { outputChem :: String
                       , outputAmount :: Int
                       , inputs :: [(String,Int)]
                       } deriving Show

parseFormulaString :: String -> [Formula]
parseFormulaString str = map parseFormula $ lines str

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

getListOfRawChemAmountsToMakeChemAmount :: Formula -> Int -> [Formula] -> [(String, Int)]
getListOfRawChemAmountsToMakeChemAmount formula numChems formulas =
  foldl
  (\acc (inputChemStr, inputChemAmount) ->
     let formulaForInput = getFormulaFor inputChemStr formulas
         numReactions = ceiling $
                        (fromIntegral numChems)
                        /
                        (fromIntegral (outputAmount formula))
     in
       if chemMadeFromOre inputChemStr formulas
       then
         acc ++ [(inputChemStr, inputChemAmount * numReactions)]
       else
         acc ++ getListOfRawChemAmountsToMakeChemAmount formulaForInput (numChems * inputChemAmount) formulas
  )
  []
  (inputs formula)

sortListOfRawChems :: [(String,Int)] -> [(String,Int)]
sortListOfRawChems l =
  sortBy (\(s1,_) (s2,_) -> compare s1 s2) l

consolidateListOfRawChems :: [(String,Int)] -> [(String,Int)]
consolidateListOfRawChems l =
  let
    sorted = sortListOfRawChems l
    groups = groupBy (\(s1,_) (s2,_) -> s1==s2) sorted
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


  ---------------------------------------------------
  --- TESTS

testFormulaListString_OneLevel =
       "2 ORE => 4 A"
  ++ "\n2 ORE => 4 B"
  ++ "\n2 ORE => 4 C"
  ++ "\n2 ORE => 4 D"
  ++ "\n3 A, 7 B, 11 C, 15 D => 1 FUEL"

test__oneLevelFormula_OneLevel :: IO ()
test__oneLevelFormula_OneLevel =
  let
    formulas = map parseFormula $ lines testFormulaListString_OneLevel
    list = getListOfRawChemAmountsToMakeChemAmount (getFormulaFor "FUEL" formulas) 1 formulas

    contains1 = elem ("A", 3) list
    contains2 = elem ("B", 7) list
  in do
    putStrLn $ if contains1 then "PASS" else "FAIL"
    putStrLn $ if contains2 then "PASS" else "FAIL"
    putStrLn $ show list

testFormulaListString_TwoLevels =
       "2 ORE => 4 A"
  ++ "\n2 ORE => 4 B"
  ++ "\n2 A , 2 B => 1 AB"
  ++ "\n3 AB => 1 FUEL"

test__oneLevelFormula_TwoLevels :: IO ()
test__oneLevelFormula_TwoLevels =
  let
    formulas = map parseFormula $ lines testFormulaListString_TwoLevels
    list = getListOfRawChemAmountsToMakeChemAmount (getFormulaFor "FUEL" formulas) 1 formulas

    contains1 = elem ("A", 6) list
    contains2 = elem ("B", 6) list
  in do
    putStrLn $ if contains1 then "PASS" else "FAIL"
    putStrLn $ if contains2 then "PASS" else "FAIL"
    putStrLn $ show list

testFormulaListString_TwoLevels_FormulaOutputsMoreThan1 =
       "2 ORE => 4 A"
  ++ "\n2 ORE => 4 B"
  ++ "\n2 A , 2 B => 2 AB"
  ++ "\n3 AB => 1 FUEL"

test__3  :: IO ()
test__3  = do
  putStrLn "\noneLevelFormula_TwoLevels_FormulaOutputsMoreThan1"
  let
    formulas = map parseFormula $ lines testFormulaListString_TwoLevels_FormulaOutputsMoreThan1
    list = getListOfRawChemAmountsToMakeChemAmount (getFormulaFor "FUEL" formulas) 1 formulas

    contains1 = elem ("A", 4) list
    contains2 = elem ("B", 4) list

  putStrLn $ if contains1 then "PASS" else "FAIL"
  putStrLn $ if contains2 then "PASS" else "FAIL"
  putStrLn $ show list
  putStrLn "\n"

testFormulaListString_TwoLevels_TwoFuelInputs_FormulasOutputMoreThan1 =
       "2 ORE => 4 A"
  ++ "\n2 ORE => 4 B"
  ++ "\n2 ORE => 4 C"
  ++ "\n2 ORE => 4 D"
  ++ "\n2 A , 2 B => 5 AB"
  ++ "\n5 C , 5 D => 2 CD"
  ++ "\n3 AB , 3 CD => 1 FUEL"

test__4 :: IO ()
test__4  = do
  putStrLn "\ntest__oneLevelFormula_TwoLevels_TwoFuelInputs"
  let
    formulas = map parseFormula $ lines testFormulaListString_TwoLevels_TwoFuelInputs_FormulasOutputMoreThan1
    list = getListOfRawChemAmountsToMakeChemAmount (getFormulaFor "FUEL" formulas) 1 formulas

    contains1 = elem ("A", 2) list
    contains2 = elem ("B", 2) list
    contains3 = elem ("C", 10) list
    contains4 = elem ("D", 10) list

  putStrLn $ if contains1 then "PASS" else "FAIL"
  putStrLn $ if contains2 then "PASS" else "FAIL"
  putStrLn $ if contains3 then "PASS" else "FAIL"
  putStrLn $ if contains4 then "PASS" else "FAIL"
  putStrLn $ show list

testFormulaListString_TwoLevels_TwoFuelInputs_RequiresConsolidation =
       "2 ORE => 4 A"
  ++ "\n2 ORE => 4 B"
  ++ "\n2 ORE => 4 C"
  ++ "\n2 ORE => 4 D"
  ++ "\n2 A , 2 B => 2 AB"
  ++ "\n5 C , 5 D => 2 CD"
  ++ "\n3 AB , 3 CD, 1 A => 1 FUEL"

test__5 :: IO ()
test__5  = do
  putStrLn "\ntest__oneLevelFormula_TwoLevels_TwoFuelInputs_RequiresConsolidation"
  let
    formulas = map parseFormula $ lines testFormulaListString_TwoLevels_TwoFuelInputs_RequiresConsolidation
    list = getListOfRawChemAmountsToMakeChemAmount (getFormulaFor "FUEL" formulas) 1 formulas
    consolidated = consolidateListOfRawChems list

    contains1 = elem ("A", 5)  consolidated
    contains2 = elem ("B", 4)  consolidated
    contains3 = elem ("C", 10) consolidated
    contains4 = elem ("D", 10) consolidated

  putStrLn $ if contains1 then "PASS" else "FAIL"
  putStrLn $ if contains2 then "PASS" else "FAIL"
  putStrLn $ if contains3 then "PASS" else "FAIL"
  putStrLn $ if contains4 then "PASS" else "FAIL"
  putStrLn $ show list


testFormulaListString_ThreeLevels_RequiresIntermediateConsolidation =
       "2 ORE => 4 A"
  ++ "\n2 ORE => 4 B"
  ++ "\n2 ORE => 4 C"
  ++ "\n2 ORE => 4 D"
  ++ "\n2 A , 2 B => 2 AB"
  ++ "\n5 C , 5 D => 2 CD"
  ++ "\n3 AB , 3 CD => 1 ABCD"
  ++ "\n1 ABCD, 1 AB => 1 FUEL"

test__6 :: IO ()
test__6  = do
  putStrLn "\ntestFormulaListString_ThreeLevels_RequiresIntermediateConsolidation"
  let
    formulas = map parseFormula $ lines testFormulaListString_ThreeLevels_RequiresIntermediateConsolidation

  putStrLn $ show formulas
  let
    list = getListOfRawChemAmountsToMakeChemAmount (getFormulaFor "FUEL" formulas) 1 formulas
    consolidated = consolidateListOfRawChems list
    actual = consolidated

    contains1 = elem ("A", 4)  actual
    contains2 = elem ("B", 4)  actual
    contains3 = elem ("C", 10) actual
    contains4 = elem ("D", 10) actual

  putStrLn $ if contains1 then "PASS" else "FAIL"
  putStrLn $ if contains2 then "PASS" else "FAIL"
  putStrLn $ if contains3 then "PASS" else "FAIL"
  putStrLn $ if contains4 then "PASS" else "FAIL"
  putStrLn $ show actual


testA :: IO ()
testA = do
  putStrLn "TEST CONSLIDATE\n"
  let a = [("A",1), ("B",1), ("A",1)]
      consolidated = consolidateListOfRawChems a
      contains = elem ("A",2) consolidated
  putStrLn $ if contains then "PASS" else "FAIL"
  putStrLn "\n"

runTests :: IO ()
runTests = do
  testA

  test__oneLevelFormula_OneLevel
  test__oneLevelFormula_TwoLevels
  test__3
  test__4
  test__5
  test__6


main = do
  runTests
  putStrLn "\n\n\n"

  let
    formulas = map parseFormula $ lines testFormulaListString4
    fuelFormula = getFormulaFor "FUEL" formulas
    listOfRawChems = getListOfRawChemAmountsToMakeChemAmount fuelFormula 1 formulas
    consolidated = consolidateListOfRawChems listOfRawChems

    oresPerChem = map (\(chemStr,i) -> (chemStr, numOresToMakeNumChems (getFormulaFor chemStr formulas) i)) consolidated

    totalOres = sum $ map snd oresPerChem

  mapM_ (putStrLn . show) formulas
  putStrLn "fuelFormula: : "
  putStrLn $ show fuelFormula
  putStrLn "listOfRawChems: "
  putStrLn $ show listOfRawChems
  putStrLn "consolidated: "
  putStrLn $ show consolidated
  putStrLn "oresPerChem:"
  putStrLn $ show oresPerChem
  putStrLn "totalOres:"
  putStrLn $ show totalOres
  putStrLn "hello"


-------------------------------------------------
---- VALIDATION

testFormula = "7 A, 1 B => 1 C"
testFormula2 = "7 A, 1 E => 1 FUEL"
testFormula3 = "10 ORE => 10 A"

testFormulaListString1 =
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

testFormulaListString3 =
     "157 ORE => 5 NZVS"
  ++ "\n165 ORE => 6 DCFZ"
  ++ "\n44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL"
  ++ "\n12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ"
  ++ "\n179 ORE => 7 PSHF"
  ++ "\n177 ORE => 5 HKGWZ"
  ++ "\n7 DCFZ, 7 PSHF => 2 XJWVT"
  ++ "\n165 ORE => 2 GPVTF"
  ++ "\n3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT"

testFormulaListString4 =
       "2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG"
  ++ "\n17 NVRVD, 3 JNWZP => 8 VPVL"
  ++ "\n53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL"
  ++ "\n22 VJHF, 37 MNCFX => 5 FWMGM"
  ++ "\n139 ORE => 4 NVRVD"
  ++ "\n144 ORE => 7 JNWZP"
  ++ "\n5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC"
  ++ "\n5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV"
  ++ "\n145 ORE => 6 MNCFX"
  ++ "\n1 NVRVD => 8 CXFTF"
  ++ "\n1 VJHF, 6 MNCFX => 4 RFSQX"
  ++ "\n176 ORE => 6 VJHF"

testFormulaListString5 =
     "171 ORE => 8 CNZTR"
    ++ "\n7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL"
    ++ "\n114 ORE => 4 BHXH"
    ++ "\n14 VRPVC => 6 BMBT"
    ++ "\n6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL"
    ++ "\n6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT"
    ++ "\n15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW"
    ++ "\n13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW"
    ++ "\n5 BMBT => 4 WPTQ"
    ++ "\n189 ORE => 9 KTJDG"
    ++ "\n1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP"
    ++ "\n12 VRPVC, 27 CNZTR => 2 XDBXC"
    ++ "\n15 KTJDG, 12 BHXH => 5 XCVML"
    ++ "\n3 BHXH, 2 VRPVC => 7 MZWV"
    ++ "\n121 ORE => 7 VRPVC"
    ++ "\n7 XCVML => 6 RJRHP"
    ++ "\n5 BHXH, 4 VRPVC => 5 LTCX"
