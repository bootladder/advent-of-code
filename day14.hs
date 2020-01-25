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

-- change 1 CA to 2 CA
testFormulaListString2 =
   "9 ORE => 2 A"
  ++ "\n8 ORE => 3 B"
  ++ "\n7 ORE => 5 C"
  ++ "\n3 A, 4 B => 1 AB"
  ++ "\n5 B, 7 C => 1 BC"
  ++ "\n4 C, 1 A => 2 CA"
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

getListOfRawChemAmountsToMakeChemAmount :: Formula -> Int -> [Formula] -> [(String, Int)]
getListOfRawChemAmountsToMakeChemAmount formula numChems formulas =
  foldl
  (\acc (inputChemStr, inputChemAmount) ->
     if chemMadeFromOre (inputChemStr) formulas
     then
       -- eg. how many "A" does it take to make numChems "CA" ?
       --  N reactions * number of chems to do 1 reaction
       let numReactions =
             trace ("\nYESSSS A RAW CHEM\n")
             trace ("fromIntegral inputChemStr: : " ++ inputChemStr)
             trace ("fromIntegral numChems: : " ++ (show numChems))
             trace ("fromIntegral den : " ++ (show $ (fromIntegral $ outputAmount (getFormulaFor inputChemStr formulas))))
             ceiling $
             (fromIntegral numChems)
             /
             (fromIntegral $ outputAmount (getFormulaFor inputChemStr formulas))
       in
         acc
         ++
         [(inputChemStr, inputChemAmount * numReactions )]

     else
       let numReactions =
             trace ("\nNOT A RAW CHEM \n")
             trace ("fromIntegral inputChemStr: : " ++ inputChemStr)
             trace ("fromIntegral inputChemAmoutn: : " ++ (show $ fromIntegral inputChemAmount))
             trace ("fromIntegral den : " ++ (show $ (fromIntegral $ outputAmount (getFormulaFor inputChemStr formulas))))
             ceiling $
             (fromIntegral inputChemAmount)
             /
             (fromIntegral $ outputAmount (getFormulaFor inputChemStr formulas))
       in
         trace ("numReactions : " ++ (show numReactions))
         (++) acc $ getListOfRawChemAmountsToMakeChemAmount (getFormulaFor inputChemStr formulas) (numReactions * inputChemAmount) formulas
  )
  []
  (inputs formula)

sortListOfRawChems :: [(String,Int)] -> [(String,Int)]
sortListOfRawChems l =
  sortBy (\(s1,_) (s2,_) -> compare s1 s2) l

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
    listOfRawChems = getListOfRawChemAmountsToMakeChemAmount fuelFormula 1 formulas
    sorted = sortListOfRawChems listOfRawChems
    consolidated = consolidateListOfRawChems sorted

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
