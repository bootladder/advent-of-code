import Data.List.Split

testInput1 =
     "<x=-1, y=0, z=2>"
  ++ "\n<x=2, y=-10, z=-7>"
  ++ "\n<x=4, y=-8, z=8>"
  ++ "\n<x=3, y=5, z=-1>"
  ++ "\n"

testInput2 =
  "<x=-8, y=-10, z=0>"
  ++ "\n<x=5, y=5, z=10>"
  ++ "\n<x=2, y=-7, z=3>"
  ++ "\n<x=9, y=-8, z=-3>"

data Moon = Moon { xPos::Int
                 , yPos::Int
                 , zPos::Int
                 , xVel::Int
                 , yVel::Int
                 , zVel::Int
                 } deriving (Show, Eq)

parseMoon :: String -> Moon
parseMoon str =
  let coordstrings = splitOn ", " $ stripBrackets str :: [String]
  in
    moonFromList $ (map getValue coordstrings)

  where stripBrackets = filter (\c -> c/='<' && c/='>')
        getValue = read . last . splitOn "="

        moonFromList :: [Int] -> Moon
        moonFromList (a:b:c:[]) = Moon a b c 0 0 0 :: Moon
        moonFromList _ = Moon 0 0 0 0 0 0 

testMoons :: [Moon]
testMoons = map parseMoon $ lines testInput2

getAllPairs :: [Moon] -> [(Moon,Moon)]
getAllPairs z =
  if length z == 2
  then [(head z, last z)]
  else
    [(head z, i) | i<-tail z ] ++ (getAllPairs $ tail z)

calculateVelocity :: Moon -> [Moon] -> Moon
calculateVelocity moon moons =
  let otherMoons = filter (\m -> m /= moon) moons
      deltaVelX = sum $ map (\m -> compareField xPos m moon) otherMoons
      deltaVelY = sum $ map (\m -> compareField yPos m moon) otherMoons
      deltaVelZ = sum $ map (\m -> compareField zPos m moon) otherMoons
  in
    moon { xVel = (xVel moon) + deltaVelX
         , yVel = (yVel moon) + deltaVelY
         , zVel = (zVel moon) + deltaVelZ
         }

  where compareField f a b =
                  if (f a) > (f b)
                  then 1
                  else if (f a) < (f b)
                       then -1
                       else 0

calculatePosition :: Moon -> Moon
calculatePosition m =
  m { xPos = (xPos m) + (xVel m)
    , yPos = (yPos m) + (yVel m)
    , zPos = (zPos m) + (zVel m)
    }


simulationStep :: [Moon] -> [Moon]
simulationStep moons =
  let calcVels = map (\m -> calculateVelocity m moons) moons
      calcPoss = map calculatePosition calcVels
  in
    calcPoss


runSimulationNSteps :: Int -> [Moon] -> [Moon]
runSimulationNSteps n moons =
  let step = simulationStep moons
  in
    if n == 0 then moons else runSimulationNSteps (n-1) step

calculateEnergy :: Moon -> Int
calculateEnergy moon =
  let pot = abs (xPos moon) + abs(yPos moon) + abs(zPos moon)
      kin = abs (xVel moon) + abs(yVel moon) + abs(zVel moon)
  in
    pot * kin

calculateTotalEnergy :: [Moon] -> Int
calculateTotalEnergy moons =
  sum $ map calculateEnergy moons


main = do
  putStrLn "hehlo"
  s <- readFile "day12-input.txt"
  let moons = map parseMoon $ lines s
      energy = calculateTotalEnergy $ runSimulationNSteps 1000 moons

  putStrLn $ show energy

printList :: (Show a) => [a] -> IO ()
printList = mapM_  (putStrLn . show)
