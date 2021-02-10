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

testMoons1 :: [Moon]
testMoons1 = map parseMoon $ lines testInput1

testMoons2 :: [Moon]
testMoons2 = map parseMoon $ lines testInput2

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

  ----

runSimulationUntilRepeat :: [Moon] -> Int
runSimulationUntilRepeat moons =
  let initialMoons = moons
      result = run' initialMoons moons 0
  in
    snd result

run' :: [Moon] -> [Moon] -> Int -> ([Moon], Int)
run' initial moons n =
  if(initial `equals` moons && n /= 0)
  then (moons, n)
  else
    let nextMoons = simulationStep moons
    in
      run' initial nextMoons (n+1)
  where
    equals ms1 ms2 =
      and (map moonEquals (zip ms1 ms2))
    moonEquals (m1,m2) =
      (xPos m1 == xPos m2) &&
      (yPos m1 == yPos m2) &&
      (zPos m1 == zPos m2) &&
      (xVel m1 == xVel m2) &&
      (yVel m1 == yVel m2) &&
      (zVel m1 == zVel m2)


runSimulationUntilRepeatX :: [Moon] -> Int
runSimulationUntilRepeatX moons =
  let initialMoons = moons
      result = runX' initialMoons moons 0
  in
    snd result

runX' :: [Moon] -> [Moon] -> Int -> ([Moon], Int)
runX' initial moons n =
  if(initial `equals` moons && n /= 0)
  then (moons, n)
  else
    let nextMoons = simulationStepX moons
    in
      runX' initial nextMoons (n+1)
  where
    equals ms1 ms2 =
      and (map moonEquals (zip ms1 ms2))
    moonEquals (m1,m2) =
      (xPos m1 == xPos m2) &&
      (xVel m1 == xVel m2)


simulationStepX :: [Moon] -> [Moon]
simulationStepX moons =
  let calcVels = map (\m -> calculateVelocityX m moons) moons
      calcPoss = map calculatePositionX calcVels
  in
    calcPoss



calculateVelocityX :: Moon -> [Moon] -> Moon
calculateVelocityX moon moons =
  let otherMoons = filter (\m -> m /= moon) moons
      deltaVelX = sum $ map (\m -> compareField xPos m moon) otherMoons
  in
    moon { xVel = (xVel moon) + deltaVelX
         }

  where compareField f a b =
                  if (f a) > (f b)
                  then 1
                  else if (f a) < (f b)
                       then -1
                       else 0

calculatePositionX :: Moon -> Moon
calculatePositionX m =
  m { xPos = (xPos m) + (xVel m)
    }


runSimulationNStepsX :: Int -> [Moon] -> [Moon]
runSimulationNStepsX n moons =
  let step = simulationStepX moons
  in
    if n == 0 then moons else runSimulationNStepsX (n-1) step

runSimulationNStepsY :: Int -> [Moon] -> [Moon]
runSimulationNStepsY n moons =
  let step = simulationStepY moons
  in
    if n == 0 then moons else runSimulationNStepsY (n-1) step


simulationStepY :: [Moon] -> [Moon]
simulationStepY moons =
  let calcVels = map (\m -> calculateVelocityY m moons) moons
      calcPoss = map calculatePositionY calcVels
  in
    calcPoss

calculateVelocityY :: Moon -> [Moon] -> Moon
calculateVelocityY moon moons =
  let otherMoons = filter (\m -> m /= moon) moons
      deltaVelY = sum $ map (\m -> compareField yPos m moon) otherMoons
  in
    moon { yVel = (yVel moon) + deltaVelY
         }

  where compareField f a b =
                  if (f a) > (f b)
                  then 1
                  else if (f a) < (f b)
                       then -1
                       else 0

calculatePositionY :: Moon -> Moon
calculatePositionY m =
  m { yPos = (yPos m) + (yVel m)
    }

runSimulationUntilRepeatY :: [Moon] -> Int
runSimulationUntilRepeatY moons =
  let initialMoons = moons
      result = runY' initialMoons moons 0
  in
    snd result

runY' :: [Moon] -> [Moon] -> Int -> ([Moon], Int)
runY' initial moons n =
  if(initial `equals` moons && n /= 0)
  then (moons, n)
  else
    let nextMoons = simulationStepY moons
    in
      runY' initial nextMoons (n+1)
  where
    equals ms1 ms2 =
      and (map moonEquals (zip ms1 ms2))
    moonEquals (m1,m2) =
      (yPos m1 == yPos m2) &&
      (yVel m1 == yVel m2)

runSimulationNStepsZ :: Int -> [Moon] -> [Moon]
runSimulationNStepsZ n moons =
  let step = simulationStepZ moons
  in
    if n == 0 then moons else runSimulationNStepsZ (n-1) step


simulationStepZ :: [Moon] -> [Moon]
simulationStepZ moons =
  let calcVels = map (\m -> calculateVelocityZ m moons) moons
      calcPoss = map calculatePositionZ calcVels
  in
    calcPoss

calculateVelocityZ :: Moon -> [Moon] -> Moon
calculateVelocityZ moon moons =
  let otherMoons = filter (\m -> m /= moon) moons
      deltaVelZ = sum $ map (\m -> compareField zPos m moon) otherMoons
  in
    moon { zVel = (zVel moon) + deltaVelZ
         }

  where compareField f a b =
                  if (f a) > (f b)
                  then 1
                  else if (f a) < (f b)
                       then -1
                       else 0

calculatePositionZ :: Moon -> Moon
calculatePositionZ m =
  m { zPos = (zPos m) + (zVel m)
    }

runSimulationUntilRepeatZ :: [Moon] -> Int
runSimulationUntilRepeatZ moons =
  let initialMoons = moons
      result = runZ' initialMoons moons 0
  in
    snd result

runZ' :: [Moon] -> [Moon] -> Int -> ([Moon], Int)
runZ' initial moons n =
  if(initial `equals` moons && n /= 0)
  then (moons, n)
  else
    let nextMoons = simulationStepZ moons
    in
      runZ' initial nextMoons (n+1)
  where
    equals ms1 ms2 =
      and (map moonEquals (zip ms1 ms2))
    moonEquals (m1,m2) =
      (zPos m1 == zPos m2) &&
      (zVel m1 == zVel m2)

main = do
  putStrLn "hehlo"
  s <- readFile "day12-input.txt"
  let moons = map parseMoon $ lines s
      energy = calculateTotalEnergy $ runSimulationNSteps 1000 moons

  putStrLn $ show energy
  putStrLn $ "Part 2"
  putStrLn $ show $ runSimulationUntilRepeat testMoons1

  putStrLn $ "Part 2"
  printList $ runSimulationNStepsX 2772 testMoons1

  putStrLn $ "Part 2 Xs"
  putStrLn $ show $ runSimulationUntilRepeatX moons

  putStrLn $ "Part 2 Ys"
  putStrLn $ show $ runSimulationUntilRepeatY moons

  putStrLn $ "Part 2 Zs"
  putStrLn $ show $ runSimulationUntilRepeatZ moons

printList :: (Show a) => [a] -> IO ()
printList = mapM_  (putStrLn . show)
