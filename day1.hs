
main = do
  s <- readFile "day1-input.txt"
  let nonEmptyLines = filter (\x -> length x > 0) (lines s)
  let masses = map read $ nonEmptyLines :: [Int]
  mapM_ (putStrLn . show) masses

  let requiredFuels = map getRequiredFuel masses
  putStrLn $ show $ sum requiredFuels

  -- part 2
  let requiredFuels = map getRequiredFuelIncludingFuel masses
  putStrLn $ show $ sum requiredFuels


getRequiredFuel :: Int -> Int
getRequiredFuel x = (floor ((fromIntegral x)/3)) - 2

getRequiredFuelIncludingFuel  :: Int -> Int
getRequiredFuelIncludingFuel x =
  let requiredFuel = getRequiredFuel x
  in
    if requiredFuel > 0
    then requiredFuel + (getRequiredFuelIncludingFuel requiredFuel)
    else 0
