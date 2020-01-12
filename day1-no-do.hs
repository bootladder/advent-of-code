main =
  readFile "day1-input.txt"
  >>= return . getMassesFromInput
  >>= return . (map getRequiredFuel)


getMassesFromInput :: String -> [Int]
getMassesFromInput s =
  let ls = filter (\x -> length x > 0) (lines s)
  in map read ls

getRequiredFuel :: Int -> Int
getRequiredFuel x = (floor ((fromIntegral x)/3)) - 2

getRequiredFuelIncludingFuel  :: Int -> Int
getRequiredFuelIncludingFuel x =
  let requiredFuel = getRequiredFuel x
  in
    if requiredFuel > 0
    then requiredFuel + (getRequiredFuelIncludingFuel requiredFuel)
    else 0
