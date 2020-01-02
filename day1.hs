
main = do
  s <- readFile "day1-input.txt"
  let nonEmptyLines = filter (\x -> length x > 0) (lines s)
  let masses = map read $ nonEmptyLines :: [Float]
  mapM_ (putStrLn . show) masses

  let requiredFuels = map getRequiredFuel masses
  putStrLn $ show $ sum requiredFuels


getRequiredFuel :: Float -> Int
getRequiredFuel x = (floor (x/3)) - 2
