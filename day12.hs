import Data.List.Split

testInput1 =
     "<x=-1, y=0, z=2>"
  ++ "\n<x=2, y=-10, z=-7>"
  ++ "\n<x=4, y=-8, z=8>"
  ++ "\n<x=3, y=5, z=-1>"
  ++ "\n"

data Moon = Moon { xPos::Int
                 , yPos::Int
                 , zPos::Int
                 , xVel::Int
                 , yVel::Int
                 , zVel::Int
                 } deriving Show

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
testMoons = map parseMoon $ lines testInput1

testPairs :: [Moon] -> [(Moon,Moon)]
testPairs z =
  if length z == 2
  then [(head z, last z)]
  else
    [(head z, i) | i<-tail z ] ++ (testPairs $ tail z)


main = do
  putStrLn testInput1

  putStrLn "hehlo"
  putStrLn $ concat $ map ((++ "\n") . show ) $ testPairs testMoons
