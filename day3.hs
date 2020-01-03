import Data.List.Split

main = do
  s <- readFile "day3-input-example.txt"
  let path1String = (splitOn "\n" s !! 0)
  let path2String = (splitOn "\n" s !! 1)
  let path1Codes = splitOn "," path1String :: [String]
  let path2Codes = splitOn "," path2String :: [String]
  let path1 = map parsePathCode path1Codes :: [(Char,Int)]
  let path2 = map parsePathCode path2Codes :: [(Char,Int)]

  let wire1Locations = convertPathToLocations path1
  let wire2Locations = convertPathToLocations path2

  let checkIntersections =
        [(loc1, loc1 == loc2) |
          loc1 <- wire1Locations,
          loc2 <- wire2Locations
                                  ]
  putStrLn $ show wire1Locations
  putStrLn $ show wire2Locations

  let intersections =
        filter (\(loc,isIntersect) -> isIntersect) checkIntersections

  putStrLn $ show intersections
  putStrLn "\nDONE"

parsePathCode :: String -> (Char, Int)
parsePathCode code =
  (head code, read $ tail code :: Int)

convertPathToLocations :: [(Char, Int)] -> [(Int,Int)]
convertPathToLocations path =
  foldl (\locs (direction, distance) ->
           locs ++
           [vectorAdd (last locs) direction distance]
        ) [(0,0)] path

vectorAdd :: (Int,Int) -> Char -> Int -> (Int,Int)
vectorAdd (x,y) direction distance =
  case direction of
    'L' -> (x - distance, y)
    'R' -> (x + distance, y)
    'U' -> (x , y + distance)
    'D' -> (x , y - distance)

