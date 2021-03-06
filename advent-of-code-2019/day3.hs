import Data.List.Split

type Segment = ((Int,Int), (Int,Int))

main = do
  s <- readFile "day3-input.txt"
  let path1String = (splitOn "\n" s !! 0)
  let path2String = (splitOn "\n" s !! 1)
  let path1Codes = splitOn "," path1String :: [String]
  let path2Codes = splitOn "," path2String :: [String]
  let path1 = map parsePathCode path1Codes :: [(Char,Int)]
  let path2 = map parsePathCode path2Codes :: [(Char,Int)]

  let segments1 = pathToSegments path1 :: [Segment]
  let segments2 = pathToSegments path2 :: [Segment]

  printSegments segments1
  printSegments segments2

  putStrLn $  (++) "Num segments 1: " $ show $ length segments1
  putStrLn $  (++) "Num segments 2: " $ show $ length segments2

  let verticalSegments1 = getVerticalSegments segments1 :: [Segment]
  let verticalSegments2 = getVerticalSegments segments2 :: [Segment]
  let horizontalSegments1 = getHorizontalSegments segments1 :: [Segment]
  let horizontalSegments2 = getHorizontalSegments segments2 :: [Segment]

  putStrLn $ (++) "Num vert segs 1: " $ show $ length verticalSegments1
  putStrLn $ (++) "Num vert segs 2: " $ show $ length verticalSegments2
  putStrLn $ (++) "Num horz segs 1: " $ show $ length horizontalSegments1
  putStrLn $ (++) "Num horz segs 2: " $ show $ length horizontalSegments2

  let possibleIntersections1 = [findIntersection v h |
                        v <- verticalSegments1,
                        h <- horizontalSegments2]

  let possibleIntersections2 = [findIntersection v h |
                        v <- verticalSegments2,
                        h <- horizontalSegments1]

  putStrLn $ "\n INTERSECTIONS:::"
  let intersections1 = filter (\mi -> case mi of
                                  Nothing -> False
                                  Just _ -> True) possibleIntersections1
  let intersections2 = filter (\mi -> case mi of
                                  Nothing -> False
                                  Just _ -> True) possibleIntersections2

  let distances1 = map (\x -> case x of
                          Just (x,y) -> abs(x)+ abs(y)
                          Nothing -> 0) (intersections1 ++ intersections2)
  putStrLn $ show intersections1
  putStrLn $ show intersections2
  putStrLn $ show distances1

  putStrLn $ ("The minimum is   ....   " ++ (show $ minimum distances1))


  putStrLn $ "\n\nPART 2"
  let allIntersections =
        map (\x -> case x of
                     Just (x,y) -> (x,y)
                     Nothing -> (0,0)) (intersections1 ++ intersections2)

  let path1Lengths = map (lengthToIntersection 0 segments1) allIntersections
  let path2Lengths = map (lengthToIntersection 0 segments2) allIntersections
  
  putStrLn $ "Path Lengths"
  putStrLn $ show path1Lengths
  putStrLn $ show path2Lengths
  putStrLn $ "Sum of Path Lengths"
  putStrLn $ show $ map (\(a,b) -> a+b) $ zip path1Lengths path2Lengths
  putStrLn $ "Minimum is"
  putStrLn $ show $ minimum $ map (\(a,b) -> a+b) $ zip path1Lengths path2Lengths
  putStrLn $ "DONE"


lengthToIntersection :: Int -> [Segment] -> (Int,Int) -> Int
lengthToIntersection acc segments intersection =
  if intersection `isInBetween` (head segments)
  then acc + (segmentLength ((fst $ head segments), intersection))
  else acc +
       (lengthToIntersection
         (segmentLength $ head segments)
         (tail segments)
         intersection)

isInBetween :: (Int,Int) -> Segment -> Bool
isInBetween p seg = 
  if (p `hasSameY` seg) && (p `hasXBetween` seg) ||
     (p `hasSameX` seg) && (p `hasYBetween` seg)
  then True
  else False

hasSameY :: (Int,Int) -> Segment -> Bool
hasSameY p seg = (snd p) == (snd $ fst seg) && (snd p) == (snd $ snd seg)

hasSameX :: (Int,Int) -> Segment -> Bool
hasSameX p seg = (fst p) == (fst $ fst seg) && (fst p) == (fst $ snd seg)

hasXBetween :: (Int,Int) -> Segment -> Bool
hasXBetween p seg = 
      let horizontalLeft  = min (fst $ fst seg) (fst $ snd seg)
          horizontalRight = max (fst $ fst seg) (fst $ snd seg)
          x = fst p
      in
        if x >= horizontalLeft && x <= horizontalRight
        then True
        else False
  
hasYBetween :: (Int,Int) -> Segment -> Bool
hasYBetween p seg = 
      let verticalTop = max (snd $ fst seg) (snd $ snd seg)
          verticalBot = min (snd $ fst seg) (snd $ snd seg)
          y = snd p
      in
        if y >= verticalBot && y <= verticalTop
        then True
        else False
  
segmentLength :: Segment -> Int
segmentLength segment = abs $ ((fst $ fst segment) - (fst $ snd segment))
                        + ((snd $ fst segment) - (snd $ snd segment))
  
printSegments :: [Segment] -> IO ()
printSegments segments =
  mapM_ (\seg -> putStrLn (
            (show $ fst seg) ++ " , " ++ (show $ snd seg) ++ "\n"))
  segments

findIntersection :: Segment -> Segment -> Maybe (Int,Int)
findIntersection vseg hseg =
  let verticalX = fst $ fst vseg
      verticalTop = max (snd $ fst vseg) (snd $ snd vseg)
      verticalBot = min (snd $ fst vseg) (snd $ snd vseg)
      horizontalY = snd $ fst hseg
      horizontalLeft = min (fst $ fst hseg) (fst $ snd hseg)
      horizontalRight = max (fst $ fst hseg) (fst $ snd hseg)
  
  in
    if verticalTop >= horizontalY && verticalBot <= horizontalY
       && horizontalLeft <= verticalX && horizontalRight >= verticalX
    then Just (verticalX, horizontalY)
    else Nothing

getVerticalSegments :: [Segment] -> [Segment]
getVerticalSegments = filter isVerticalSegment
  where isVerticalSegment segment =
          (fst $ fst segment) == (fst $ snd segment)
          && (snd $ fst segment) /= (snd $ snd segment)

getHorizontalSegments :: [Segment] -> [Segment]
getHorizontalSegments = filter isHorizontalSegment
  where isHorizontalSegment segment =
          (snd $ fst segment) == (snd $ snd segment)
          && (fst $ fst segment) /= (fst $ snd segment)
          

pathToSegments :: [(Char,Int)] -> [Segment]
pathToSegments path = foldl
                 (\segments jump ->
                     segments ++ [nextSegment jump (last segments)])
                 [((0,0),(0,0))] path
  
nextSegment :: (Char,Int) -> ((Int,Int),(Int,Int)) -> ((Int,Int),(Int,Int))
nextSegment jump lastPath =
  (snd lastPath, vectorAdd (snd lastPath) (fst jump) (snd jump) )

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

