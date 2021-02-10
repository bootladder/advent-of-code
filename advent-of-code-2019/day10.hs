import Debug.Trace
import Data.List
import Data.Complex

myTrace comment subject = trace (comment ++ show subject) subject

testAsteroids1 =
  ".#..#\n"
  ++ ".....\n"
  ++ "#####\n"
  ++ "....#\n"
  ++ "...##\n"

testAsteroids2 =
  "......#.#.\n"
  ++ "#..#.#....\n"
  ++ "..#######.\n"
  ++ ".#.#.###..\n"
  ++ ".#..#.....\n"
  ++ "..#....#.#\n"
  ++ "#..#....#.\n"
  ++ ".##.#..###\n"
  ++ "##...#..#.\n"
  ++ ".#....####\n"

testAsteroids3 =
   "#.#...#.#.\n"
  ++ ".###....#.\n"
  ++ ".#....#...\n"
  ++ "##.#.#.#.#\n"
  ++ "....#.#.#.\n"
  ++ ".##..###.#\n"
  ++ "..#...##..\n"
  ++ "..##....##\n"
  ++ "......#...\n"
  ++ ".####.###.\n"


testAsteroidCoordinates1 = parseAllAsteroidCoordinates testAsteroids1
testAsteroidCoordinates2 = parseAllAsteroidCoordinates testAsteroids2
testAsteroidCoordinates3 = parseAllAsteroidCoordinates testAsteroids3


parseAllAsteroidCoordinates :: String -> [(Int,Int)]
parseAllAsteroidCoordinates s =
  let
    rows = lines s
    indexedRows = zip [0..] rows :: [(Int, String)]

    z = foldl
        (\acc indexedRow ->
           let rowIndex = fst indexedRow
               row = snd indexedRow :: String
               indexedColumns = zip [0..] row :: [(Int,Char)]
               indexedColumnsWithAsteroids = filter
                                             (\(_,c) -> c == '#')
                                             indexedColumns :: [(Int,Char)]

               -- x,y coordinates is col,row coordinates
               asteroidCoordinates = map
                                     (\col -> (col, rowIndex))
                                     (map fst indexedColumnsWithAsteroids) :: [(Int,Int)]

               in acc ++ asteroidCoordinates
        )
        []
        indexedRows
  in
    z


countAsteroidsInLineOfSight :: [(Int,Int)] -> (Int,Int) -> Int
countAsteroidsInLineOfSight allAsteroids asteroid =
  let count = length $
              filter
              (areCoordinatesInLineOfSight allAsteroids asteroid)
              allAsteroids
  in
    count - 1  -- remove 1 extra due to inclusive match

calcSlope :: (Int,Int) -> (Int,Int) -> Float
calcSlope (x1',y1') (x2',y2') =
  let x1:y1:x2:y2:_ = map fromIntegral [x1',y1',x2',y2']
  in
    ((y2 - y1) / (x2 - x1)) :: Float

isBetween :: (Int,Int) -> (Int,Int) -> (Int,Int) -> Bool
isBetween (x0,y0) (xA,yA) (xB,yB) =
  if x0 <= max xA xB && x0 >= min xA xB &&
     y0 <= max yA yB && y0 >= min yA yB
  then True
  else False


areCoordinatesInLineOfSight :: [(Int,Int)] -> (Int,Int) -> (Int,Int) -> Bool
areCoordinatesInLineOfSight allAsteroids a1 a2 =

  let slope = calcSlope a1 a2

      asteroidsInBetween =
        filter (\a -> isBetween a a1 a2) allAsteroids

      asteroidsInLine =
        filter (\a -> slope == calcSlope a1 a) asteroidsInBetween

  in
    -- self will miss due to slope NaN, other end will hit
    if length asteroidsInLine > 1
    then False
    else True


main1 :: IO ()
main1 = do
  s <- readFile "day10-input.txt"

  putStrLn $ testAsteroids1

  let asteroids = parseAllAsteroidCoordinates s

  let counts = map
               (\a -> (a, countAsteroidsInLineOfSight asteroids a))
               asteroids

      bestAsteroid = maximumBy
                     (\((_,_),a) ((_,_),b) -> compare a b)
                     counts

  putStrLn $ show bestAsteroid

  putStrLn $ "\nPART 2\n"

  ------------------------------------------

testZappingField1 :: String
testZappingField1 =
     ".#....#####...#..\n"
  ++ "##...##.#####..##\n"
  ++ "##...#...#.#####.\n"
  ++ "..#.....X...###..\n"
  ++ "..#.#.....#....##\n"

testZappingField2 :: String
testZappingField2 =
     ".#..##.###...#######\n"
  ++ "##.############..##.\n"
  ++ ".#.######.########.#\n"
  ++ ".###.#######.####.#.\n"
  ++ "#####.##.#.##.###.##\n"
  ++ "..#####..#.#########\n"
  ++ "####################\n"
  ++ "#.####....###.#.#.##\n"
  ++ "##.#################\n"
  ++ "#####.##.###..####..\n"
  ++ "..######..##.#######\n"
  ++ "####.##.####...##..#\n"
  ++ ".#####..#.######.###\n"
  ++ "##...#.##########...\n"
  ++ "#.##########.#######\n"
  ++ ".####.#.###.###.#.##\n"
  ++ "....##.##.###..#####\n"
  ++ ".#.#.###########.###\n"
  ++ "#.#.#.#####.####.###\n"
  ++ "###.##.####.##.#..##\n"


testZappingAsteroidCoordinates :: [(Int,Int)]
testZappingAsteroidCoordinates =
  parseAllAsteroidCoordinates testZappingField2

laserGunCoordinates :: (Int,Int)
laserGunCoordinates = (11,13)

testZappingAsteroidCoordinatesSelfRemoved :: [(Int,Int)]
testZappingAsteroidCoordinatesSelfRemoved =
  filter (\(x,y)-> not (x == 11 && y == 13)) testZappingAsteroidCoordinates

testZappingAsteroidDescriptors :: [AsteroidDescriptor]
testZappingAsteroidDescriptors =
  map (makeAsteroidDescriptor laserGunCoordinates) testZappingAsteroidCoordinatesSelfRemoved 

makeAsteroidDescriptor :: (Int,Int) -> (Int,Int) -> AsteroidDescriptor
makeAsteroidDescriptor (xLaser,yLaser) (xAsteroid, yAsteroid) =
  AsteroidDescriptor { x_absolute = xAsteroid
                     , y_absolute = yAsteroid
                     , x_relative = x_rel
                     , y_relative = y_rel
                     , mag_relative = magnitude z_rel
                     , phase_relative = phase'
                     }
  where
    x_rel = xAsteroid - xLaser
    y_rel = yAsteroid - yLaser
    x = fromIntegral x_rel
    y = fromIntegral y_rel
    z_rel = (x :+ y)
    phase''' = 90.0 + (phase z_rel) * 360.0 / (2*pi)
    phase' = if phase''' < 0.0 then phase''' + 360.0 else phase'''

testSortedZappingAsteroidsByPhase :: [AsteroidDescriptor]
testSortedZappingAsteroidsByPhase =
  sortBy
  (\a1 a2 -> compare (phase_relative a1) (phase_relative a2))
  testZappingAsteroidDescriptors

testZappingAsteroidsGroupedBySortedPhaseSortedByMagnitude :: [[AsteroidDescriptor]]
testZappingAsteroidsGroupedBySortedPhaseSortedByMagnitude =
  let grouped = groupBy
                (\a1 a2 -> phase_relative a1 == phase_relative a2)
                testSortedZappingAsteroidsByPhase
  in
    map (\list -> sortBy (\a1 a2 -> mag_relative a1 `compare` mag_relative a2) list) grouped


zapNAsteroids :: Int ->
                 [[AsteroidDescriptor]] ->
                 ([AsteroidDescriptor],[[AsteroidDescriptor]])
zapNAsteroids n asteroidPhaseGroups =
  zapNAsteroids' n ([],asteroidPhaseGroups) 0

zapNAsteroids' :: Int ->
                  ([AsteroidDescriptor],[[AsteroidDescriptor]]) ->
                  Int ->
                  ([AsteroidDescriptor],[[AsteroidDescriptor]])
zapNAsteroids' n (zappedAsteroids, asteroidPhaseGroups) index =
  --terminate recursion
  if n == 0
  then (zappedAsteroids, asteroidPhaseGroups)
  else
    -- wrap around 360 degrees
    if index == (length asteroidPhaseGroups)
    then zapNAsteroids' (n) (zappedAsteroids, asteroidPhaseGroups) 0
    else
      -- skip empty phase groups
      if (length $ asteroidPhaseGroups !! index) == 0
      then zapNAsteroids' (n) (zappedAsteroids, asteroidPhaseGroups) (index+1)
      else
        let
          zapped = head (asteroidPhaseGroups !! index)
          remaining =
            (take index asteroidPhaseGroups)
            ++ [(drop 1 (asteroidPhaseGroups !! index))]
            ++ (drop (index+1) asteroidPhaseGroups)
      in
        zapNAsteroids' (n-1) (zappedAsteroids++[zapped] , remaining) (index+1)
    
    

type Magnitude = Float
type Phase     = Float

data AsteroidDescriptor =
  AsteroidDescriptor { x_absolute :: Int
                     , y_absolute :: Int
                     , x_relative :: Int
                     , y_relative :: Int
                     , mag_relative :: Float
                     , phase_relative :: Float
                     } deriving Show

-- polar phase is in degrees, with zero being up, going Clockwise
-- rounded
rect2polar :: (Int,Int) -> (Magnitude,Phase)
rect2polar (x',y') =
  let x = fromIntegral x'
      y = fromIntegral y'
      z = (x :+ y)
      phase''' = 90.0 + (phase z) * 360.0 / (2*pi)
      phase' = if phase''' < 0.0 then phase''' + 360.0 else phase'''
  in
    (magnitude z, phase')
  
main :: IO ()
main = do
  --mapM_ (putStrLn . show)  $ testZappingAsteroidsGroupedBySortedPhaseSortedByMagnitude 

  --mapM_ (putStrLn . show ) $ fst $ zapNAsteroids 299 testZappingAsteroidsGroupedBySortedPhaseSortedByMagnitude


  s <- readFile "day10-input.txt"
  let
    mainLaserGunCoordinates = (19,14)
    coords = parseAllAsteroidCoordinates s
    selfFiltered = filter (\(x,y)-> not (x == 19 && y == 14)) coords
    descriptors = map (makeAsteroidDescriptor mainLaserGunCoordinates ) selfFiltered
    sorted =
      sortBy
      (\a1 a2 -> compare (phase_relative a1) (phase_relative a2))
      descriptors
    groupedSorted = let grouped = groupBy
                                  (\a1 a2 -> phase_relative a1 == phase_relative a2)
                                  sorted
                    in
                      map
                      (\list -> sortBy (\a1 a2 -> mag_relative a1 `compare` mag_relative a2) list)
                      grouped

  mapM_ (putStrLn . show ) $ fst $ zapNAsteroids 200 groupedSorted
  print "helllo"
