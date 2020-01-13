import Debug.Trace
import Data.List

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


main :: IO ()
main = do
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
