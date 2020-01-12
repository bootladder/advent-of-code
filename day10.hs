
testAsteroids1 =
  ".#..#\n"
  ++ ".....\n"
  ++ "#####\n"
  ++ "....#\n"
  ++ "...##\n"


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

main :: IO ()
main = do
  s <- readFile "day10-input.txt"

  putStrLn $ testAsteroids1
