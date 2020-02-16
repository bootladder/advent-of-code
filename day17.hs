import Intcode
import Data.Char
import Data.Array
import Debug.Trace

-- the input string is printed top to bottom
-- so (0,0) is the top left, (X,Y) is bottom right
stringToGrid :: String -> Array (Int,Int) Char
stringToGrid str =
  let
    ySize = length $ lines str
    xSize = length $ head $ lines str

    getRowIndicies y =
      let rowValues = (lines str) !! y
      in map (\x -> ((x,y), rowValues !! x) ) [0..xSize - 1]

    indicies = concat $ map getRowIndicies [0..ySize - 1]
  in
    array (((0),(0)),(xSize-1,ySize-1)) $ indicies

gridToString :: Array (Int,Int) Char -> String
gridToString grid =
  let (xSize,ySize) = snd $ bounds grid
      showRow y = map (\x -> grid ! (x,y) ) [0..xSize]
  in
    concat $ map (\y -> (++) (showRow y) "\n") [0..ySize]


getScaffoldingMapString :: IO String
getScaffoldingMapString =
  do
    inputString <- readFile "day17-input.txt" >>= (pure . init)

    let
      csInitial = defaultComputerState { program = parseProgram inputString}
      csNext = runProgram csInitial
      scaffoldingMapString = map chr $ outputBuffer csNext

    return $ reverse $ dropWhile (== '\n') $ reverse scaffoldingMapString  --remove newlines

getIntersections :: Array (Int,Int) Char -> [(Int,Int)]
getIntersections grid =
  filter (\index -> isIntersection grid index) $ indices grid

isIntersection :: Array (Int,Int) Char -> (Int,Int) -> Bool
isIntersection grid (x,y) =
  let (xMax, yMax) = snd $ bounds grid
  in
    --trace ("(x,y) = (" ++ (show x) ++ ", " ++ (show y) ++ ")" ++ " = " ++ (show $ grid ! (x,y))) $
    if x == 0 || y == 0 || x == xMax || y == yMax  || (grid ! (x,y) /= '#')
    then False
    else
      if (grid ! (x+1,y)) == '#' &&
         (grid ! (x-1,y)) == '#' &&
         (grid ! (x,y+1)) == '#' &&
         (grid ! (x,y-1)) == '#'
      then True
      else False

main :: IO ()
main = do
  scaffoldingMapString <- getScaffoldingMapString
  let
    scaffoldingMapGrid = stringToGrid scaffoldingMapString

  putStrLn $ scaffoldingMapString
  putStrLn $ show scaffoldingMapGrid
  putStrLn $ show $ bounds scaffoldingMapGrid
  putStrLn $ gridToString scaffoldingMapGrid

  let
    intersections = getIntersections scaffoldingMapGrid
    alignmentParameters = map (\(x,y) -> x*y) intersections
    answer = sum $ map (\(x,y) -> x*y) intersections

  putStrLn $ show intersections
  putStrLn $ show alignmentParameters
  putStrLn $ show answer
  putStrLn "hello"
