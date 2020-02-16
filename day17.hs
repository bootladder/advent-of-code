import Intcode
import Data.Char
import Data.Array

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

    return $ dropWhile (== '\n') $ reverse scaffoldingMapString  --remove newlines

main :: IO ()
main = do
  scaffoldingMapString <- getScaffoldingMapString
  let
    scaffoldingMapGrid = stringToGrid scaffoldingMapString

  putStrLn $ scaffoldingMapString
  putStrLn $ show scaffoldingMapGrid
  putStrLn $ show $ bounds scaffoldingMapGrid
  putStrLn $ gridToString scaffoldingMapGrid
  putStrLn "hello"
