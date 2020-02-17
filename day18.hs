import Data.Array
import Data.Char

type Grid = Array (Int,Int) Char

data Key = Key { keyName :: Char
               , keyPos :: (Int,Int)
               } deriving Show

data Door = Door { doorName :: Char
                 , doorPos :: (Int,Int)
                 } deriving Show

data World = World { grid :: Grid
                   , keyDistances :: [(Key,Int)]
                   , keys :: [Key]
                   , pos :: (Int,Int)
                   , cost :: Int
                   }

data Direction = Direction | North | South | East | West deriving (Enum, Eq)

-- the input string is printed top to bottom
-- so (0,0) is the top left, (X,Y) is bottom right
stringToGrid :: String -> Grid
stringToGrid str =
  let
    ySize = length $ lines str
    xSize = length $ head $ lines str

    getRowIndices y =
      let rowValues = (lines str) !! y
      in map (\x -> ((x,y), rowValues !! x) ) [0..xSize - 1]

    indices = concat $ map getRowIndices [0..ySize - 1]
  in
    array (((0),(0)),(xSize-1,ySize-1)) $ indices

gridToString :: Grid -> String
gridToString grid =
  let (xSize,ySize) = snd $ bounds grid
      showRow y = map (\x -> grid ! (x,y) ) [0..xSize]
  in
    concat $ map (\y -> (++) (showRow y) "\n") [0..ySize]


readGridFromFile :: String -> IO (Grid)
readGridFromFile filename =
  do
    inputString <- readFile filename >>= (pure . init)
    return $ stringToGrid inputString

getKeys :: Grid -> [Key]
getKeys grid =
  let
    keyIndices = filter (\index -> isKeyLocation grid index) $ indices grid
  in map (\i -> Key {keyPos = i, keyName = grid ! i}) keyIndices

isKeyLocation :: Grid -> (Int,Int) -> Bool
isKeyLocation grid (x,y) =
  let (xMax, yMax) = snd $ bounds grid
  in
    if x == 0 || y == 0 || x == xMax || y == yMax
    then False
    else
      if (isLower $ grid ! (x,y))
      then True
      else False

getDoors :: Grid -> [Door]
getDoors grid =
  let
    doorIndices = filter (\index -> isDoorLocation grid index) $ indices grid
  in map (\i -> Door {doorPos = i, doorName = grid ! i}) doorIndices

isDoorLocation :: Grid -> (Int,Int) -> Bool
isDoorLocation grid (x,y) =
  let (xMax, yMax) = snd $ bounds grid
  in
    if x == 0 || y == 0 || x == xMax || y == yMax
    then False
    else
      if (isUpper $ grid ! (x,y))
      then True
      else False

getKeyDistances :: Grid -> (Int,Int) -> Direction -> Int -> [(Key, Int)]
getKeyDistances grid pos direction distance =
  let
    dirs = filter (/= direction) $ availableDirections grid pos
  in
    []

availableDirections :: Grid -> (Int,Int) -> [Direction]
availableDirections grid pos =
  filter (isAvailableDirection grid pos) [North,South,East,West]

isAvailableDirection :: Grid -> (Int,Int) -> Direction -> Bool
isAvailableDirection grid (x,y) North = grid ! (x,y-1) == '.'
isAvailableDirection grid (x,y) South = grid ! (x,y+1) == '.'
isAvailableDirection grid (x,y) East  = grid ! (x+1,y) == '.'
isAvailableDirection grid (x,y) West  = grid ! (x-1,y) == '.'


main = do

  grid <- readGridFromFile "day18-input-1.txt"
  let keys = getKeys grid
  let doors = getDoors grid

  putStrLn $ gridToString grid
  putStrLn $ show keys
  putStrLn $ show doors
  putStrLn "Hello"
