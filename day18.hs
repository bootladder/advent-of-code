import Data.Array
import Data.Char
import Data.List
import Debug.Trace

type Grid = Array (Int,Int) Char

data Key = Key { keyName :: Char
               , keyPos :: (Int,Int)
               } deriving (Show, Eq)

data Door = Door { doorName :: Char
                 , doorPos :: (Int,Int)
                 } deriving Show

data World = World { grid :: Grid
                   , keyDistances :: [(Key,Int)]
                   , keys :: [Key]
                   , doors :: [Door]
                   , pos :: (Int,Int)
                   , cost :: Int
                   }

instance Show World where
  show world =
    concat [ "WORLD\n"
           , (gridToString $ grid world)
           , "Pos: " ++ (show $ pos world) ++ "\n"
           , "Keys: " ++ (show $ keys world) ++ "\n"
           , "Cost: " ++ (show $ cost world) ++ "\n"
           ]

data Direction = Direction | North | South | East | West | Nowhere deriving (Enum, Eq, Show)

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

getPosition :: Grid -> (Int,Int)
getPosition grid =
  head $ filter (\index -> isPositionMarkerLocation grid index) $ indices grid
  where
    isPositionMarkerLocation grid index =
      grid ! index == '@'

getKeyDistances :: World -> [(Key, Int)]
getKeyDistances world =
  let
    initialDirection = Nowhere
    initialDistance = 0
    initialPosition = pos world
  in
    getKeyDistances'' (grid world) initialPosition initialDistance initialDirection

getKeyDistances'' :: Grid -> (Int,Int) -> Int -> Direction -> [(Key,Int)]
getKeyDistances'' grid pos distance direction =
  let
    directionsToGo = filter (/= backwards direction) $ availableDirections (grid) (pos)
  in
    --trace (("DirsToGo" ++ show directionsToGo) ++ " , pos " ++ (show pos) ++ " , dir " ++ (show direction))$
    if isKeyLocation grid pos
    then [(Key {keyPos = pos, keyName = grid ! pos}, distance)]
    else
      concat $ map
      (\dir -> getKeyDistances'' grid (move pos dir) (distance+1) dir)
      directionsToGo

move :: (Int,Int) -> Direction -> (Int,Int)
move (x,y) direction =
  case direction of
    North -> (x,y-1)
    South -> (x,y+1)
    East -> (x+1,y)
    West -> (x-1,y)

backwards :: Direction -> Direction
backwards dir = case dir of
    North -> South
    South -> North
    East -> West
    West -> East
    Nowhere -> Nowhere


availableDirections :: Grid -> (Int,Int) -> [Direction]
availableDirections grid pos =
  filter (isAvailableDirection grid pos) [North,South,East,West]

isAvailableDirection :: Grid -> (Int,Int) -> Direction -> Bool
isAvailableDirection grid (x,y) North = grid ! (x,y-1) == '.' || isLower (grid ! (x,y-1))
isAvailableDirection grid (x,y) South = grid ! (x,y+1) == '.' || isLower (grid ! (x,y+1))
isAvailableDirection grid (x,y) East  = grid ! (x+1,y) == '.' || isLower (grid ! (x+1,y))
isAvailableDirection grid (x,y) West  = grid ! (x-1,y) == '.' || isLower (grid ! (x-1,y))


pickupKey :: World -> Key -> Int -> World
pickupKey world key keyCost =
  let matchingDoor = getMatchingDoors world (keyName key)
  in
    case matchingDoor of
      [] ->
        world { grid = (grid world) // [ (keyPos key, '@')
                                       , (pos world, '.')
                                       ]
              , keyDistances = []
              , keys = filter (/= key) (keys world)
              , pos = keyPos key
              , cost = (cost world) + keyCost
              }
      [door] ->
        world { grid = (grid world) // [ (keyPos key, '@')
                                       , (pos world, '.')
                                       , (doorPos $ door, '.')
                                       ]
              , keyDistances = []
              , keys = filter (/= key) (keys world)
              , pos = keyPos key
              , cost = (cost world) + keyCost
              }

getMatchingDoors :: World -> Char -> [Door]
getMatchingDoors world name =
    filter (\door -> (doorName door) == toUpper name) (doors world)

getAllPossibleEndWorlds :: World -> [World]
getAllPossibleEndWorlds world =
  let
    keyDistances = getKeyDistances world
    nextWorlds = map (\(key,distance) -> pickupKey world (key) (distance)) keyDistances
  in
    if isWorldEnded world
    then [world]
    else
      concat $ map getAllPossibleEndWorlds nextWorlds

isWorldEnded :: World -> Bool
isWorldEnded world =
  if (keys world) == []
  then True
  else False

main = do

  grid <- readGridFromFile "day18-input-4.txt"
  let keys = getKeys grid
  let doors = getDoors grid

  putStrLn $ gridToString grid
  --putStrLn $ show keys
  --putStrLn $ show doors

  let initialWorld = World { grid = grid
                           , keyDistances = []
                           , keys = keys
                           , doors = doors
                           , pos = getPosition grid
                           , cost = 0
                           }
      keyDistances = getKeyDistances initialWorld

  putStrLn $ show $ keyDistances

  let nextWorld = pickupKey initialWorld (fst $ head keyDistances) (snd $ head keyDistances)
      nextKeyDistances = getKeyDistances nextWorld

  putStrLn $ show $ nextWorld
  putStrLn $ show $ nextKeyDistances

  let world3 = pickupKey nextWorld (fst $ head nextKeyDistances) (snd $ head nextKeyDistances)
      keyDistances3 = getKeyDistances world3

  putStrLn $ show $ world3
  --done
  --putStrLn $ show $ keyDistances3

  let allPossibleEndWorlds = getAllPossibleEndWorlds initialWorld
      somePossibleEndWorlds = take 10000 allPossibleEndWorlds

  --putStrLn $ show somePossibleEndWorlds

  let shortestWorld = minimumBy
                      (\world1 world2 -> compare (cost world1) (cost world2))
                      somePossibleEndWorlds

  putStrLn $ "The shortest world cost is " ++ show shortestWorld
  putStrLn $ "The number of worlds is : " ++ (show $ length allPossibleEndWorlds)
  putStrLn "Hello"
