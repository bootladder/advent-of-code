module KeyDoorMazeWorld where

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
                   , keys :: [Key]
                   , takenKeyNames :: [Char]
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
           , "KeysTaken: " ++ (show $ takenKeyNames world) ++ "\n"
           , "Cost: " ++ (show $ cost world) ++ "\n"
           ]

data Direction = Direction | North | South | East | West | Nowhere deriving (Enum, Eq, Show)


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
  --trace "\nSTARTING getKeyDistances\n" $
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
    then
      let key = Key {keyPos = pos, keyName = grid ! pos}
      in
        --trace ("Found Key : " ++ (show key)) $
        [(key, distance)]
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
              , keys = filter (/= key) (keys world)
              , pos = keyPos key
              , cost = (cost world) + keyCost
              , takenKeyNames = (takenKeyNames world) ++ [keyName key]
              }
      [door] ->
        world { grid = (grid world) // [ (keyPos key, '@')
                                       , (pos world, '.')
                                       , (doorPos $ door, '.')
                                       ]
              , keys = filter (/= key) (keys world)
              , pos = keyPos key
              , cost = (cost world) + keyCost
              , takenKeyNames = (takenKeyNames world) ++ [keyName key]
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
    if (cost world) > 150
    then []
    else if isWorldEnded world
         then
           trace ("END OF WORLD : " ++ (show world))
           [world]
         else
           concat $ map getAllPossibleEndWorlds nextWorlds

isWorldEnded :: World -> Bool
isWorldEnded world =
  if (keys world) == []
  then True
  else False

