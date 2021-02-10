module FindKeyDistances where

import Grid
import Data.Char


data Key = Key { keyName :: Char
               , keyPos :: (Int,Int)
               , otherKeyDistancesAndDoors ::[ (Char, Int, [Char])]
               } deriving (Show, Eq)

data Direction = Direction | North | South | East | West | Nowhere deriving (Enum, Eq, Show)

getAllKeysDistancesAndDoors :: Grid -> [Key]
getAllKeysDistancesAndDoors grid =
  let
    keyNamesAndPositions = findKeyNamesAndPositions grid
    keyPositions = map snd keyNamesAndPositions
    allOfTheKeyDistancesAndDoors =
      map
      (findKeyDistancesAndDoorsFromPosition grid)
      keyPositions

    keys = map
           (\(name,pos) -> Key { keyPos = pos
                               , keyName = name
                               , otherKeyDistancesAndDoors =
                                   findKeyDistancesAndDoorsFromPosition grid pos})
           keyNamesAndPositions
    in keys


findKeyNamesAndPositions :: Grid -> [(Char, Position)]
findKeyNamesAndPositions grid =
  let positionsAndNames = gridFilter grid
                          (\_ val -> isLower val || val == '@')
  in
    map (\(pos,name) -> (name,pos)) positionsAndNames

findKeyDistancesAndDoorsFromPosition :: Grid ->
                                        (Int,Int) ->
                                        [(Char, Int, [Char])]
findKeyDistancesAndDoorsFromPosition grid pos =
  let gridWithPositionRemoved = gridInsertValue grid pos '.'
  in
    recurse'' gridWithPositionRemoved pos 0 [] Nowhere


recurse'' :: Grid ->
             (Int,Int) ->
             Int ->
             [Char] ->
             Direction ->
             [(Char,Int,[Char])]
recurse'' grid pos distance doors direction =
  let
    directionsToGo = filter (/= backwards direction) $ availableDirections (grid) (pos)

    valueAtPosition = gridLookup grid pos
  in
    if isLower valueAtPosition
    then
      let gridWithKeyRemoved = gridInsertValue grid pos '.'
      in
        [(valueAtPosition, distance, doors)]
        ++ recurse'' gridWithKeyRemoved pos distance doors direction

    else if isUpper valueAtPosition
    then
      let gridWithDoorRemoved = gridInsertValue grid pos '.'
      in
        recurse'' gridWithDoorRemoved pos distance (doors ++ [valueAtPosition]) direction
    else
      concat $ map
      (\dir -> recurse'' grid (move pos dir) (distance+1) doors dir)
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
isAvailableDirection grid (x,y) dir =
  let posToCheck = case dir of
        North -> (x,y-1)
        South -> (x,y+1)
        East -> (x+1,y)
        West -> (x-1,y)

      posContents = gridLookup grid posToCheck
  in
    posContents == '.' || isLower posContents ||
    isUpper posContents || posContents == '@'

