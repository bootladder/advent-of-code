module Grid where

import Data.Array

type Position = (Int,Int)
type Grid = Array Position Char


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


gridFilter :: Grid -> (Position -> Char -> Bool) -> [(Position, Char)]
gridFilter grid f =
  let indexMatches =
        filter (\i -> f i (grid ! i)) $ indices grid
  in
    map (\i -> (i, grid ! i)) indexMatches

gridLookup :: Grid -> (Int,Int) -> Char
gridLookup grid pos = grid ! pos

gridInsertValue :: Grid -> Position -> Char -> Grid
gridInsertValue grid pos val = grid // [(pos,val)]
