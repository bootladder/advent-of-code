module FindShortestPath where

import FindKeyDistances
import Data.Char

findAllPossiblePaths :: [Key] -> [Char]
findAllPossiblePaths keys =
  let firstKey = head $ filter (\k -> keyName k == '@') keys
  in
    findAllPossiblePathsStartingAt keys firstKey [] [] 0

findAllPossiblePathsStartingAt :: [Key] ->
                                  Key ->
                                  [Char] ->
                                  [Char] ->
                                  Int ->
                                  [Char]
findAllPossiblePathsStartingAt keys startingKey doorsNamesOpened keyNamesFound distance =
  let
    keysWithStartingKeyRemoved =
      map (\key ->
             key { otherKeyDistancesAndDoors =
                   filter (\(name,_,_) ->
                             name /= (keyName startingKey))
                   (otherKeyDistancesAndDoors key)
                 }
          )
      keys


    keysWithDoorRemoved =
      map (\key ->
             key { otherKeyDistancesAndDoors =
                   map (\(a,b,doors) ->
                          (a,b,
                            (
                              filter (\door ->
                                        (keyName startingKey) /= door)
                              doors
                            ))
                       )
                       (otherKeyDistancesAndDoors key)
                 }
          )
      keysWithStartingKeyRemoved

    possibleNextChoices = filter
                          (\(name, _, doors) ->
                             (doors == []))
                          $
                          otherKeyDistancesAndDoors startingKey

  in
    if length possibleNextChoices == 0
    then
      [keyName startingKey]
    else
      concat $ map (\(name,d,doors) ->
                       findAllPossiblePathsStartingAt
                       keysWithDoorRemoved
                       (head $ filter (\k -> keyName k == name) keysWithStartingKeyRemoved)
                       (doorsNamesOpened ++ [toUpper name])
                       (keyNamesFound ++ [name])
                       (distance + d)
                   )
      possibleNextChoices



