module FindShortestPath where

import FindKeyDistances
import Data.Char
import Debug.Trace

getKeyByName :: Char -> [Key] -> Key
getKeyByName name keys =
  head $ filter (\k -> keyName k == name) keys

removeKeyByName :: Char -> [Key] -> [Key]
removeKeyByName name keys =
  let removeKeyFromKeys ks =
        filter (\key -> keyName key /= name) ks

      removeKeyFromOtherKeys otherKeys =
        filter
        (\(name'',_,_) -> name'' /= name)
        otherKeys

      removeKeyFromOtherKeysAllKeys ks =
        map
        (\key -> key {otherKeyDistancesAndDoors =
                         removeKeyFromOtherKeys (otherKeyDistancesAndDoors key)
                     })
        ks

  in
    (removeKeyFromKeys $ removeKeyFromOtherKeysAllKeys keys)

removeDoorNameFromKeys :: Char -> [Key] -> [Key]
removeDoorNameFromKeys name keys =
  let
    removeDoorNameFromKey key =
      key { otherKeyDistancesAndDoors =
              map
              (\(a,b,doors) -> (a,b,filter ((/=) name) doors))
              (otherKeyDistancesAndDoors key)
          }
  in
    map removeDoorNameFromKey keys

findAllPossiblePaths :: [Key] -> [Char]
findAllPossiblePaths keys =
    findAllPossiblePathsStartingAt keys '@' [] [] 0

findAllPossiblePathsStartingAt :: [Key] ->
                                  Char ->
                                  [Char] ->
                                  [Char] ->
                                  Int ->
                                  [Char]
findAllPossiblePathsStartingAt keys startingKeyName doorsNamesOpened keyNamesFound distance =
  let
    keysWithDoorRemoved =
      removeDoorNameFromKeys (toUpper startingKeyName) keys

    keysWithStartingKeyRemoved =
      removeKeyByName startingKeyName keysWithDoorRemoved

    startingKeyWithDoorRemoved = getKeyByName startingKeyName keysWithDoorRemoved


    possibleNextChoices = filter
                          (\(name, _, doors) ->
                             (doors == []))
                          $
                          otherKeyDistancesAndDoors startingKeyWithDoorRemoved

  in
    --trace ("\nCURRENT DISTANCE : " ++ show distance) $
    --trace ("\nSTARTING KEY WITH DOOR REMOVED : " ++ show startingKeyWithDoorRemoved) $
    --trace ("\nKEYS WITH DOOR REMOVED AND STARTING KEY REMOVED: " ++ show keysWithStartingKeyRemoved) $
    --trace ("\nNUM CHOICES : " ++ show possibleNextChoices) $
    if length possibleNextChoices == 0
    then
      keyNamesFound
    else
      concat $
      map (\(name,d,doors) ->
              findAllPossiblePathsStartingAt
              keysWithStartingKeyRemoved
              name
              (doorsNamesOpened ++ [toUpper name])
              (keyNamesFound ++ [name])
              (distance + d)
          )
      possibleNextChoices



