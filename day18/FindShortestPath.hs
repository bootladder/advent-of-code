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
      removeKeyByName (keyName startingKey) keys

    keysWithDoorRemoved =
      removeDoorNameFromKeys (toUpper $ keyName startingKey) keysWithStartingKeyRemoved

    possibleNextChoices = filter
                          (\(name, _, doors) ->
                             (doors == []))
                          $
                          otherKeyDistancesAndDoors startingKey

  in
    trace ("\nCURRENT DISTANCE : " ++ show distance) $
    trace ("\nSTARTING KEY : " ++ show startingKey) $
    trace ("\nNUM CHOICES : " ++ show possibleNextChoices) $
    if length possibleNextChoices == 0
    then
      [keyName startingKey]
    else
      concat $
      map (\(name,d,doors) ->
              findAllPossiblePathsStartingAt
              keysWithDoorRemoved
              (getKeyByName name keysWithStartingKeyRemoved)
              (doorsNamesOpened ++ [toUpper name])
              (keyNamesFound ++ [name])
              (distance + d)
          )
      possibleNextChoices



