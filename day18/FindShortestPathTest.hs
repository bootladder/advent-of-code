module FindShortestPathTest where

import Test.Hspec
import FindShortestPath
import Grid
import FindKeyDistances

main :: IO()
main =
  do
    spec1
    spec2

spec1 =
  hspec $
  before
  (do
      grid <- readGridFromFile "day18-input-2.txt"
      --putStrLn $ gridToString grid
      return grid
  ) $ do

  describe "Remove Starting Key, Input 2 Data" $ do
    it "Should Remove Key a" $ \grid -> do
      let keys = getAllKeysDistancesAndDoors grid
          keysWithOneRemoved = removeKeyByName 'a' keys

      let matches = filter (\k -> keyName k == 'a') keysWithOneRemoved

      length matches `shouldBe` 0

    it "Should remove a from otherkeylist" $ \grid -> do
      let keys = getAllKeysDistancesAndDoors grid
          keysWithOneRemoved = removeKeyByName 'a' keys

      let matches = filter
                    (\(n,_,_) -> n == 'a')
                    (otherKeyDistancesAndDoors (head keysWithOneRemoved))

      length matches `shouldBe` 0


  describe "Get Key By Name Input 2 Data" $ do
    it "" $ \grid -> do
      let keys = getAllKeysDistancesAndDoors grid
          keyA = getKeyByName 'a' keys

      keyName keyA `shouldBe` 'a'


  describe "Remove Door from DoorList in Keys Input 2 Data" $ do
    it "" $ \grid -> do
      let keys = getAllKeysDistancesAndDoors grid
          keysWithDoorRemoved = removeDoorNameFromKeys 'B' keys

      let
          keyC = getKeyByName 'c' keysWithDoorRemoved
          matches = filter
                    (\(_,_,doors) -> 'B' `elem` doors)
                    (otherKeyDistancesAndDoors keyC)

      --putStrLn $ show keyC
      length matches `shouldBe` 0


spec2 =
  hspec $
  before
  (do
      grid <- readGridFromFile "day18-input-2.txt"
      putStrLn $ gridToString grid
      return grid
  ) $ do

  describe "Finds Shortest Path using Input 2 Data" $ do
    it "" $ \grid -> do

      let keys = getAllKeysDistancesAndDoors grid
      let allPossiblePaths = findAllPossiblePaths keys

      --putStrLn $ show keys
      putStrLn $ "All possible Paths: " ++ show allPossiblePaths
