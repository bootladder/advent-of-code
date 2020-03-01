module PracticeSpec where

import Test.Hspec
import Grid
import KeyDoorMazeWorld

main :: IO()
main = hspec $
  before
  (do
      x <- readFile "day18-input-1.txt"
      return (x,"something else")
  ) $ do

  describe "Finds Keys using Input 1 Data" $ do
    it "finds key a and b" $ \(s1,s2) -> do

      grid <- readGridFromFile "day18-input-1.txt"
      let keyNamesAndPositions = findKeyNamesAndPositions grid

      (('a', (7,1)) `elem` keyNamesAndPositions)  `shouldBe` True
      (('b', (1,1)) `elem` keyNamesAndPositions)  `shouldBe` True

  describe "Finds Keys using Input 2 Data" $ do
    it "finds key f,d,e" $ \(s1,s2) -> do

      grid <- readGridFromFile "day18-input-2.txt"
      let keyNamesAndPositions = findKeyNamesAndPositions grid

      (('f', (1,1)) `elem` keyNamesAndPositions)  `shouldBe` True
      (('d', (1,3)) `elem` keyNamesAndPositions)  `shouldBe` True
      (('e', (7,1)) `elem` keyNamesAndPositions)  `shouldBe` True


  describe "Finds Key Distances and Doors Input 1" $ do
    it "@ to b is 4 with door A and @ to a is 2 with no doors" $
      \(s1,s2) -> do
      grid <- readGridFromFile "day18-input-1.txt"

      let keyDistancesAndDoors =
            findKeyDistancesAndDoorsFromPosition grid (5,1)

      (('b',4,['A']) `elem` keyDistancesAndDoors) `shouldBe` True
      (('a',2,[]) `elem` keyDistancesAndDoors) `shouldBe` True

  describe "Finds Key Distances and Doors Input 2 from e" $ do
    it "e to f is 6 with doors [E,D] and e to c is 14 with doors [C,A,B]" $ \(s1,s2) -> do
      grid <- readGridFromFile "day18-input-2.txt"

      putStrLn $ gridToString grid

      let keyDistancesAndDoors =
            findKeyDistancesAndDoorsFromPosition grid (7,1)

      putStrLn $ show keyDistancesAndDoors

      (('f',6,['E','D']) `elem` keyDistancesAndDoors) `shouldBe` True
      (('c',14,['C','A','B']) `elem` keyDistancesAndDoors) `shouldBe` True

