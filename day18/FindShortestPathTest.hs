module FindShortestPathTest where

import Test.Hspec
import FindShortestPath
import Grid
import FindKeyDistances

main :: IO()
main =
  do
    spec1

spec1 =
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

      putStrLn $ show allPossiblePaths

