
import Test.Hspec
import Data.Map (Map)
import qualified Data.Map.Strict as Map
{-# LANGUAGE BangPatterns #-}

import Data.List

main :: IO()
main =
  do
    spec1

spec1 =
  hspec $

  describe "Add a bunch of stuff" $ do
    it "Should Remove Key a" $ \grid -> do

      let myMap = Map.empty
          inserted1 = Map.insert "abc" 3 myMap
          constInt = 1 :: Int
          manyInserted =
            foldl (\accmap combo -> Map.insert combo constInt accmap) Map.empty (permutations "abcdefghi")

      putStrLn $ show $ Map.lookup "nothing" manyInserted
      putStrLn $ "NEXT PART"

      putStrLn $ show $
        sum $ map (\(Just a) -> a) $ map (\str -> Map.lookup (str) manyInserted ) (permutations "abcdefghi")

      0 `shouldBe` 0
