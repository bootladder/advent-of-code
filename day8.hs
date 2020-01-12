import Data.List.Split
import Data.List

data Image = Image { width :: Int
                   , height :: Int
                   , contents :: [Int]
                   }deriving Show

testImage :: Image
testImage = Image 3 2 $ parseImage "123456789012"

parseImage :: String -> [Int]
parseImage s = map (\c -> read [c]) s :: [Int]

image2Layers :: Image -> [[Int]]
image2Layers image = chunksOf ((width image) * (height image)) (contents image)

part1 :: Image -> Int
part1 image =
  let layers = image2Layers image

      numZerosOfLayers = map (length . filter (== 0))  layers
      numOnesOfLayers = map (length . filter (== 1))  layers
      numTwosOfLayers = map (length . filter (== 2))  layers
      zipped = zip3 numZerosOfLayers numOnesOfLayers numTwosOfLayers

      fewestZeros = minimumBy (\(zero,_,_) (a,_,_) -> zero `compare` a) zipped
      ones (_,b,_) = b
      twos (_,_,c) = c
      answer = (ones fewestZeros) * (twos fewestZeros)
  in
    answer

main :: IO ()
main = do
  s <- (readFile "day8-input.txt") >>= (pure . reverse . dropWhile (=='\n') . reverse)

  let inputImage = Image 25 6 $ parseImage s

  putStrLn $ "The answer is " ++ (show $ part1 inputImage)

  putStrLn $ "\n\npart 2"
