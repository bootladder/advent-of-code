import Data.List.Split
import Data.List

data Image = Image { width :: Int
                   , height :: Int
                   , contents :: [Int]
                   }deriving Show

testImageRaw :: String
testImageRaw = "123456789012"

testImageWidth  = 3
testImageHeight = 2

parseImage :: String -> [Int]
parseImage s = map (\c -> read [c]) s :: [Int]

image2Layers :: Image -> [[Int]]
image2Layers image = chunksOf ((width image) * (height image)) (contents image)
  

main :: IO ()
main = do
  s <- (readFile "day8-input.txt") >>= (pure . reverse . dropWhile (=='\n') . reverse)

  let testImage = Image testImageWidth testImageHeight $ parseImage testImageRaw

  let inputImage = Image 25 6 $ parseImage s

  let layers = image2Layers inputImage

  let numZerosOfLayers = map (length . filter (== 0))  layers
  let numOnesOfLayers = map (length . filter (== 1))  layers
  let numTwosOfLayers = map (length . filter (== 2))  layers
  let zipped = zip3 numZerosOfLayers numOnesOfLayers numTwosOfLayers

  let fewestZeros = minimumBy (\(zero,one,two) (a,b,c) -> zero `compare` a) zipped
  let ones (a,b,c) = b
  let twos (a,b,c) = c
  let answer = (ones fewestZeros) * (twos fewestZeros)

  putStrLn $ show $ fewestZeros
  putStrLn $ "The answer is " ++ (show $ answer)
