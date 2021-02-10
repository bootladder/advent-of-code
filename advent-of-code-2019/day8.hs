import Data.List.Split
import Data.List

data EncodedImage = EncodedImage { width :: Int
                   , height :: Int
                   , contents :: [Int]
                   }deriving Show

type DecodedImage = [[Int]]

testImage :: EncodedImage
testImage = EncodedImage 3 2 $ parseImage "123456789012"

parseImage :: String -> [Int]
parseImage s = map (\c -> read [c]) s :: [Int]

image2Layers :: EncodedImage -> [[Int]]
image2Layers image = chunksOf ((width image) * (height image)) (contents image)

part1 :: EncodedImage -> Int
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


  -----------------------------------

renderEncodedImage :: EncodedImage -> String
renderEncodedImage image =
  let
    decodedImage = decodeImage image
    renderedRows = map renderRow decodedImage

  in
    unlines renderedRows

renderRow :: [Int] -> String
renderRow is = map (\i -> if i == 0 then ' ' else 'X') is


decodeImage :: EncodedImage -> DecodedImage
decodeImage e =
  let
    layers = image2Layers e
    layeredPixels = transpose layers
    visibleLayer = map topVisiblePixel layeredPixels
    rowsOfPixels = chunksOf (width e) visibleLayer
  in rowsOfPixels


-- skip pixel values == 2
topVisiblePixel :: [Int] -> Int
topVisiblePixel layersOfPixel = head $ dropWhile (== 2) layersOfPixel

readInputImage :: IO String
readInputImage = (readFile "day8-input.txt") >>= (pure . reverse . dropWhile (=='\n') . reverse)

main :: IO ()
main = do
  s <- readInputImage

  let inputImage = EncodedImage 25 6 $ parseImage s

  putStrLn $ "The answer is " ++ (show $ part1 inputImage)

  putStrLn $ "\n\npart 2"

  putStrLn $ renderEncodedImage inputImage
