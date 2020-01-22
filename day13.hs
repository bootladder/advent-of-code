import Intcode
import Data.Array
import Data.List.Split

boardSizeRows :: Int
boardSizeRows = 27

boardSizeCols :: Int
boardSizeCols = 50

gameBoard :: Array (Int,Int) Int
gameBoard = listArray ((0,0), (boardSizeCols,boardSizeRows))
            [0 | _ <- [0..boardSizeCols] , _<- [0..boardSizeRows]]

applyOutputInstructionsToGameBoard :: Array (Int,Int) Int -> [[Int]] -> Array (Int,Int) Int
applyOutputInstructionsToGameBoard gb instructions =
  foldl (\accBoard instruction ->

           accBoard // [((instruction !! 0, instruction !! 1), instruction !! 2)]

           ) gb instructions

main1 :: IO ()
main1 = do
  s <- readFile "day13-input.txt"
  let inputProgram = listArray (0,4000) $ (map read $ splitOn "," s) ++ (repeat 0)

      out = runProgram $ defaultComputerState {program=inputProgram}
      outBuf = outputBuffer out
      outputInstructions = chunksOf 3 outBuf

      newBoard = applyOutputInstructionsToGameBoard gameBoard outputInstructions

  putStrLn $ show out
  putStrLn $ show $ length $ filter (\e -> e == 2) (elems newBoard)


data Game = Game { board :: Array (Int,Int) Int
                 , score :: Int
                 , ballPosX :: Int
                 , paddlePosX :: Int
                 } deriving Show

renderGame :: Game -> String
renderGame game =
  let
    rows = map rowN [0..boardSizeRows]

  in unlines rows

  where rowN n = concat $ map showCell [(board game) ! (x,n) | x <- [0..boardSizeCols]]
        showCell num =
          case num of
            0 -> " "
            1 -> "x"
            2 -> "o"
            3 -> "_"
            4 -> "O"
            _ -> "?"

applyOutputInstructionsToGame :: Game -> [[Int]] -> Game
applyOutputInstructionsToGame game instructions =
  foldl (\accGame instruction ->
           if instruction !! 0 == -1 && instruction !! 1 == 0
           then accGame {score = instruction !! 2}
           else
             accGame { board = (board accGame) //
                               [((instruction !! 0, instruction !! 1), instruction !! 2)]
                     }

           ) game instructions

initialGame :: Game
initialGame = Game { score = 0
                   , board = gameBoard
                   , ballPosX = 0
                   , paddlePosX = 0
                   }

main :: IO ()
main = do
  main1
  putStrLn "Part 2"

  s <- readFile "day13-input.txt"
  let inputProgram = listArray (0,4000) $ (map read $ splitOn "," s) ++ (repeat 0)


  let programWithQuarter = inputProgram // [(0, 2)]
      out = runProgram $ defaultComputerState {program=programWithQuarter}
      initialOutputBuffer = outputBuffer out
      initialOutputInstructions = chunksOf 3 initialOutputBuffer

      gameAfterFirstOutput = applyOutputInstructionsToGame
                             initialGame
                             initialOutputInstructions

  putStrLn $ renderGame gameAfterFirstOutput


  putStrLn "Consuming the initial output buffer"

  let csAfterFirstOutput = out {outputBuffer=[]}

      initialPaddleXPos = 22 :: Int
      initialBallXPos = 20 :: Int

      initialJoystickInput = if initialPaddleXPos > initialBallXPos
                             then (-1)
                             else 1


      csAfterFirstInput = runProgram $ csAfterFirstOutput {inputBuffer = take 1 $ repeat $ -1}

      gameAfterFirstInput = applyOutputInstructionsToGame
                            gameAfterFirstOutput
                            (chunksOf 3 (outputBuffer csAfterFirstInput))

  putStrLn $ renderGame gameAfterFirstInput
  putStrLn $ showComputerState csAfterFirstInput
  putStrLn "Done"
