import Intcode
import Data.Array
import Data.List.Split
import Debug.Trace
import Control.Concurrent.Thread.Delay

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

  in (unlines rows) ++ "\n" ++ strBall ++ "\n" ++ strPaddle ++ "\n" ++ strScore

  where rowN n = concat $ map showCell [(board game) ! (x,n) | x <- [0..boardSizeCols]]
        showCell num =
          case num of
            0 -> " "
            1 -> "x"
            2 -> "o"
            3 -> "_"
            4 -> "O"
            _ -> "?"

        strBall = "Ball: " ++ (show $ ballPosX game)
        strPaddle = "Paddle: " ++ (show $ paddlePosX game)
        strScore = "Score: " ++ (show $ score game)

applyOutputInstructionsToGame :: Game -> [[Int]] -> Game
applyOutputInstructionsToGame game instructions =
  foldl (\accGame instruction ->
           -- score
           if instruction !! 0 == -1 && instruction !! 1 == 0
           then accGame {score = instruction !! 2}
           else
             accGame { board = (board accGame) //
                               [((instruction !! 0, instruction !! 1), instruction !! 2)]

                     , ballPosX = if instruction !! 2 == 4
                                  then instruction !! 0
                                  else (ballPosX accGame)

                     , paddlePosX = if instruction !! 2 == 3
                                  then instruction !! 0
                                  else (paddlePosX accGame)
                     }

           ) game instructions

blankGame :: Game
blankGame = Game { score = 0
                   , board = gameBoard
                   , ballPosX = 0
                   , paddlePosX = 0
                   }

playGame :: Game -> ComputerState -> IO (Game,ComputerState)
playGame game cs =
  let
    joystickInput = if (paddlePosX game) < (ballPosX game)
                    then (1)
                    else if (paddlePosX game) > (ballPosX game)
                         then (-1)
                         else 0

    csNext = runProgram $ cs { inputBuffer = [joystickInput]
                             , outputBuffer = []}

    gameNext = applyOutputInstructionsToGame
               game
               (chunksOf 3 (outputBuffer csNext))

  in
    if (status cs == HALTED)
    then pure (gameNext, csNext)
    else
      do
        putStr "\ESC[2J"
        putStrLn (renderGame gameNext)
        delay 10000
        playGame gameNext csNext


main :: IO ()
main = do
  main1
  putStrLn "Part 2"

  s <- readFile "day13-input.txt"
  let inputProgram = listArray (0,4000) $ (map read $ splitOn "," s) ++ (repeat 0)


  let programWithQuarter = inputProgram // [(0, 2)]
      csInitial = runProgram $ defaultComputerState {program=programWithQuarter}
      initialOutputBuffer = outputBuffer csInitial
      initialOutputInstructions = chunksOf 3 initialOutputBuffer

      gameAfterFirstOutput = applyOutputInstructionsToGame
                             blankGame
                             initialOutputInstructions

      -- by inspection
      initialPaddleXPos = 22 :: Int
      initialBallXPos = 20 :: Int
      gameWithInitialPaddleAndBallPos =
        gameAfterFirstOutput { ballPosX = initialBallXPos
                             , paddlePosX = initialPaddleXPos
                             }


  putStrLn $ renderGame gameWithInitialPaddleAndBallPos


  putStrLn "Consuming the initial output buffer"

  let csReadyForFirstInput = csInitial {outputBuffer=[]}

  (endGame, endCs) <- playGame gameWithInitialPaddleAndBallPos csReadyForFirstInput

  putStrLn $ renderGame endGame
  putStrLn $ showComputerState endCs
  putStrLn "Done"
