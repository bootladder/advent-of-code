import Intcode
import Data.Array
import Data.List.Split
import Debug.Trace
import Control.Concurrent.Thread.Delay
import Control.Concurrent
import Control.Concurrent.MVar
import System.IO

data Direction = DUMMY|North|South|West|East deriving (Enum, Show, Eq)
data RobotStatus = Clear|Blocked deriving Show
data InputMode = Automatic|Manual deriving Show

data Maze = Maze { pos :: (Int,Int)
                 , direction :: Direction
                 , backDirection :: Direction
                 , maze :: Array (Int,Int) Int
                 , robotStatus :: RobotStatus
                 , numWalls :: Int
                 , inputMode :: InputMode
                 } deriving Show

showMaze ::Maze -> String
showMaze m =
  let maze' = maze m // [(pos m, 1)]
      showRow row =
        "\n" ++
        map
        (\col -> case (maze') ! (col,row) of
            0 -> ' '
            1 -> '0'
            7 -> 'x'
            _ -> '?'
        )
        [-mazeInitialSize .. mazeInitialSize]

      mazeStats = (show $ pos m) ++ (show $ direction m)
  in
    (concat $ map showRow [-mazeInitialSize .. mazeInitialSize])
    ++ "\n" ++
    (mazeStats)


mazeInitialSize :: Int
mazeInitialSize = 30

mazeInitial :: Maze
mazeInitial = Maze { pos = (0,0)
                   , direction = North
                   , backDirection = South
                   , robotStatus = Clear
                   , inputMode = Automatic
                   , numWalls = 0
                   , maze = listArray (((-mazeInitialSize),(-mazeInitialSize)),(mazeInitialSize,mazeInitialSize)) $ repeat 0
                   }

playMaze :: ComputerState -> Maze -> MVar Char -> IO (ComputerState, Maze)
playMaze cs maze' mvar =
  do
    -- print state
     putStrLn ("\ESC[2J \ESC[H MAZE: " ++ showMaze maze')
     if (numWalls maze') >= 3
       then putStrLn "BLOCKED!!"
       else putStrLn
            $ "not blocked: numWalls = "
            ++ (show $ numWalls maze')

     delay 1000000

     -- Get User Input
     -- go into manual mode if user starts typing
     userInput <-
       case (inputMode maze') of
         Automatic -> tryTakeMVar mvar
         Manual -> fmap Just $ takeMVar mvar

     inputDirection <-
        case userInput of
          Just inputChar ->

            case (inputMode maze') of

              Automatic -> pure North -- go into manual mode
                --playMaze cs maze'{inputMode=Manual} mvar

              Manual -> takeMVar mvar >>= pure . parseDirection

          Nothing ->
            case (inputMode maze') of
              Automatic -> pure $ getAutomaticDirection maze'
              Manual -> takeMVar mvar >>= pure . parseDirection


     let
       mazeWithNewDirection = maze' {direction = inputDirection}

       csNext =
         runProgram cs {inputBuffer = [fromEnum $ (direction mazeWithNewDirection)]}
       result =
         head $ outputBuffer csNext

       in
        case result of
          -- WALL In that Direction
          0 ->
            let newMaze =
                  -- if totally blocked, go backwards by reversing the backDirection
                  if (numWalls maze') >= 3
                  then
                    mazeWithNewDirection
                    { robotStatus = Clear
                    , numWalls = (numWalls maze') + 1
                    , direction = backDirection maze'
                    , backDirection = directionToTheBack $ backDirection maze'
                    }
                  else
                    mazeWithNewDirection
                    { robotStatus = Blocked
                    , numWalls = (numWalls maze') + 1
                    }

            in
              playMaze
              csNext {inputBuffer = [], outputBuffer = []}
              newMaze
              mvar

          -- EMPTY SPACE In that Direction:  Move Forward
          1 ->
            let newMaze =
                  moveForward mazeWithNewDirection
                  { robotStatus = Clear
                  , backDirection = directionToTheBack (direction mazeWithNewDirection)
                  , numWalls = 0
                  }
            in playMaze
               csNext {inputBuffer = [], outputBuffer = []}
               newMaze
               mvar

          -- FOUND THE END
          2 ->
            do
              putStrLn "\n\n\n I FOUND THE END \n\n\n"
              pure (csNext,mazeWithNewDirection)


parseDirection :: Char -> Direction
parseDirection c =
  case c of
    'h' -> West
    'j' -> South
    'k' -> North
    'l' -> East
    _ -> DUMMY

turnRight :: Maze -> Maze
turnRight maze' = maze' { direction = directionToTheRight (direction maze') }

directionToTheRight :: Direction -> Direction
directionToTheRight d =
  case d of
    North -> East
    East -> South
    South -> West
    West -> North
    DUMMY -> DUMMY

directionToTheBack :: Direction -> Direction
directionToTheBack d =
  case d of
    North -> South
    South -> North
    East -> West
    West -> East
    DUMMY -> DUMMY


moveForward :: Maze -> Maze
moveForward maze' =
  let (row,col) = pos maze'
  in
    maze' { pos =
            case direction maze' of
              North -> (row,col+1)
              East ->  (row+1,col)
              South -> (row,col-1)
              West -> (row-1,col)

           , maze = (maze maze') // [(pos maze', 7)]
          }


getAutomaticDirection :: Maze -> Direction
getAutomaticDirection maze' =
  case (robotStatus maze') of
    Clear -> (direction maze')
    Blocked ->
      if (backDirection maze') == directionToTheRight (direction maze')
      then directionToTheRight . directionToTheRight $ (direction maze')
      else directionToTheRight (direction maze')



main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  s <- readFile "day15-input.txt"
  let inputProgram = parseProgram s
      csInitial = defaultComputerState { computerId = "hello id"
                                       , program = inputProgram}

  mvar <- newEmptyMVar
  forkIO $ loop mvar

  (cs,maze') <- playMaze csInitial mazeInitial mvar

  putStrLn $ showMaze maze'
  putStrLn $ showComputerState cs
  putStrLn "Hello"

loop :: MVar Char -> IO ()
loop mvar = do
  c <- getChar; tryPutMVar mvar c; putStrLn ("The Input is : " ++ (show c)) ;
  loop mvar
