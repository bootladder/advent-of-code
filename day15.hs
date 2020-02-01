import Intcode
import Data.Array
import Data.List.Split
import Debug.Trace
import Control.Concurrent.Thread.Delay
import Control.Concurrent
import Control.Concurrent.MVar
import System.IO

data Direction = DUMMY|North|South|West|East deriving (Enum, Show, Eq)
data RobotStatus = Clear|Blocked|Finished deriving (Show,Eq)
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
  let maze' = maze m // [(pos m, 1)] -- add the robot
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
    (concat $ map showRow $ reverse [-mazeInitialSize .. mazeInitialSize])
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

     let availableDirections = getAvailableDirections cs
     putStrLn $ ("AvailableDirections = " ++ show availableDirections)
     putStrLn $ ("BackDirection = " ++ (show $ backDirection maze'))

     delay $ 10000 * 2

     -- Get User Input
     -- go into manual mode if user starts typing
     userInput <-
       case (inputMode maze') of
         Automatic -> fmap (\mc ->  case mc of
                                      Nothing -> '0'
                                      Just c -> c
                           )
                      $ tryTakeMVar mvar
         Manual -> takeMVar mvar

     let modeCheck =
           case userInput of
             '0'  -> Automatic
             _    -> Manual

     let inputDirection =
           case (inputMode maze') of
             Manual    -> parseDirection userInput
             Automatic -> getAutomaticDirection maze' availableDirections


     let
       csNext = runProgram cs {inputBuffer = [fromEnum inputDirection]}
       result = head $ outputBuffer csNext

       newMaze = updateMaze
                 maze' { direction = inputDirection
                       , inputMode = modeCheck
                       }
                 result

     if (robotStatus newMaze) == Finished
       then
         do
           putStrLn "\n\n\n I FOUND THE END \n\n\n"
           pure (csNext,newMaze)
       else
         playMaze
         csNext {inputBuffer = [], outputBuffer = []}
         newMaze
         mvar

getAvailableDirections :: ComputerState -> [Direction]
getAvailableDirections cs =
  let
    clockwiseDirections = [North,East,South,West]
  in
    filter (\d -> checkDirectionAvailable cs d) clockwiseDirections

checkDirectionAvailable :: ComputerState -> Direction -> Bool
checkDirectionAvailable cs dir =
  let newCs = runProgram cs {inputBuffer = [fromEnum dir]}
  in
    if outputBuffer newCs == [0] then False else True

updateMaze :: Maze -> Int -> Maze
updateMaze maze' result =
  case result of
    -- WALL In that Direction
    0 ->
      let newMaze =
            -- if totally blocked, go backwards by reversing the backDirection
            if (numWalls maze') >= 3
            then
              maze'
              { robotStatus = Clear
              , numWalls = (numWalls maze') + 1
              , direction = backDirection maze'
              , backDirection = directionToTheBack $ backDirection maze'
              }
            else
              maze'
              { robotStatus = Blocked
              , numWalls = (numWalls maze') + 1
              }

      in
        newMaze

    -- EMPTY SPACE In that Direction:  Move Forward and update the backDirection
    1 -> moveForward maze'
         { robotStatus = Clear
         , backDirection = directionToTheBack (direction maze')
         , numWalls = 0
         }

    -- FOUND THE END
    2 -> maze' { robotStatus = Finished}



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

getAutomaticDirection :: Maze -> [Direction] -> Direction
getAutomaticDirection maze' dirs =
  
  
  let myDirection = directionToTheBack $ backDirection maze'
      oneRight = directionToTheRight myDirection
      twoRight = directionToTheRight $ directionToTheRight  myDirection
      threeRight = directionToTheRight $ directionToTheRight $ directionToTheRight myDirection
  in
    case length dirs of
      --if there is only one direction, go there
      1 -> head dirs
      -- if there are 2 directions, go forward
      2 -> head $ filter (\dir -> dir /= (backDirection maze')) dirs
      _ ->
        -- if right is possible, go right.  Else go straihght
        if elem oneRight dirs
        then oneRight
        else if elem myDirection dirs
        then myDirection
        else threeRight

__getAutomaticDirection :: Maze -> Direction
__getAutomaticDirection maze' =
  case (robotStatus maze') of
    -- clear and looking straight, try going right.
    Clear -> if (direction maze') == (directionToTheBack $ direction maze')
             then directionToTheRight (direction maze')
             else directionToTheBack (backDirection maze')
    -- clear and right blocked, go straight
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
