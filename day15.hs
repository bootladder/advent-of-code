import Intcode
import Data.Array
import Control.Concurrent.Thread.Delay
import Control.Concurrent
import System.IO

data Direction = DUMMY|North|South|West|East deriving (Enum, Show, Eq)
data InputMode = Automatic|Manual deriving Show

data Maze = Maze { pos :: (Int,Int)
                 , direction :: Direction
                 , backDirection :: Direction
                 , maze :: Array (Int,Int) Int
                 , inputMode :: InputMode
                 , moveList :: [(Int,Int)]
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
                   , inputMode = Automatic
                   , moveList = []
                   , maze = listArray (((-mazeInitialSize),(-mazeInitialSize)),(mazeInitialSize,mazeInitialSize)) $ repeat 0
                   }

playMaze :: ComputerState -> Maze -> MVar Char -> IO (ComputerState, Maze)
playMaze cs maze' mvar =
  do
    -- print state
     putStrLn ("\ESC[2J \ESC[H MAZE: " ++ showMaze maze')

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

       newMaze = moveForward $
                 maze' { direction = inputDirection
                       , backDirection = directionToTheBack inputDirection
                       , inputMode = modeCheck
                       }

     if (result == 2)
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



parseDirection :: Char -> Direction
parseDirection c =
  case c of
    'h' -> West
    'j' -> South
    'k' -> North
    'l' -> East
    _ -> DUMMY

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
              DUMMY -> (0,0)

           , maze = (maze maze') // [(pos maze', 7)]
          }

getAutomaticDirection :: Maze -> [Direction] -> Direction
getAutomaticDirection maze' dirs =
  let myDirection = directionToTheBack $ backDirection maze'
  in
    case length dirs of

      --if there is only one direction, go there
      1 -> head dirs

      -- if there are 2 directions, go forward (ie. the one that is not backward)
      2 -> head $ filter (\dir -> dir /= (backDirection maze')) dirs

      -- if right is possible, go right.  Else go straihght
      _ ->
        if (directionToTheRight myDirection) `elem ` dirs
        then directionToTheRight myDirection
        else myDirection



main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  s <- readFile "day15-input.txt"
  let inputProgram = parseProgram s
      csInitial = defaultComputerState { computerId = "hello id"
                                       , program = inputProgram}

  mvar <- newEmptyMVar
  _ <- forkIO $ loop mvar

  (cs,maze') <- playMaze csInitial mazeInitial mvar

  putStrLn $ showMaze maze'
  putStrLn $ showComputerState cs
  putStrLn "Hello"

loop :: MVar Char -> IO ()
loop mvar = do
  c <- getChar; putMVar mvar c; putStrLn ("The Input is : " ++ (show c)) ;
  loop mvar
