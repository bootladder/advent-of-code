import Intcode
import Data.Array
import Data.List.Split
import Debug.Trace
import Control.Concurrent.Thread.Delay

data Direction = DUMMY|North|South|West|East deriving (Enum, Show)
data Maze = Maze { pos :: (Int,Int)
                 , direction :: Direction
                 , maze :: Array (Int,Int) Int
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
    ++
    (mazeStats)


mazeInitialSize = 30

mazeInitial = Maze { pos = (0,0)
                   , direction = North
                   , maze = listArray (((-mazeInitialSize),(-mazeInitialSize)),(mazeInitialSize,mazeInitialSize)) $ repeat 0
                   }

playMaze :: ComputerState -> Maze -> IO (ComputerState, Maze)
playMaze cs maze' =
  do delay 100000
     putStrLn ("MAZE: " ++ showMaze maze')
     userInput <- getChar

     let
       inputDirection = parseDirection userInput
       mazeWithNewDirection = maze' {direction = inputDirection}

       csNext =
         runProgram cs {inputBuffer = [fromEnum $ (direction mazeWithNewDirection)]}
       result =
         head $ outputBuffer csNext

       in
        case result of
          -- WALL In that Direction
          0 -> playMaze
              csNext {inputBuffer = [], outputBuffer = []}
              mazeWithNewDirection

          -- EMPTY SPACE In that Direction:  Move Forward
          1 -> playMaze
              csNext {inputBuffer = [], outputBuffer = []}
              $ moveForward mazeWithNewDirection

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

main :: IO ()
main = do
  s <- readFile "day15-input.txt"
  let inputProgram = parseProgram s
      csInitial = defaultComputerState { computerId = "hello id"
                                       , program = inputProgram}

  (cs,maze') <- playMaze csInitial mazeInitial

  putStrLn $ showMaze maze'
  putStrLn $ showComputerState cs
  putStrLn "Hello"

