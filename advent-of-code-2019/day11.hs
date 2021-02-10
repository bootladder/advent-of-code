import Data.List.Split
import Data.Array
import Intcode

import Debug.Trace

myTrace :: (Show a) => String -> a -> a
myTrace comment t = trace (comment ++ show t) t

data Direction = UP|DOWN|LEFT|RIGHT|NOWHERE deriving Show

changeDirection :: Direction -> Int -> Direction
changeDirection dir i =
  case i of
    0 -> case dir of
      UP -> LEFT
      LEFT -> DOWN
      DOWN -> RIGHT
      RIGHT -> UP
      NOWHERE -> NOWHERE

    1 -> case dir of
      UP -> RIGHT
      RIGHT -> DOWN
      DOWN -> LEFT
      LEFT -> UP
      NOWHERE -> NOWHERE

    _ -> NOWHERE

moveRobot :: (Int,Int) -> Direction -> (Int,Int)
moveRobot (x,y) UP = (x,y+1)
moveRobot (x,y) DOWN = (x,y-1)
moveRobot (x,y) LEFT = (x-1,y)
moveRobot (x,y) RIGHT = (x+1,y)
moveRobot _ NOWHERE = (-999,-999)

data RobotDescriptor = RobotDescriptor { grid :: Array (Int,Int) Int
                                       , counterGrid :: Array (Int,Int) Int
                                       , location :: (Int,Int)
                                       , direction :: Direction
                                       } deriving Show

initialGridSize = 100

initialGrid :: Array (Int,Int) Int
initialGrid = (listArray ((0,0),(initialGridSize,initialGridSize)) [0 | x<-[0..], y<-[0..]])

initialCounterGrid :: Array (Int,Int) Int
initialCounterGrid = (listArray ((0,0),(initialGridSize,initialGridSize)) [0 | x<-[0..], y<-[0..]])

initialRobotDescriptor :: RobotDescriptor
initialRobotDescriptor =
  let mid = round $ (fromIntegral initialGridSize)/2
  in
    RobotDescriptor { grid = initialGrid
                    , counterGrid = initialGrid
                    , location = (mid,mid)
                    , direction = UP}

initialRobotDescriptorPart2 :: RobotDescriptor
initialRobotDescriptorPart2 =
  let mid = round $ (fromIntegral initialGridSize)/2
  in
    RobotDescriptor { grid = initialGrid // [((mid,mid),1)]
                    , counterGrid = initialGrid
                    , location = (mid,mid)
                    , direction = UP}

renderGrid :: RobotDescriptor -> Int -> String
renderGrid rd size = renderGrid'' (grid rd) size

renderCounterGrid :: RobotDescriptor -> Int -> String
renderCounterGrid rd size = renderGrid'' (counterGrid rd) size

renderGrid'' grid size =
  concat $ map (renderRow grid) $ reverse [0..size]
  where renderRow grid rowNum =
          [if (grid ! (i,rowNum) == 0)
            then '-'
            else '#'
          | i<-[0..size]] ++ "\n"

main :: IO ()
main = do
  s <- readFile "day11-input.txt"
  let inputProgram = listArray (0,4000) $ (map read $ splitOn "," s) ++ (repeat 0)
  --putStrLn $ show $ inputProgram

  --let cs = runProgramWithInputBuffer inputProgram [0]
  --  in putStrLn $ show $ cs

  let cs = defaultComputerState {computerId = "robot program"
                                ,program=inputProgram}

  --let finalRobotDescriptor = fakeioLoop cs initialRobotDescriptor fakeOutputBuffer
  finalRobotDescriptor <- ioLoop cs initialRobotDescriptorPart2 

  putStrLn $ show finalRobotDescriptor
  putStrLn $ renderGrid finalRobotDescriptor initialGridSize
  putStrLn $ renderCounterGrid finalRobotDescriptor initialGridSize
  putStrLn $ show $ length $ filter (== '#') $ renderCounterGrid finalRobotDescriptor initialGridSize
  putStrLn "\n"
  putStrLn "Done"


fakeOutputBuffer :: [Int]
fakeOutputBuffer = [1,0,0,0,1,0,1,0,0,1,1,0,1,0]

fakeioLoop :: ComputerState ->
              RobotDescriptor ->
              [Int] ->
              RobotDescriptor
fakeioLoop cs rd outputBuf =
  if length outputBuf == 0
  then rd
  else
    let
      nextOutBuf = take 2 outputBuf
      robotColor = (grid rd) ! (location rd)
      newColor = (head $ nextOutBuf)
      newDirectionCommand = (last $ nextOutBuf)
      newDirection =
        changeDirection (direction rd) newDirectionCommand
      newGrid = (grid rd) // [((location rd), newColor)]
      newLocation = moveRobot (location rd) newDirection
      newRd = rd{ grid=newGrid
                , location=newLocation
                , direction=newDirection
                }
    in
      fakeioLoop cs newRd $ drop 2 outputBuf

ioLoop :: ComputerState -> RobotDescriptor -> IO RobotDescriptor
ioLoop cs rd =
  do
    putStrLn "STARTING LOOP"
    let
      robotColor = (grid rd) ! (location rd)
      newcs = runProgram cs{ inputBuffer = [robotColor]
                           , outputBuffer = []
                           }
    --putStrLn $ show newcs

    case (status newcs) of
           HALTED -> return rd
           WAITFORINPUT _ ->
             -- paint the square, turn the robot
             -- add square to counterGrid
             -- advance the robot, put the new square color
             -- in the input buffer
             let newColor = (head $ outputBuffer newcs)
                 newDirectionCommand = (last $ outputBuffer newcs)
                 newDirection =
                   changeDirection (direction rd) newDirectionCommand
                 newGrid = (grid rd) // [((location rd), newColor)]
                 newCounterGrid = (counterGrid rd) // [((location rd), 1)]
                 newLocation = moveRobot (location rd) newDirection
                 newRd = rd{ grid=newGrid
                           , counterGrid=newCounterGrid
                           , location=newLocation
                           , direction=newDirection
                           }
             in
               do-- putStrLn $ "NEW GRID: " ++ (renderGrid newRd initialGridSize)
                 -- putStrLn $ "NEW DIRECTION: " ++ (show $ direction newRd)
                 -- putStrLn $ "NEW LOCATION: " ++ (show $ location newRd)
                 -- putStrLn $ "CONTINUE"
                  ioLoop newcs newRd
           _ ->
             do
               putStrLn $ "FAIL: INVALID STATE"
               return rd



