
main = do

  grid <- readGridFromFile "day18-input-1.txt"
  let keys = getKeys grid
  let doors = getDoors grid

  putStrLn $ gridToString grid
  putStrLn $ show keys
  putStrLn $ show doors

  let initialWorld = World { grid = grid
                           , keys = keys
                           , doors = doors
                           , pos = getPosition grid
                           , cost = 0
                           , takenKeyNames = []
                           }

  let 

  let allPossibleEndWorlds = getAllPossibleEndWorlds initialWorld

  let shortestWorld = minimumBy
                      (\world1 world2 -> compare (cost world1) (cost world2))
                      allPossibleEndWorlds

  putStrLn $ "The first world finished cost is " ++ (show $ head allPossibleEndWorlds)
  putStrLn $ "The shortest world cost is " ++ show shortestWorld
  putStrLn $ "The number of worlds is : " ++ (show $ length allPossibleEndWorlds)
  putStrLn "Hello"
