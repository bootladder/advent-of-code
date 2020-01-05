import Data.List.Split

simpletest = "\
\COM)B\n\
\B)C\n\
\C)D\n\
\D)E\n\
\E)F\n\
\B)G\n\
\G)H\n\
\D)I\n\
\E)J\n\
\J)K\n\
\K)L"

simpletest2 = "\
\COM)B\n\
\B)C\n\
\C)D\n\
\D)E\n\
\E)F\n\
\B)G\n\
\G)H\n\
\D)I\n\
\E)J\n\
\J)K\n\
\K)L\n\
\K)YOU\n\
\I)SAN"

type Edge = (String,String)

adjacencyList = parseAdjacencyList simpletest
nodes = findUniqueNodes adjacencyList
orbitCounts = map (numOrbits adjacencyList) nodes

--part 2
adjacencyList2 = parseAdjacencyList simpletest2
nodes2 = findUniqueNodes adjacencyList2
distances = distancesToNode "SAN" adjacencyList2

main = do
  s <- readFile "day6-input.txt"
  let adjacencyList = parseAdjacencyList s
      nodes = findUniqueNodes adjacencyList
      orbitCounts = map (numOrbits adjacencyList) nodes

  putStrLn $ "ANSWER IS : " ++ (show $ sum orbitCounts)

  putStrLn "PART 2::"
  let distances = distancesToNode "SAN" adjacencyList
      youDistance = distanceOfNodeInResult "YOU" distances


  putStrLn $ "THE ANSWER IS :: " ++ (show youDistance)
  putStrLn $ "Subract 2 from it for the actual answer"
  putStrLn "DONE"

parseAdjacencyList :: String -> [Edge]
parseAdjacencyList str =
  let lists = map (splitOn ")") $ lines str
  in map (\[a,b] -> (a,b)) lists

findUniqueNodes :: [Edge] -> [String]
findUniqueNodes adjList =
  foldr addIfNotFound [] adjList
  where
    addIfNotFound (a,b) list =
      let findAs = 
            if findNode a list
            then list
            else list ++ [a]
      in
        if findNode b findAs
        then findAs
        else findAs ++ [b]

    findNode a list =
      length (filter (\x -> x == a) list) > 0

numOrbits :: [Edge] -> String -> Int
numOrbits adjList node =
  if node == "COM"
  then 0
  else
    1 + (numOrbits adjList (findParent adjList node))

findParent :: [Edge] -> String -> String
findParent adjList node =
  fst $ head $ filter (\(a,b) -> b == node) adjList

neighborsOf :: String -> [Edge] -> [String]
neighborsOf node adjList =
  foldr (addIfNeighbor node) [] adjList
  where
    addIfNeighbor node (a,b) list =
      if node == a
      then list ++ [b]
      else if node == b
           then list ++ [a]
           else list
      

-- starting at the node, radiate outward adding 1 to the distances for each neighbor away
distancesToNode :: String -> [Edge] -> [(String, Int)]
distancesToNode node adjList =
  let allNodes = findUniqueNodes adjList
      initialResult = zip allNodes $ repeat (-1)
  in
    traverse node initialResult adjList 0

  where traverse node result adjList distance =
          if distanceOfNodeInResult node result /= -1 then result
          else
            let newResult = updateResult (node, distance) result
                neighbors = neighborsOf node adjList
            in
              foldr
              (\neighbor result -> traverse neighbor result adjList (distance+1))
              newResult neighbors


indexOfNodeInResult :: String -> [(String,Int)] -> Int
indexOfNodeInResult node result =
  if (node `matches` head result)
  then 0
  else (+) 1 $ indexOfNodeInResult node $ tail result
  where
    matches node (n,_) = node == n

distanceOfNodeInResult :: String -> [(String,Int)] -> Int
distanceOfNodeInResult node result =
  let match = head $ filter (\(n,_) -> n == node) result
  in snd match


updateResult :: (String,Int) -> [(String,Int)] -> [(String,Int)]
updateResult new result =
  let newNode = fst new
      index = indexOfNodeInResult newNode result
      splitted = splitAt index result
  in
    (fst splitted) ++ [new] ++ (tail $ snd splitted)

