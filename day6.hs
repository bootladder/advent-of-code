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

type Edge = (String,String)

adjacencyList = parseAdjacencyList simpletest
nodes = findUniqueNodes adjacencyList
orbitCounts = map (numOrbits adjacencyList) nodes


main = do
  s <- readFile "day6-input.txt"
  let adjacencyList = parseAdjacencyList s
      nodes = findUniqueNodes adjacencyList
      orbitCounts = map (numOrbits adjacencyList) nodes

  putStrLn $ "ANSWER IS : " ++ (show $ sum orbitCounts)
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
