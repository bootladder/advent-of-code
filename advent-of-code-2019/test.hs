

doStuff :: Int -> IO Int
doStuff input = do
  s <- getLine
  let i = read s
  return $ i + input

main = do
  let z = [doStuff 1, doStuff 2]

  myList <- mapM doStuff

  -- if I were to input 100 and then 200, how do I get a list [101,202] ?

  --let myList = [102,202]

  --putStrLn $ show $ sum myList
  putStrLn "Done"
  
