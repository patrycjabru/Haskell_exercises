main = do
    putStrLn "Hello, what's your name?"
    return ()
    name <- getLine
    putStrLn $ "Hello " ++ name ++ " good to see you!" 
