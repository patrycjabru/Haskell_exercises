{-#LANGUAGE ScopedTypeVariables#-}
--działa
noWhiteSpace :: String -> String
noWhiteSpace [] = []
--noWhiteSpace (x:xs) | x == ' ' = noWhiteSpace xs
--                    | otherwise = x : noWhiteSpace xs
noWhiteSpace l@(x:xs) = filter test l
    where test x = x /= ' '
   
   
--działa
(.) :: (b -> c) -> (a -> b) -> a -> c
(.) f g x = f(g x)


--prawie działa
triple [] = []
triple l@(x:xs) = (map (replicate 3) l)

--nie działa
--map2 f = foldl ((:).f) []

--działa
--map3 f = foldr ((:).f) []

--działa
pobierz = do
    x <- getLine
    let y = length $ filter (== ' ') x 
    print (y+1)
    
--działa    
digs :: Integral x => x -> [x]
digs 0 = []
digs x = x `mod` 10 : digs (x `div` 10) 

sumaCyfr = do
    input <- getLine
    let x = (read input :: Int) 
    let y = digs x
    let z = foldl (+) 0 y
    print z
    
    
