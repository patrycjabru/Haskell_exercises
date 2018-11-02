zip2 :: [a] -> [b] -> [(a,b)]
zip2 [] _ = []
zip2 _ [] = []
zip2 (x:xs) (y:ys) = (x,y) : zip2 xs ys

bakterie :: (Int, Int) -> (Int, Int) 
bakterie (a,b) = (b,2*a+b)

liczBakterie (0,(a,b)) = (0,(a,b))
liczBakterie (s,(a,b)) = liczBakterie (s-1,( bakterie (a,b) ))
