fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

sign :: Double -> Double
sign x | x > 0  = 1
       | x == 0 = 0
       | x < 0 = -1

sum2a :: (Int, Int) -> Int
sum2a (m, n) = m + n

sum2b :: [Int] -> Int
sum2b (m:n:_) = m + n

sum2c :: Int -> Int -> Int
sum2c m n = m + n

zip2 :: ([a],[b]) -> [(a,b)]
zip2 ([],[]) = []
zip2 ((a:b),(c:d)) = (a,c) : zip2(b,d)

bakterie :: (Int,Int) -> (Int,Int)
bakterie (a,b) = (b,((2*a)+b))

liczBakterie :: (Int,(Int,Int)) -> (Int,(Int,Int))
liczBakterie (0,(a,b)) = (0,(a,b))
liczBakterie (s,(a,b)) = liczBakterie ((s-1),bakterie(a,b)) 

sumaCyfr :: Int -> Int
sumaCyfr (x) | x<10 = x
             | otherwise = (x `mod` 10)+sumaCyfr(x `div` 10)

scyfry :: Int -> Int
scyfry (x) | x<10 = x
           | otherwise = scyfry (sumaCyfr(x))


usunDup :: [Int] -> [Int]
usunDup [] = []
usunDup (h:t) = 
