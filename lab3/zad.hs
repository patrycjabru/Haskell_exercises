--zad1
mojeLiczby = [f x | x <- lista, p x]
             where f = \a -> 2 * a          -- f mnoży liczbę razy 2
                   lista = [1..10]          -- lista początkowa
                   p = \b -> b `mod` 2 == 0 -- p wybiera liczby parzyste
 
mojeLiczby1 = filter test (map (*2) [1..10])
    where test x = mod x 4 == 0
          
--zad2
--generatorOperator :: (lewa -> prawa -> wynik) -> lewa -> (prawa -> wynik)
--generatorOperator = \operator liczba -> liczba `opetrator`

--zad3
myReverse :: String -> String
myReverse = foldl  (flip (:)) []

--zad4
policzISumuj :: (Int -> Int) -> Int -> Int -> Int
policzISumuj dzialanie a b = foldl (+) 0 (map dzialanie [a..b]) 

--zad5 
pierwsze :: [Int] -> [Int]
pierwsze [] = []
pierwsze (x:tail) | x < 2 = pierwsze tail
                  | podziel x == [] = (x:pierwsze tail)
                  | otherwise = pierwsze tail
                  
podziel :: Int -> [Int]
podziel n = filter test [2..n-1]
    where test x = mod n x == 0
          
--zad6
--conajmniejn2 :: [Int] -> Int -> [Int]
--conajmniejn2 list n = filter (test) list
--    where test x = 
