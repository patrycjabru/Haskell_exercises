{-# LANGUAGE FlexibleInstances #-}
import Data.Char

--zad1
class Intable x where
        toInt :: x -> Int

instance Intable Int where 
        toInt x = x

instance Intable [Char] where 
        toInt x = toInt [ digitToInt c | c <- x, c `elem` ['0'..'9']]

instance Intable [Int] where
        toInt [] = 0
        toInt x = foldl (\acc x -> acc * 10 + x ) 0 x


mySuperAdd :: (Intable a, Intable b) => a->b->Int
mySuperAdd x y = toInt x + toInt y



--zad2
data Osoba = Osoba
        { imie :: String
        , nazwisko    :: String
        , pesel    :: String
        } deriving (Show)
  
instance Ord Osoba where 
        (>) (Osoba _ nazw_1 _) (Osoba _ nazw_2 _) = (nazw_1 > nazw_2)
        (<) (Osoba _ nazw_1 _) (Osoba _ nazw_2 _) = (nazw_1 < nazw_2)

instance Eq Osoba where
        (==) (Osoba _ _ pesel_1) (Osoba _ _ pesel_2) = (pesel_1 == pesel_2)
        
--zad3
find osoba = osoba == Osoba "aaa" "aaa" "111111111"
func predykat [] = Nothing
func predykat (h:lista) | predykat h == True = Just h
                        | otherwise = func predykat lista




