import Data.List (sortBy)
import Text.Read (readMaybe) 
import Data.Maybe
import Data.Char
import Data.Map (Map)
import qualified Data.Map

--z1 - działa
data Student = Student {id :: Int, name :: String, dateOfBirth :: String} deriving Show
data Score = Score {studentId :: Int, course1Score :: Int, course2Score :: Int, course3Score :: Int} deriving Show

data StudentWithScores = StudentWithScores {id' :: Int, name' :: String, dateOfBirth' :: String, course1Score' :: Int, course2Score' :: Int, course3Score' :: Int} 

--z2 - działa
instance Show StudentWithScores where 
    show (StudentWithScores id name dateOfBirth course1Score course2Score course3Score) = show id ++ " " ++ name ++ " " ++ dateOfBirth ++ " " ++ show course1Score ++ " " ++ show course2Score ++ " " ++ show course3Score

--z3 - działa 
instance Eq StudentWithScores where
    (==) (StudentWithScores id1 _ _ _ _ _) (StudentWithScores id2 _ _ _ _ _) = (id1 == id2) 
    
--z4 - nie całe
students :: [Student]
students = [Student 26453 "Kristalee Copperwaite" "2000", Student 33596 "Roeberta Naden" "1997"]
scores :: [Score]
scores = [Score 26453 93 97 80, Score 40241 86 85 87, Score 33596 82 60 80]


instance Eq Student where
    (==) (Student _ _ d1) (Student _ _ d2) = d1==d2
instance Ord Student where
    (<) (Student _ _ d1) (Student _ _ d2) = d1 < d2 
    (<=) (Student _ _ d1) (Student _ _ d2) = d1 <= d2 
    (>) (Student _ _ d1) (Student _ _ d2) = d1 > d2 
    (>=) (Student _ _ d1) (Student _ _ d2) = d1 >= d2 
    
--sortByDate [] = []
--sortByDate (x:t) = 
--    let smallerSorted = sortByDate [a | a <- t, a <= x]
--        biggerSorted = sortByDate [a | a <- t, a > x]
--    in smallerSorted ++ [x] ++ biggerSorted

--sortBy:
compareDate (Student _ _ d1) (Student _ _ d2)
    | (d1 == d2) = EQ
    | (d1 < d2) = LT
    | otherwise = GT 
    
sortByDate = sortBy compareDate

compareName (Student _ n1 _) (Student _ n2 _) 
    | n1 == n2 = EQ
    | n1 < n2 = LT
    | otherwise = GT
    
sortByName = sortBy compareName

--NIE MOJE
--Zdefiniuj funkcję sortującą rekordy wyników tak, żeby rekordy były posortowane najpierw po wyniku z pierwszego kursu, a następnie (dla remisów) po id studneta
sortByScore :: [Score] -> [Score]
sortByScore = sortBy cmp 
    where cmp a b = let w = compare (course1Score a) (course1Score b) in if w == EQ then compare (studentId a) (studentId b) else w

--Zdefiniuj funkcję sortującą rekordy wyników tak, żeby rekordy z większą sumaryczną ilością punktów były wcześniej od tych z mniejszą sumaryczną ilością punktów
sortByTotalScore :: [Score] -> [Score]
sortByTotalScore = sortBy cmp 
    where cmp a b = compare (sum a) (sum b)
          sum sc = course1Score sc + course2Score sc + course3Score sc


--z5 - działa
toStudentWithScores :: Student -> Score -> Maybe StudentWithScores
toStudentWithScores (Student s1 s2 s3) (Score sc1 sc2 sc3 sc4)
    | s1 == sc1 = Just (StudentWithScores s1 s2 s3 sc2 sc3 sc4)
    | otherwise = Nothing
        
--z6 - działa (NIE MOJE)
findById :: [Score] -> Int -> [Score]
findById [] _ = []
--findById sc id' = filter (id' == studentId) sc
findById sc id = filter (\s -> studentId s == id) sc

--z7 - działa
findBy :: (Score -> Bool) -> [Score] -> [Score]
findBy f scrs = filter f scrs

--z8 - działa
mapBy :: (Score -> Score) -> [Score] -> [Score]
mapBy f scrs = map f scrs

--z9 - działa
reduceBy :: Foldable t => ([a1] -> a2 -> [a1]) -> t a2 -> [a1]
reduceBy f scrs = foldl f [] scrs

--z10 - działa
mapToJoin :: Student -> [Score] -> [Maybe StudentWithScores]
mapToJoin _ [] = []
mapToJoin student (h:t) = toStudentWithScores student h : mapToJoin student t

--z11 - działa (NIE MOJE)
joinStep1 :: [Student] -> [Score] -> [(Student, [Score])]
--joinStep (st:stt) sc = 
joinStep1 st sc = map (\s -> (s,findById sc $ Main.id s)) st

--z12 - działa
joinStep2 :: [(Student, [Score])] -> [[Maybe StudentWithScores]]
joinStep2 [] = []
joinStep2 (x:xs) = 
    let st = fst x
        (sc:_) = snd x
    in [toStudentWithScores st sc] : joinStep2 xs
    
--z13 - działa
joinStep3 :: [[Maybe StudentWithScores]] -> [StudentWithScores]
joinStep3 [] = []
joinStep3 ((x:_):xs) | x == Nothing = joinStep3 xs
                     | otherwise = (fromJust x) : joinStep3 xs

--z14 - działa
join :: [Student] -> [Score] -> [StudentWithScores]
join st sc = c
    where a = joinStep1 st sc
          b = joinStep2 a
          c = joinStep3 b
          
--z15 - działa?
class (Ord a) => Id a where
    toInt :: a -> Int

--z16 - działa? (NIE MOJE)
class HashId x where
    getId :: (Id b) => x b -> b
    
    
--z17
--class Repository a where
--    insert :: (HasId b, Id c) => (b c) -> a (b c) c -> a (b c) c
