 
--chyba działa
($) :: (t1 -> t2) -> t1 -> t2
f $ x = f x

--działa
pobierz = do
    x <- getLine
    y <- getLine
    z <- getLine
    putStr((reverse z)++"\n")
    putStr((reverse y)++"\n")
    putStr((reverse x)++"\n")
