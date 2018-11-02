import System.IO  
import Data.Char
--zad1 - działa
main = do
    let num = 5
    guessing num 1

guessing num guesses = do
    input <- getLine
    let guess = (read input :: Int)
    let check = checkGuess num guess
    putStr (check++"\n")
    if (check /= "Trafione" && guesses == 3) then 
        do putStr "Nie zgadles\n" 
    else if (check /= "Trafione" && guesses < 3) then 
        do guessing num (guesses+1)
    else
        return () 
        
checkGuess num guess | num == guess = "Trafione"
                     | num > guess = "Wiecej"
                     | otherwise = "Mniej"
                        
                        
--zad2 - działa
odwr = do
    input <- getLine
    let revStr = reverse input
    putStr (revStr++"\n")
    if input == " " then do
       return ()
                    else do
       odwr
       
--zad3  - działa 
toUpp = do  
    handle <- openFile "test" ReadMode
    handleW <- openFile "test2" WriteMode
    contents <- hGetContents handle  
    let upperContents = map toUpper contents
    putStr upperContents
    hPutStr handleW upperContents
    hClose handleW
    hClose handle
