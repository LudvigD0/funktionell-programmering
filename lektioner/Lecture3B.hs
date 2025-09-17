
import Data.Char (isAlpha)
import Test.QuickCheck

hello :: IO ()
hello = putStrLn "Hellow World"

greeting :: IO()
greeting = do
    putStr "What is your name?\n> "
    name <- getLine
    putStrLn ("Tjenare" ++ name ++ "!")


backup :: FilePath -> IO ()
backup file = do
    txt <- readFile file
    writeFile (file ++ ".bak") txt


{-
whatsTheDifference :: IO ()
whatsTheDifference = do
    putStr "Give me a number:\n> "
    x <- readLn
    putStr "Give me a number:\n> "
    y <- readLn
    putStrLn ("The diff is: " ++ show (x -  y))
-}

whatsTheDifference :: IO ()
whatsTheDifference = do
    putStr "Give me a number:\n> "
    x <- getLine
    putStr "Give me a number:\n> "
    y <- getLine
    putStrLn ("The diff is: " ++ show (read x -  read y))


lengthFile :: FilePath -> IO Int
lengthFile file = do
    txt <- readFile file
    return (length (lines txt))

{- dixt :: FilePath
dict = "/usr/share/dict/words" -}



askForNumber :: IO Int
askForNumber = do
    putStr "Give a number:\n> "
    n <- readLn
    return n


calcDiff :: IO Int
calcDiff = do 
    x <- askForNumber
    y <- askForNumber
    return (x -y)

whatsTheDifference' :: IO ()
whatsTheDifference' = do 
    diff <- calcDiff
    putStrLn ("The difference is: " ++ show diff)



{- data Suit = Hearts | Spades | Diamonds | Clubs deriving(Eq,Read)
data Rank = Numeric Int | Jack | Queen | King | Ace deriving (Eq, Read)
data Card = Card Rank Suit

type Hand = [Card]

instance Show Suit where
    show Hearts = ""

 -}

--test = elements Test      väljer slumpmässigt värde från listan.