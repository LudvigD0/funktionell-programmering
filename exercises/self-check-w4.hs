printFullName :: IO ()

printFullName = do
    putStr "What is your first name?: \n>"
    firstName <- getLine
    putStr "What is your last name?: \n>"
    lastName <- getLine
    putStrLn ("Your full name is: "++firstName ++ " " ++ lastName)

genEven :: Gen Integer = do
