


main :: IO ()
main = putStrLn "Testing"

data Student = Student
    {
        name :: String
        , socialNum :: Int
        , email :: String
        , courses :: [(Course, Grade)]
    }

instance Show Student where
    show (Student n sn _ _) = n ++ " (" ++ show sn ++ ")"

instance Eq Student where
    s1 == s2 = socialNum s1 == socialNum s2

instance Show Grade where
    show grade = case grade of
        U     -> "U"
        Three -> "3"
        Four  -> "4"
        Five  -> "5"

--student kräver 4 olika saker

data Grade = U | Three | Four | Five deriving Eq



data Course = Course
    { 
        code :: Code
        , credits :: Double
        , prereqs :: [Course]
    }

instance Show Course where
    show = show . code

instance Eq Course where
    c1 == c2 = code c1 == code c2

data Code = CTH String | GU String | SAM String String deriving Eq


instance Show Code where
    show (CTH c)     = c
    show (GU  c)     = c
    show (SAM c1 c2) = c1 ++ "-" ++ c2



tda555, dit992, dit013 :: Course
tda555 = Course (SAM "TDA555" "DIT441") 7.5 []
dit992 = Course (GU "DIT961") 7.5 [tda555]
dit013 = Course (GU "DIT013") 6 []

mats, lise, sofie :: Student
mats = Student "Mats" 1234 "mats@gerdes.nl" [(tda555, U)]
lise = Student "Lise" 3132 "lise@gerdes.nl" [(tda555, Three), (dit013, Four)]
sofie = Student "Sofie" 6586 "sofie@gerdes.nl" []


passedCourses :: Student -> [Course]
passedCourses = map fst . filter p . courses
    where
        p (c, g) = g /= U 


canRead student course = and [c `elem` (passedCourses student) | c <- prereqs course]


csn :: Student -> Double
csn student 
    | total > 45 = 5000
    | total > 22 = 2000
    | otherwise  = 1000
    where 
        total = sum $ map credits $ passedCourses student



{- mapRow :: (a -> b) -> Row a -> Row b
mapRow f r = case r of
    Empty -> Empty
    AddLeft x r -> AddLeft (f x) -}