import Prelude hiding (Maybe, Just, Nothing)


data List a = Nil | Cons a (List a) deriving Show


xs :: List Int
--xs = 1 `Cons` (2 `Cons` (3 `Cons` Nil))
xs = Cons 1 (Cons 2 (Cons 3 (Cons 6 Nil)))


ys :: List String
--ys = "alex" `Cons` ("gerdes" `Cons` Nil)
ys = Cons "Alex" (Cons "Gerdes" Nil)


prod :: Num a => List a -> a
prod Nil         = 1
prod (Cons x xs) = x * prod xs

showL :: Show a => List a -> String

showL xs = "[" ++ go xs ++ "]"
    where
        go Nil = ""
        go (Cons x Nil) = show x
        go (Cons x xs) = show x ++ ", " ++ go xs


toList :: [a] -> List a
toList xs = foldr (Cons) Nil xs


data Name = First String | Middle Name String deriving Show


ludvig :: Name
ludvig =  Middle (Middle (First  "Hamster") "Hamster" ) "Ludvig"


data DoubleTree = Leaf String
                | Pair DoubleTree DoubleTree

data TrippleTree = Lov String
                | Cool Int
                | Tripple TrippleTree TrippleTree TrippleTree

{- instance Show TrippleTree where
    show (Lov loveit)                 = loveit
    show (Cool coolint)               = show coolint
    show (Tripple first second third) = "[" ++ show first ++ show second ++ "    " ++ show third
 -}

instance Show TrippleTree where
    show tris = "[" ++ recursivetest tris ++ "]"

recursivetest :: TrippleTree -> String
recursivetest (Lov v) = v
recursivetest (Cool i) = show i
recursivetest (Tripple a b c) = recursivetest a ++ ", " ++ recursivetest b ++ ", " ++ recursivetest c


data Maybe a = Nothing | Just a

testingalittle :: Maybe Int
testingalittle = Just 5


testing :: TrippleTree
testing = Tripple (Tripple (Cool 12) (Lov "Hamster") (Cool 2003)) (Lov "Cool") (Cool 23)




data Car = Model String | Year Car Int deriving Show

test :: Car
test = Year ( Model "Volvo hamster") 2013


data Nat = Zero | Succ Nat deriving (Eq, Ord, Show)

add :: Nat -> Nat -> Nat
add a Zero     = a
add a (Succ b) = Succ (a `add` b)


len :: [a] -> Nat
len [] = Zero
len (x:xs) = Succ (len xs)

sub :: Nat -> Nat -> Nat
sub Zero (Succ b)     = error "This will result in a negative Natural Number, which doesn't exist"
sub a Zero            = a
sub (Succ a) (Succ b) = (sub a b)

prop_associative :: Nat -> Nat -> Nat -> Bool
prop_associative a b c | add (add a b) c == add a (add b c) = True


data Diagram 
  = Question String Diagram Diagram
  | Action String Diagram
  | Done
  deriving (Show, Eq)


isSunny, park, work :: Diagram
isSunny = Question "Is it sunny outside?" park work
park    = Action "Go to the park! And write some Haskell code!" Done
work    = Action "Write some Haskell code!" Done


mapAction :: (String -> String) -> Diagram -> Diagram
mapAction f Done                  = Done
mapAction f (Action str rest)  = Action (f str) (mapAction f rest)
mapAction f (Question str yes no) = Question str (mapAction f yes) (mapAction f no)









data Student = Student
  { name      :: String
  , socialNum :: Int
  , email     :: String
  , courses   :: [(Course, Grade)] 
  } deriving Show

data Grade = U | Three | Four | Five deriving (Eq, Show)

data Course = Course
  { code    :: Code
  , credits :: Double
  , preReqs :: [Course]
  } deriving Show

data Examiner = Examiner
    {
        examinerName :: String
        , examinerSocialNum :: Int
    } deriving Show



instance Eq Course where
  c1 == c2 = code c1 == code c2

data Code = CTH String | GU String | SAM String String deriving (Eq, Show)



mats, lise, sofie :: Student
mats  = Student "Mats"  1234 "mats@gbla.nl" [(tda555, U)]
lise  = Student "Lise"  3132 "lise@bla.nl" [(tda555, Three)]
sofie = Student "Sofie" 6586 "sofie@bla.nl" []

tda555 :: Course
tda555 = Course (SAM "TDA555" "DIT441") 7.5 []

grade :: Course -> Grade -> Student -> Student
grade c g s  = s { courses = (c,g) : courses s}

data CourseInstance = CourseInstance 
    { course :: Course
    , examiner :: Examiner
    , students :: [Student]
    , period :: String
    , ta :: [String]
    } deriving Show

alexExaminer :: Examiner
alexExaminer = Examiner {examinerName = "Alex", examinerSocialNum = 1234}
--alexExaminer = Examiner "Alex" 1234


courseInstance1 :: CourseInstance
courseInstance1 = CourseInstance tda555 alexExaminer [lise, mats] "HT2025" ["Daniel Andersson"]





data Row a 
  = Empty
  | AddLeft a (Row a)
  | AddRight (Row a) a
  deriving (Eq, Show)

-- Smart constructors
(<|) :: a -> Row a -> Row a
x <| r = AddLeft x r

(|>) :: Row a -> a -> Row a
r |> x = AddRight r x

-- Fix fixity
infixr 6 <|
infixl 5 |>


start :: Row a -> Maybe a
start row = case row of
  Empty        -> Nothing
  AddLeft x _  -> Just x
  AddRight r _ -> start r


end :: Row a -> Maybe a
end = undefined