import Test.QuickCheck


data Suit = Hearts | Diamonds | Spades | Clubs


genSuit :: Gen Suit
genSuit = elements [Hearts, Diamonds, Spades, Clubs]


instance Arbitrary Suit where
    arbitrary = genSuit


data Card = Rank | Suit deriving Show



{- genCard :: Gen Card
genCard = do
    r <- genRankFreq
    s <- genSuit
    return (Card r s)
 -}

--en variant som också fungerar:
{- genCard :: Gen Card
genCard = do
    r <- genRankFreq
    s <- arbitrary
    return (Card r s) -}
--en fråga till chat: är arbitrary lite som när man skriver deriving Show,
-- att show kan lika gärna generera något vi inte vll ha men om vi specifikt skriver genSuit så kan vi få det resultat vi vill ha

--förstå sig på exakt vad arbitrary gör i jämförelse med Show ^^^^^

data Student = Student 
    {
        name :: String
        , surname :: String
        , age :: Int
    } {- deriving Eq -}

instance Show Student where
    show :: Student -> String
    show (Student name _ age ) = name ++ show age

instance Eq Student where
   {-  name == name = True
    student = (Student name _ _)  -}
    (Student name1 _ _) == (Student name2 _ _) = name1 == name2 
    --(Sttudent name1 )
  
 

alex = Student "Alex" "Gerdes" 35
ludvig = Student "Ludvig " "Dahlgren" 12


data RekTest = Lala String | Fet String String RekTest deriving Show

--instance Show RekTest where


lst :: RekTest -> String
lst (Lala s)      =  s
lst (Fet l r xs) = lst xs


testi = (Fet "Testing" "Testinggggg" (Fet "Oooof" "Ooof" (Lala "Lasttttelemenet")))

testlist :: [RekTest]
testlist = [(Fet "Testing" "Testinggggg" (Fet "Oooof" "Ooof" (Lala "asdasdas"))), (Fet "Testing" "Testinggggg" (Fet "Oooof" "Ooof" (Lala "Lasttttelemenet")))]

{- 
mapping :: (a -> b) -> [a] -> [b]
mapping = undefined
-}

mapping :: (String -> String) -> RekTest -> RekTest
mapping f l = case l of
    Lala s    -> Lala (f s)
    Fet s1 s2 xs -> Fet (f s1) (f s2) (mapping f xs)
    

runtest = mapping (\x -> if  (head x) == 'T' then "Poop" ++ (tail x) else x) testi