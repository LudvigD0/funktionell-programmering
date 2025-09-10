
--modeling data - göra egna typer

data Test = Test | Testttt



data Suit = Spades | Clubs | Hearts | Diamonds  deriving (Show,Eq)

data Subscription = Free | Pro | Business --deriving (Show,Eq)


colour :: Suit -> String
colour Spades = "Black"
colour Clubs = "Black"
colour _ = "Red"


data Colour = Red | Black deriving (Eq, Show)
colour' :: Suit -> Colour
colour' Spades = Black
colour' Clubs = Black
colour' _ = Red

data Rank = Numeric Int | Jack | Queen | King | Ace deriving (Show,Eq)

rankBeats :: Rank -> Rank -> Bool
rankBeats _ Ace = False
rankBeats Ace _ = True
rankBeats _ King = False
rankBeats King _ = True
rankBeats _ Queen = False
rankBeats Queen _ = True
rankBeats _ Jack = False
rankBeats Jack _ = True
rankBeats (Numeric x) (Numeric y) = x > y

prop_rank :: Rank -> Rank -> Bool
prop_rank r1 r2 = rankBeats r1 r2 || rankBeats r2 r1

data Card = Card Rank Suit deriving (Eq, Show)

aceOfSpades :: Card
aceOfSpades = Card Ace Spades

suit :: Card -> Suit
suit (Card r s) same

data Card' = Card' {rank' :: Rank, suit' :: Suit} deriving (Eq, Show)

type Hand [Card]

hand :: Hand
hand = [aceOfSpades, Card Ace Hearts, Card Queen Spades, Card Jack]



head :: [Int] -> Int
head (x:xs) = x


length :: [Int] -> Int
length [] = 0
length (x:xs) = length xs + 1