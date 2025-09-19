{- |
Module      : Lab2
Description : Skeleton for lab 2: Blackjack
Copyright   : (c) TDA555/DIT441, Introduction to Functional Programming
License     : BSD
Maintainer  : alexg@chalmers.se
Stability   : experimental

Authors     : <list your names here>
Lab group   : <group number>
-}

module Blackjack where

-- Import necessary modules
import Cards
import RunGame
import Test.QuickCheck hiding (shuffle)

--Create different testcards
aCard1 :: Card
aCard1 = Card (Numeric 8) Hearts

aCard2 :: Card
aCard2 = Card Ace Spades

aCard3 :: Card
aCard3 = Card King Hearts


--Create a test hand
aHand :: Hand
aHand = aCard1:(aCard2:[])

a21 :: Hand
a21 = aCard2:(aCard3: [])

-- Task A1 --

hand2 :: Hand
hand2 = [Card (Numeric 2) Hearts, Card Jack Spades]

sizeSteps :: [Int]
sizeSteps = 
  [ size hand2
  , size (Card (Numeric 2) Hearts : (Card Jack Spades : []))
  , 1 + size (Card Jack Spades : [])
  , 1 + (1 + size [])
  , 1 + (1 + 0)
  , 1 + 1
  , 2 
  ]

-- Task A2 --

suitUnicode :: Suit -> String
suitUnicode Spades =  "\x2660"
suitUnicode Hearts =  "\x2665"
suitUnicode Clubs =  "\x2663"
suitUnicode Diamonds =  "\x2666"

rankString :: Rank -> String
rankString (Numeric n) = show(n) 
rankString rank = show rank

displayCard :: Card -> String
displayCard card = rankString((rank card)) ++ ", " ++ (suitUnicode (suit card))

display :: Hand -> String
display hand = unlines [displayCard card | card <- hand]

-- Task A3 --

valueRank :: Rank -> Int
valueRank Ace = 11
valueRank Queen = 10
valueRank King = 10
valueRank Jack = 10
valueRank (Numeric n) = n

valueCard :: Card -> Int
valueCard card = valueRank (rank card)

numberOfAces :: Hand -> Int
numberOfAces hand = length [card | card <- hand, rank card == Ace]

value :: Hand -> Int
value hand
    | total <= 21 = total
    | otherwise = total - (numberOfAces hand * 10) 
    where 
      total = sum [valueCard card | card <- hand]

-- Task A4 --

gameOver :: Hand -> Bool
gameOver hand = value hand > 21

winner :: Hand -> Hand -> Player
winner guestHand bankHand 
  | gameOver guestHand = Bank
  | gameOver bankHand = Guest
  | value guestHand == value bankHand = Bank
  | value guestHand < value bankHand = Bank
  | value guestHand > value bankHand = Guest

--------------------------------------------------------------------------------
-- Part B
---------------------------------------------------------------------------------

-- Task B1 --

allRanks :: [Rank]
allRanks = [Numeric x | x <- [2..10]] ++ [Jack, Queen, King, Ace]

allSuits :: [Suit]
allSuits = [Hearts, Spades, Diamonds, Clubs]

fullDeck :: Deck
fullDeck = [Card x y | x <- allRanks, y <- allSuits]

prop_size_fullDeck :: Bool
prop_size_fullDeck = size fullDeck == 52

-- Task B2 --

draw :: Deck -> Hand -> (Deck, Hand)
draw [] _ = error "draw: The deck is empty"
draw (x:xs) hand = (xs,x : hand)


-- Task B3 --


playBank :: Deck -> Hand
playBank deck = playBank' deck []

playBank' deck bankHand
  | value bankHand >= 16 = bankHand
  | otherwise = playBank' deck' bankHand'
    where (deck', bankHand') = draw deck bankHand

-- Task B4 --

{- pick :: Double -> Deck -> Card
pick double deck
  | double > 1 = error "pick: Värdet är för stort"
  | null deck = error "pick: Listan är tom!"
  | otherwise = go double deck
  where
    go 0 (x:xs) = x
    go double' (x:xs)
      | double' /= (1.0 / fromIntegral (length (x:xs)) ) = pick double' xs
      | double' == (1.0 / fromIntegral (length (x:xs)) ) = x
    -} 

pick :: Double -> Deck -> Card
pick n deck
  | null deck = error "Empty deck"
  | n < 0 = error "Negative n has to be between 0 and 1"
  | n > 1 = error "Too large"
  | otherwise = deck !! round (fromIntegral (length deck -1) * n)


shuffle :: [Double] -> Deck -> Deck
shuffle doubles deck = go doubles deck
  where
    go [] _ = []
    go _ [] = []
    go (x:xs) d = pick x d : go xs modifiedDeck
      where
        i = round (fromIntegral (length d-1) * x)
        modifiedDeck = take i d ++ drop (i + 1) d


--måste göra så att shuffle funktionen faktiskt tar bort värdet, antingen i pick funktionen eller shuffle
--problemet ligger i att pick kommer att använda decket där vi alltid tar bort det-
--första värdet men vi tar ju aldrig bort det värdet vi faktiskt tar ur kortleken


runShuffle :: IO Deck
runShuffle = do
  Rand ds <- generate arbitrary
  return (shuffle ds fullDeck)

-- Task B5 --

belongsTo :: Card -> Deck -> Bool
c `belongsTo` []      = False
c `belongsTo` (c':cs) = c == c' || c `belongsTo` cs

prop_shuffle :: Card -> Deck -> Rand -> Bool
prop_shuffle card deck (Rand randomlist) = 
  card `belongsTo` deck == card `belongsTo` shuffle randomlist deck


--kolla så storleken på mitt deck inte ändras när jag shufflar, alltså att det behåller samma storlek
prop_size_shuffle :: Rand -> Deck -> Bool
prop_size_shuffle (Rand randomList) deck = length deck == length result
  where
    result = shuffle randomList deck



-- Task B6 --

-- follow the instructions on Canvas

implementation = Interface
  {  iFullDeck  = fullDeck
  ,  iValue     = value
  ,  iDisplay   = display
  ,  iGameOver  = gameOver
  ,  iWinner    = winner
  ,  iDraw      = draw
  ,  iPlayBank  = playBank
  ,  iShuffle   = shuffle
  }

main :: IO ()
main = runGame implementation