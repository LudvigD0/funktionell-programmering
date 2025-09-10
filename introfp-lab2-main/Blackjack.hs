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

--skapar två testkort
aCard1 :: Card
aCard1 = Card (Numeric 8) Hearts

aCard2 :: Card
aCard2 = Card Ace Spades
--Create a test hand
aHand :: Hand
aHand = aCard1:(aCard2:[])

-- Task A1 --

hand2 :: Hand
hand2 = [Card (Numeric 2) Hearts, Card Jack Spades]

sizeSteps :: [Int]
sizeSteps = [
  size hand2,
  size (Card (Numeric 2) Hearts : (Card Jack Spades : [])),
  1 + size (Card Jack Spades : []),
  1 + (1 + size [])
  1 + (1 + 0)
  1 + 1
  2
]

-- Task A2 --

display :: Hand -> String
display = undefined

displayCard :: Card -> String
displayCard 

-- Task A3 --

value :: Hand -> Int
value = undefined

-- Task A4 --
--
gameOver :: Hand -> Bool
gameOver = undefined

winner :: Hand -> Hand -> Player
winner = undefined

--------------------------------------------------------------------------------
-- Part B
---------------------------------------------------------------------------------

-- Task B1 --
fullDeck :: Deck
fullDeck = undefined

prop_size_fullDeck :: Bool
prop_size_fullDeck = size fullDeck == 52

-- Task B2 --

draw :: Deck -> Hand -> (Deck, Hand)
draw = undefined

-- Task B3 --

playBank :: Deck -> Hand
playBank = undefined

-- Task B4 --

pick :: Double -> Deck -> Card
pick = undefined

shuffle :: [Double] -> Deck -> Deck
shuffle = undefined

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

prop_size_shuffle :: Rand -> Deck -> Bool
prop_size_shuffle (Rand randomlist) deck = undefined

-- Task B6 --

-- follow the instructions on Canvas

