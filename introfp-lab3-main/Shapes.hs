{- |
Module      : Shapes
Description : Types and functions for shapes. The list of all tetris pieces.
Copyright   : (c) TDA555/DIT441, Introduction to Functional Programming
License     : BSD
Maintainer  : alexg@chalmers.se
Stability   : experimental
-}

module Shapes where

import Data.List (transpose)
import Data.Maybe (isNothing)
import Test.QuickCheck

-- * Shapes

data Colour = Black | Red | Green | Yellow | Blue | Purple | Cyan | Grey
  deriving (Eq, Bounded, Enum, Show)

type Square = Maybe Colour

-- | A geometric shape is represented as a list of lists of squares. Each square
-- can be empty or filled with a block of a specific colour.

type Row   = [Square]
data Shape = Shape [Row] deriving Eq

rows :: Shape -> [Row]
rows (Shape rs) = rs

-- * Showing shapes

showShape :: Shape -> String
showShape (Shape rows) = unlines [showRow r | r <- rows]
 where
  showRow r = [showSquare s | s <- r]
    
  showSquare Nothing      = '.'
  showSquare (Just Black) = '#' -- can change to '█' on linux/mac
  showSquare (Just Grey)  = 'g' -- can change to '▓'
  showSquare (Just c)     = head (show c)

instance Show Shape where
  show = showShape
  showList ss r = unlines (map show ss) ++ r

-- * The shapes used in the Tetris game

-- | All 7 tetrominoes (all combinations of 4 connected blocks),
-- see <https://en.wikipedia.org/wiki/Tetromino>
allShapes :: [Shape]
allShapes = [Shape (makeSquares s) | s <- shapes] 
 where
   makeSquares = map (map colour)
   colour c    = lookup c [ ('I', Red),  ('J', Grey),  ('T', Blue), ('O', Yellow)
                          , ('Z', Cyan), ('L', Green), ('S', Purple) ]
   shapes = [["I",
              "I",
              "I",
              "I"],
             [" J",
              " J",
              "JJ"],
             [" T",
              "TT",
              " T"],
             ["OO",
              "OO"],
             [" Z",
              "ZZ",
              "Z "],
             ["LL",
              " L",
              " L"],
             ["S ",
              "SS",
              " S"]]

-- * Some simple functions

-- ** A1
emptyShape :: (Int, Int) -> Shape
emptyShape (y,x) = Shape (replicate y (replicate x Nothing))

-- ** A2

-- | The size (height and width) of a shape (rows x columns)

shapeSize :: Shape -> (Int, Int)
shapeSize shape 
  | null (rows shape) = (0,0)
  | otherwise = (y, x)
    where
      y = length (rows shape) --height
      x = length (head (rows shape)) --width

    

-- ** A3

-- | Count how many non-empty squares a shape contains
blockCount :: Shape -> Int
blockCount shape = length nonEmptySquares
  where
    con = concat (rows shape)
    nonEmptySquares = [t | t <- con, t /= Nothing]

-- * The Shape invariant

-- ** A4
-- | Shape invariant (shapes have at least one row, at least one column,
-- and are rectangular)


prop_Shape :: Shape -> Bool
prop_Shape shape
  | nrow <= 0 = False
  | cols <= 0 = False
  | otherwise = and [length r == cols | r <- rows shape]
    where
      (nrow, cols) = shapeSize shape 

-- * Test data generators

-- ** A5
-- | A random generator for colours
genColour :: Gen Colour
genColour = elements [Black , Red , Green , Yellow , Blue , Purple , Cyan , Grey]

instance Arbitrary Colour where
  arbitrary = genColour

-- ** A6
-- | A random generator for shapes
genShape :: Gen Shape
genShape = elements allShapes

instance Arbitrary Shape where
  arbitrary = genShape

-- * Transforming shapes

-- ** A7
-- | Rotate a shape 90 degrees
rotateShape :: Shape -> Shape
rotateShape shape = Shape (reverse (transpose (rows shape)))

--not using reverse will make the test pass but the shape will not be rotated correctly when looking in the terminal

-- ** A8
-- | shiftShape adds empty squares above and to the left of the shape
shiftShape :: (Int, Int) -> Shape -> Shape
shiftShape (y, x) shape = Shape (shiftDown' y (Shape right))
  where
    right = shiftRight' x shape

shiftRight' x shape = [replicate x Nothing ++ t | t <- rows shape]

shiftDown' y shape = replicate y (replicate col Nothing) ++ (rows shape)
  where
    (row, col) = shapeSize shape
  

-- ** A9
-- | padShape adds empty square below and to the right of the shape
padShape :: (Int, Int) -> Shape -> Shape
padShape (y, x) shape = Shape (shiftUp' y (Shape left))
  where
    left = shiftLeft' x shape

shiftLeft' x shape = [t ++ replicate x Nothing | t <- rows shape]

shiftUp' y shape = (rows shape) ++ replicate y (replicate col Nothing)
  where
    (row, col) = shapeSize shape

-- ** A10
-- | pad a shape to a given size
padShapeTo :: (Int, Int) -> Shape -> Shape
padShapeTo (sizeY, sizeX) shape = padShape (newRowLength, newColLength) shape
    where
      newRowLength = (max shsizeY sizeY) - row
      newColLength = (max shsizeX sizeX) - col
      (row, col) = shapeSize shape
      (shsizeY, shsizeX) = shapeSize shape

-- * Comparing and combining shapes

-- ** B1

-- | Test if two shapes overlap
overlaps :: Shape -> Shape -> Bool
overlaps sh1 sh2 =  or [rowsOverlap sh1Row sh2Row | (sh1Row, sh2Row) <- zip (rows sh1) (rows sh2)]

rowsOverlap :: Row -> Row -> Bool
rowsOverlap sh1Row sh2Row = or [x1 /= Nothing && x2 /= Nothing | (x1, x2) <- zip sh1Row sh2Row]


-- ** B2
-- | zipShapeWith, like 'zipWith' for lists

{- zipShapeWith f sh1 sh2 = Shape [(go f sh1Row sh2Row) | (sh1Row, sh2Row) <- zip (rows sh1) (rows sh2)] 
  where 
    go f (x:xs) (y:ys) = f x y : go f xs ys
    go _ _ _ = []
 -}
 --the function above does the exact same thing, just not declared in the way the assignment wants (and more complicated)


zipShapeWith :: (Square -> Square -> Square) -> Shape -> Shape -> Shape
zipShapeWith f sh1 sh2 = Shape (zipWith (zipWith f) (rows sh1) (rows sh2))

-- ** B3
-- | Combine two shapes. The two shapes should not overlap.
-- The resulting shape will be big enough to fit both shapes.
combine :: Shape -> Shape -> Shape
s1 `combine` s2 | overlaps s1 s2 = error "Combine: The two shapes are overlapping"
                | otherwise = (zipShapeWith (\x y -> 
                  if x /= Nothing && y == Nothing then x
                  else if y /= Nothing && x == Nothing then y 
                  else Nothing
                ) (padShapeTo size s1) (padShapeTo size s2))
                  where 
                    size = (\(a,b) (c,d) -> (max a c, max b d)) (shapeSize s1) (shapeSize s2)
                    





