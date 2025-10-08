module CheckPartA where

import Control.Exception (evaluate)
import Data.List (nub, transpose, (\\))
import Data.Maybe
import Test.Hspec
import Test.QuickCheck
import GameInterface

-- Import the student's code
import Shapes ( Shape(..), Square, Row, Colour(..)
              , emptyShape, shapeSize, blockCount, prop_Shape, rotateShape
              , shiftShape, padShape, padShapeTo, rows
              , genColour
              -- Part B
              , overlaps, zipShapeWith, combine
              )

import Tetris ( Tetris(..), Pos, Piece, wellSize, wellHeight, wellWidth
              , prop_Tetris, addWalls, drawTetris, move, tick
              -- Parc C
              , collision, tick, stepTetris, movePiece, rotate, rotatePiece
              , dropNewPiece, startTetris, clearLines
              )

-- ToDo: define newtype for Shape and implement shrinking. 

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

genShape :: Gen Shape
genShape = elements allShapes

genEmpty :: Gen Shape
genEmpty = curry emptyShape <$> choose (0, 100) <*> choose (0, 100)

(<+>) :: Gen a -> Gen b -> Gen (a, b)
g1 <+> g2 = (,) <$> g1 <*> g2

apply :: Int -> (a -> a) -> a -> a
apply n f = (!! n) . iterate f

applyM :: Int -> (a -> Maybe a) -> a -> Maybe a
applyM 0 _ x = Just x
applyM n f x = case f x of
  Just y  -> applyM (n-1) f y
  Nothing -> Nothing

pos :: Tetris -> Pos
pos = fst . piece

shape :: Tetris -> Shape
shape = snd . piece

p0 :: Piece
p0 = ((0,0), head allShapes)

t0 :: Tetris
t0 = Tetris p0 (emptyShape wellSize) allShapes

eqt :: Tetris -> Tetris -> Bool
Tetris p0 w0 s0 `eqt` Tetris p1 w1 s1 = p0 == p1 && w0 == w1 && s0 == s1

eqtM :: Tetris -> Maybe (Int, Tetris) -> Bool
eqtM t = maybe False (eqt t . snd)

walls :: Shape -> [Row]
walls (Shape rs) = [head rs, last rs, head (transpose rs), last (transpose rs)]

black :: Square
black = Just Black

mkRow :: Square -> Row
mkRow = replicate wellWidth 

blackWalls :: Shape -> Bool
blackWalls = all (all (== black)) . walls

extract :: (Int, Int) -> (Int, Int) -> Shape -> Shape
extract (r, c) (m, n) (Shape rs) = 
  Shape $ map (take n) $ take m $ map (drop c) $ drop r rs

getDoubles :: IO [Double]
getDoubles = generate $ infiniteListOf $ choose (0, 1)

prop_step :: Tetris -> Bool
prop_step t = and
  [ prop_Shape (snd (piece t)) 
  , shapeSize (well t) == wellSize 
  , prop_Shape (drawTetris t)
  , not (collision t) ]

genWell :: Gen (Int, Shape)
genWell = do 
  n <- choose (0, wellHeight)
  let bs = replicate n (mkRow black)
      ws = replicate (wellHeight - n) (mkRow Nothing)
  return (n, Shape $ ws ++ bs)

main :: IO ()
main = hspec $ do
  describe "Task C1: overlaps" $ do
    it "Too far to the left should collide: "  $ 
      collision $ move (0, -1) t0
    it "Too far to the right should collide: " $ 
      collision $ move (0, wellWidth) t0
    it "Too far down should collide" $ 
      collision $ move (wellHeight, 0) t0
    it "Collision with the well: " $ 
      collision $ t0 { well = well t0 `combine` (allShapes !! 0) }
    it "No collision: " $ conjoin $ 
      map not [collision t0, collision (move (1,0) t0), collision (move (0,1) t0)]

  describe "Task C1: tick" $ do
    it "Tick should move the piece down: " $ case tick t0 of
      Just (0, t1) -> pos t1 == (1, 0)
      _            -> False

  describe "Task C2: stepTetris" $ do
    it "A MoveDown event should drop the piece: " $ property $
      forAll (choose (1, 10)) $ \n -> 
        case applyM n (fmap snd . stepTetris MoveDown) t0 of
          Just tn -> pos tn == (n, 0)
          _       -> False

  describe "Task C3: movePiece" $ do
    it "Calling with 1 should move one pos to the right: " $ 
      movePiece 1 t0 `eqt` t0 { piece = ((0, 1), shape t0) }

    it "Calling with (-1) should move one pos to the left: " $ 
      movePiece (-1) (movePiece 1 t0) `eqt` t0

    it "A move resulting in a collision should be ignored: " $ 
      movePiece (-1) t0 `eqt` t0

    it "Should handle the MoveLeft action: " $ 
      movePiece (-1) t0 `eqtM` stepTetris MoveLeft t0 

    it "Should handle the MoveRight action: " $ 
      movePiece 1 t0 `eqtM` stepTetris MoveRight t0 

  describe "Task C4: rotate" $ do
    it "Rotates piece: " $ shape (rotate t0) == rotateShape (snd p0)

  describe "Task C5: adjust" $ do
    it "The adjust function is optional and not tested: " True
  
  describe "Task C6: rotatePiece" $ do
    it "Should rotate if no collision: " $ let t1 = apply 5 (movePiece 1) t0 in
      shape (rotatePiece t1) == rotateShape (snd p0)

    it "Handle the Rotate action: " $ let t1 = apply 5 (movePiece 1) t0 in
      rotatePiece t1 `eqtM` stepTetris Rotate t1

  describe "Task C7: dropNewPiece" $ do
    it "Should change the well, get a shape from the supply, and not collide: " $ 
      case dropNewPiece t0 of
        Just (_, t1) -> 
          conjoin [ well t0 /= well t1  -- this can be more precise, but would give away the answer
                  , head (shapes t0) == shape t1
                  , not (collision t1) 
                  ]
        _ -> property False

  describe "Task C8: startTetris" $ do
    ds <- runIO getDoubles 
    it "The supply should offer all kinds of shapes: " $ 
      let t = startTetris ds in null (allShapes \\ take 1000 (shapes t))
    
  describe "Task C9: clearLines" $ do
    it "Should clear the right amount of lines: " $ property $ forAll genWell $ 
      \(n, w) -> let (m, s) = clearLines w in conjoin
        [ n === m 
        , drop n (rows s) === take (wellHeight - n) (rows w)
        , blockCount (Shape $ take n (rows s)) === 0
        ]

  describe "Task C10: test all" $ do
    t <- startTetris <$> runIO getDoubles 
    it "Play random moves, all steps should be valid: " $ property $ 
      forAll genActions $
        all snd . catMaybes . scanr step (Just (t0, True))

step action m = do 
  (t1, _) <- m
  (n, t2) <- stepTetris action t1
  return (t2, prop_step t2)

genActions :: Gen [Action]
genActions = listOf (elements [Tick, MoveLeft, MoveRight, MoveDown, Rotate])
