module CheckPartB where

import Data.List (nub, transpose, (\\))
import Test.Hspec
import Test.QuickCheck

-- Import the student's code
import Shapes ( Shape(..), Square, Row, Colour(..)
              , emptyShape, shapeSize, blockCount, prop_Shape, rotateShape
              , shiftShape, padShape, padShapeTo, rows
              , genColour
              -- Part B
              , overlaps, zipShapeWith, combine
              )

import Tetris ( Tetris(..), wellSize
              , prop_Tetris, addWalls, drawTetris, move, tick
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

t0 :: Tetris
t0 = Tetris ((0, 0), allShapes !! 0) (emptyShape wellSize) allShapes

walls :: Shape -> [Row]
walls (Shape rs) = [head rs, last rs, head (transpose rs), last (transpose rs)]

blackWalls :: Shape -> Bool
blackWalls = all (all (== Just Black)) . walls

extract :: (Int, Int) -> (Int, Int) -> Shape -> Shape
extract (r, c) (m, n) (Shape rs) = 
  Shape $ map (take n) $ take m $ map (drop c) $ drop r rs

main :: IO ()
main = hspec $ do
  describe "Task B1: overlaps" $ do
    it "Tetrominos should not overlap with an empty shape: " $ property $
      forAll genEmpty $ \s -> all (not . overlaps s) allShapes

    it "A tetromino overlaps with itself: " $ 
      and $ zipWith overlaps allShapes allShapes

    it "A tetromino overlaps with any other tetromino: " $ property $
      forAll (genShape <+> genShape) $ uncurry overlaps

  describe "Task B2: zipShapeWith" $ do
    it "Zipping with an empty shape of equal size: " $ property $ 
      forAll genShape $ \s -> let e = emptyShape (shapeSize s) in 
        zipShapeWith const s e == s

    it "Check size: " $ property $ 
      forAll (genEmpty <+> genEmpty) $ \(x, y) -> 
        let s        = zipShapeWith const x y 
            (xr, xc) = shapeSize x
            (yr, yc) = shapeSize y
        in  shapeSize s == (min xr yr, min xc yc)
        
  describe "Task B3: combine" $ do
    it "Combining with an empty shape of same size: " $ forAll genShape $ \s ->
      let e = emptyShape (shapeSize s) in combine s e == s && combine e s == s

    it "Should crash on overlapping shapes: " $ property $ 
      forAll genShape $ \s -> print (combine s s) `shouldThrow` anyException

  describe "Task B4: prop_Tetris" $ do
    it "Should detect incorrect well size" $
      let t = t0 { well = emptyShape (0, 0) } in not (prop_Tetris t)

    it "Should detect invalid shape" $ 
      let t = t0 { piece = ((0, 0), emptyShape (0, 0)) } in not (prop_Tetris t)

  describe "Task B5: addWalls" $ do
    it "The size should increase: " $ property $ forAll genEmpty $ \s ->
      let (m, n) = shapeSize s in shapeSize (addWalls s) == (m+2, n+2)

    it "The border should be black" $ property $ forAll genEmpty $ \s ->
      blackWalls (addWalls s)

  describe "Task B6: drawTetris" $ do
    it "Should be a valid shape of the right size with black walls: " $
      let s = drawTetris t0
      in  conjoin [shapeSize s == (22, 12), prop_Shape s, blackWalls s] 

    let g = choose (0, 9)
    it "The falling piece should be drawn: " $ property $ forAll (g <+> g) $ 
      \(x, y) -> let t = drawTetris (move (x, y) t0) 
                     s = snd (piece t0)
                 in  extract (x+1, y+1) (shapeSize s) t === s

  describe "Task B7: move" $ do
    it "Should (only) change the position of the piece: " $ property $ \m n -> 
      let t1 = t0 { piece = ((m, n), allShapes !! 0) }
          tm = move (m, n) t0 
      in  conjoin [piece tm == piece t1, well tm == well t1, shapes tm == shapes t1]

  describe "Task B8: tick" $ do
    let g = choose (0, 9)
    it "Expands the right amount" $ property $ forAll (g <+> g) $ \to ->
      case tick (move to t0) of
        Just (0, t1) -> let (m, n) = fst (piece t1) in (m-1, n) == to
        _            -> False

