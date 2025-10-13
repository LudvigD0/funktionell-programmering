{- |
Module      : Tetris
Description : The Tetris game (main module)
Copyright   : (c) TDA555/DIT441, Introduction to Functional Programming
License     : BSD
Maintainer  : alexg@chalmers.se
Stability   : experimental

Authors : <Elliot Forst, Ludvig Dahlgren, Gabriel Hasan>
Lab group : <Group 51>
-}

module Tetris where

import ConsoleGUI       -- cabal install ansi-terminal 
import Shapes

--------------------------------------------------------------------------------
-- * The code that puts all the piece together

main ::IO ()
main = runGame tetrisGame

tetrisGame :: Game Tetris
tetrisGame = Game 
  { startGame     = startTetris
  , stepGame      = stepTetris
  , drawGame      = drawTetris
  , gameInfo      = defaultGameInfo prop_Tetris
  , tickDelay     = defaultDelay
  , gameInvariant = prop_Tetris
  }

--------------------------------------------------------------------------------
-- * The various parts of the Tetris game implementation

type Pos   = (Int, Int)    -- (row index, col index)
type Piece = (Pos, Shape)
type Well  = Shape

-- | The state of the game consists of three parts:
data Tetris = Tetris 
  { piece  :: Piece    -- ^ The position and shape of the falling piece
  , well   :: Shape    -- ^ The well (the playing field), where the falling pieces pile up
  , shapes :: [Shape]  -- ^ An infinite supply of random shapes
  }

-- | The size of the well
wellSize :: (Int, Int)
wellSize   = (wellHeight, wellWidth)
wellWidth  = 10
wellHeight = 20

-- | Starting position for falling pieces
startPosition :: Pos
startPosition = (0, wellWidth `div` 2 - 1)

-- | Pos addition
add :: Pos -> Pos -> Pos
(h1, w1) `add` (h2, w2) = (h1 + h2, w1 + w2)

-- | Move the falling piece into position
place :: Piece -> Shape
place (v, s) = shiftShape v s

--B4
-- | An invariant that startTetris and stepTetris should uphold
prop_Tetris :: Tetris -> Bool
prop_Tetris t = prop_Shape s && shapeSize(well t) == wellSize 
  where 
    (pos, s) = piece t


--B5
-- | Add black walls around a shape
addWalls :: Shape -> Shape
addWalls s = Shape (lob: [[Just Black] ++ t ++ [Just Black] | t <- rows s] ++ [lob])
  where
    lob = replicate (snd (shapeSize s) + 2) (Just Black)


--B6
-- | Visualize the current game state. This is what the user will see
-- when playing the game.
drawTetris :: Tetris -> Shape
drawTetris (Tetris piece well _) = addWalls (combine (shiftShape pos s) well)  
  where
    (pos, s) = piece
--B6 END

-- | The initial game state
startTetris :: [Double] -> Tetris
startTetris rs = Tetris (startPosition, piece) well supply
 where
  well         = emptyShape wellSize
  piece:supply = [allShapes !! (min (floor ((fromIntegral (length allShapes)) * x)) 6) | x <- rs]

-- | React to input. The function returns 'Nothing' when it's game over,
-- and @'Just' (n,t)@, when the game continues in a new state @t@.
stepTetris :: Action -> Tetris -> Maybe (Int, Tetris)
stepTetris action t = case action of 
  Tick      -> tick t 
  MoveDown  -> tick t
  MoveLeft  -> Just (0, (movePiece (-1) t))
  MoveRight -> Just (0, (movePiece 1 t))
  Rotate    -> Just (0, rotatePiece t)
 

--B7
move :: (Int, Int) -> Tetris -> Tetris
move (y,x) (Tetris ((py,px), s) well shapes) = Tetris ((py+y, px+x), s) well shapes


--B8
tick :: Tetris -> Maybe (Int, Tetris)
tick t
  | collision t' = dropNewPiece t
  | otherwise    = Just (0, t')
  where
    t' = move (1, 0) t



--C1
collision :: Tetris -> Bool
collision (Tetris ((py, px), s) well shapes) = 
  px < 0
  || px + snd (shapeSize s) > wellWidth
  || py + fst (shapeSize s) > wellHeight
  || overlaps well (place ((py, px), s))
--using || that means "or", we use this instead of guards
--when one of the conditions is true, it returns true which means collision

--C2
--We added moveDown action in stepTetris

--C3
movePiece :: Int -> Tetris -> Tetris
movePiece i t
  | collision (move (0, i) t) = t                     --checking collision on the future placement
  | otherwise                 = move (0, i) t



--C4
rotate :: Tetris -> Tetris
rotate t@(Tetris {piece = (pos,s )}) = t{ piece = (pos, rotateShape s)}
--using record pattern matching to clarify where pos and s comes from
--we also return with record syntax for clarity purposes


--C5
adjust :: Tetris -> Tetris
adjust t@(Tetris ((py,px), s) well shapes)
  | px + snd (shapeSize s) > wellWidth  = movePiece dist t
  | py + fst (shapeSize s) > wellHeight = move (dist, 0) t --this row allows one rotation before it hits the ground
  | otherwise                           = t
  where 
    (row, col) = shapeSize s
    dist = (min row col) - (max row col) --calculating the distance
--using min and max, since we allow rotation at bottom when shape is wider



--C6
rotatePiece :: Tetris -> Tetris
rotatePiece t 
  | not (collision (rotate t)) =  rotate t
  | not (collision (adjust $ rotate t)) = adjust $ rotate t --dollar sign instead of paranthases
  | otherwise = t --if there a collision, then return just t

--C7
dropNewPiece :: Tetris -> Maybe (Int, Tetris)
dropNewPiece (Tetris piece well (shape:shapes)) --instead of using head & tail, we use pattern matching
  | overlaps (place (startPosition, shape)) s = Nothing
  | otherwise = Just (n, Tetris (startPosition, shape) s shapes)
  where
    (n, s) = clearLines (combine (place piece) well) --using place to update correct position before combining


--C8
--Fixed startTetris


--C9
clearLines :: Shape -> (Int, Shape)
clearLines shape = (length newRows, (Shape (newRows ++ newS))) --adding the empty rows before the filtered rows
  where
    newS = filter (elem Nothing) (rows shape) --using filter to bring along the rows containing "Nothing"
    newRows = replicate (length (rows shape) - (length newS)) (replicate col Nothing) --calculates new empty rows, based on the new rows
    (_, col) = shapeSize shape

--C10
--Implemented clearLines in dropNewPiece

