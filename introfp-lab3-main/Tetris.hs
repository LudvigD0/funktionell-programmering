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
  piece:supply = [allShapes !! (floor ((fromIntegral (length allShapes)) * x)) | x <- rs]

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
collision (Tetris ((py, px), s) well shapes)
  | px < 0                              = True
  | px + snd (shapeSize s) > wellWidth  = True
  | py + fst (shapeSize s) > wellHeight = True
  | overlaps well (place ((py, px), s)) = True
  | otherwise                           = False

--C2
--We added moveDown action in stepTetris

--C3
movePiece :: Int -> Tetris -> Tetris
movePiece i t
  | collision (move (0, i) t) = t                     --checking collision on the future placement
  | otherwise                 = move (0, i) t



--C4
rotate :: Tetris -> Tetris
rotate Tetris { piece = (pos, s), well, shapes } = Tetris (pos, rotateShape s) well shapes 
--using record pattern matching to clarify where pos and s comes from


--C5
adjust :: Tetris -> Tetris
adjust t@(Tetris ((py,px), s) well shapes)
  | px + snd (shapeSize s) > wellWidth  = movePiece dist t
  | py + fst (shapeSize s) > wellHeight = move (dist, 0) t
  | otherwise                           = t
  where 
    (row, col) = shapeSize s
    dist = (min row col) - (max row col) --calculating the dist since this adjust function also allow rotation on the bottom when shape is more wide than narrow



--C6
rotatePiece :: Tetris -> Tetris
rotatePiece t 
  | not (collision (rotate t)) =  rotate t
  | not (collision (adjust $ rotate t)) = adjust $ rotate t
  | otherwise = t

--C7
dropNewPiece :: Tetris -> Maybe (Int, Tetris)
dropNewPiece (Tetris piece well shapes)
  | overlaps (place (startPosition, (head shapes))) s = Nothing
  | otherwise = Just (n, Tetris (startPosition, (head shapes)) s (tail shapes))
  where
    (n, s) = clearLines (combine (place piece) well) 


--C8
--Fixed startTetris


--C9
clearLines :: Shape -> (Int, Shape)
clearLines shape = (length newRows, (Shape (newRows ++ newS)))
  where
    newS = [row | row <- rows shape, elem Nothing row]                  --retunerar en row när det finns Nothing i den, alltså är den ej "klar"
    newRows = replicate (length (rows shape) - (length newS)) (replicate col Nothing)
    (row, col) = shapeSize shape
    

--C10
--Implemented clearLines in dropNewPiece

