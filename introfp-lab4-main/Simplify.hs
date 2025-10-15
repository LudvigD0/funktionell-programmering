{- |
Module      : Simplify
Description : Skeleton for Lab 4: simplifying polynomials.
Copyright   : (c) TDA555/DIT441, Introduction to Functional Programming
License     : BSD
Maintainer  : alexg@chalmers.se
Stability   : experimental

Authors     : <list your names here>
Lab group   : <group number>
-}

module Simplify where

import Parser
import Poly
import Test.QuickCheck

-- Use the following simple data type for binary operators
data BinOp = AddOp | MulOp deriving (Eq)

--------------------------------------------------------------------------------
-- * A1

data Expr
  = Const Int            -- integer literals
  | XPow  Int            -- x^n, where n >= 0
  | Bin   BinOp Expr Expr  -- a ⊕ b, where ⊕ ∈ {AddOp, MulOp}
  deriving Eq


--------------------------------------------------------------------------------
-- * A2

prop_Expr :: Expr -> Bool
prop_Expr (Const x) = True 
prop_Expr (XPow y)  = y >= 0
prop_Expr (Bin _ l r) = prop_Expr l && prop_Expr r


invalid1 = prop_Expr (Bin AddOp (Const 1) (XPow (-5)))
invalid2 = prop_Expr (XPow (-3))
valid = prop_Expr (Bin MulOp (Const 8) (XPow 3))
valid1 = prop_Expr (Bin AddOp (Const (-4)) (XPow 4))
valid2 = prop_Expr (Const 4)
valid3 = prop_Expr (XPow 5)


quickCheckTest n k = k >= 0 ==> prop_Expr (Bin AddOp (Const n) (XPow (k)))


--------------------------------------------------------------------------------
-- * A3


instance Show Expr where
  show (Const n) = show n
  show (XPow n)  = if n == 1 then "x" else "x^" ++ show n

  show (Bin op l r) = showChild op l ++ sep op ++ showChild op r
    where
      sep AddOp = " + "
      sep MulOp = " * "

      showChild :: BinOp -> Expr -> String
      showChild MulOp e@(Bin AddOp _ _) = "(" ++ show e ++ ")"
      showChild MulOp (Const n) | n < 0 = "(" ++ show n ++ ")"
      showChild _     e                 = show e


--exempel
showEx  = (Bin AddOp (Bin AddOp (XPow 3) (XPow 4)) (Const 3))
showEx2 = (Bin MulOp (Bin AddOp (XPow 3) (Const 3)) (Const 6))



--------------------------------------------------------------------------------
-- * A4



instance Arbitrary Expr where
  arbitrary = sized go
    where
      go n
        | n <= 1    = atom
        | otherwise = oneof
            [ atom
            , Bin <$> elements [AddOp, MulOp] <*> go n' <*> go n'
            ]
        where
          n'   = n `div` 2
          atom = oneof
            [ Const <$> chooseInt (-5, 5)
            , XPow  <$> chooseInt (0, 5)
            ]

-- we tried: quickCheck (prop_Expr :: Expr -> Bool)
-- +++ OK, passed 100 tests.

--------------------------------------------------------------------------------
-- * A5

eval :: Int -> Expr -> Int
eval x e = case e of
  Const n             -> n
  XPow n              -> x ^ n                 -- n antas vara ≥ 0 enligt din invariant
  Bin AddOp l r       -> eval x l + eval x r
  Bin MulOp l r       -> eval x l * eval x r

-- Write properties!


prop_eval_add_homo :: Int -> Expr -> Expr -> Bool
prop_eval_add_homo x e1 e2 =
  eval x (Bin AddOp e1 e2) == eval x e1 + eval x e2

prop_eval_mul_homo :: Int -> Expr -> Expr -> Bool
prop_eval_mul_homo x e1 e2 =
  eval x (Bin MulOp e1 e2) == eval x e1 * eval x e2

-- Identiteter
prop_eval_add_id_left  :: Int -> Expr -> Bool
prop_eval_add_id_left  x e = eval x (Bin AddOp (Const 0) e) == eval x e

prop_eval_add_id_right :: Int -> Expr -> Bool
prop_eval_add_id_right x e = eval x (Bin AddOp e (Const 0)) == eval x e

prop_eval_mul_id_left  :: Int -> Expr -> Bool
prop_eval_mul_id_left  x e = eval x (Bin MulOp (Const 1) e) == eval x e

prop_eval_mul_id_right :: Int -> Expr -> Bool
prop_eval_mul_id_right x e = eval x (Bin MulOp e (Const 1)) == eval x e

-- Kommutativitet (+, *)
prop_eval_add_comm :: Int -> Expr -> Expr -> Bool
prop_eval_add_comm x e1 e2 =
  eval x (Bin AddOp e1 e2) == eval x (Bin AddOp e2 e1)

prop_eval_mul_comm :: Int -> Expr -> Expr -> Bool
prop_eval_mul_comm x e1 e2 =
  eval x (Bin MulOp e1 e2) == eval x (Bin MulOp e2 e1)

-- Associativitet (+, *)
prop_eval_add_assoc :: Int -> Expr -> Expr -> Expr -> Bool
prop_eval_add_assoc x e1 e2 e3 =
  eval x (Bin AddOp (Bin AddOp e1 e2) e3)
    == eval x (Bin AddOp e1 (Bin AddOp e2 e3))

prop_eval_mul_assoc :: Int -> Expr -> Expr -> Expr -> Bool
prop_eval_mul_assoc x e1 e2 e3 =
  eval x (Bin MulOp (Bin MulOp e1 e2) e3)
    == eval x (Bin MulOp e1 (Bin MulOp e2 e3))

-- Distributivitet
prop_eval_distrib :: Int -> Expr -> Expr -> Expr -> Bool
prop_eval_distrib x a b c =
  eval x (Bin MulOp a (Bin AddOp b c))
    == eval x (Bin AddOp (Bin MulOp a b) (Bin MulOp a c))

-- Potenser
prop_eval_pow1 :: Int -> Bool
prop_eval_pow1 x = eval x (XPow 1) == x

prop_eval_pow0 :: Int -> Bool
prop_eval_pow0 x = eval x (XPow 0) == 1

--------------------------------------------------------------------------------
-- * A6


exprToPoly :: Expr -> Poly
exprToPoly e = case e of
  Const n             -> fromList [n]        -- konstantpolynom
  XPow k              -> xPoly ^ k           -- x^k
  Bin AddOp l r       -> exprToPoly l + exprToPoly r
  Bin MulOp l r       -> exprToPoly l * exprToPoly r
  where
    -- x = fromList [1,0] eftersom Poly använder listor i "högst-grad-först"-format
    xPoly = fromList [1,0]

-- Define (and check) prop_exprToPoly, which checks that evaluating the
-- polynomial you get from exprToPoly gives the same answer as evaluating
-- the expression.

prop_exprToPoly :: Int -> Expr -> Bool
prop_exprToPoly i e = eval i e == evalPoly i (exprToPoly e)

--------------------------------------------------------------------------------
-- * A7
-- Now define the function going in the other direction.

-- Smart constructors (kill junk)
mkAdd :: Expr -> Expr -> Expr
mkAdd (Const 0) e         = e
mkAdd e         (Const 0) = e
mkAdd e1        e2        = Bin AddOp e1 e2

mkMul :: Expr -> Expr -> Expr
mkMul (Const 0) _         = Const 0
mkMul _         (Const 0) = Const 0
mkMul (Const 1) e         = e
mkMul e         (Const 1) = e
mkMul e1        e2        = Bin MulOp e1 e2

mkTerm :: Int -> Int -> Expr
mkTerm _ 0 = Const 0
mkTerm 0 c = Const c
mkTerm k 1 = XPow k
mkTerm k c = mkMul (Const c) (XPow k)

-- Poly -> Expr (coeffs are highest-degree-first)  [1,2,3] = x^2 + 2x + 3
polyToExpr :: Poly -> Expr
polyToExpr p =
  let cs  = toList p                     -- e.g. x^2+2x+3 -> [1,2,3]
      deg = length cs - 1
  in foldr mkAdd (Const 0) (zipWith mkTerm [deg,deg-1..0] cs)


-- Write (and check) a quickCheck property for this function similar to
-- question 6. 

prop_polyToExpr :: Int -> Poly -> Bool
prop_polyToExpr x p =
  evalPoly x p == eval x (polyToExpr p)


--------------------------------------------------------------------------------
-- * A8

simplify :: Expr -> Expr
simplify = polyToExpr . exprToPoly

--------------------------------------------------------------------------------
-- * A9

prop_noJunk :: Expr -> Bool
prop_noJunk (Const x)    = 
prop_noJunk (XPow x)     = 
prop_noJunk (Bin op x y) = prop_noJunk 

--------------------------------------------------------------------------------
-- * A10

type Difficulty = Int

diffFile :: FilePath
diffFile = "difficulty.txt"

readDifficulty :: IO Difficulty
readDifficulty = undefined

writeDifficulty :: Difficulty -> IO ()
writeDifficulty = undefined

--------------------------------------------------------------------------------
-- * A11

play :: IO ()
play = undefined

--------------------------------------------------------------------------------
-- * A12

parseExpr :: String -> Maybe Expr
parseExpr = undefined

--------------------------------------------------------------------------------
