{- |
Module      : Simplify
Description : Skeleton for Lab 4: simplifying polynomials.
Copyright   : (c) TDA555/DIT441, Introduction to Functional Programming
License     : BSD
Maintainer  : alexg@chalmers.se
Stability   : experimental

Authors     : <Ludvig Dahlgren, Elliot Frost, Gabbe Hasan>
Lab group   : <51>
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

prop_eval_add :: Int -> Expr -> Expr -> Bool
prop_eval_add x e1 e2 = eval x (Bin AddOp e1 e2) == eval x e1 + eval x e2

prop_eval_mul :: Int -> Expr -> Expr -> Bool
prop_eval_mul x e1 e2 =
  eval x (Bin MulOp e1 e2) == eval x e1 * eval x e2


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
prop_noJunk = go . simplify  
  where
    go (Const _)         = True
    go (XPow n)          = n /= 0 
    go (Bin AddOp l r)   =
      not (isZero l || isZero r || bothConst l r) && go l && go r
    go (Bin MulOp l r) =
      not (isZero l || isZero r || isOne l || isOne r || bothConst l r) && go l && go r

    isZero  (Const 0) = True
    isZero  _ = False
    isOne   (Const 1) = True  
    isOne   _ = False
    isConst (Const _) = True  
    isConst _ = False
    bothConst a b = isConst a && isConst b


--------------------------------------------------------------------------------
-- * A10



type Difficulty = Int

diffFile :: FilePath
diffFile = "difficulty.txt"

readDifficulty :: IO Difficulty
readDifficulty = do
  contents <- readFile "difficulty.txt"
  return (read contents)


writeDifficulty :: Difficulty -> IO ()
writeDifficulty d = writeFile "difficulty.txt" (show d)    -- write plain number, no newline


--------------------------------------------------------------------------------
-- * A11

play :: IO ()
play = do
  d0 <- readDifficulty
  loop d0
  where
    loop d = do
      -- generate an expression sized by difficulty + a small x (avoid overflow)
      e <- generate (resize (max 1 d) (arbitrary :: Gen Expr))
      x <- generate (chooseInt (1,4))
      let s       = simplify e
          correct = eval x s

      putStrLn $ "Simplify the following expression with x = " ++ show x
      putStrLn ""
      print s
      putStr "> "
      ans <- getLine
      case (reads ans :: [(Int,String)]) of
        [(guess,"")] ->
          if guess == correct
            then do
              putStrLn "Well done!"
              let d' = d + 1
              writeDifficulty d'
              loop d'
            else do
              putStrLn $ "No, it should have been " ++ show correct ++ "."
              let d' = max 0 (d - 1)
              writeDifficulty d'
              loop d'
        _ -> do
          putStrLn "Please enter an integer."
          loop d


--------------------------------------------------------------------------------
-- * A12

parseExpr :: String -> Maybe Expr
parseExpr = parse (trim pExpr)
  where
    pExpr   = chainl pTerm   (op '+' AddOp)
    pTerm   = chainl pFactor (op '*' MulOp)
    pFactor = (Const <$> trim int) <|> pXPow <|> parens pExpr
    pXPow   = trim $ XPow <$> (char 'x' *> ((char '^' *> nat) <|> pure 1))
    op c o  = Bin o <$ trim (char c)



prop_showParse_semantic :: Int -> Expr -> Bool
prop_showParse_semantic x e = go (parseExpr (show e))
  where
    go (Just e') = eval x e == eval x e'
    go Nothing = False


--------------------------------------------------------------------------------
