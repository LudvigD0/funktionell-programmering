import Prelude hiding (gcd)
import Data.List 

-- | Question 1 ----------------------------------------------------------------

f :: a -> [a] -> [a]
f x (y:z:zs) = y : x : f x (z:zs)
f _  xs      = xs

g :: [a] -> [[a]] -> [a]
g xs xss = concat (f xs xss)

{-
  g " " ["intro", "func", "prog"]
= { def g }
  (concat (f " " ["intro", "func", "prog"])
= { def f }
  concat ("intro" : " " : f " " ["func", "prog"])
= { def f }
  concat ("intro" : " " : "func" : " " : f " " ["prog"])
= { def f }
  concat ("intro" : " " : "func" : " " : ["prog"])
= { list notation }  
  concat ["intro", " ", "func", " ", "prog"]
= { apply concat }
  "intro func prog"
-}






-- | Question 2 ----------------------------------------------------------------

gcd :: (Int, Int) -> Int
gcd (x, 0) = x
gcd (x, y) = gcd (y, x `mod` y)

coprime :: Int -> Int -> Bool
coprime x y = gcd (x, y) == 1

-- | Question 3 ----------------------------------------------------------------

data DTree a
  = Decision a 
  | Question String (DTree a) (DTree a)
  deriving (Eq, Show)

mapDecision :: (a -> b) -> DTree a -> DTree b
mapDecision f d = case d of 
  Decision x        -> Decision (f x)
  Question q yes no -> Question q (mapDecision f yes) (mapDecision f no)

-- | Question 4 ----------------------------------------------------------------

takeDecision :: DTree a -> IO a
takeDecision (Decision d) = return d
takeDecision (Question q yes no) = do
  putStr $ q ++ "  (answer y/n)\n> "
  answer <- getLine
  takeDecision $ if answer == "y" then yes else no

-- | Question 5 ----------------------------------------------------------------

type Pixel = (Int, Int)

data BillBoard = BB { size :: (Int, Int), actives :: [Pixel] } deriving Show

inactives :: BillBoard -> Int
inactives (BB (n, m) ps) = n * m - length ps

invert :: BillBoard -> BillBoard
invert (BB (n, m) ps) = BB (n, m) ([(i, j) | i <- [0..n-1], j <- [0..m-1]] \\ ps)

-- | Question 6 ----------------------------------------------------------------

prop_odd :: (Int, Int) -> Bool
prop_odd (x, y) = odd x || odd y

prop_lcm :: (Int, Int) -> Bool
prop_lcm (x, y) = lcm x y == x * y

-- | Question 7 ----------------------------------------------------------------

data Customer = Customer
  { name  :: String
  , phone :: Int
  , plan  :: Plan
  } deriving Show

data Plan 
  = Subscription Int Int
  | Prepaid Int 
  deriving Show

data PhoneCompany = PC
  { customers :: [Customer]
  , vat       :: String         -- can be Int as well
  }

-- | Question 8 ----------------------------------------------------------------

type HTML = [Tag]

data Tag
  = Text  String
  | Open  String
  | Close String
  deriving (Eq, Show)

render :: HTML -> String
render = concatMap go
 where
  go x = case x of
    Text  txt -> txt
    Open  tag -> "<"  ++ tag ++ ">"
    Close tag -> "</" ++ tag ++ ">"

checkHtml :: HTML -> Bool
checkHtml = go []
 where
  go stack [] = null stack             -- all tags are closed
  go stack (x:xs) = case x of
    Text _  -> go stack xs             -- text is always OK, continue with the rest
    Open t  -> go (t:stack) xs         -- put an open tag on the stack
    Close t -> case stack of
      []       -> False                -- missing open tag
      (t':ts)  -> t == t' && go ts xs  -- tags should match and check the rest

-- | Question 9 ----------------------------------------------------------------

type Name = String

data Expr 
  = Num Int             -- Literal integer
  | Add Expr Expr       -- Addition
  | Mul Expr Expr       -- Multiplication division
  | Let Name Expr Expr  -- let x = e1 in e2
  | Var Name            -- variable    
  deriving (Show) 

eval :: Expr -> Maybe Int
eval = go []
 where
  go env expr = case expr of
    Num n -> return n
    Add x y -> do
      x' <- go env x 
      y' <- go env y
      return (x' + y')
    Mul x y -> do
      x' <- go env x
      y' <- go env y
      return (x' * y')
    Let n x y -> do
      x' <- go env x 
      go ((n, x'):env) y
    Var n -> lookup n env

