import Test.QuickCheck (Gen, generate, sample, oneof, arbitrary, elements, choose)

data Tree a = Empty | Node (Tree a) a (Tree a) deriving (Eq, Show)

leaf :: a -> Tree a
leaf x = Node Empty x Empty


t :: Tree Int
t = Node (Node (leaf 2) 3 (leaf 4)) 6 (Node (leaf 7) 8 (leaf 9))

sizeTree :: Tree a -> Int
sizeTree t = case t of
    Empty      -> 0
    Node l _ r -> sizeTree l + 1 + sizeTree r

height :: Tree a -> Int
height t = case t of
    Empty      -> -1
    Node l _ r -> 1 + max (height l) (height r)



mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f tree = case tree of
    Empty      -> Empty
    Node l x r -> Node (mapTree f l) (f x) (mapTree f r)


data Expr
    = Num Int
    | Add Expr Expr
    | Mul Expr Expr
    deriving (Eq)

e1, e2 :: Expr
e1 = Mul (Add (Num 1) (Num 2)) (Num 3)
e2 = Add (Num 1) (Mul (Num 2) (Num 3))


showExpr :: Expr -> String
showExpr expr = case expr of
    Num n   -> show n
    Add x y -> showExpr x ++ " + " ++ showExpr y
    Mul x y -> showFactor x ++ " * " ++ showFactor y
  where
    showFactor (Add x y) = "(" ++ showExpr (Add x y) ++ ")"
    showFactor e         = showExpr e


instance Show Expr where
    show = showExpr

instance Num Expr where
    (+) = Add
    (*) = Mul
    fromInteger = Num . fromInteger



eval :: Expr -> Int
eval expr = case expr of 
    Num n   -> n
    Add x y -> eval x + eval y
    Mul x y -> eval x * eval y


genExprBad :: Gen Expr
genExprBad = oneof [genNum, genOp]
    where
        genNum = do
            n <- choose (0,10)
            return (Num n)

        genOp = do
            op <- elements [Add, Mul]
            e1 <- genExprBad
            e2 <- genExprBad
            return (op e1 e2)

genExpr :: Int -> Gen Expr
genExpr n
      | n < 2 = genNum
      | otherwise = genOp
    where
      genNum = do
          n <- choose (0,10)
          return (Num n)
  
      genOp = do
          op <- elements [Add, Mul]
          m <- choose (1, n-1)
          e1 <- genExpr (n - m)
          e2 <- genExpr m
          return (op e1 e2)


difficulty :: Int
difficulty = 4

quiz :: IO ()
quiz = do 
    expr <- generate (genExpr difficulty)
    putStrLn $ "Solve this expression: " ++ showExpr expr
    putStr "> "
    answer <- readLn
    if eval expr == answer
        then putStrLn "Well done!"
        else putStrLn $ "Bummer, it should have been " ++ show (eval expr)




(.+) :: Expr -> Expr -> Expr
Num 0 .+ x     = x
x .+ Num 0     = x
Num x .+ Num y = Num (x + y)
x .+ y         = Add x y

