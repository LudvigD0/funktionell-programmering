




interLeave :: [a] -> [a] -> [a]
interLeave [] [] = []
interLeave [] (y:ys) = y : interLeave [] ys
interLeave (x:xs) [] = x : interLeave xs []
interLeave (x:xs) (y:ys) = x : y : interLeave xs ys





data Some a = One a | Two a a deriving (Eq, Show)


flatten :: [Some a] -> [a]
flatten []     = []
flatten (x:xs) = convert x
    where
        convert (One x)   = x : flatten xs
        convert (Two x y) = x : y : flatten xs



type Name = String
type Number = Int
data PhoneBook = Empty | Insert Name Number PhoneBook deriving Show

filterBook :: (Name -> Bool) -> PhoneBook -> PhoneBook
filterBook _ Empty = Empty
filterBook p (Insert name number phonebook) = if p name then (Insert name number (filterBook p phonebook)) else (filterBook p phonebook)

phoneBook :: PhoneBook
phoneBook = Insert "Alex" 6154 (Insert "Dave" 1059 Empty)

























data Expr
    = Val Value
    | Add Expr Expr
    | Mul Expr Expr
    | And Expr Expr
    | Or Expr Expr
    | If Expr Expr Expr
    deriving Show

data Value = Num Int | Bool Bool deriving Show


int :: Int -> Expr
int n = Val (Num n)

true, false :: Expr
true = Val (Bool True)
false = Val (Bool False)



valueToBool :: Expr -> Bool
valueToBool (Val (Bool b)) = b
valueToBool (And x y) = (valueToBool x) && (valueToBool y) 
valueToBool (Or x y) = (valueToBool x) || (valueToBool y)

valueToInt :: Expr -> Int
valueToInt (Val (Num n)) = n
valueToInt (Add x y)     = (valueToInt x) + (valueToInt y)
valueToInt (Mul x y)     = case (x, y) of
  (Val (Num a), Val (Num b)) -> (valueToInt x) * (valueToInt y)
  _         -> error "testing lala"
  
  
  
  

{- valueToInt :: Expr -> Int
valueToInt x = const  -}

eval :: Expr -> Value

eval (Val (Num x))          = Num (valueToInt (Val (Num x))) 
eval (Val (Bool b))         = Bool (valueToBool (Val (Bool b)))

eval (Add x y)              = Num ((valueToInt x) + (valueToInt y))
eval (Mul x y)              = Num ((valueToInt x) * (valueToInt y))
eval (And x y)              = Bool ((valueToBool x) && (valueToBool y))
eval (Or x y)               = Bool ((valueToBool x) || (valueToBool y))
eval (If x y z)             = if valueToBool x then eval y else eval z


{- 

data Test = Lala | Boba

testing :: Test -> Test -> Bool
testing x y = case (x, y) of
  (Lala, Lala) -> True
  (Boba, Boba) -> True

  otherwise -> False

 -}






















data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show
type List a = [(Tree a, Int)]



empty :: List a
empty = []

add :: a -> List a -> List a
x `add` ((l, n) : (r, m) : ts) | n == m = (Node l x r, 2 * n + 1) : ts
x `add` ts = (Leaf x, 1) : ts



size :: List a -> Int
size []     = 0
size (x:xs) = (snd x) + size xs






fromList :: [a] -> List a
fromList []     = empty
fromList (x:xs) = add x (fromList xs)



hd :: List a -> a
hd (x:xs) = case (fst x) of 
  (Leaf x) -> x
  (Node l x r) -> x