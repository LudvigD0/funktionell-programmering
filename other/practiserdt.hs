


data List a = Nil | Cons a (List a) deriving Show


xs :: List Int
--xs = 1 `Cons` (2 `Cons` (3 `Cons` Nil))
xs = Cons 1 (Cons 2 (Cons 3 (Cons 6 Nil)))


ys :: List String
--ys = "alex" `Cons` ("gerdes" `Cons` Nil)
ys = Cons "Alex" (Cons "Gerdes" Nil)


prod :: Num a => List a -> a
prod Nil         = 1
prod (Cons x xs) = x * prod xs

showL :: Show a => List a -> String

showL xs = "[" ++ go xs ++ "]"
    where
        go Nil = ""
        go (Cons x Nil) = show x
        go (Cons x xs) = show x ++ ", " ++ go xs


toList :: [a] -> List a
toList xs = foldr (Cons) Nil xs


data Name = First String | Middle Name String deriving Show

ludvig :: Name
ludvig = Middle


data Car = 