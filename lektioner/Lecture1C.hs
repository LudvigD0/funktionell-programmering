
a :: Int
a = 42


f :: Float
f = 0.5

e :: String
e = "Testing"

c :: Char
c = 't'

plusOne :: Int -> Int
plusOne x = x + 1


multiply :: Int -> Int -> Int
multiply x y = x * y

divide :: Double -> Double -> Double
divide a b = a / b

greet name = "Tets" ++ name



xs = [1,2,3,4,5,6,7]



tup2 :: (String, Int)
tup2 = ("test", 47)

tup3 = (a, b, xs)
tup4 = (1, True, 'C', false)

fun :: String -> (String, Int)
fun name = (name, length name)


