import Data.Text.Internal.Fusion.Size (larger)
qsort [] = []

qsort (x:xs) = qsort smaller ++ [] ++ qsort larger
    where
        smaller = [a | a <- xs, a <= x]
        larger = [b | b <- xs, b > x]