g :: ([a] -> p) -> [([a] -> p) -> [a] -> p] -> p
g x (y : ys) = g (y x) ys
g x y = x []

f :: (Num a, Ord a) => a -> [a] -> a -> [a]
f 1 ys _ = ys
f x (y : ys) z = if x > z then f (x -1) (x : ys) z else (y : ys)

h :: [Int] -> Int -> [Int] -> Bool
h [] x y = if x == 1 then h y (length y) [] else True
h (a : as) x y = h as (x + a) y