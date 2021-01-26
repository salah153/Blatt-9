data MultTree a = DataKnoten a | IndexKnoten a a [MultTree a] deriving (Show)

t1 :: MultTree Int
t1 = IndexKnoten 3 42 [IndexKnoten 3 15 [DataKnoten 3, DataKnoten 11, DataKnoten 12], IndexKnoten 19 42 [DataKnoten 42, DataKnoten 23]]

verzweigungsgrad :: MultTree a -> Int
verzweigungsgrad (IndexKnoten _ _ []) = 0
verzweigungsgrad (IndexKnoten _ _ ((IndexKnoten c d (x : xs)) : (y : ys))) = max (max (len (x : xs)) (len (IndexKnoten c d (x : xs) : xs))) (verzweigungsgrad y)
verzweigungsgrad (IndexKnoten _ _ x) = len x

len :: [a] -> Int
len (x : xs) = 1 + len xs
len [] = 0

datenListe :: MultTree a -> [a]
datenListe (DataKnoten a) = [a]
datenListe (IndexKnoten _ _ []) = []
datenListe (IndexKnoten a b (x : xs)) = datenListe x ++ datenListe (IndexKnoten a b xs)

datenIntervalle :: MultTree Int -> MultTree Int
datenIntervalle (IndexKnoten _ _ []) = IndexKnoten minBound maxBound []
datenIntervalle (IndexKnoten _ _ ((DataKnoten x) : xs)) = IndexKnoten (getMin (getValue (DataKnoten x : xs))) (getMax (getValue (DataKnoten x : xs))) (DataKnoten x : xs)
datenIntervalle (IndexKnoten a b x) = IndexKnoten a b (datenIntervalleList x)

getMinIndex :: [MultTree Int] -> [Int]
getMinIndex [] = []
getMinIndex ((IndexKnoten a b _) : xs) = max a b : getMinIndex xs

getMaxIndex :: [MultTree Int] -> [Int]
getMaxIndex [] = []
getMaxIndex ((IndexKnoten a b _) : ys) = min a b : getMaxIndex ys

getValue :: [MultTree Int] -> [Int]
getValue [] = []
getValue ((DataKnoten y) : ys) = y : getValue ys

datenIntervalleList :: [MultTree Int] -> [MultTree Int]
datenIntervalleList [] = []
datenIntervalleList (x : xs) = datenIntervalle x : datenIntervalleList xs

getMax :: [Int] -> Int
getMax [] = minBound
getMax (x : xs) = if x > getMax xs then x else getMax xs

getMin :: [Int] -> Int
getMin [] = maxBound
getMin (x : xs) = if x < getMin xs then x else getMin xs

contains :: Int -> MultTree Int -> Bool
contains x (DataKnoten y) = x == y
contains x (IndexKnoten a b as)
  | a > x || x > b = False
  | a == x || b == x = True
  | otherwise = containsList x as

containsList :: Int -> [MultTree Int] -> Bool
containsList x [] = False
containsList x (a : as) = if contains x a then True else containsList x as