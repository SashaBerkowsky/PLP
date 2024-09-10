-- 2.1
curry :: ((a, b) -> c) -> a -> b -> c
curry f a b = f(a, b)

-- 2.2
uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (a, b) = f a b

-- 3.1
sum :: Num a => [a] -> a
sum = foldr (+) 0 

elem :: Eq a => a -> [a] -> Bool
elem e = foldr (\x rec -> x == e || rec) False

concat :: [a] -> [a] -> [a]
concat = flip (foldr (:)) 

filter :: (a -> Bool) -> [a] -> [a]
filter f = foldr (\x rec -> if f x then x : rec else rec) []

map :: (a -> a) -> [a] -> [a]
map f = foldr (\x rec -> f x : rec) []

-- 3.2
mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun f = foldr1 (\x rec -> if f x rec then x else rec)

-- 3.3
sumasParciales :: Num a => [a] -> [a]
sumasParciales = foldl (\rec x -> rec ++ (if null rec then [x] else [x + last rec])) []

-- 3.4 ACLARACION: guardo en la pos 0 el valor, pos 1 si es par o no
sumaAlt :: Num a => [a] -> a
sumaAlt = snd . foldr (\x (pos, acc) -> (pos + 1, if even pos then x - acc else x + acc)) (0, 0)

-- 4.1
permutaciones :: [a] -> [[a]]
permutaciones = 