-- 2.1
valorAbsoluto :: Float -> Float
valorAbsoluto = abs

-- 2.2
bisiesto:: Int -> Bool
bisiesto x  | x `mod` 4 == 0 && (x `mod` 100 /= 0 || x `mod` 400 == 0)  = True
            | otherwise = False

-- 2.3
factorial :: Int -> Int
factorial 1 = 1
factorial x = x * factorial (x-1)

-- 2.4
esPrimo :: Int -> Bool
esPrimo 1 = False
esPrimo n = length (factores n) == 2
    where factores n = [x | x <- [1..n], mod n x == 0]

cantDivisoresPrimos :: Int -> Int
cantDivisoresPrimos 0 = 0
cantDivisoresPrimos x = length (filter esPrimo [y | y <- [1..x], mod x y == 0])

-- 3.1
inverso :: Float -> Maybe Float
inverso 0 = Nothing
inverso x = Just (1/x)

-- 3.2
aEntero :: Either Int Bool -> Int
aEntero (Left x) = x
aEntero (Right x) | True = 1
                  | False = 0

-- 4.1
limpiar :: String -> String -> String
limpiar _ [] = []
limpiar [] y = y
limpiar (x:xs) y = limpiar xs (filter (/= x)  y)

-- 4.2
difPromedio :: [Float] -> [Float]
difPromedio x = map (\x -> x - promedio) x
            where promedio = sum x / fromIntegral (length x)

-- 4.3
todosIguales :: [Int] -> Bool
todosIguales [] = True
todosIguales [x] = True
todosIguales (x:xs) = x == head xs && todosIguales xs

-- 5.1
data AB a = Nil | Bin (AB a) a (AB a)

vacioAB :: AB a -> Bool
vacioAB Nil = True
vacioAB _ = False

-- 5.2
negacionAB :: AB Bool -> AB Bool
negacionAB Nil = Nil
negacionAB (Bin l v r) = Bin (negacionAB l) (not v) (negacionAB r)

-- 5.3
productoAB :: AB Int -> Int
productoAB Nil = 1
productoAB (Bin l v r) = productoAB l * v * productoAB r