--mod es resto div division / division exacta (para floats)
-- Int(enteros,+ - * div mod(resto)),Float,Bool(True False && || not)
doble :: Int-> Int
doble  x = 2*x

suma :: Float -> Float  -> Float
suma x y = x + y 


norma x1 x2= sqrt(x1**2 + x2**2)


maximo x y | x>y = x
    | otherwise = y   

esCero n | n == 0 = True
    | n /= 0 = False -- no es  /=  

mayorAUno ::Float -> Bool
mayorAUno n | n>1 = True -- tira error siempre q le de algo menor a 1
            | otherwise = False

cuantasRaices b c | b**2 - 4*c > 0 = 2
                  | b**2 - 4*c == 0 = 1
                  | otherwise = 0

esPar x | mod x 2 == 0 = True 
        | otherwise = False

esImpar x = not(esPar x)

absoluto :: Int -> Int
absoluto x | x >= 0 = x
           | otherwise = -x 

maximoEntreDos :: Int -> Int -> Int
maximoEntreDos x y | x>y = x
                   | otherwise = y 

minimoEntreDos x y | x<y = x
                   | otherwise = y 

maximoAbsoluto :: Int -> Int -> Int
maximoAbsoluto x y | absoluto x > absoluto y = absoluto x 
                   | otherwise = absoluto y

maximo3 :: Int -> Int -> Int -> Int
maximo3 x y z | x>(maximoEntreDos y z) = x
              | y>(maximoEntreDos z x) = y
              | otherwise = z

algunoEs0 :: Float -> Float -> Bool
algunoEs0 x y = (x == 0 || y == 0) 

ambosSon0 :: Float -> Float -> Bool
ambosSon0 x y = (x == 0 && y == 0)

esMultiploDe ::  Int -> Int -> Bool
esMultiploDe x y = mod (maximoEntreDos x y) (minimoEntreDos x y) == 0 

digitoUnidades :: Int -> Int
digitoUnidades x = mod x 10

digitoDecenas :: Int -> Int
digitoDecenas x = div (mod x 100) 10