ejemplo = [1,2,3,4]  

ejemploHead = head [1,2,3,4] -- es 1
ejemploTail = tail [1,2,3,4]  -- es [2,3,4]

ejemploPush = 1 : [2,3,4]

sumatoria [] = 0 
sumatoria xs = sumatoria(tail xs) + head xs  

longitud [] = 0
longitud xs = longitud(tail xs) + 1

pertenece  :: Int -> [Int] -> Bool
pertenece x [] = False
pertenece x xs | x == head xs = True
               | otherwise = pertenece x (tail xs)

productoria :: [Int] -> Int
productoria [] = 1
productoria xs = productoria(tail xs) * head xs  

sumarN :: Int -> [Int] -> [Int]
sumarN n [] = []
sumarN n xs = (head xs + n) : sumarN n (tail xs)

sumarElPrimero :: [Int] -> [Int]
sumarElPrimero xs = sumarN (head xs) xs

ultimo xs | longitud xs == 1 = head xs
          | otherwise = ultimo (tail xs)

sumarElUltimo :: [Int] -> [Int]
sumarElUltimo xs = sumarN (ultimo xs) xs

esPar x | mod x 2 == 0 = True
        | otherwise = False

paresAux :: [Int] -> [Int] -> [Int]
paresAux xs xs2 | xs == [] = xs2
             | esPar (head xs) == False =  paresAux(tail xs) xs2
             | otherwise = paresAux (tail xs) ((head xs) : xs2)

pares xs = paresAux xs []

esMulpitploN n m | mod m n == 0 = True
                 | otherwise = False
multiplosDeNAux :: Int -> [Int] -> [Int] -> [Int]
multiplosDeNAux n xs xs2 | xs == [] = xs2
             | esMulpitploN n (head xs) == False =  multiplosDeNAux n (tail xs) xs2
             | otherwise = multiplosDeNAux n (tail xs) ((head xs) : xs2)

multiplosDeN :: Int -> [Int] -> [Int]
multiplosDeN n xs = multiplosDeNAux n xs []

reversoAux :: [Int] -> [Int] -> [Int]
reversoAux [] xs2 = xs2
reversoAux xs xs2 = reversoAux (tail xs) (head xs : xs2)

reverso :: [Int] -> [Int]
reverso xs = reversoAux xs []

maximoAux :: [Int] -> Int -> Int
maximoAux xs x | xs == [] = x
               | head xs > x = maximoAux (tail xs) (head xs)
               | otherwise = maximoAux (tail xs) x

maximo :: [Int] -> Int
maximo xs = maximoAux xs 0

sacarXAux x xs xs2 | head xs == x = (reverso xs2) ++ (tail xs)
                   | otherwise = sacarXAux x (tail xs) (head xs:xs2)  

ordenarAux :: [Int] -> [Int] -> [Int]
ordenarAux [] xs2 = xs2
ordenarAux xs xs2 = ordenarAux (sacarXAux (maximo xs) xs []) ((maximo xs) : xs2)

ordenar xs = ordenarAux xs []

quitar x xs = sacarXAux x xs []