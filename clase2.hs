--ejemplo patternMatching
esCero 0 = True 
esCero _ = False



--Clases de tipo
-- funcion identidad, funcion que devuelve un 
-- valor del mismo tipo que el que recibe

--cualquiera sea x la F. recibira una funcion y retornara una del mismo tipo

identidad :: t -> t --recibe un tipo t y devuelve un tipo t (cualquier tipo)
identidad x = x 

primero :: tx -> ty -> tx
primero x y = x 

segundo :: tx -> ty -> ty
segundo x y = y

restriccionTipo :: t -> t -> Bool
restriccionTipo x y  = True

-- Num es una clase para que a se evalue solo en numeros
-- si no estuviese esta restriccion podria hacer 3 * True que da error
triple ::  Num a => a -> a 
triple x = 3*x

-- Ord verifica que los tipos se puedan evaluar con desigualdades
maximo :: Ord p => p -> p -> p
maximo x y | x>=y = x
    |       otherwise =y

--Eq para verificar que se puede usar == o /= en a->a
distintos :: Eq a => a -> a -> Bool 
distintos x y = x /= y

--Eq Ord Num no son todas las clases de tipos, en el ppp estan todos

-- usando :t {nombre-de-func} en el compilador dice el tipo inferido por defecto 

cantidadDeSoluciones :: (Num t, Ord t) => t -> t -> Int
cantidadDeSoluciones b c | d > 0 = 2
                         | d == 0 =1
                         | otherwise = 0
                         where d = b^2 - 4*c

--Tuplas

sumaR2 :: (Float,Float) -> (Float,Float) -> (Float,Float) 
sumaR2 v w = ((fst v + fst w),(snd v + snd w))
-- sumaR2  (vx,vy) (wx,wy) = (vx+wx,vy+wy)

esOrigen :: (Float,Float) ->Bool
esOrigen (0,0) = True
esOrigen (_,_) = False

angulo45 :: (Float,Float) -> Bool
angulo45 (x,y) = x == y 

normaVectorial :: (Float,Float) -> Float
normaVectorial v = sqrt((fst v)^2 + (snd v)^2)

normaSuma :: (Float,Float) -> (Float,Float) -> Float
normaSuma v w = normaVectorial (sumaR2 v w)

--funciones binarias
--notacion fija f x y =
-- notacion infija 5 `div` 3
divisionIn x y = x `div` y -- notacion infija

divisionFij x y = div x y 

--ejercicios 
estanRelacionados :: (Float, Float) -> Bool
estanRelacionados v | fst v <= 3 &&  snd v <= 3 = True
                    | (fst v > 3 && fst v <=7) && (snd v > 3 && snd v <=7) = True
                    | fst v > 7 &&  snd v > 7 = True
                    | otherwise = False

prodInt :: (Float,Float) -> (Float,Float) -> Float
prodInt v w = fst v * fst w + snd v * snd w

todoMenor :: (Float,Float) -> (Float,Float) -> Bool
todoMenor v w = fst v < fst w && snd v < snd w

distanciaPuntos :: (Float,Float) -> (Float,Float) -> Float
distanciaPuntos v w = sqrt((fst v - fst w)^2 + (snd v - snd w)^2)

sumaTerna :: (Int, Int,Int) -> Int
sumaTerna (x,y,z) =  x+y+z

esPar x = x `mod` 2 == 0

posicPrimerPar :: (Int, Int, Int) -> Int
posicPrimerPar  (x,y,z) | esPar x = 1
                | esPar y = 2
                | esPar z = 3
                | otherwise = 4
crearPar :: a->b->(a,b)
crearPar a b = (a,b)

invertir :: a->b->(b,a)
invertir a b = crearPar b a