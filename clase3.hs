--4! = 4.3.2.1. = 4.3!
-- n! = n.(n-1)!

factorial n | n == 0 = 1
            | n > 0 = n * factorial (n-1)

esPar :: Int -> Bool
-- esPar n | n == 0 = True
--         | n == 1 = False    
--         |   otherwise = esPar(n-2)

esPar n | n == 0 = True
        | otherwise = not (esPar(n-1))  

fib:: Integer -> Integer
fib n | n == 0 = 0
      | n == 1 = 1
      | otherwise = fib(n-1) + fib(n-2)

parteEntera:: Float -> Int
parteEntera x | x<1 = 0
              | otherwise = (parteEntera(x-1) + 1)

naturalMultiploDe3:: Int -> Bool
naturalMultiploDe3 n | n == 0 = True
                     | n <0 = False
                     | otherwise = (naturalMultiploDe3(n-3))

sumaImpares n | n == 1 = 1
              | otherwise = sumaImpares(n-1) + 2*n-1

medioFact n | n == 1 = 1
            | n == 2 = 2
            | otherwise = medioFact(n-2) * n


ultimaCifra x = mod x 10

sumaDigitos n | mod n 10 == 0 = n
              | otherwise = sumaDigitos(div n 10 ) + (ultimaCifra n)

todosIguales:: Int -> Bool
todosIguales n | n < 10 = True
               | (ultimaCifra n == ultimaCifra (div n 10)) = todosIguales(div n 10)
               | otherwise = False