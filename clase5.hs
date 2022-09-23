sumaDivisoresHasta :: Int -> Int -> Int
sumaDivisoresHasta n 0 = 0
sumaDivisoresHasta n i | i>n = 0
                       | mod n i /= 0 = sumaDivisoresHasta n (i-1)
                       | mod n i == 0 = sumaDivisoresHasta n (i-1) + i 

sumaDivisores :: Int -> Int
sumaDivisores n = sumaDivisoresHasta n n 



menorDivisorDesde n i | i > n =  0
                      | mod n i  == 0 = i  
                      | otherwise = menorDivisorDesde n (i+1)
                      | n == i = i  

menorDivisor n = menorDivisorDesde n 2 

esPrimo n | menorDivisor n == n = True
          | otherwise = False

siguientePrimo n | esPrimo (n+1) = n+1
                 | otherwise = siguientePrimo(n+1)  

eNesimoPrimo 1 = 2
eNesimoPrimo n = siguientePrimo( eNesimoPrimo(n-1) )

factorial 0 = 1
factorial n = factorial(n-1) * n

menorFactDesdeAux n i | factorial i > n = factorial i 
                      | otherwise = menorFactDesdeAux n (i+1)

menorFactDesde n = menorFactDesdeAux n 1

mayorFactHastaAux n i | factorial i < n = factorial i 
                      | otherwise = mayorFactHastaAux n (i-1)

mayorFactHasta n = mayorFactHastaAux n n

esFactAux n i | n == factorial i = True
           | n > factorial i = esFactAux n (i+1)
           | n < factorial i  = False

esFact n = esFactAux n 1

fib n | n == 0 = 0
      | n == 1 = 1
      | otherwise = fib(n-1) + fib(n-2)

esFibAux n i | n == fib i = True
             | n > fib i = esFibAux n (i+1)
             | n < fib i = False

esFib n = esFibAux n 1