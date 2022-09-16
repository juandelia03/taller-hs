factorial n | n == 0 = 1
            | n > 0 = n * factorial (n-1)


sumatoria 0 = 0
sumatoria x = sumatoria (x-1) + x

f1 0 = 1 
f1 x = f1(x-1) + 2**x 

f2 1 q = q
f2 n q = f2 (n-1) q + q^n

f3 0 q = 0
f3 n q = f2 (2*n) q

f4 0 q = 0
f4 n q = f2 (2*n) q - f2 (n-1) q  

eAprox :: Integer -> Double
eAprox 0 = 1
eAprox n = eAprox (n-1) + 1/ fromIntegral(factorial n)

e = eAprox 10

sumatoriaDoble 0 m = f2 m 0
sumatoriaDoble n m = sumatoriaDoble (n-1) (m) + f2 m n


sumaPotencias q n m = f2 n q * f2 m q

division p 1 = p
division p q = division p (q-1) + p/q

sumaRacionales 1 q = division 1 q
sumaRacionales p q = sumaRacionales (p-1) q + division p q