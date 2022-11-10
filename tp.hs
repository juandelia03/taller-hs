type Complejo = (Float, Float)

re :: Complejo -> Float
re c = fst c

im :: Complejo -> Float
im c = snd c

suma :: Complejo -> Complejo -> Complejo
suma c i = (fst c+ fst i, snd c + snd i)

producto :: Complejo -> Complejo -> Complejo
producto c i = ((fst c * fst i)-(snd c * snd i),(fst c * snd i )+(snd c * fst i))

conjugado :: Complejo -> Complejo
conjugado c = (fst c,-(snd c))

inverso :: Complejo -> Complejo
inverso c = ((fst (conjugado c)) / ((modulo c)^2 ), (snd (conjugado c)) / ((modulo c)^2 ))

cociente :: Complejo -> Complejo -> Complejo
cociente c i = producto c (inverso i)

potencia :: Complejo -> Integer -> Complejo
potencia c 0 = (1,0)
potencia c n = producto (potencia c (n-1)) c

raicesCuadratica :: Float -> Float -> Float -> (Complejo, Complejo)
raicesCuadratica a b c = (suma ((-b / (2*a)),0) (cociente (fst(raizCuadrada ((discriminanteReal a b c), 0))) (2*a,0)),
                          suma ((-b / (2*a)),0) (cociente (snd(raizCuadrada ((discriminanteReal a b c), 0))) (2*a,0)))

discriminanteReal :: Float -> Float -> Float -> Float
discriminanteReal a b c = b^2 - (4*a*c)

modulo :: Complejo -> Float
modulo c = sqrt ((fst c)**2+(snd c)**2)

resta :: Complejo -> Complejo -> Complejo
resta c i = (fst c - fst i, snd c - snd i)

distancia:: Complejo -> Complejo -> Float
distancia c i = modulo (resta c i)

argumento :: Complejo -> Float
argumento c | c == (0,0) = 0
               | fst c == 0 && snd c > 0 = pi/2
               | fst c == 0 && snd c < 0 = 3*pi/2
               | snd c == 0 && fst c > 0 = 0
               | snd c == 0 && fst c < 0 = pi
               | fst c > 0 && (atan ((snd c)/(fst c))) > 0 = atan ((snd c)/(fst c))
               | fst c > 0 && (atan ((snd c)/(fst c))) < 0 = (atan ((snd c)/(fst c))) + (2 * pi)
               | fst c < 0 && snd c < 0 && (atan ((snd c)/(fst c)) - pi) > 0 = atan ((snd c)/(fst c)) - pi
               | fst c < 0 && snd c < 0 && (atan ((snd c)/(fst c)) - pi) < 0 = (atan ((snd c)/(fst c)) - pi) + (2* pi)
               | fst c < 0 && snd c > 0 = atan ((snd c)/(fst c)) + pi


pasarACartesianas :: Float -> Float -> Complejo
pasarACartesianas r o = (r*cos(o),r*sin(o))

raizCuadrada :: Complejo -> (Complejo,Complejo)
raizCuadrada c = (pasarACartesianas ((modulo c)**(1/2)) (argumento(c)/2), pasarACartesianas ((modulo c)**(1/2)) ((argumento(c)+2*pi)/2))

raicesCuadraticaCompleja :: Complejo -> Complejo -> Complejo -> (Complejo,Complejo)
raicesCuadraticaCompleja a b c = (cociente (suma (producto b (-1,0)) (fst (raizCuadrada (discriminante a b c)))) (producto (2,0) a), 
                                  cociente (suma (producto b (-1,0)) (snd (raizCuadrada (discriminante a b c)))) (producto (2,0) a))

discriminante :: Complejo -> Complejo -> Complejo -> Complejo
discriminante a b c =  suma (potencia b 2) (producto (producto a c) (-4,0))

raicesNEsimasAux :: Float -> Float -> [Complejo]
raicesNEsimasAux n k | k==0 = [(pasarACartesianas 1 0)]
                     | otherwise = (pasarACartesianas 1 ((2*k*pi)/n)) : (raicesNEsimasAux n (k-1))

raicesNEsimas :: Integer -> [Complejo]
raicesNEsimas n = raicesNEsimasAux (fromIntegral n) (fromIntegral(n-1))

sonRaicesNEsimas :: Integer -> [Complejo] -> Float -> Bool
sonRaicesNEsimas n [] e = True
sonRaicesNEsimas n cs e | modulo (resta (potencia (head(cs)) n) (1,0)) < e = sonRaicesNEsimas n (tail(cs)) e
                        | otherwise = False
