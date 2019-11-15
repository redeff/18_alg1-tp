type Circulo = [Integer]

--Esta funcion rota el circulo en sentido anti-horario hasta que el primer elemento sea el 1
ordenar :: Circulo -> Circulo
ordenar [] = []
ordenar (x:xs) | x==1 = (x:xs)
               | otherwise = ordenar (xs++[x])


sonCirculosIguales :: Circulo -> Circulo -> Bool
sonCirculosIguales a b = ordenar a == ordenar b

--La funcion evalua si la suma de cada par de numeros consecutivos de una lista suma primo. (no chequea el primero y el ultimo)
esListaPrima :: [Integer] -> Bool
esListaPrima xs | length xs == 0 = True 
                | length (tail xs) == 0 = True
                | otherwise = esPrimo (head xs + head (tail xs)) && esListaPrima (tail xs)

esCirculoPrimo :: Circulo -> Bool
esCirculoPrimo [] = True
esCirculoPrimo xs = esPrimo (head xs + last xs) && esListaPrima xs

--Se utiliza que: p es primo <=> p|(p-1)!+1
esPrimo :: Integer -> Bool
esPrimo n = mod (fact (n-1)) n == (n-1)
   where fact :: Integer -> Integer
         fact 0 = 1
         fact n = n * fact (n-1)

--PERMUTACIONES:
shift :: Integer -> [Integer] -> [Integer]
shift _ [] = []
shift k (x:xs) | x < k  =     x : shift k xs
               | x >= k = (x+1) : shift k xs

poner :: Integer -> [Integer] -> [Integer]
poner k xs = k : (shift k xs)

ponerCadaUna :: Integer -> [[Integer]] -> [[Integer]]
ponerCadaUna _ [] = []
ponerCadaUna k (x:xs) = poner k x : ponerCadaUna k xs

permutaciones :: Integer -> [[Integer]]
permutaciones 0 = [[]]
permutaciones n = annadir  n
    where annadir :: Integer -> [[Integer]]
          annadir 0 = []
          annadir k = annadir (k-1) ++ ponerCadaUna k (permutaciones (n-1))



estaRepetidoPrimero :: [Circulo] -> Bool
estaRepetidoPrimero [] = False
estaRepetidoPrimero (x:xs) = pertenece x xs

pertenece :: Circulo -> [Circulo] -> Bool
pertenece _ [] = False
pertenece a (x:xs) | sonCirculosIguales a x == True = True
                   | otherwise = pertenece a xs 

eliminarNoPrimos :: [Circulo] -> [Circulo]
eliminarNoPrimos [] = []
eliminarNoPrimos (x:xs) | esCirculoPrimo x = x : eliminarNoPrimos xs
                        | otherwise = eliminarNoPrimos xs

filtrarRepetidos :: [Circulo] -> [Circulo]
filtrarRepetidos [] = []
filtrarRepetidos (x:xs) | estaRepetidoPrimero (x:xs) == True = filtrarRepetidos xs
                        | otherwise = x : filtrarRepetidos xs

listaCirculosPrimos :: Integer -> [Circulo]
listaCirculosPrimos n = filtrarRepetidos (eliminarNoPrimos (permutaciones n))


contarCirculosPrimos :: Integer -> Integer
contarCirculosPrimos n = long (listaCirculosPrimos n)
    where long :: [Circulo] -> Integer
          long [] = 0
          long (x:xs) = 1 + long xs