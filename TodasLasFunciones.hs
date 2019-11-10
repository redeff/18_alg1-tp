type Circulo = [Integer]
ordenar :: Circulo -> Circulo
ordenar [] = []
ordenar (x:xs) | x==1 = (x:xs)
               | otherwise = ordenar (xs++[x])

sonCirculosIguales :: Circulo -> Circulo -> Bool
sonCirculosIguales a b = ordenar a == ordenar b

esListaPrima :: [Integer] -> Bool
esListaPrima xs | length xs == 0 = True
                | length (tail xs) == 0 = True
                | esPrimo (head xs + head (tail xs)) && esListaPrima (tail xs) = True

esCirculoPrimo :: Circulo -> Bool
esCirculoPrimo [] = True
esCirculoPrimo xs = esPrimo (head xs + last xs) && esListaPrima xs

esPrimo n = mod fact (n-1) n == (-1)

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
