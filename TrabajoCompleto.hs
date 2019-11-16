type Circulo = [Integer]

--EJERCICIO 1
sonCirculosIguales :: Circulo -> Circulo -> Bool
sonCirculosIguales a b = ordenar a == ordenar b

--Esta funcion rota el circulo en sentido anti-horario hasta que el primer elemento sea el 1
ordenar :: Circulo -> Circulo
ordenar [] = []
ordenar (x:xs) | x==1 = (x:xs)
               | otherwise = ordenar (xs++[x])

--EJERCICIO 2:
--dado un valor k, la funcion shift suma 1 a los enteros de la lista mayores o iguales a k
shift :: Integer -> [Integer] -> [Integer]
shift _ [] = []
shift k (x:xs) | x < k  =     x : shift k xs
               | x >= k = (x+1) : shift k xs

--Agrega el elemento k a la cabeza de la lista y aplica la funcion shift a la misma
--De esta forma, el elemento que se agregue al frente de la lista, no se repetira en la misma.
poner :: Integer -> [Integer] -> [Integer]
poner k xs = k : (shift k xs)

--dada una lista de listas, pone k en cada una
ponerCadaUna :: Integer -> [[Integer]] -> [[Integer]]
ponerCadaUna _ [] = []
ponerCadaUna k (x:xs) = poner k x : ponerCadaUna k xs

--Genera una lista con todas las permutaciones de los enteros entre 1 y n como listas de enteros.
--Para dar las permutaciones de n, la funcion utiliza las permutaciones de n-1.
--De cada lista de las permutaciones de n-1, genera n listas de longitud n, poniendole a esta lista los numeros de 1 hasta n, con la funcion ponerCadaUno.
--Teniendo en cuenta que la cantidad de listas de permutaciones de n-1 es (n-1)!, y a partir de cada una de ellas obtuvimos n nuevas listas. Obtenemos (n-1)!*n listas nuevas.
--Siendo n! listas distintas, conformadas con los numeros de 1 a n sin repetir
permutaciones :: Integer -> [[Integer]]
permutaciones 0 = [[]]
permutaciones n = annadir n
    where annadir :: Integer -> [[Integer]]
          annadir 0 = []
          annadir k = annadir (k-1) ++ ponerCadaUna k (permutaciones (n-1))

--EJERCICIO 3
--Determina si un circulo es primo, revisando si cada suma de consecutivos es prima, inclusive el primero con el ultimo de la lista.
esCirculoPrimo :: Circulo -> Bool
esCirculoPrimo [] = True
esCirculoPrimo xs = esPrimo (head xs + last xs) && esListaPrima xs

--La funcion evalua si la suma de cada par de numeros consecutivos de una lista suma primo. (no chequea el primero y el ultimo)
esListaPrima :: [Integer] -> Bool
esListaPrima xs | length xs == 0 = True
                | length (tail xs) == 0 = True
                | otherwise = esPrimo (head xs + head (tail xs)) && esListaPrima (tail xs)

--Se utiliza que: p es primo <=> p>1 && p|(p-1)!+1
esPrimo :: Integer -> Bool
esPrimo n
  | n > 1     = mod (fact (n-1)) n == (n-1)
  | otherwise = False
  where
    fact :: Integer -> Integer
    fact 0 = 1
    fact n = n * fact (n-1)

--EJERCICIO 4
--Determina si el primer circulo de la lista circulos esta repetido en otro lugar de esa lista
estaRepetidoPrimero :: [Circulo] -> Bool
estaRepetidoPrimero [] = False
estaRepetidoPrimero (x:xs) = pertenece x xs

--Dados un circulo y una lista de circulos, devuelve si este se encuentra en la lista
pertenece :: Circulo -> [Circulo] -> Bool
pertenece _ [] = False
pertenece a (x:xs) | sonCirculosIguales a x == True = True
                   | otherwise = pertenece a xs

--EJERCICIO 5
--Dado n >= 2, devuelve una lista de todos los circulos primos distintos de longitud n
listaCirculosPrimos :: Integer -> [Circulo]
listaCirculosPrimos n = filtrarRepetidos (eliminarNoPrimos (permutaciones n))

--Dada una lista de circulos, devuelve la lista solo con los circulos primos de esta.
eliminarNoPrimos :: [Circulo] -> [Circulo]
eliminarNoPrimos [] = []
eliminarNoPrimos (x:xs) | esCirculoPrimo x = x : eliminarNoPrimos xs
                        | otherwise = eliminarNoPrimos xs

--Dada una lista de circulos, devuelve la lista sin circulos repetidos
filtrarRepetidos :: [Circulo] -> [Circulo]
filtrarRepetidos [] = []
filtrarRepetidos (x:xs) | estaRepetidoPrimero (x:xs) == True = filtrarRepetidos xs
                        | otherwise = x : filtrarRepetidos xs

--EJERCICIO 6
--devuelve la cantidad de circulos primos distintos de orden n, para n >= 2.
contarCirculosPrimos :: Integer -> Integer
contarCirculosPrimos n = long (listaCirculosPrimos n)
    where long :: [Circulo] -> Integer
          long [] = 0
          long (x:xs) = 1 + long xs
