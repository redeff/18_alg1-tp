Trabajo Práctico Taller de Álgebra I
====================================


esCirculoPrimo :: Circulo -> Bool
esCirculoPrimo xs | length xs == 0 = False
                  |length xs == 1 = esprimo (head xs)
                  | length (tail xs) == 0 = True
                  | esprimo (head xs + head (tail xs)) && esCirculoPrimo (tail xs) = True
				          |otherwise = False
tal ves se puede refinar un poco, pero en general lo que hace es ver si la suma de los dos primeros elementos de una lista son primos, entonces sigo viendo si la suma de los proximos son primos y asi hasta que la tail es vacia, y si es vacia, es true. 
