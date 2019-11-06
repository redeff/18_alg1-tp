-- Transformamos tu funcion en una que chequea si una lista es prima, y dps la usamos
-- para ver si un ciculo es primo
esListaPrima :: [Integer] -> Bool
esListaPrima xs | length xs == 0 = True
--              | length xs == 1 = esprimo (2 * head xs)
                | length (tail xs) == 0 = True
                | esPrimo (head xs + head (tail xs)) && esListaPrima (tail xs) = True

esCirculoPrimo :: Circulo -> Bool
esCirculoPrimo [] = True
esCirculoPrimo xs = esPrimo (head xs + last xs) && esListaPrima xs