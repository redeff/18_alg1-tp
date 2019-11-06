type Circulo = [Integer]
--rota el circulo hasta que empieza con el 1
ordenar :: Circulo -> Circulo
ordenar (x:xs) | x==1 = (x:xs)
               | otherwise = ordenar (xs++[x])

--decide si dos circulos cualesquiera son iguales
sonCirculosIguales :: Circulo -> Circulo -> Bool
sonCirculosIguales a b = ordenar a == ordenar b
