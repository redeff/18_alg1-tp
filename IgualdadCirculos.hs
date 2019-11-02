type Circulo = [Integer]

--define si dos circulos que empiezan en el 1 son iguales
ordenadosIguales :: Circulo -> Circulo -> Bool
ordenadosIguales [] [] = True
ordenadosIguales _ [] = False --por si el tamaño de los circulos es distinto
ordenadosIguales [] _ = False
ordenadosIguales (x:xs) (y:ys) = x == y && ordenadosIguales xs ys

--rota el circulo hasta que empieza con el 1
ordenar :: Circulo -> Circulo
ordenar (x:xs) | x==1 = (x:xs)
               | otherwise = ordenar (xs++[x])

--decide si dos circulos cualesquiera son iguales
sonCirculosIguales :: Circulo -> Circulo -> Bool
sonCirculosIguales a b = ordenadosIguales (ordenar a) (ordenar b)
