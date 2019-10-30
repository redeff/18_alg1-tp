Trabajo Práctico Taller de Álgebra I
====================================

type Circulo = [Integer]

ordenadosIguales :: Circulo -> Circulo -> Bool
ordenadosIguales [] [] = True
ordenadosIguales _ [] = False
ordenadosIguales [] _ = False
ordenadosIguales (x:xs) (y:ys) = x == y && ordenadosIguales xs ys

ordenar :: Circulo -> Circulo
ordenar (x:xs) | x==1 = (x:xs)
               | otherwise = ordenar (xs++[x])

sonCirculosIguales :: Circulo -> Circulo -> Bool
sonCirculosIguales a b = ordenadosIguales (ordenar a) (ordenar b)
