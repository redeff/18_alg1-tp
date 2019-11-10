
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

{-
permutaciones :: Integer -> [[Integer]]
permutaciones 0 = [[]]
permutaciones n = annadir n where
    rec = permutaciones (n-1)
    annadir 0 = []
    annadir k = annadir (k-1) ++ ponerCadaUna k rec
-}