-- Problema 2 del proyecto Euler
-- Suma de los términos pares de la sucesión de Fibonacci
-- =====================================================================

-- ---------------------------------------------------------------------
-- Cada término de la sucesión de Fibonacci se obtiene sumando los dos
-- anteriores. Comenzando con 1 y 2, los 10 primeros términos son
--    1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...
-- Calcular la suma de los términos pares (2, 8, 34, ...) de la sucesión
-- de Fibonacci que sean menores que cuatro millones.
-- ---------------------------------------------------------------------

-- 1ª solución: enrnarbej
-- ======================

euler2a :: Integer
euler2a = sum [x | x <- fibonacci 110
                 , even x
                 , x <= 4000000]

fibonacci :: Int -> [Integer]
fibonacci 1 = [1,1]
fibonacci n = f ++ [sum (drop (n-2) f)]
  where f = fibonacci (n-1)

-- Cálculo
--    λ> euler2a
--    4613732
--    (0.01 secs, 11,818,928 bytes)

-- ---------------------------------------------------------------------

-- 2ª solución: enrnarbej
-- ======================

euler2b :: Integer -> Integer -> Integer
euler2b n i | f > n     = 0
            | otherwise = f + euler2b n (i+3)
  where
    f = fibonacci2 i

fibonacci2 :: Integer -> Integer
fibonacci2 n = round ((golden^n-(1-golden)^n)/(sqrt 5))
  where golden = (1+sqrt 5)/2

-- Cálculo
--    λ> euler2b 4000000 3
--    4613732
--    (0.00 secs, 0 bytes)

-- Comentario: La segunda definición usa números decimales y se producen
-- errores por redondeo. Por ejemplo,
--    λ> fibonacci 100 !! 76
--    5527939700884757
--    λ> fibonacci2 77
--    5527939700884755
--    λ> [x | x <- [1..90], fibonacci 90 !! (fromInteger (x-1)) /= fibonacci2 x]
--    [76,77,78,79,80,81,82,83,84,85,86,87,88,89,90]

-- ---------------------------------------------------------------------
