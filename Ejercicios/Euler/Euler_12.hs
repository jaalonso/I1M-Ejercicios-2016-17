-- Problema 12 del proyecto Euler
-- Número triangular con muchos divisores.
-- =====================================================================

-- ---------------------------------------------------------------------
-- La sucesión de los números triangulares se genera sumando los números
-- naturales. Por ejemplo, el 7º número triangular es
--    1 + 2 + 3 + 4 + 5 + 6 + 7 = 28.
--
-- Los primeros números triangulares son
--    1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
--
-- Los divisores de los primeros números triangulares son
--    1: 1
--    3: 1,3
--    6: 1,2,3,6
--   10: 1,2,5,10
--   15: 1,3,5,15
--   21: 1,3,7,21
--   28: 1,2,4,7,14,28
--
-- Se observa que 28 es el primer número triangular con  al menos cinco
-- divisores. 
--
-- ¿Cuál es el primer número triangular con al menos 500 divisores?
-- ---------------------------------------------------------------------

import Data.Numbers.Primes (primeFactors)
import Data.List           (partition)

-- 1ª solución: enrnarbej
-- ======================

euler12a :: Int -> Integer
euler12a n =
  head [triangleNumber x | x <- [1..]
                         , numeroDivisores (triangleNumber x) >= n]

triangleNumber :: Integer -> Integer
triangleNumber n = n*(n+1) `div` 2

numeroDivisores :: Integer -> Int
numeroDivisores n =
  product [length x + 1 | x <- repetidos (primeFactors n)]

repetidos :: Eq a =>[a] -> [[a]]
repetidos [] = []
repetidos (x:xs) = [x:fst p] ++ repetidos (snd p)
  where
    p = partition (==x) xs
           
-- Cálculo:
--   λ> euler12a 500
--   76576500
--   (0.73 secs, 1,525,748,952 bytes)

-- ---------------------------------------------------------------------

