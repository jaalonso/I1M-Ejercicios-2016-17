-- Problema 21 del proyecto Euler
-- Números amigos
-- =====================================================================

-- ---------------------------------------------------------------------
-- Se dice que dos números son amigos si y sólo si la suma de los
-- divisores de cada uno es igual al otro. Por ejemplo, los divisores de
-- 220 son 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 y 110. La suma de estos
-- números equivale a 284. A su vez, los divisores de 284 son 1, 2, 4,
-- 71 y 142. Su suma equivale a 220. Por tanto, 220 y 284 son amigos 
-- 
-- Evaluar la suma de todos los pares de números amigos menores que
-- 10000. 
-- ---------------------------------------------------------------------

-- 1ª solución: enrnarbej
-- ======================

euler21a :: Integer -> Integer
euler21a n = sum [x | x <- [1..n-1]
                    , isAmicable x] 

isAmicable :: Integer  -> Bool
isAmicable a = (sum (divisores b) == a) && b /= a
  where b = sum (divisores a)

divisores :: Integer -> [Integer]
divisores n = [x | x <- [1.. n `div` 2 + 1]
                 , mod n x == 0]

-- Cálculo:
--    λ> euler21a 10000
--    31626
--    (14.66 secs, 8,580,333,624 bytes)

-- ---------------------------------------------------------------------



