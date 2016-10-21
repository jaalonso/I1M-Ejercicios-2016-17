-- Problema 3 del Proyecto Euler
-- Mayor factor primo
-- =====================================================================

-- ---------------------------------------------------------------------
-- Los factores primos de 13195 son 5, 7, 13 y 29.
-- 
-- ¿Cuál es el mayor factor primo de 600851475143?
-- ---------------------------------------------------------------------

import Data.Numbers.Primes (primeFactors)

-- 1ª solución: enrnarbej
-- ======================

euler3a :: Integer -> Integer
euler3a n = maximum (primeFactors n)

-- Cálculo:
--    λ> euler3a 600851475143
--    6857
--    (0.01 secs, 0 bytes)

-- ---------------------------------------------------------------------

-- 2ª solución: luimotmar
-- ======================

euler3b :: Integer -> Integer
euler3b n = last (factoresPrimos n)
 
factoresPrimos :: Integer -> [Integer]
factoresPrimos n = [x | x <- (factores n)
                      , factores x == [1,x]]

factores :: Integer -> [Integer]
factores n = [x | x <- [1..n]
                , mod n x == 0]

-- Cálculos:
--    λ> euler3b 2750161
--    2750161
--    (1.96 secs, 1,141,979,912 bytes)
--    λ> euler3a 2750161
--    2750161
--    (0.00 secs, 0 bytes)

-- ---------------------------------------------------------------------
