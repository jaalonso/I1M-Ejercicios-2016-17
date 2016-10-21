-- Problema 7 del Proyecto Euler
-- Primo 10001-ésimo.
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Los seis primeros números primos son: 2, 3, 5, 7, 11, and 13. Por lo
-- que el sexto número primo es 13.
--
-- ¿Cuá es el primo 10001-ésimo?
-- ---------------------------------------------------------------------

import Data.Numbers.Primes (primes)

-- 1ª solución:migibagar
-- =====================

euler7a = listaPrimos !! 10000

listaPrimos = [x | x <- [1..], primo x]

primo n = factores n == [1,n]

factores n = [x | x <- [1..n], n `mod` x == 0]

-- Cálculo:
--    λ> euler7a
--    104743
--    (244.12 secs, 139,356,301,080 bytes)

-- ---------------------------------------------------------------------

-- 2ª solución: enrnarbej
-- ======================

euler7b :: Int -> Integer
euler7b n = last (take n primes)

-- Cálculo
--    λ> euler7b 10001
--    104743
--    (0.03 secs, 30,894,048 bytes)

-- ---------------------------------------------------------------------
