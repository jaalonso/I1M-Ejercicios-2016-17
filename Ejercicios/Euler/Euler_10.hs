-- Problema 10 del proyecto Euler
-- Suma de primos
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- La suma de los primos menores que 10 es 2 + 3 + 5 + 7 = 17.
--
-- Calcular la suma de los primos menores que dos millones.
-- ---------------------------------------------------------------------

import Data.Numbers.Primes (primes)

-- 1ª solución: enrnarbej
-- ======================

euler10a :: Integer -> Integer
euler10a n = sum [p | p <- take (primeBelow n - 1) primes]

primeBelow :: Integer -> Int
primeBelow n = head [r | (p,r) <- zip primes [1..] , p > n]

-- Cálculo
--    λ> euler10a 2000000
--    142913828922
--    (0.88 secs, 1,540,393,472 bytes)

-- ---------------------------------------------------------------------

