-- Problema 41 del proyecto Euler
-- Primo pandigital
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Se dice que un número de n dígitos es pandigital si usa cada uno de
-- los dígitos de 1 a n exactamente una vez. Por ejemplo, 2143 es un
-- número pandigital de 4 dígitos que además es primo.
--
-- ¿Cuál es el mayor primo pandigital con n dígitos que existe?
-- ---------------------------------------------------------------------

-- 1ª solución: enrnarbej
-- ======================

import Data.List
import Data.Numbers.Primes

euler41 :: [Integer] -> Integer
euler41 = head.filter isPrime.reverse.sort.map toInt.permutations

isPandigital :: Integer -> Bool
isPandigital n = dn == nub dn
  where dn = digits n

digits :: Integer -> [Integer]
digits n = [read [a] | a <- show n] 

toInt :: [Integer] -> Integer
toInt xs = foldl' (\x y -> x*10 + y) 0 xs

-- Prelude> euler41 [1..7]
-- 7652413

