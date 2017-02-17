-- Problema 52 del proyecto Euler
-- Múltiplos permutados
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Se observa que el número 125874 tiene los mismo dígitos que su doble,
-- 251748, aunque en distinto orden.
--
-- Calcular el menor entero positivo, x, tal que x, 2x, 3x, 4x, 5x y 6x
-- tengan los mismos dígitos.
-- ---------------------------------------------------------------------

-- paumacpar
-- =========

import Data.List

permutedMultiples :: Integer -> Integer
permutedMultiples k = head (auxP [1..])
  where auxP (x:xs)
          | and [permutable i x | i <- [1..k]] = x : auxP xs
          | otherwise                          = auxP xs 

permutable :: Integer -> Integer -> Bool
permutable x n = iguales (show n) (show (n*x))

iguales :: String -> String -> Bool
iguales xs ys = subconjunto xs ys && subconjunto ys xs

subconjunto :: String -> String -> Bool
subconjunto xs ys = auxS xs ys
  where auxS (x:xs) ys | any (==x) ys = auxS xs [y | y <- ys, y /= x]
                       | otherwise = False 
        auxS [] _ = True

-- La solución es
--    λ> permutedMultiples 6
--    142857
