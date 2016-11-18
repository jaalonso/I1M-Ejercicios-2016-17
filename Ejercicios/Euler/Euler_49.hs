-- Problema 49 de proyecto Euler
-- Primos permutados en progresión aritmética
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- La progresión aritmética 1487, 4817, 8147, en la que la diferencia
-- entre los términos es 3330, tiene sos singularidades:
--    (1) cada uno de los tres términos es un número primo y
--    (2) los dígitos de los términos son distintas permutaciones de los
--        mismos dígitos. 
--
-- No existe ninguna sucesión con esta propiedad para los números con
-- menos de 4 dígitos, pero existe otra progresión aritmética creciente
-- con números de 4 dígitos verificando la propiedad.
--
-- ¿Cuál es el número de 12 dígitos obtenido al concatenar los dígitos
-- de los tres términos de dicha progresión?
-- ---------------------------------------------------------------------


-- 1ª solución: enrnarbej
-- ======================

import Data.Numbers.Primes 

euler49 :: [[Integer]]
euler49 = [[a,b,c] | a <- fourDigitsPrimes
                   , b <- dropWhile (<=a) fourDigitsPrimes
                   , let c = 2*b-a
                   , isPrime c
                   , c < 10000
                   , esPermutada [a,b,c]]

fourDigitsPrimes :: [Integer]
fourDigitsPrimes = takeWhile (<10000) (dropWhile (<1000) primes)

esPermutada :: [Integer] -> Bool
esPermutada [a,b,c] =
  digits a `iguales` digits b &&
  digits b `iguales` digits c

digits :: Integer -> [Integer]
digits n = [read [a] | a <- show n] 

iguales :: Eq a => [a] -> [a] -> Bool
iguales xs ys = subconjunto xs ys && subconjunto ys xs

subconjunto :: Eq a => [a] -> [a] -> Bool
subconjunto xs ys = all (`elem` ys) xs

-- Prelude> euler49
-- [[1487,4817,8147],[2969,6299,9629]]
