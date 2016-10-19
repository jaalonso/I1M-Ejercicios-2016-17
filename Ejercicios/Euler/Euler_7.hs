-- Problema 7 del Proyecto Euler
-- Primo 10001-ésimo.
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio. Definir la función
--    primo :: Int -> Integer
-- tal que (primo n) es el primo n-ésimo. Por ejemplo,
--    primo 4  ==  9
-- Usar primo para calcular el primo 10001-ésimo.
-- ---------------------------------------------------------------------

-- migibagar
euler7 = listaPrimos !! 10000

listaPrimos = [x | x <- [1..], primo x]

primo n = factores n == [1,n]

factores n = [x | x <- [1..n], n `mod` x == 0]

-- ghci> euler7
-- 104743
