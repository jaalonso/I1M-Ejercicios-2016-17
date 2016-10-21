-- Problema 5 del proyecto Euler
-- Menor múltiplo. 
-- =====================================================================

-- ---------------------------------------------------------------------
-- El número 2520 es el menor número divisible por los números desde 1
-- hasta 10. 
-- 
-- ¿Cuál es el menor número divisible por los números del 1 al 20? 
-- ---------------------------------------------------------------------


-- 1ª solución: enrnarbej
-- ======================

euler5a :: [Integer] -> Integer
euler5a [a]    = a  
euler5a (x:xs) = lcm x (euler5a xs)

-- Cálculo:
--    λ> euler5a [1..20]
--    232792560
--    (0.00 secs, 0 bytes)

-- ---------------------------------------------------------------------

-- 2ª solución: luimotmar
-- ======================

euler5b :: Integer -> Integer
euler5b n = head (multiplos20 (product [1..n])

-- (multiplo10 n) se verifica si n es divisible por los números desde 1
-- hasta 10. Por ejemplo,
--    multiplo10 2520  ==  True
multiplo10 :: Integer -> Bool
multiplo10 n = take 10 (factores n) == [1..10]

-- (factores n) es la lista de los factores de n. Por ejemplo,
--    factores 24  ==  [1,2,3,4,6,8,12,24]
factores :: Integer -> [Integer]
factores n = [x | x <- [1..n]
                , n `rem` x == 0]

-- (multiplos10 n) es la lista de los números menores que n divisibles
-- por los números de 1 a 10. Por ejemplo,
--    multiplos10 10000  ==  [2520,5040,7560]
multiplos10 :: Integer -> [Integer]
multiplos10 n = [x | x <- [20,40..n]
                   , multiplo10 x]

-- (multiplo20 n) se verifica si n es divisible por los números desde 1
-- hasta 20. Por ejemplo,
--    multiplo20 232792560  ==  True
multiplo20 :: Integer -> Bool
multiplo20 n = take 20 (factores n) == [1..20]

-- (multiplos20 n) es la lista de los números menores que n divisibles
-- por los números de 1 a 20. 
multiplos20 :: Integer -> [Integer]
multiplos20 n = [x | x <- [40,80..n]
                   , multiplo20 x]

-- ---------------------------------------------------------------------
                  
 
