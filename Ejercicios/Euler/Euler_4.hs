-- Problema 4 del proyecto Euler
-- Mayor capicúa producto de dos números de 3 cifras.
-- =====================================================================

-- ---------------------------------------------------------------------
-- Un capicúa es un número que es igual leído de izquierda a derecha que
-- de derecha a izquierda. El mayor capicúa formado por el producto de
-- dos números de 2 cifras es 9009 = 91 × 99. 
-- 
-- Calcular el mayor capicúa que es el producto de dos números de 3
-- cifras, 
-- ---------------------------------------------------------------------

-- 1ª solución: enrnarbej
-- ======================

euler4a :: Integer
euler4a = maximum [a*b | a <- [100..999]
                       , b <- [a..999]
                       , isPalindrome (a*b)]

isPalindrome :: Integer -> Bool
isPalindrome n = ns == reverse ns
  where ns = show n


-- Cálculo
--    λ> euler4a
--    906609
--    (0.28 secs, 279,387,568 bytes)

-- ---------------------------------------------------------------------


