-- Problema 30 del proyecto Euler
-- Números como suma de quintas potencias de sus dígitos
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Sorprendentemente, sólo hay tres números que se pueden escribir como
-- sumas de cuartas potencias de sus dígitos:
--    1634 = 1^4 + 6^4 + 3^4 + 4^4
--    8208 = 8^4 + 2^4 + 0^4 + 8^4
--    9474 = 9^4 + 4^4 + 7^4 + 4^4
-- Puesto que 1 = 1^4 no es una suma, no se ha incluido. 
--
-- La suma de los números anteriores es 1634 + 8208 + 9474 = 19316.
--
-- Calcular la suma de todos los números que se pueden escribir como
-- suma de las quintas potencias de sus dígitos.
-- ---------------------------------------------------------------------

-- 1ª solución: enrnarbej
-- ======================

euler30 :: Integer -> Integer
euler30 n = sum (filter isFifth [2..10^(n+1)])

isFifth :: Integer -> Bool
isFifth n = n == sum (map (^5) (digits n))

digits :: Integer -> [Integer]
digits n = [read [a] | a <- show n] 

-- Prelude> euler30 5
-- 443839
-- (25.77 secs, 45,362,262,480 bytes)
