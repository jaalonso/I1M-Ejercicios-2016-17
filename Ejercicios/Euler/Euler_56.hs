-- Problema 56 del proyecto Euler
-- Máximo de las sumas de dígitos de potencias
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Se consideran los números naturals de la forma a^b, con a y b menores
-- que 100. ¿Cuál es el máxio de la suma de sus dígitos?
-- ---------------------------------------------------------------------

-- 1ª solución: enrnarbej
-- ======================

euler56 :: Integer -> Integer
euler56 n = maximum [sum (digits (a^b)) | a <- [1..n-1] , b <-[1..n-1]]

digits :: Integer -> [Integer]
digits n = [read [a] | a <- show n] 

-- Prelude> euler56 100
-- 972
-- (2.60 secs, 5,287,440,376 bytes)
