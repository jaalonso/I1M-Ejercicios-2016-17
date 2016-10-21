-- Problema 16 del proyecto Euler
-- Suma de dígitos de potencias.
-- =====================================================================

-- ---------------------------------------------------------------------
-- 2^15 = 32768 y la suma de sus dígitos es 3 + 2 + 7 + 6 + 8 = 26.
--
-- ¿Cuál es la suma de los dígitos del número 2^1000?
-- ---------------------------------------------------------------------

-- 1ª solución: enrnarbej
-- ======================

euler16a :: Integer
euler16a = sum (digits (2^1000))

digits :: Integer -> [Integer]
digits n = [read [x] | x <- show n]

-- Cálculo:
--    λ> euler16a
--    1366
--    (0.00 secs, 0 bytes)

-- ---------------------------------------------------------------------
