-- Problema 20 del proyecto Euler
-- Suma de los dígitos del factorial.
-- =====================================================================

-- ---------------------------------------------------------------------
-- n! siginifica n × (n-1) × ... × 3 × 2 × 1
--
-- Por ejemplo, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800, y la suma de
-- los dígitos de 10! es 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.
--
-- Calcular la suma de los dígitos de 100!
-- ---------------------------------------------------------------------

-- 1ª solución: enrnarbej
-- ======================

euler20a :: Integer -> Integer
euler20a n = sum (digits (product [1..n]))

digits :: Integer -> [Integer]
digits n = [read [x] | x <- show n]

-- Cálculo
--    λ> euler20a 100
--    648
--    (0.00 secs, 0 bytes)

-- ---------------------------------------------------------------------

