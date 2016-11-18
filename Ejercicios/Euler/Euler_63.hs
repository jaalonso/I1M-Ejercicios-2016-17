-- Problema 63 del proyecto Euler
-- Números de n dígitos que son potencias n-ésimas
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- El número 16807 tiene 5 dígitos y es una quinta potencia: 16807=7^5.
-- Análogamente, el número de 9 dígitos 134217728 es una novena
-- potencia: 134217728=8^9.
--
-- ¿Cuántos números de n dígitos son potencias n-ésimas?
-- ---------------------------------------------------------------------

-- 1ª solución: enrnarbej
-- ======================

euler63 :: Int
euler63 = length (concat (map powerful xs))
  where xs = takeWhile (\x -> powerful x /= []) [1..]

powerful :: Int -> [Int]
powerful n = takeWhile (\x -> (length (show (n^x))) == x) [1..]

-- Prelude> euler63 + 1
-- 49
-- (0.01 secs, 77,040 bytes)

