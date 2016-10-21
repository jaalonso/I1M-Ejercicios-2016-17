-- Problema 14 del proyecto Euler
-- Sucesión de Collatz más larga.
-- =====================================================================

-- ---------------------------------------------------------------------
-- Se define la siguiente regla para calcular el siguiente elemento de
-- una sucesión de números enteros positivos 
--    n → n/2    (si n es par)
--    n → 3n + 1 (si n es impar)
-- Usando la regla anterior, y comenzando en 13, se genera la siguiente
-- sucesión 
--    13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
-- Se observa que la sucesión anterior (comenzando en 13 y finalizando
-- en 1) contiene 10 términos. Aunque no se ha probado, la conjetura de
-- Collatz afirma que comenzando en cualquier número se alcanza siempre
-- el 1. 
--
-- ¿Qué número, menor que un millón, produce la cadena más larga?
-- ---------------------------------------------------------------------


-- 1ª solución: enrnarbej
-- ======================

euler14a :: Integer
euler14a =
  snd (maximum [(length (collatz n) + 1, n ) | n <-[1..1000000]])

collatz :: Integer -> [Integer]
collatz 1 = []
collatz n | even n    = [n `div` 2] ++ collatz ( div n 2)
          | otherwise = [3*n+1] ++ collatz (3*n+1)

-- Cálculo:
--    λ> euler14a
--    837799
--    (93.98 secs, 63,443,887,376 bytes)

-- ---------------------------------------------------------------------


