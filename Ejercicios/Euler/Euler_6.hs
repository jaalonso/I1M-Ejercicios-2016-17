-- Problema 6 del Proyecto Euler
-- Diferencia entre el cuadrado de las sumas y la suma de los cuadrados.
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- La suma de los cuadrados de los 10 primeros números es
--    1^2 + 2^2 + ... + 10^2 = 385
-- El cuadrado de la suma de los 10 primeros números es
--    (1 + 2 + ... + 10)^2 = 55^2 = 3025
-- Luego, la diferencia entre el cuadrado de la suma de los 10 primeros
-- números y la suma de los cuadrados de los 10 primeros números es
-- 3025 - 385 = 2640.
-- 
-- Calcular la diferencia entre el cuadrado de la suma de los 100
-- primeros números y la suma de los cuadrados de los 100 primeros
-- números.  
-- ---------------------------------------------------------------------

-- 1ª solución: migibagar
-- ======================

euler6a n = cuadradoSuma n  - suma n

cuadradoSuma n = ((n*(n+1)) `div` 2)^2

suma n = sum [x^2 | x <- [1..n]]

-- El cálculo de la solución es
--    λ> euler6a 100
--    25164150
--    (0.01 secs, 0 bytes)

-- ---------------------------------------------------------------------

-- 2ª solución: enrnarbej
-- ======================

euler6b :: Integer -> Integer
euler6b n = - n*(n+1)*(2*n+1) `div` 6 + (n*(n+1) `div` 2)^2

-- Cálculo
--    λ> euler6b 100
--    25164150
--    (0.01 secs, 0 bytes)

-- ---------------------------------------------------------------------


