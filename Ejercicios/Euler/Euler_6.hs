-- Problema 6 del Proyecto Euler
-- Diferencia entre el cuadrado de las sumas y la suma de los cuadrados.
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio. El cuadrado de la suma de los 10 primeros números es
--    (1 + 2 + ... + 10)^2 = 55^2 = 3025
-- La suma de los cuadrados de los 10 primeros números es
--    1^2 + 2^2 + ... + 10^2 = 385
-- Luego, la diferencia entre el cuadrado de la suma de los 10 primeros
-- números y la suma de los cuadrados de los 10 primeros números es
-- 3025 − 385 = 2640.
-- 
-- Definir la función 
--    euler6 :: Integer -> Integer
-- tal que (euler6 n) es la diferencia entre el cuadrado de la suma
-- de los n primeros números y la suma de los cuadrados de los n
-- primeros números. Por ejemplo, 
--    euler6 10 == 2640
-- 
-- Usando euler6, calcular la diferencia entre el cuadrado de la
-- suma de los 100 primeros números y la suma de los cuadrados de los
-- 100 primeros números. 
-- ---------------------------------------------------------------------

-- migibagar
euler6 n = cuadradoSuma n  - suma n

cuadradoSuma n = ((n*(n+1)) `div` 2)^2

suma n = sum [x^2 | x <- [1..n]]

-- El cálculo de la solución es
--   >>> euler6 100
--   25164150
