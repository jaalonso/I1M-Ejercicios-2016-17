-- Problema 55 del proyecto Euler
-- Números de Lychrel
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Si tomamos el 47, lo invertimos y sumamos obtenemos 47 + 74 = 121,
-- que es capicúa.
--
-- No todos los números produce un capicúa tan rápidamente. Por ejemplo, 
--     349 +  943 = 1292
--    1292 + 2921 = 4213
--    4213 + 3124 = 7337
-- Es decir, para el 349 se necesitan 3 iteraciones para llegar a un
-- capicua. 
--
-- Anque no se ha probado, se conjetura que algunos números (como el
-- 196) nunca producen un capicúa. Los números que nunca producen un
-- capicúa con el procedimiento de invertir y sumar se llaman números de
-- Lychrel.
--
-- En este problema supondremos que todo número menor que 10.000 o
-- alcanza un capicúa en menos de 50 iteraciones o no lo alcanza
-- nunca. De hecho, se ha comprobado que 4668731596684224866951378664 es
-- el menor número que necesita más de 50 iteraciones para alcanzar un
-- capicúa.
--
-- Sorprendentemente, hay números capicúas que son números de
-- Lychrel. El primero es el 4994.
--
-- ¿Cuántos números de Lychrel hay que sean menores que 10.000?
-- ---------------------------------------------------------------------


-- 1ª solución: enrnarbej
-- ======================

euler55 :: Integer -> Int
euler55 n =
  length (filter (\x -> isLychrel 1 (x+read(reverse(show (x))))) [1..n])

isPalindrome :: Integer -> Bool
isPalindrome n = reverse s == s
  where s = show n

isLychrel :: Integer -> Integer -> Bool
isLychrel i n
  | i > 50          = True
  | isPalindrome n  = False
  | otherwise       = isLychrel (i+1) (n + read(reverse(show (n))))

-- Prelude> euler55 10000
-- 249
-- (0.32 secs, 545,444,560 bytes)
