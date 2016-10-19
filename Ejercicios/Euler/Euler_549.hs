-- Divisibilidad de factoriales
-- ============================

-- ---------------------------------------------------------------------
-- El menor número m tal que 10 divide a m! es m = 5.
-- El menor número m tal que 25 divide a m! es m = 10.
--
-- Sea s(n) el menor número n tal que n divide a m!. Por ejemplo, 
-- s(10) = 5 y s(25) = 10.
-- 
-- Sea S(n) la suma de los s(i) para 2 ≤ i ≤ n. Por ejemplo,
-- S(100) = 2012.
-- 
-- Calcular S(10^8).
-- ---------------------------------------------------------------------

-- juaorture
cabeza :: [Integer] -> Integer
cabeza [] = 0
cabeza xs = head xs

factorial :: Integer -> Integer
factorial n = product [1..n]

factores' :: Integer -> [Integer]
factores' n = [a | a <- [2..n]
                 , n `mod` a == 0]

s :: Integer -> Integer
s n = cabeza [a | a <- [2..n]
                , (factorial a) `mod` n == 0]

ss :: Integer -> Integer
ss n = sum [s a | a <- [1..n]]
