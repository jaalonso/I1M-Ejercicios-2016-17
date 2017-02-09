-- Problema 9 del proyecto Euler
-- Terna pitagórica especial
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Una terna pitagórica es una terna de números naturales (a,b,c) tal
-- que a<b<c y a^2+b^2=c^2. Por ejemplo (3,4,5) es una terna pitagórica. 
--
-- Existe exactamente una terna pitagórica (a,b,c) tal que
-- a + b + c = 1000. Calcular el producto abc.
-- ---------------------------------------------------------------------

-- albcercid
-- =========

euler9 :: [Integer]
euler9 = [a*b*(1000-a-b) | a <- [1..333],
                           b <- [333..500],
                           (1000-a-b)^2 == a^2 + b^2]

-- Cálculo         
--    λ> euler9
--    [31875000]
