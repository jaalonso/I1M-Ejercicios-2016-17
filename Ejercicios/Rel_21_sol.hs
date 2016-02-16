-- I1M 2015-16: Relación 21 (13 de febrero de 2016)
-- Cálculo numérico: Diferenciación y métodos de Herón y de Newton.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- En esta relación se definen funciones para resolver los siguientes
-- problemas de cálculo numérico:
-- + diferenciación numérica,
-- + cálculo de la raíz cuadrada mediante el método de Herón,
-- + cálculo de los ceros de una función por el método de Newton y
-- + cálculo de funciones inversas.

-- ---------------------------------------------------------------------
-- Importación de librerías                                           --
-- ---------------------------------------------------------------------

import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Diferenciación numérica                                            --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Definir la función
--    derivada :: Double -> (Double -> Double) -> Double -> Double
-- tal que (derivada a f x) es el valor de la derivada de la función f
-- en el punto x con aproximación a. Por ejemplo, 
--    derivada 0.001 sin pi  ==  -0.9999998333332315
--    derivada 0.001 cos pi  ==  4.999999583255033e-4
-- ---------------------------------------------------------------------

-- manvermor manpende blaruiher alvalvdom1 javperlag
derivada :: Double -> (Double -> Double) -> Double -> Double
derivada a f x = (f (x+a) - f x) / a

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Definir las funciones
--    derivadaBurda :: (Double -> Double) -> Double -> Double
--    derivadaFina  :: (Double -> Double) -> Double -> Double
--    derivadaSuper :: (Double -> Double) -> Double -> Double
-- tales que 
--    * (derivadaBurda f x) es el valor de la derivada de la función f 
--      en el punto x con aproximación 0.01,
--    * (derivadaFina f x) es el valor de la derivada de la función f 
--      en el punto x con aproximación 0.0001.
--    * (derivadaSuper f x) es el valor de la derivada de la función f 
--      en el punto x con aproximación 0.000001.
-- Por ejemplo,
--    derivadaBurda cos pi  ==  4.999958333473664e-3
--    derivadaFina  cos pi  ==  4.999999969612645e-5
--    derivadaSuper cos pi  ==  5.000444502911705e-7
-- ---------------------------------------------------------------------

-- manvermor manpende blaruiher alvalvdom1 javperlag
derivadaBurda :: (Double -> Double) -> Double -> Double
derivadaBurda = derivada 0.01

-- manvermor manpende blaruiher alvalvdom1 javperlag
derivadaFina :: (Double -> Double) -> Double -> Double
derivadaFina = derivada 0.0001

-- manvermor manpende blaruiher alvalvdom1 javperlag
derivadaSuper :: (Double -> Double) -> Double -> Double
derivadaSuper = derivada 0.000001

-- ---------------------------------------------------------------------
-- Ejercicio 1.3. Definir la función
--    derivadaFinaDelSeno :: Double -> Double
-- tal que (derivadaFinaDelSeno x) es el valor de la derivada fina del
-- seno en x. Por ejemplo,
--    derivadaFinaDelSeno pi  ==  -0.9999999983354436
-- ---------------------------------------------------------------------

-- manvermor manpende blaruiher alvalvdom1 javperlag
derivadaFinaDelSeno :: Double -> Double
derivadaFinaDelSeno = derivadaFina sin

-- ---------------------------------------------------------------------
-- Cálculo de la raíz cuadrada                                        --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. En los siguientes apartados de este ejercicio se va a
-- calcular la raíz cuadrada de un número basándose en la siguiente
-- propiedades:
-- + Si y es una aproximación de la raíz cuadrada de x, entonces 
--   (y+x/y)/2 es una aproximación mejor. 
-- + El límite de la sucesión definida por 
--       x_0     = 1 
--       x_{n+1} = (x_n+x/x_n)/2
--   es la raíz cuadrada de x.
--
-- Definir, por recursión, la función
--    raiz :: Double -> Double
-- tal que (raiz x) es la raíz cuadrada de x calculada usando la
-- propiedad anterior con una aproximación de 0.00001 y tomando como
-- valor inicial el 1. Por ejemplo, 
--    raiz 9  ==  3.000000001396984
-- ---------------------------------------------------------------------

-- manvermor manpende blaruiher alvalvdom1
raiz :: Double -> Double
raiz x = raizA 1
    where raizA y | loEs y = y
                  | otherwise = raizA (mejorar y)
          loEs y =  abs (y^2 -x) < 0.00001
          mejorar y = (y+x/y) / 2

-- javperlag
raiz1 :: Double -> Double
raiz1 x = aproxRaiz 1 x 

aproxRaiz n x | abs(n^2-x)< 0.00001 = n 
              | otherwise           = aproxRaiz ((n + x/n)/2) x

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Definir el operador 
--    (~=) :: Double -> Double -> Bool
-- tal que (x ~= y) si |x-y| < 0.001. Por ejemplo,
--    3.05 ~= 3.07        ==  False
--    3.00005 ~= 3.00007  == True
-- ---------------------------------------------------------------------

-- manvermor manpende blaruiher alvalvdom1 javperlag
infix 5 ~=
(~=) :: Double -> Double -> Bool
x ~= y = abs (x-y) < 0.001

-- ---------------------------------------------------------------------
-- Ejercicio 2.3. Comprobar con QuickCheck que si x es positivo,
-- entonces 
--    (raiz x)^2 ~= x
-- ---------------------------------------------------------------------

-- manvermor manpende blaruiher alvalvdom1 javperlag
-- La propiedad es
prop_raiz :: Double -> Bool
prop_raiz x = (raiz (abs x))^2 ~= abs x

-- La comprobación es
--    ghci> quickCheck prop_raiz
--    OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 2.4. Definir por recursión la función
--    until' :: (a -> Bool) -> (a -> a) -> a -> a
-- tal que (until' p f x) es el resultado de aplicar la función f a x el
-- menor número posible de veces, hasta alcanzar un valor que satisface
-- el predicado p. Por ejemplo, 
--    until' (>1000) (2*) 1  ==  1024
-- Nota: until' es equivalente a la predefinida until.
-- ---------------------------------------------------------------------

-- manvermor manpende alvalvdom1
until' :: (a -> Bool) -> (a -> a) -> a -> a
until' p f x | p x       = x
             | otherwise = until' p f (f x)

-- No es por recursión.
-- blaruiher javperlag
until'2 :: (a -> Bool) -> (a -> a) -> a -> a
until'2 p f x = head (dropWhile (not.p) (iterate f x))

-- ---------------------------------------------------------------------
-- Ejercicio 2.5. Definir, por iteración con until, la función
--    raizI :: (Fractional a, Ord a) => a -> a
-- tal que (raizI x) es la raíz cuadrada de x calculada usando la
-- propiedad anterior. Por ejemplo, 
--    raizI 9  ==  3.000000001396984
-- ---------------------------------------------------------------------

-- manvermor manpende blaruiher alvalvdom1 javperlag
raizI :: (Fractional a, Ord a) => a -> a
raizI x = until' loEs mejorar 1
    where loEs y    = abs (y^2 -x) < 0.00001
          mejorar y = (y+x/y) / 2

-- ---------------------------------------------------------------------
-- Ejercicio 2.6. Comprobar con QuickCheck que si x es positivo,
-- entonces 
--    (raizI x)^2 ~= x
-- ---------------------------------------------------------------------

-- manvermor manpende blaruiher alvalvdom1 javperlag
-- La propiedad es
prop_raizI :: Double -> Bool
prop_raizI x = (raizI (abs x))^2 ~= abs x

-- La comprobación es
--    ghci> quickCheck prop_raizI
--    OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ceros de una función                                               --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Los ceros de una función pueden calcularse mediante el
-- método de Newton basándose en las siguientes propiedades:
-- + Si b es una aproximación para el punto cero de f, entonces 
--   b-f(b)/f'(b) es una mejor aproximación.
-- + El límite de la sucesión x_n definida por
--      x_0     = 1 
--      x_{n+1} = x_n-f(x_n)/f'(x_n)
--   es un cero de f.
-- 
-- Definir por recursión la función
--    puntoCero :: (Double -> Double) -> Double
-- tal que (puntoCero f) es un cero de la función f calculado usando la
-- propiedad anterior. Por ejemplo, 
--    puntoCero cos  ==  1.5707963267949576
-- ---------------------------------------------------------------------

-- manvermor manpende blaruiher alvalvdom1
puntoCero :: (Double -> Double) -> Double
puntoCero f = puntoCeroAux f 1
    where puntoCeroAux f x | verifica x = x
                           | otherwise  = puntoCeroAux f (mejorar x)
          verifica b = abs (f b) < 0.00001
          mejorar b  = b - (f b) / derivadaFina f b 

-- javperlag
puntoCero2 :: (Double -> Double) -> Double
puntoCero2 f = aproxCero f 1

aproxCero f n
    | abs (f n) < 0.00001 = n
    | otherwise           = aproxCero f (n -(f n)/(derivadaSuper f n))  

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Definir, por iteración con until, la función
--    puntoCeroI :: (Double -> Double) -> Double
-- tal que (puntoCeroI f) es un cero de la función f calculado usando la
-- propiedad anterior. Por ejemplo, 
--    puntoCeroI cos  ==  1.5707963267949576
-- ---------------------------------------------------------------------

-- manvermor manpende blaruiher alvalvdom1 javperlag
puntoCeroI :: (Double -> Double) -> Double
puntoCeroI f = until' verifica mejorar 1
    where verifica b = abs (f b) < 0.00001
          mejorar b  = b - (f b) / derivadaFina f b

-- ---------------------------------------------------------------------
-- Funciones inversas                                                 --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 4. En este ejercicio se usará la función puntoCero para
-- definir la inversa de distintas funciones.
-- ---------------------------------------------------------------------
-- Ejercicio 4.1. Definir, usando puntoCero, la función
--    raizCuadrada :: Double -> Double
-- tal que (raizCuadrada x) es la raíz cuadrada de x. Por ejemplo,
--    raizCuadrada 9  ==  3.000000002941184
-- ---------------------------------------------------------------------

-- manvermor manpende javperlag
raizCuadrada :: Double -> Double
raizCuadrada a = puntoCero f 
    where f y = y^2 -a 

-- blaruiher alvalvdom1
raizCuadrada2 :: Double -> Double
raizCuadrada2 a = puntoCero (\x -> x^2 - a)


-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Comprobar con QuickCheck que si x es positivo,
-- entonces 
--    (raizCuadrada x)^2 ~= x
-- ---------------------------------------------------------------------

-- manvermor manpende blaruiher alvalvdom1 javperlag
-- La propiedad es
prop_raizCuadrada :: Double -> Bool
prop_raizCuadrada x = (raizCuadrada (abs x))^2 ~= abs x

-- La comprobación es
--    ghci> quickCheck prop_raizCuadrada
--    OK, passed 100 tests.


-- ---------------------------------------------------------------------
-- Ejercicio 4.3. Definir, usando puntoCero, la función
--    raizCubica :: Double -> Double
-- tal que (raizCubica x) es la raíz cuadrada de x. Por ejemplo,
--    raizCubica 27  ==  3.0000000000196048
-- ---------------------------------------------------------------------

-- manvermor manpende javperlag
raizCubica :: Double -> Double
raizCubica a = puntoCero f 
    where f y = y^3 -a 

-- blaruiher alvalvdom1
raizCubica2 :: Double -> Double
raizCubica2 a = puntoCero (\x -> x^3 - a)

-- ---------------------------------------------------------------------
-- Ejercicio 4.4. Comprobar con QuickCheck que si x es positivo,
-- entonces 
--    (raizCubica x)^3 ~= x
-- ---------------------------------------------------------------------

-- manvermor manpende blaruiher alvalvdom1 javperlag
-- La propiedad es
prop_raizCubica :: Double -> Bool
prop_raizCubica x = (raizCubica (abs x))^3 ~= abs x

-- La comprobación es
--    ghci> quickCheck prop_raizCubica
--    OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 4.5. Definir, usando puntoCero, la función
--    arcoseno :: Double -> Double
-- tal que (arcoseno x) es el arcoseno de x. Por ejemplo,
--    arcoseno 1  == 1.5665489428306574
-- ---------------------------------------------------------------------

-- manvermor manpende javperlag
arcoseno :: Double -> Double
arcoseno a = puntoCero f
    where f y = sin y - a

-- blaruiher alvalvdom1
arcoseno2 :: Double -> Double
arcoseno2 a = puntoCero (\x -> sin x - a)

-- ---------------------------------------------------------------------
-- Ejercicio 4.6. Comprobar con QuickCheck que si x está entre 0 y 1,
-- entonces 
--    sin (arcoseno x) ~= x
-- ---------------------------------------------------------------------

-- manvermor manpende blaruiher alvalvdom1
-- La propiedad es
prop_arcoseno :: Double -> Bool
prop_arcoseno x = sin (arcoseno (abs y)) ~= abs y
    where y = x - fromIntegral (truncate x)

-- La comprobación es
--    ghci> quickCheck prop_arcoseno
--    OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 4.7. Definir, usando puntoCero, la función
--    arcocoseno :: Double -> Double
-- tal que (arcoseno x) es el arcoseno de x. Por ejemplo,
--    arcocoseno 0  == 1.5707963267949576
-- ---------------------------------------------------------------------

-- manvermor manpende javperlag
arcocoseno :: Double -> Double
arcocoseno a = puntoCero f
    where f y = cos y - a
                           
-- blaruiher alvalvdom1
arcocoseno2 :: Double -> Double
arcocoseno2 a = puntoCero(\x -> cos x - a)

-- ---------------------------------------------------------------------
-- Ejercicio 4.8. Comprobar con QuickCheck que si x está entre 0 y 1,
-- entonces 
--    cos (arcocoseno x) ~= x
-- ---------------------------------------------------------------------

-- manvermor manpende blaruiher alvalvdom1
-- La propiedad es
prop_arcocoseno :: Double -> Bool
prop_arcocoseno x = cos (arcocoseno (abs y)) ~= abs y
    where y = x - fromIntegral (truncate x)


-- La comprobación es
--    ghci> quickCheck prop_arcocoseno
--    OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 4.9. Definir, usando puntoCero, la función
--    inversa :: (Double -> Double) -> Double -> Double
-- tal que (inversa g x) es el valor de la inversa de g en x. Por
-- ejemplo, 
--    inversa (^2) 9  ==  3.000000002941184
-- ---------------------------------------------------------------------

-- manvermor manpende javperlag
inversa :: (Double -> Double) -> Double -> Double
inversa g a = puntoCero f
    where f x = g x - a

-- blaruiher alvalvdom1
inversa2 :: (Double -> Double) -> Double -> Double
inversa2 g a = puntoCero (\x -> g x - a)
              
-- ---------------------------------------------------------------------
-- Ejercicio 4.10. Redefinir, usando inversa, las funciones raizCuadrada,
-- raizCubica, arcoseno y arcocoseno.
-- ---------------------------------------------------------------------

-- manvermor manpende blaruiher alvalvdom1 javperlag
raizCuadrada' = inversa (^2)
raizCubica'   = inversa (^3)
arcoseno'     = inversa sin
arcocoseno'   = inversa cos
