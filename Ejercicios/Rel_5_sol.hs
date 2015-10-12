-- I1M 2015-16: Rel_5.hs (7 de octubre de 2015)
-- Definiciones por recursión.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- En esta relación se presentan ejercicios con definiciones por
-- recursión correspondientes al tema 6 cuyas transparencias se 
-- encuentran en  
--    http://www.cs.us.es/~jalonso/cursos/i1m-15/temas/tema-6.html
 
-- ---------------------------------------------------------------------
-- Importación de librerías auxiliares                                --
-- ---------------------------------------------------------------------

import Test.QuickCheck
import Data.List

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Definir por recursión la función
--    potencia :: Integer -> Integer -> Integer
-- tal que (potencia x n) es x elevado al número natural n. Por ejemplo,  
--    potencia 2 3  ==  8
-- ---------------------------------------------------------------------

-- guache pabmorgar manvermor carruirui3 manvazbar1 alvalvdom1 erisancha
potencia :: Integer -> Integer -> Integer
potencia x 0 = 1
potencia x n = x * (potencia x (n-1))

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Comprobar con QuickCheck que la función potencia es
-- equivalente a la predefinida (^).
-- ---------------------------------------------------------------------

-- guache pabmorgar manvermor carruirui3 manvazbar1 alvalvdom1 erisancha

-- La propiedad es
prop_potencia :: Integer -> Integer -> Property
prop_potencia x n = n>=0 ==> potencia x n == x^n

-- La comprobación es
--   *Main> quickCheck prop_potencia
--   +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Dados dos números naturales, a y b, es posible
-- calcular su máximo común divisor mediante el Algoritmo de
-- Euclides. Este algoritmo se puede resumir en la siguiente fórmula:
--    mcd(a,b) = a,                   si b = 0
--             = mcd (b, a módulo b), si b > 0
-- 
-- Definir la función 
--    mcd :: Integer -> Integer -> Integer
-- tal que (mcd a b) es el máximo común divisor de a y b calculado
-- mediante el algoritmo de Euclides. Por ejemplo,
--    mcd 30 45  ==  15
-- ---------------------------------------------------------------------

-- guache pabmorgar manvermor carruirui3 manvazbar1 alvalvdom1 erisancha
mcd :: Integer -> Integer -> Integer
mcd a 0 = a
mcd a b = mcd b (mod a b)

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Definir y comprobar la propiedad prop_mcd según la
-- cual el máximo común divisor de dos números a y b (ambos mayores que
-- 0) es siempre mayor o igual que 1 y además es menor o igual que el
-- menor de los números a  y b. 
-- ---------------------------------------------------------------------

-- guache

-- La propiedad es
prop_mcd :: Integer -> Integer -> Property
prop_mcd a b = and[a/=b,a>0,b>0] ==> mcd a b >=1 && mcd a b <= min a b

-- Comentario: La definición anterior se puede mejorar.

-- Su comprobación es
--    *Main> quickCheck prop_mcd
--    +++ OK, passed 100 tests.

-- pabmorgar manvermor carruirui3 manvazbar1 alvalvdom1 erisancha
prop_mcd2 :: Integer -> Integer -> Property
prop_mcd2 a b = a > 0 && b > 0 ==> mcd a b >= 1 && mcd a b <= min a b

-- ---------------------------------------------------------------------
-- Ejercicio 2.3. Teniendo en cuenta que buscamos el máximo común
-- divisor de a y b, sería razonable pensar que el máximo común divisor
-- siempre sería igual o menor que la mitad del máximo de a y b. Definir
-- esta propiedad y comprobarla.  
-- ---------------------------------------------------------------------

-- guache erisancha
-- La propiedad es
prop_mcd_div :: Integer -> Integer -> Property
prop_mcd_div a b = and [a/=b, a>0, b>0] ==> mcd a b <= div (max a b) 2

-- La comprobación es
--    *Main> quickCheck prop_mcd_div
--    +++ OK, passed 100 tests.

-- pabmorgar manvermor carruirui3 manvazbar1 alvalvdom1
prop_mcd_div2 :: Integer -> Integer -> Property
prop_mcd_div2 a b = a > 0 && b > 0 ==> mcd a b <= (max a b)`div`2

--    ghci> quickCheck prop_mcd_div2
--    *** Failed! Falsifiable (after 2 tests): 
--    1
--    1

-- ---------------------------------------------------------------------
-- Ejercicio 3.1, Definir por recursión la función
--    pertenece :: Eq a => a -> [a] -> Bool
-- tal que (pertenece x xs) se verifica si x pertenece a la lista xs. Por
-- ejemplo, 
--    pertenece 3 [2,3,5]  ==  True
--    pertenece 4 [2,3,5]  ==  False
-- ---------------------------------------------------------------------

-- guache manvermor carruirui3 manvazbar1 erisancha
pertenece :: Eq a => a -> [a] -> Bool
pertenece x []     = False
pertenece x (y:ys) = x == y || pertenece x ys

-- pabmorgar
pertenece1 :: Eq a => a -> [a] -> Bool
pertenece1 _ [] = False
pertenece1 x (y:ys) | x == y    = True 
                    | otherwise = pertenece x ys 

-- Comentario: La definición anterior se puede simplificar.

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Comprobar con QuickCheck que pertenece es equivalente
-- a elem. 
-- ---------------------------------------------------------------------

-- guache pabmorgar manvermor carruirui3 manvazbar1 erisancha

-- La propiedad es
prop_pertenece :: Eq a => a -> [a] -> Bool
prop_pertenece x xs = pertenece x xs == elem x xs

-- La comprobación es
--    *Main> quickCheck prop_pertenece
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. Definir por recursión la función
--    concatenaListas :: [[a]] -> [a]
-- tal que (concatenaListas xss) es la lista obtenida concatenando las
-- listas de xss. Por ejemplo,
--    concatenaListas [[1..3],[5..7],[8..10]]  ==  [1,2,3,5,6,7,8,9,10]
-- ---------------------------------------------------------------------

-- pabmorgar manvermor carruirui3 manvazbar1 alvalvdom1 erisancha
concatenaListas :: [[a]] -> [a]
concatenaListas []       = [] 
concatenaListas (xs:xss) = xs ++  concatenaListas xss

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Comprobar con QuickCheck que concatenaListas es
-- equivalente a concat. 
-- ---------------------------------------------------------------------

-- pabmorgar manvermor carruirui3 manvazbar1 alvalvdom1 erisancha

-- La propiedad es
prop_concat :: Eq a => [[a]] -> Bool
prop_concat xss = concatenaListas xss == concat xss

-- La comprobación es
--    ghci> quickCheck prop_concat
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 5.1. Definir por recursión la función
--    coge :: Int -> [a] -> [a]
-- tal que (coge n xs) es la lista de los n primeros elementos de
-- xs. Por ejemplo, 
--    coge   3  [4..12]  ==  [4,5,6]
--    coge (-3) [4..12]  ==  []
-- ---------------------------------------------------------------------

-- pabmorgar manvermor carruirui3 alvalvdom1 erisancha
coge :: Int -> [a] -> [a]
coge n  _ | n <= 0 = [] 
coge n []          = [] 
coge n (x:xs)      = x : coge (n-1) xs

-- ---------------------------------------------------------------------
-- Ejercicio 5.2. Comprobar con QuickCheck que coge es equivalente a
-- take. 
-- ---------------------------------------------------------------------

-- pabmorgar manvermor carruirui3 alvalvdom1 erisancha

-- La propiedad es
prop_coge :: Int -> [Int] -> Bool
prop_coge n xs = coge n xs == take n xs

-- La comprobación es
--    ghci> quickCheck prop_coge
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 6.1. Definir, por recursión, la función 
--    sumaCuadradosR :: Integer -> Integer
-- tal que (sumaCuadradosR n) es la suma de los cuadrados de los números
-- de 1 a n. Por ejemplo, 
--    sumaCuadradosR 4  ==  30 
--    sumaCuadradosR 0  ==   0 
-- ---------------------------------------------------------------------
 
-- pabmorgar manvermor carruirui3 erisancha
sumaCuadradosR :: Integer -> Integer
sumaCuadradosR 0 = 0
sumaCuadradosR n = n^2 + sumaCuadradosR (n-1)

-- ---------------------------------------------------------------------
-- Ejercicio 6.2. Comprobar con QuickCheck si sumaCuadradosR n es igual a
-- n(n+1)(2n+1)/6. 
-- ---------------------------------------------------------------------

-- pabmorgar manvermor carruirui3 erisancha
-- La propiedad es
prop_SumaCuadrados :: Integer -> Property
prop_SumaCuadrados n =  
    n >= 0 ==> n*(n+1)*(2*n+1)`div`6 == sumaCuadradosR n 

-- La comprobación es
--    ghci> quickCheck prop_SumaCuadrados
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 6.3. Definir, por comprensión, la función 
--    sumaCuadradosC :: Integer --> Integer
-- tal que (sumaCuadradosC n) es la suma de los cuadrados de los números
-- de 1 a n. Por ejemplo, 
--    sumaCuadradosC 4  ==  30 
-- ---------------------------------------------------------------------

--pabmorgar manvermor carruirui3 erisancha
sumaCuadradosC :: Integer -> Integer
sumaCuadradosC n = sum [x^2 | x <- [1..n]]

-- ---------------------------------------------------------------------
-- Ejercicio 6.4. Comprobar con QuickCheck que las funciones
-- sumaCuadradosR y sumaCuadradosC son equivalentes sobre los números
-- naturales. 
-- ---------------------------------------------------------------------

-- pabmorgar manvermor carruirui3 erisancha

-- La propiedad es
prop_sumaCuadradosR :: Integer -> Property
prop_sumaCuadradosR n =
    n >= 0 ==> sumaCuadradosR n == sumaCuadradosC n 

-- La comprobación es
--    ghci> quickCheck prop_sumaCuadradosR
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 7.1. Definir, por recursión, la función
--    digitosR :: Integer -> [Integer]
-- tal que (digitosR n) es la lista de los dígitos del número n. Por
-- ejemplo, 
--    digitosR 320274  ==  [3,2,0,2,7,4]
-- ---------------------------------------------------------------------

-- pabmorgar manvermor carruirui3 erisancha
digitosR :: Integer -> [Integer]
digitosR n = reverse (digitosR1 n)

digitosR1 n | n < 10    = [n]
            | otherwise = (n `rem` 10) : digitosR1 (n `div` 10)

-- ---------------------------------------------------------------------
-- Ejercicio 7.2. Definir, por comprensión, la función
--    digitosC :: Integer -> [Integer]
-- tal que (digitosC n) es la lista de los dígitos del número n. Por
-- ejemplo, 
--    digitosC 320274  ==  [3,2,0,2,7,4]
-- Indicación: Usar las funciones show y read.
-- ---------------------------------------------------------------------

-- pabmorgar manvermor carruirui3
digitosC :: Integer -> [Integer]
digitosC n = [read [c] | c <- show n ]

-- ---------------------------------------------------------------------
-- Ejercicio 7.3. Comprobar con QuickCheck que las funciones digitosR y
-- digitosC son equivalentes.
-- ---------------------------------------------------------------------

-- pabmorgar manvermor carruirui3

-- La propiedad es
prop_digitos :: Integer -> Property
prop_digitos n = n>= 0 ==> digitosC n == digitosR n 

-- La comprobación es
--    ghci> quickCheck prop_digitos
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 8.1. Definir, por recursión, la función 
--    sumaDigitosR :: Integer -> Integer
-- tal que (sumaDigitosR n) es la suma de los dígitos de n. Por ejemplo,
--    sumaDigitosR 3     ==  3
--    sumaDigitosR 2454  == 15
--    sumaDigitosR 20045 == 11
-- ---------------------------------------------------------------------

-- pabmorgar manvermor 
sumaDigitosR :: Integer -> Integer
sumaDigitosR n = sum (digitosR n)

-- carruirui3
sumaDigitosR2 :: Integer -> Integer
sumaDigitosR2 0 = 0
sumaDigitosR2 n = n `mod` 10 + sumaDigitosR2 (n `div` 10)

-- ---------------------------------------------------------------------
-- Ejercicio 8.2. Definir, sin usar recursión, la función 
--    sumaDigitosNR :: Integer -> Integer
-- tal que (sumaDigitosNR n) es la suma de los dígitos de n. Por ejemplo,
--    sumaDigitosNR 3     ==  3
--    sumaDigitosNR 2454  == 15
--    sumaDigitosNR 20045 == 11
-- ---------------------------------------------------------------------

-- pabmorgar  manvermor carruirui3
sumaDigitosNR :: Integer -> Integer
sumaDigitosNR n = sum (digitosC n)

-- ---------------------------------------------------------------------
-- Ejercicio 8.3. Comprobar con QuickCheck que las funciones sumaDigitosR
-- y sumaDigitosNR son equivalentes.
-- ---------------------------------------------------------------------

-- pabmorgar manvermor carruirui3

-- La propiedad es
prop_sumaDigitos :: Integer -> Property
prop_sumaDigitos n = n>= 0 ==> sumaDigitosNR n == sumaDigitosR n 

-- La comprobación es
--    ghci> quickCheck prop_sumaDigitos
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 9.1. Definir, por recursión, la función 
--    listaNumeroR :: [Integer] -> Integer
-- tal que (listaNumeroR xs) es el número formado por los dígitos xs. Por
-- ejemplo, 
--    listaNumeroR [5]        == 5
--    listaNumeroR [1,3,4,7]  == 1347
--    listaNumeroR [0,0,1]    == 1
-- ---------------------------------------------------------------------

-- pabmorgar manvermor carruiui3
listaNumeroR :: [Integer] -> Integer
listaNumeroR []     = 0 
listaNumeroR (x:xs) = x*10^(length (xs)) + listaNumeroR xs

-- ---------------------------------------------------------------------
-- Ejercicio 9.2. Definir, por comprensión, la función 
--    listaNumeroC :: [Integer] -> Integer
-- tal que (listaNumeroC xs) es el número formado por los dígitos xs. Por
-- ejemplo, 
--    listaNumeroC [5]        == 5
--    listaNumeroC [1,3,4,7]  == 1347
--    listaNumeroC [0,0,1]    == 1
-- ---------------------------------------------------------------------

-- pabmorgar manvermor carruirui3
listaNumeroC :: [Integer] -> Integer
listaNumeroC xs = sum [y*10^n | (y,n) <- zip (reverse xs) [0..]]

-- ---------------------------------------------------------------------
-- Ejercicio 9.3. Comprobar con QuickCheck que las funciones
-- listaNumeroR y listaNumeroC son equivalentes.
-- ---------------------------------------------------------------------

-- pabmorgar manvermor carruirui3
-- La propiedad es
prop_listaNumero :: [Integer] -> Bool
prop_listaNumero xs = listaNumeroC xs == listaNumeroR xs 

-- La comprobación es
--    ghci> quickCheck prop_listaNumero
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 10. Definir la función 
--    capicua :: Integer -> Bool
-- tal que (capicua n) se verifica si los dígitos que n son las mismos
-- de izquierda a derecha que de derecha a izquierda. Por ejemplo,
--    capicua 1234  =  False
--    capicua 1221  =  True
--    capicua 4     =  True
-- ---------------------------------------------------------------------

-- pabmorgar manvermor carruirui3
capicua :: Integer -> Bool
capicua n =  digitosR n == reverse (digitosR n)

-- ---------------------------------------------------------------------
-- Ejercicio 11.1. Definir, por recursión, la función 
--    mayorExponenteR :: Integer -> Integer -> Integer 
-- tal que (mayorExponenteR a b) es el exponente de la mayor potencia de
-- a que divide b. Por ejemplo,
--    mayorExponenteR 2 8    ==  3
--    mayorExponenteR 2 9    ==  0
--    mayorExponenteR 5 100  ==  2
--    mayorExponenteR 2 60   ==  2
-- ---------------------------------------------------------------------

-- carruirui3
mayorExponenteR :: Integer -> Integer -> Integer
mayorExponenteR a b 
    | mcd a b == 1 = 0
    | otherwise    = 1 + mayorExponenteR a (b `div` a)

-- ---------------------------------------------------------------------
-- Ejercicio 11.2. Definir, por comprensión, la función 
--    mayorExponenteC :: Integer -> Integer -> Integer 
-- tal que (mayorExponenteC a b) es el exponente de la mayor potencia de
-- a que divide a b. Por ejemplo,
--    mayorExponenteC 2 8    ==  3
--    mayorExponenteC 5 100  ==  2
--    mayorExponenteC 5 101  ==  0
-- ---------------------------------------------------------------------

mayorExponenteC :: Integer -> Integer -> Integer
mayorExponenteC a b = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 12.1. La suma de la serie
--    1/1^2 + 1/2^2 + 1/3^2 + 1/4^2 + ...
-- es pi^2/6. Por tanto, pi se puede aproximar mediante la raíz cuadrada
-- de 6 por la suma de la serie.
-- 
-- Definir, por comprensión, la función aproximaPiC tal que 
-- (aproximaPiC n) es la aproximación  de pi obtenida mediante n
-- términos de la serie. Por ejemplo,  
--    aproximaPiC 4    == sqrt(6*(1/1^2 + 1/2^2 + 1/3^2 + 1/4^2))
--                     == 2.9226129861250305
--    aproximaPiC 1000 == 3.1406380562059946
-- ---------------------------------------------------------------------

-- pabmorgar carruirui3
aproximaPiC n = sqrt (6*sum[1/x^2| x <- [1..n]])

-- manvermor
aproximaPiC2 n = sqrt (sum [6/x^2 | x <- [1..n]])

-- ---------------------------------------------------------------------
-- Ejercicio 12.2. Definir, por recursión, la función aproximaPiR tal
-- que (aproximaPiR n) es la aproximación  de pi obtenida mediante n
-- términos de la serie. Por ejemplo,  
--    aproximaPiR 4    == sqrt(6*(1/1^2 + 1/2^2 + 1/3^2 + 1/4^2))
--                     == 2.9226129861250305
--    aproximaPiR 1000 == 3.1406380562059946
-- ---------------------------------------------------------------------

-- pabmorgar manvermor carruirui3
aproximaPiR n = sqrt(6*aproximaPiR'  n)
    where aproximaPiR' 1 = 1 
          aproximaPiR' n = 1/n^2 + aproximaPiR' (n-1)

-- ---------------------------------------------------------------------
-- Ejercicio 13.1. Comprobar con QuickCheck si la función mcd definida
-- en el ejercicio 2.1 es equivalente a la función gcd
-- ---------------------------------------------------------------------

-- pabmorgar manvermor carruirui3

-- La propiedad es
prop_mcd_gcd :: Integer -> Integer -> Bool
prop_mcd_gcd a b = mcd a b == gcd a b 

-- La comprobación es
-- *** Failed! Falsifiable (after 5 tests and 2 shrinks):
-- 0
-- -1

-- ---------------------------------------------------------------------
-- Ejercicio 13.2. Definir la función 
--    mcdE :: Integer -> Integer -> Integer
-- tal que (mcdE a b) es el máximo común divisor de a y b calculado
-- mediante el algoritmo de Euclides, pero extendido a los números
-- negativos. Por ejemplo, 
--    mcdE 30 45  ==  15
--    mcdE (-2) 0 ==  2
--    mcdE (-4) 6 ==  2 
--    mcdE 0 4    ==  4 
--    mcdE 0 0    ==  0
-- ---------------------------------------------------------------------

-- pabmorgar manvermor carruirui3
mcdE :: Integer -> Integer -> Integer
mcdE a 0 = abs a
mcdE a b = mcdE b (mod a b)

-- ---------------------------------------------------------------------
-- Ejercicio 13.3. Comprobar con QuickCheck si las funciones mcdE  y gcd
-- son equivalentes. 
-- ---------------------------------------------------------------------

-- pabmorgar manvermor carruirui3

-- La propiedad es
prop_mcdE_gcd :: Integer -> Integer -> Bool
prop_mcdE_gcd a b = mcdE a b == gcd a b

-- La comprobación es
--    ghci> quickCheck prop_mcdE_gcd
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 13.4. Comprobar con QuickCheck que (mcd a b) es un divisor
-- de a y de b.
-- ---------------------------------------------------------------------

-- pabmorgar

-- La propiedad es
prop_mcdE_esDivisor :: Integer -> Integer -> Property
prop_mcdE_esDivisor a b =  
    a > 0 && b > 0 ==> 
     a `rem` (mcd a b) == 0 && 
     b `rem` (mcd a b) == 0

-- Comentario: La definición anterior se puede mejorar.

-- La comprobación es
--    *Main> quickCheck prop_mcdE_esDivisor
--    +++ OK, passed 100 tests.

-- manvermor carruirui3
prop_mcdE_esDivisor2 :: Integer -> Integer -> Property
prop_mcdE_esDivisor2 a b = 
    c /= 0 ==> mod a c == 0 && mod b c == 0
    where c = mcdE a b

-- La comprobación es
--    *Main> quickCheck prop_mcdE_esDivisor
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 13.4. Comprobar con QuickCheck que todos los divisores
-- comunes de a y b son divisores de (mcdE a b).
-- ---------------------------------------------------------------------

-- pabmorgar

-- La propiedad es
prop_mcdE_esMaximo :: Integer -> Integer -> Integer -> Property
prop_mcdE_esMaximo a b c =   
    a > 0 && b> 0 ==> 
      and [divide x (mcdE a b) | x <- divisores a, divide x b]

divisores :: Integer -> [Integer]
divisores n = [x | x <- [1..n], rem n x == 0]

divide :: Integer -> Integer -> Bool
divide x y = rem x y == 0

-- La comprobación es
--    ghci> quickCheck prop_mcdE_esMaximo
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 14.1. Definir, por comprensión, la función 
--    mcdC :: Integer -> Integer -> Integer
-- tal que (mcdC a b) es el máximo común divisor de a y b. Por ejemplo, 
--    mcdC 30 45  ==  15
--    mcdC (-2) 0 ==  2
--    mcdC (-4) 6 ==  2 
--    mcdC 0 4    ==  4 
--    mcdC 0 0    ==  0
-- ---------------------------------------------------------------------

mcdC :: Integer -> Integer -> Integer
mcdC = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 14.2. Comprobar con QuickCheck si las funciones mcdC  y gcd
-- son equivalentes. 
-- ---------------------------------------------------------------------

-- La propiedad es
prop_mcdC_gcd :: Integer -> Integer -> Bool
prop_mcdC_gcd a b = undefined

-- La comprobación es
