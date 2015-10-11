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

potencia :: Integer -> Integer -> Integer
potencia = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Comprobar con QuickCheck que la función potencia es
-- equivalente a la predefinida (^).
-- ---------------------------------------------------------------------

-- La propiedad es
prop_potencia :: Integer -> Integer -> Property
prop_potencia x n = undefined

-- La comprobación es

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

mcd :: Integer -> Integer -> Integer
mcd = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Definir y comprobar la propiedad prop_mcd según la
-- cual el máximo común divisor de dos números a y b (ambos mayores que
-- 0) es siempre mayor o igual que 1 y además es menor o igual que el
-- menor de los números a  y b. 
-- ---------------------------------------------------------------------

-- La propiedad es
prop_mcd :: Integer -> Integer -> Property
prop_mcd a b = undefined

-- Su comprobación es

-- ---------------------------------------------------------------------
-- Ejercicio 2.3. Teniendo en cuenta que buscamos el máximo común
-- divisor de a y b, sería razonable pensar que el máximo común divisor
-- siempre sería igual o menor que la mitad del máximo de a y b. Definir
-- esta propiedad y comprobarla.  
-- ---------------------------------------------------------------------

-- La propiedad es
prop_mcd_div :: Integer -> Integer -> Property
prop_mcd_div a b = undefined

-- La comprobación es

-- ---------------------------------------------------------------------
-- Ejercicio 3.1, Definir por recursión la función
--    pertenece :: Eq a => a -> [a] -> Bool
-- tal que (pertenece x xs) se verifica si x pertenece a la lista xs. Por
-- ejemplo, 
--    pertenece 3 [2,3,5]  ==  True
--    pertenece 4 [2,3,5]  ==  False
-- ---------------------------------------------------------------------

pertenece :: Eq a => a -> [a] -> Bool
pertenece = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Comprobar con quickCheck que pertenece es equivalente
-- a elem. 
-- ---------------------------------------------------------------------

-- La propiedad es
prop_pertenece :: Eq a => a -> [a] -> Bool
prop_pertenece x xs = undefined

-- La comprobación es

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. Definir por recursión la función
--    concatenaListas :: [[a]] -> [a]
-- tal que (concatenaListas xss) es la lista obtenida concatenando las listas de
-- xss. Por ejemplo,
--    concatenaListas [[1..3],[5..7],[8..10]]  ==  [1,2,3,5,6,7,8,9,10]
-- ---------------------------------------------------------------------
 
concatenaListas :: [[a]] -> [a]
concatenaListas = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Comprobar con QuickCheck que concatenaListas es
-- equivalente a concat. 
-- ---------------------------------------------------------------------

-- La propiedad es
prop_concat :: Eq a => [[a]] -> Bool
prop_concat xss = undefined

-- La comprobación es

-- ---------------------------------------------------------------------
-- Ejercicio 5.1. Definir por recursión la función
--    coge :: Int -> [a] -> [a]
-- tal que (coge n xs) es la lista de los n primeros elementos de
-- xs. Por ejemplo, 
--    coge   3  [4..12]  ==  [4,5,6]
--    coge (-3) [4..12]  ==  []
-- ---------------------------------------------------------------------

coge :: Int -> [a] -> [a]
coge = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 5.2. Comprobar con QuickCheck que coge es equivalente a
-- take. 
-- ---------------------------------------------------------------------

-- La propiedad es
prop_coge :: Int -> [Int] -> Bool
prop_coge n xs = undefined

-- La comprobación es

-- ---------------------------------------------------------------------
-- Ejercicio 6.1. Definir, por recursión, la función 
--    sumaCuadradosR :: Integer -> Integer
-- tal que (sumaCuadradosR n) es la suma de los cuadrados de los números
-- de 1 a n. Por ejemplo, 
--    sumaCuadradosR 4  ==  30 
-- ---------------------------------------------------------------------

sumaCuadradosR :: Integer -> Integer
sumaCuadradosR = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 6.2. Comprobar con QuickCheck si sumaCuadradosR n es igual a
-- n(n+1)(2n+1)/6. 
-- ---------------------------------------------------------------------

-- La propiedad es
prop_SumaCuadrados :: Integer -> Property
prop_SumaCuadrados n = undefined

-- La comprobación es

-- ---------------------------------------------------------------------
-- Ejercicio 6.3. Definir, por comprensión, la función 
--    sumaCuadradosC :: Integer --> Integer
-- tal que (sumaCuadradosC n) es la suma de los cuadrados de los números
-- de 1 a n. Por ejemplo, 
--    sumaCuadradosC 4  ==  30 
-- ---------------------------------------------------------------------

sumaCuadradosC :: Integer -> Integer
sumaCuadradosC n = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 6.4. Comprobar con QuickCheck que las funciones
-- sumaCuadradosR y sumaCuadradosC son equivalentes sobre los números
-- naturales. 
-- ---------------------------------------------------------------------

-- La propiedad es
prop_sumaCuadradosR :: Integer -> Property
prop_sumaCuadradosR n = undefined

-- La comprobación es

-- ---------------------------------------------------------------------
-- Ejercicio 7.1. Definir, por recursión, la función
--    digitosR :: Integer -> [Integer]
-- tal que (digitosR n) es la lista de los dígitos del número n. Por
-- ejemplo, 
--    digitosR 320274  ==  [3,2,0,2,7,4]
-- ---------------------------------------------------------------------

digitosR :: Integer -> [Integer]
digitosR n = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 7.2. Definir, por comprensión, la función
--    digitosC :: Integer -> [Integer]
-- tal que (digitosC n) es la lista de los dígitos del número n. Por
-- ejemplo, 
--    digitosC 320274  ==  [3,2,0,2,7,4]
-- Indicación: Usar las funciones show y read.
-- ---------------------------------------------------------------------

digitosC :: Integer -> [Integer]
digitosC n = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 7.3. Comprobar con QuickCheck que las funciones digitosR y
-- digitosC son equivalentes.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_digitos :: Integer -> Property
prop_digitos n = undefined
  
-- La comprobación es

-- ---------------------------------------------------------------------
-- Ejercicio 8.1. Definir, por recursión, la función 
--    sumaDigitosR :: Integer -> Integer
-- tal que (sumaDigitosR n) es la suma de los dígitos de n. Por ejemplo,
--    sumaDigitosR 3     ==  3
--    sumaDigitosR 2454  == 15
--    sumaDigitosR 20045 == 11
-- ---------------------------------------------------------------------

sumaDigitosR :: Integer -> Integer
sumaDigitosR n = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 8.2. Definir, sin usar recursión, la función 
--    sumaDigitosNR :: Integer -> Integer
-- tal que (sumaDigitosNR n) es la suma de los dígitos de n. Por ejemplo,
--    sumaDigitosNR 3     ==  3
--    sumaDigitosNR 2454  == 15
--    sumaDigitosNR 20045 == 11
-- ---------------------------------------------------------------------

sumaDigitosNR :: Integer -> Integer
sumaDigitosNR n = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 8.3. Comprobar con QuickCheck que las funciones sumaDigitosR
-- y sumaDigitosNR son equivalentes.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_sumaDigitos :: Integer -> Property
prop_sumaDigitos n = undefined

-- La comprobación es

-- ---------------------------------------------------------------------
-- Ejercicio 9.1. Definir, por recursión, la función 
--    listaNumeroR :: [Integer] -> Integer
-- tal que (listaNumeroR xs) es el número formado por los dígitos xs. Por
-- ejemplo, 
--    listaNumeroR [5]        == 5
--    listaNumeroR [1,3,4,7]  == 1347
--    listaNumeroR [0,0,1]    == 1
-- ---------------------------------------------------------------------

listaNumeroR :: [Integer] -> Integer
listaNumeroR xs = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 9.2. Definir, por comprensión, la función 
--    listaNumeroC :: [Integer] -> Integer
-- tal que (listaNumeroC xs) es el número formado por los dígitos xs. Por
-- ejemplo, 
--    listaNumeroC [5]        == 5
--    listaNumeroC [1,3,4,7]  == 1347
--    listaNumeroC [0,0,1]    == 1
-- ---------------------------------------------------------------------

listaNumeroC :: [Integer] -> Integer
listaNumeroC xs = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 9.3. Comprobar con QuickCheck que las funciones
-- listaNumeroR y listaNumeroC son equivalentes.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_listaNumero :: [Integer] -> Bool
prop_listaNumero xs = undefined

-- La comprobación es

-- ---------------------------------------------------------------------
-- Ejercicio 10. Definir la función 
--    capicua :: Integer -> Bool
-- tal que (capicua n) se verifica si los dígitos que n son las mismos
-- de izquierda a derecha que de derecha a izquierda. Por ejemplo,
--    capicua 1234  =  False
--    capicua 1221  =  True
--    capicua 4     =  True
-- ---------------------------------------------------------------------

capicua :: Integer -> Bool
capicua n = undefined

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

mayorExponenteR :: Integer -> Integer -> Integer 
mayorExponenteR a b = undefined

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

aproximaPiC n = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 12.2. Definir, por recursión, la función aproximaPiR tal
-- que (aproximaPiR n) es la aproximación  de pi obtenida mediante n
-- términos de la serie. Por ejemplo,  
--    aproximaPiR 4    == sqrt(6*(1/1^2 + 1/2^2 + 1/3^2 + 1/4^2))
--                     == 2.9226129861250305
--    aproximaPiR 1000 == 3.1406380562059946
-- ---------------------------------------------------------------------

aproximaPiR n = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 13.1. Comprobar con QuickCheck si la función mcd definida
-- en el ejercicio 2.1 es equivalente a la función gcd
-- ---------------------------------------------------------------------

-- La propiedad es
prop_mcd_gcd :: Integer -> Integer -> Bool
prop_mcd_gcd a b = undefined

-- La comprobación es

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

mcdE :: Integer -> Integer -> Integer
mcdE = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 13.3. Comprobar con QuickCheck si las funciones mcdE  y gcd
-- son equivalentes. 
-- ---------------------------------------------------------------------

-- La propiedad es
prop_mcdE_gcd :: Integer -> Integer -> Bool
prop_mcdE_gcd a b = undefined

-- La comprobación es

-- ---------------------------------------------------------------------
-- Ejercicio 13.4. Comprobar con QuickCheck que (mcd a b) es un divisor
-- de a y de b.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_mcdE_esDivisor :: Integer -> Integer -> Property
prop_mcdE_esDivisor a b = undefined

-- La comprobación es

-- ---------------------------------------------------------------------
-- Ejercicio 13.4. Comprobar con QuickCheck que todos los divisores
-- comunes de a y b son divisores de (mcdE a b).
-- ---------------------------------------------------------------------

-- La propiedad es
prop_mcdE_esMaximo :: Integer -> Integer -> Integer -> Property
prop_mcdE_esMaximo a b c = undefined

-- La comprobación es

-- La propiedad es
prop_mcdE_esMaximo2 :: Integer -> Integer -> Integer -> Property
prop_mcdE_esMaximo2 a b c = undefined

-- La comprobación es

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
