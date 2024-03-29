-- I1M 2016-17: Rel_5.hs (21 de octubre de 2016)
-- Definiciones por recursi�n (2).
-- Departamento de Ciencias de la Computaci�n e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducci�n                                                       --
-- ---------------------------------------------------------------------

-- En esta relaci�n se presentan ejercicios con definiciones por
-- recursi�n correspondientes al tema 6 cuyas transparencias se 
-- encuentran en  
--    http://www.cs.us.es/~jalonso/cursos/i1m-16/temas/tema-6.html
 
-- ---------------------------------------------------------------------
-- Importaci�n de librer�as auxiliares                                --
-- ---------------------------------------------------------------------

import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Definir, por recursi�n, la funci�n 
--    sumaCuadradosR :: Integer -> Integer
-- tal que (sumaCuadradosR n) es la suma de los cuadrados de los n�meros
-- de 1 a n. Por ejemplo, 
--    sumaCuadradosR 4  ==  30 
-- ---------------------------------------------------------------------

sumaCuadradosR :: Integer -> Integer
sumaCuadradosR = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Comprobar con QuickCheck si sumaCuadradosR n es igual a
-- n(n+1)(2n+1)/6. 
-- ---------------------------------------------------------------------

-- La propiedad es
prop_SumaCuadrados :: Integer -> Property
prop_SumaCuadrados n = undefined

-- La comprobaci�n es

-- ---------------------------------------------------------------------
-- Ejercicio 1.3. Definir, por comprensi�n, la funci�n 
--    sumaCuadradosC :: Integer --> Integer
-- tal que (sumaCuadradosC n) es la suma de los cuadrados de los n�meros
-- de 1 a n. Por ejemplo, 
--    sumaCuadradosC 4  ==  30 
-- ---------------------------------------------------------------------

sumaCuadradosC :: Integer -> Integer
sumaCuadradosC n = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 1.4. Comprobar con QuickCheck que las funciones
-- sumaCuadradosR y sumaCuadradosC son equivalentes sobre los n�meros
-- naturales. 
-- ---------------------------------------------------------------------

-- La propiedad es
prop_sumaCuadradosR :: Integer -> Property
prop_sumaCuadradosR n = undefined

-- La comprobaci�n es

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Definir, por recursi�n, la funci�n
--    digitosR :: Integer -> [Integer]
-- tal que (digitosR n) es la lista de los d�gitos del n�mero n. Por
-- ejemplo, 
--    digitosR 320274  ==  [3,2,0,2,7,4]
-- ---------------------------------------------------------------------

digitosR :: Integer -> [Integer]
digitosR n = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Definir, por comprensi�n, la funci�n
--    digitosC :: Integer -> [Integer]
-- tal que (digitosC n) es la lista de los d�gitos del n�mero n. Por
-- ejemplo, 
--    digitosC 320274  ==  [3,2,0,2,7,4]
-- Indicaci�n: Usar las funciones show y read.
-- ---------------------------------------------------------------------

digitosC :: Integer -> [Integer]
digitosC n = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 2.3. Comprobar con QuickCheck que las funciones digitosR y
-- digitosC son equivalentes.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_digitos :: Integer -> Property
prop_digitos n = undefined
  
-- La comprobaci�n es

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Definir, por recursi�n, la funci�n 
--    sumaDigitosR :: Integer -> Integer
-- tal que (sumaDigitosR n) es la suma de los d�gitos de n. Por ejemplo,
--    sumaDigitosR 3     ==  3
--    sumaDigitosR 2454  == 15
--    sumaDigitosR 20045 == 11
-- ---------------------------------------------------------------------

sumaDigitosR :: Integer -> Integer
sumaDigitosR n = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Definir, sin usar recursi�n, la funci�n 
--    sumaDigitosNR :: Integer -> Integer
-- tal que (sumaDigitosNR n) es la suma de los d�gitos de n. Por ejemplo,
--    sumaDigitosNR 3     ==  3
--    sumaDigitosNR 2454  == 15
--    sumaDigitosNR 20045 == 11
-- ---------------------------------------------------------------------

sumaDigitosNR :: Integer -> Integer
sumaDigitosNR n = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 3.3. Comprobar con QuickCheck que las funciones sumaDigitosR
-- y sumaDigitosNR son equivalentes.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_sumaDigitos :: Integer -> Property
prop_sumaDigitos n = undefined

-- La comprobaci�n es

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. Definir, por recursi�n, la funci�n 
--    listaNumeroR :: [Integer] -> Integer
-- tal que (listaNumeroR xs) es el n�mero formado por los d�gitos xs. Por
-- ejemplo, 
--    listaNumeroR [5]        == 5
--    listaNumeroR [1,3,4,7]  == 1347
--    listaNumeroR [0,0,1]    == 1
-- ---------------------------------------------------------------------

listaNumeroR :: [Integer] -> Integer
listaNumeroR xs = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Definir, por comprensi�n, la funci�n 
--    listaNumeroC :: [Integer] -> Integer
-- tal que (listaNumeroC xs) es el n�mero formado por los d�gitos xs. Por
-- ejemplo, 
--    listaNumeroC [5]        == 5
--    listaNumeroC [1,3,4,7]  == 1347
--    listaNumeroC [0,0,1]    == 1
-- ---------------------------------------------------------------------

listaNumeroC :: [Integer] -> Integer
listaNumeroC xs = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 4.3. Comprobar con QuickCheck que las funciones
-- listaNumeroR y listaNumeroC son equivalentes.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_listaNumero :: [Integer] -> Bool
prop_listaNumero xs = undefined

-- La comprobaci�n es

-- ---------------------------------------------------------------------
-- Ejercicio 5.1. Definir, por recursi�n, la funci�n 
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
-- Ejercicio 5.2. Definir, por comprensi�n, la funci�n 
--    mayorExponenteC :: Integer -> Integer -> Integer 
-- tal que (mayorExponenteC a b) es el exponente de la mayor potencia de
-- a que divide a b. Por ejemplo,
--    mayorExponenteC 2 8    ==  3
--    mayorExponenteC 5 100  ==  2
--    mayorExponenteC 5 101  ==  0
-- ---------------------------------------------------------------------

mayorExponenteC :: Integer -> Integer -> Integer
mayorExponenteC a b = undefined
