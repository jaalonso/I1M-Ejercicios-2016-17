-- I1M 2016-17: Rel_4.hs (18 de octubre de 2016)
-- Definiciones por recursi�n (1)
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
-- Ejercicio 1.1. Definir por recursi�n la funci�n
--    potencia :: Integer -> Integer -> Integer
-- tal que (potencia x n) es x elevado al n�mero natural n. Por ejemplo,  
--    potencia 2 3  ==  8
-- ---------------------------------------------------------------------

potencia :: Integer -> Integer -> Integer
potencia = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Comprobar con QuickCheck que la funci�n potencia es
-- equivalente a la predefinida (^).
-- ---------------------------------------------------------------------

-- La propiedad es
prop_potencia :: Integer -> Integer -> Property
prop_potencia x n = undefined

-- La comprobaci�n es

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Dados dos n�meros naturales, a y b, es posible
-- calcular su m�ximo com�n divisor mediante el Algoritmo de
-- Euclides. Este algoritmo se puede resumir en la siguiente f�rmula:
--    mcd(a,b) = a,                   si b = 0
--             = mcd (b, a m�dulo b), si b > 0
-- 
-- Definir la funci�n 
--    mcd :: Integer -> Integer -> Integer
-- tal que (mcd a b) es el m�ximo com�n divisor de a y b calculado
-- mediante el algoritmo de Euclides. Por ejemplo,
--    mcd 30 45  ==  15
-- ---------------------------------------------------------------------

mcd :: Integer -> Integer -> Integer
mcd = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Definir y comprobar la propiedad prop_mcd seg�n la
-- cual el m�ximo com�n divisor de dos n�meros a y b (ambos mayores que
-- 0) es siempre mayor o igual que 1 y adem�s es menor o igual que el
-- menor de los n�meros a  y b. 
-- ---------------------------------------------------------------------

-- La propiedad es
prop_mcd :: Integer -> Integer -> Property
prop_mcd a b = undefined

-- Su comprobaci�n es

-- ---------------------------------------------------------------------
-- Ejercicio 2.3. Teniendo en cuenta que buscamos el m�ximo com�n
-- divisor de a y b, ser�a razonable pensar que el m�ximo com�n divisor
-- siempre ser�a igual o menor que la mitad del m�ximo de a y b. Definir
-- esta propiedad y comprobarla.  
-- ---------------------------------------------------------------------

-- La propiedad es
prop_mcd_div :: Integer -> Integer -> Property
prop_mcd_div a b = undefined

-- La comprobaci�n es

-- ---------------------------------------------------------------------
-- Ejercicio 3.1, Definir por recursi�n la funci�n
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

-- La comprobaci�n es

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. Definir por recursi�n la funci�n
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

-- La comprobaci�n es

-- ---------------------------------------------------------------------
-- Ejercicio 5.1. Definir por recursi�n la funci�n
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

-- La comprobaci�n es

