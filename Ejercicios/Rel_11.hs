-- I1M 2016-17: Rel_11.hs (2 de diciembre de 2016)
-- Evaluaci�n perezosa y listas infinitas.
-- Departamento de Ciencias de la Computaci�n e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducci�n                                                       --
-- ---------------------------------------------------------------------

-- En esta relaci�n se presentan ejercicios con listas infinitas y
-- evaluaci�n perezosa. Estos ejercicios corresponden al tema 10 cuyas
-- transparencias se encuentran en  
--    http://www.cs.us.es/~jalonso/cursos/i1m-16/temas/tema-10.html

-- ---------------------------------------------------------------------
-- Importaci�n de librer�as auxiliares                                  
-- ---------------------------------------------------------------------

import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Definir, por recursi�n, la funci�n 
--    repite :: a -> [a]
-- tal que (repite x) es la lista infinita cuyos elementos son x. Por
-- ejemplo, 
--    repite 5           ==  [5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,...
--    take 3 (repite 5)  ==  [5,5,5]
-- 
-- Nota: La funci�n repite es equivalente a la funci�n repeat definida
-- en el preludio de Haskell.
-- ---------------------------------------------------------------------

repite :: a -> [a]
repite x = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Definir, por comprensi�n, la funci�n 
--    repiteC :: a -> [a]
-- tal que (repiteC x) es la lista infinita cuyos elementos son x. Por
-- ejemplo, 
--    repiteC 5           ==  [5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,...
--    take 3 (repiteC 5)  ==  [5,5,5]
--
-- Nota: La funci�n repiteC es equivalente a la funci�n repeat definida
-- en el preludio de Haskell.
-- ---------------------------------------------------------------------

repiteC :: a -> [a]
repiteC x = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Definir, por recursi�n, la funci�n 
--    repiteFinitaR :: Int-> a -> [a]
-- tal que (repiteFinitaR n x) es la lista con n elementos iguales a
-- x. Por ejemplo, 
--    repiteFinitaR 3 5  ==  [5,5,5]
--
-- Nota: La funci�n repiteFinitaR es equivalente a la funci�n replicate
-- definida en el preludio de Haskell.
-- ---------------------------------------------------------------------

repiteFinitaR :: Int -> a -> [a]
repiteFinitaR n x = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Definir, por comprensi�n, la funci�n 
--    repiteFinitaC :: Int-> a -> [a]
-- tal que (repiteFinitaC n x) es la lista con n elementos iguales a
-- x. Por ejemplo, 
--    repiteFinitaC 3 5  ==  [5,5,5]
--
-- Nota: La funci�n repiteFinitaC es equivalente a la funci�n replicate
-- definida en el preludio de Haskell.
-- ---------------------------------------------------------------------

repiteFinitaC :: Int -> a -> [a]
repiteFinitaC n x = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 2.3. Definir, usando repite, la funci�n 
--    repiteFinita :: Int-> a -> [a]
-- tal que (repiteFinita n x) es la lista con n elementos iguales a
-- x. Por ejemplo, 
--    repiteFinita 3 5  ==  [5,5,5]
--
-- Nota: La funci�n repiteFinita es equivalente a la funci�n replicate
-- definida en el preludio de Haskell.
-- ---------------------------------------------------------------------

repiteFinita :: Int -> a -> [a]
repiteFinita n x = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 2.4. Comprobar con QuickCheck que las funciones
-- repiteFinitaR, repiteFinitaC y repiteFinita son equivalentes a
-- replicate. 
--
-- Nota. Al hacer la comprobaci�n limitar el tama�o de las pruebas como
-- se indica a continuaci�n
--    quickCheckWith (stdArgs {maxSize=7}) prop_repiteFinitaEquiv
-- ---------------------------------------------------------------------

-- La propiedad es
prop_repiteFinitaEquiv :: Int -> Int -> Bool
prop_repiteFinitaEquiv n x = undefined

-- La comprobaci�n es

-- ---------------------------------------------------------------------
-- Ejercicio 2.5. Comprobar con QuickCheck que la longitud de
-- (repiteFinita n x) es n, si n es positivo y 0 si no lo es.
--
-- Nota. Al hacer la comprobaci�n limitar el tama�o de las pruebas como
-- se indica a continuaci�n
--    quickCheckWith (stdArgs {maxSize=30}) prop_repiteFinitaLongitud
-- ---------------------------------------------------------------------

-- La propiedad es
prop_repiteFinitaLongitud :: Int -> Int -> Bool
prop_repiteFinitaLongitud n x = undefined

-- La comprobaci�n es

-- ---------------------------------------------------------------------
-- Ejercicio 2.6. Comprobar con QuickCheck que todos los elementos de 
-- (repiteFinita n x) son iguales a x.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_repiteFinitaIguales :: Int -> Int -> Bool
prop_repiteFinitaIguales n x = undefined

-- La comprobaci�n es

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Definir, por comprensi�n, la funci�n
--    ecoC :: String -> String
-- tal que (ecoC xs) es la cadena obtenida a partir de la cadena xs
-- repitiendo cada elemento tantas veces como indica su posici�n: el
-- primer elemento se repite 1 vez, el segundo 2 veces y as�
-- sucesivamente. Por ejemplo, 
--    ecoC "abcd"  ==  "abbcccdddd"
-- ---------------------------------------------------------------------

ecoC :: String -> String
ecoC xs = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Definir, por recursi�n, la funci�n
--    ecoR :: String -> String
-- tal que (ecoR xs) es la cadena obtenida a partir de la cadena xs
-- repitiendo cada elemento tantas veces como indica su posici�n: el
-- primer elemento se repite 1 vez, el segundo 2 veces y as�
-- sucesivamente. Por ejemplo, 
--    ecoR "abcd"  ==  "abbcccdddd"
-- ---------------------------------------------------------------------

ecoR :: String -> String
ecoR xs = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir, por recursi�n, la funci�n
--    itera :: (a -> a) -> a -> [a]
-- tal que (itera f x) es la lista cuyo primer elemento es x y los
-- siguientes elementos se calculan aplicando la funci�n f al elemento
-- anterior. Por ejemplo, 
--    ghci> itera (+1) 3
--    [3,4,5,6,7,8,9,10,11,12,{Interrupted!}
--    ghci> itera (*2) 1
--    [1,2,4,8,16,32,64,{Interrupted!}
--    ghci> itera (`div` 10) 1972
--    [1972,197,19,1,0,0,0,0,0,0,{Interrupted!}
-- 
-- Nota: La funci�n repite es equivalente a la funci�n iterate definida
-- en el preludio de Haskell.
-- ---------------------------------------------------------------------

itera :: (a -> a) -> a -> [a]
itera f x = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 5.1. Definir, por recursi�n, la funci�n
--    agrupaR :: Int -> [a] -> [[a]]
-- tal que (agrupaR n xs) es la lista formada por listas de n elementos
-- consecutivos de la lista xs (salvo posiblemente la �ltima que puede
-- tener menos de n elementos). Por ejemplo, 
--    ghci> agrupaR 2 [3,1,5,8,2,7]
--    [[3,1],[5,8],[2,7]]
--    ghci> agrupaR 2 [3,1,5,8,2,7,9] 
--    [[3,1],[5,8],[2,7],[9]]
--    ghci> agrupaR 5 "todo necio confunde valor y precio"
--    ["todo ","necio"," conf","unde ","valor"," y pr","ecio"]
-- ---------------------------------------------------------------------------- 

agrupaR :: Int -> [a] -> [[a]]
agrupaR = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 5.2. Definir, de manera no recursiva con iterate, la funci�n
--    agrupa :: Int -> [a] -> [[a]]
-- tal que (agrupa n xs) es la lista formada por listas de n elementos
-- consecutivos de la lista xs (salvo posiblemente la �ltima que puede
-- tener menos de n elementos). Por ejemplo, 
--    ghci> agrupa 2 [3,1,5,8,2,7]
--    [[3,1],[5,8],[2,7]]
--    ghci> agrupa 2 [3,1,5,8,2,7,9] 
--    [[3,1],[5,8],[2,7],[9]]
--    ghci> agrupa 5 "todo necio confunde valor y precio"
--    ["todo ","necio"," conf","unde ","valor"," y pr","ecio"]
-- ---------------------------------------------------------------------------- 

agrupa :: Int -> [a] -> [[a]]
agrupa n = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 5.3. Comprobar con QuickCheck que todos los grupos de
-- (agrupa n xs) tienen longitud n (salvo el �ltimo que puede tener una
-- longitud menor). 
-- ---------------------------------------------------------------------------- 

-- La propiedad es
prop_AgrupaLongitud :: Int -> [Int] -> Property
prop_AgrupaLongitud n xs = undefined

-- La comprobaci�n es

-- ----------------------------------------------------------------------------
-- Ejercicio 5.4. Comprobar con QuickCheck que combinando todos los
-- grupos de ((agrupa n xs)) se obtiene la lista xs. 
-- ---------------------------------------------------------------------------- 

-- La segunda propiedad es
prop_AgrupaCombina :: Int -> [Int] -> Property
prop_AgrupaCombina n xs = undefined

-- La comprobaci�n es

-- ---------------------------------------------------------------------
-- Ejercicio 6.1. Sea la siguiente operaci�n, aplicable a cualquier
-- n�mero entero positivo:  
--    * Si el n�mero es par, se divide entre 2.
--    * Si el n�mero es impar, se multiplica por 3 y se suma 1.
-- Dado un n�mero cualquiera, podemos considerar su �rbita, es decir,
-- las im�genes sucesivas al iterar la funci�n. Por ejemplo, la �rbita
-- de 13 es
--    13, 40, 20, 10, 5, 16, 8, 4, 2, 1, 4, 2, 1,...
-- Si observamos este ejemplo, la �rbita de 13 es peri�dica, es decir,
-- se repite indefinidamente a partir de un momento dado). La conjetura
-- de Collatz dice que siempre alcanzaremos el 1 para cualquier n�mero
-- con el que comencemos. Ejemplos:  
--    * Empezando en n = 6 se obtiene 6, 3, 10, 5, 16, 8, 4, 2, 1.
--    * Empezando en n = 11 se obtiene: 11, 34, 17, 52, 26, 13, 40, 20,
--      10, 5, 16, 8, 4, 2, 1. 
--    * Empezando en n = 27, la sucesi�n tiene 112 pasos, llegando hasta
--      9232 antes de descender a 1:  27, 82, 41, 124, 62, 31, 94, 47,
--      142, 71, 214, 107, 322, 161, 484, 242, 121, 364, 182, 91, 274,
--      137, 412, 206, 103, 310, 155, 466, 233, 700, 350, 175, 526, 263,
--      790, 395, 1186, 593, 1780, 890, 445, 1336, 668, 334, 167, 502,
--      251, 754, 377, 1132, 566, 283, 850, 425, 1276, 638, 319, 958,
--      479, 1438, 719, 2158, 1079, 3238, 1619, 4858, 2429, 7288, 3644,
--      1822, 911, 2734, 1367, 4102, 2051, 6154, 3077, 9232, 4616, 2308,
--      1154, 577, 1732, 866, 433, 1300, 650, 325, 976, 488, 244, 122,
--      61, 184, 92, 46, 23, 70, 35, 106, 53, 160, 80, 40, 20, 10, 5,
--      16, 8, 4, 2, 1. 
-- 
-- Definir la funci�n
--    siguiente :: Integer -> Integer
-- tal que (siguiente n) es el siguiente de n en la sucesi�n de
-- Collatz. Por ejemplo,
--    siguiente 13  ==  40
--    siguiente 40  ==  20
-- ---------------------------------------------------------------------

siguiente n = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 6.2. Definir, por recursi�n, la funci�n 
--    collatzR :: Integer -> [Integer]
-- tal que (collatzR n) es la �rbita de CollatzR de n hasta alcanzar el
-- 1. Por ejemplo,
--    collatzR 13  ==  [13,40,20,10,5,16,8,4,2,1]
-- ---------------------------------------------------------------------

collatzR :: Integer -> [Integer]
collatzR = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 6.3. Definir, sin recursi�n y con iterate, la funci�n 
--    collatz :: Integer -> [Integer]
-- tal que (collatz n) es la �rbita de Collatz d n hasta alcanzar el
-- 1. Por ejemplo,
--    collatz 13  ==  [13,40,20,10,5,16,8,4,2,1]
-- Indicaci�n: Usar takeWhile e iterate.
-- ---------------------------------------------------------------------

collatz :: Integer -> [Integer]
collatz n = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 6.4. Definir la funci�n
--    menorCollatzMayor :: Int -> Integer
-- tal que (menorCollatzMayor x) es el menor n�mero cuya �rbita de
-- Collatz tiene m�s de x elementos. Por ejemplo,
--    menorCollatzMayor 100  ==  27
-- ---------------------------------------------------------------------

menorCollatzMayor :: Int -> Integer
menorCollatzMayor x = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 6.5. Definir la funci�n
--    menorCollatzSupera :: Integer -> Integer
-- tal que (menorCollatzSupera x) es el menor n�mero cuya �rbita de
-- Collatz tiene alg�n elemento mayor que x. Por ejemplo,
--    menorCollatzSupera 100  ==  15
-- ---------------------------------------------------------------------

menorCollatzSupera :: Integer -> Integer
menorCollatzSupera x = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir, usando takeWhile y map, la funci�n
--    potenciasMenores :: Int -> Int -> [Int]
-- tal que (potenciasMenores x y) es la lista de las potencias de x
-- menores que y. Por ejemplo,
--    potenciasMenores 2 1000  ==  [2,4,8,16,32,64,128,256,512]
-- ---------------------------------------------------------------------

potenciasMenores :: Int -> Int -> [Int]
potenciasMenores x y = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 8.1. Definir, usando la criba de Erat�stenes, la constante
--    primos :: Integral a => [a]
-- cuyo valor es la lista de los n�meros primos. Por ejemplo,
--    take 10 primos  ==  [2,3,5,7,11,13,17,19,23,29]
-- ---------------------------------------------------------------------

primos :: Integral a => [a]
primos = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 8.2. Definir, usando primos, la funci�n
--    primo :: Integral a => a -> Bool
-- tal que (primo n) se verifica si n es primo. Por ejemplo,
--    primo 7  ==  True
--    primo 9  ==  False
-- ---------------------------------------------------------------------

primo :: Int -> Bool
primo n = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 8.3. Definir la funci�n
--    sumaDeDosPrimos :: Int -> [(Int,Int)]
-- tal que (sumaDeDosPrimos n) es la lista de las distintas
-- descomposiciones de n como suma de dos n�meros primos. Por ejemplo, 
--    sumaDeDosPrimos 30  ==  [(7,23),(11,19),(13,17)]
--    sumaDeDosPrimos 10  ==  [(3,7),(5,5)]
-- Calcular, usando la funci�n sumaDeDosPrimos, el menor n�mero que
-- puede escribirse de 10 formas distintas como suma de dos primos.
-- ---------------------------------------------------------------------

sumaDeDosPrimos :: Int -> [(Int,Int)]
sumaDeDosPrimos n = undefined

-- El c�lculo es

-- ---------------------------------------------------------------------
-- � La lista infinita de factoriales,                                --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 9.1. Definir, por comprensi�n, la funci�n
--    factoriales1 :: [Integer]
-- tal que factoriales1 es la lista de los factoriales. Por ejemplo,
--    take 10 factoriales1  ==  [1,1,2,6,24,120,720,5040,40320,362880]
-- ---------------------------------------------------------------------

factoriales1 :: [Integer]
factoriales1 = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 9.2. Definir, usando zipWith, la funci�n
--    factoriales2 :: [Integer]
-- tal que factoriales2 es la lista de los factoriales. Por ejemplo,
--    take 10 factoriales2  ==  [1,1,2,6,24,120,720,5040,40320,362880]
-- ---------------------------------------------------------------------

factoriales2 :: [Integer]
factoriales2 = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 9.3. Definir, por recursi�n, la funci�n
--    factoriales3 :: [Integer]
-- tal que factoriales3 es la lista de los factoriales. Por ejemplo,
--    take 10 factoriales3  ==  [1,1,2,6,24,120,720,5040,40320,362880]
-- ---------------------------------------------------------------------

factoriales3 :: [Integer]
factoriales3 = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 9.4. Definir, usando scanl1, la funci�n
--    factoriales4 :: [Integer]
-- tal que factoriales4 es la lista de los factoriales. Por ejemplo,
--    take 10 factoriales4  ==  [1,1,2,6,24,120,720,5040,40320,362880]
-- ---------------------------------------------------------------------

factoriales4 :: [Integer]
factoriales4 = 1 : scanl1 (*) [1..]

-- ---------------------------------------------------------------------
-- Ejercicio 9.5. Definir, usando iterate, la funci�n
--    factoriales5 :: [Integer]
-- tal que factoriales5 es la lista de los factoriales. Por ejemplo,
--    take 10 factoriales5  ==  [1,1,2,6,24,120,720,5040,40320,362880]
-- ---------------------------------------------------------------------

factoriales5 :: [Integer]
factoriales5 = undefined

-- ---------------------------------------------------------------------
-- � La sucesi�n de Fibonacci                                         --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 10.1. La sucesi�n de Fibonacci est� definida por
--    f(0) = 0
--    f(1) = 1
--    f(n) = f(n-1)+f(n-2), si n > 1.
-- 
-- Definir la funci�n
--    fib :: Integer -> Integer
-- tal que (fib n) es el n-�simo t�rmino de la sucesi�n de Fibonacci. 
-- Por ejemplo,
--    fib 8  ==  21
-- ---------------------------------------------------------------------

fib :: Integer -> Integer
fib = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 10.2. Definir, por comprensi�n, la funci�n
--    fibs1 :: [Integer]
-- tal que fibs1 es la sucesi�n de Fibonacci. Por ejemplo,
--    take 10 fibs1  ==  [0,1,1,2,3,5,8,13,21,34]
-- ---------------------------------------------------------------------

fibs1 :: [Integer]
fibs1 = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 10.3. Definir, por recursi�n, la funci�n
--    fibs2 :: [Integer]
-- tal que fibs2 es la sucesi�n de Fibonacci. Por ejemplo,
--    take 10 fibs2  ==  [0,1,1,2,3,5,8,13,21,34]
-- ---------------------------------------------------------------------

fibs2 :: [Integer]
fibs2 = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 10.4. Definir, por recursi�n con zipWith, la funci�n
--    fibs3 :: [Integer]
-- tal que fibs3 es la sucesi�n de Fibonacci. Por ejemplo,
--    take 10 fibs3  ==  [0,1,1,2,3,5,8,13,21,34]
-- ---------------------------------------------------------------------

fibs3 :: [Integer]
fibs3 = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 10.5. Definir, por recursi�n con acumuladores, la funci�n 
--    fibs4 :: [Integer]
-- tal que fibs4 es la sucesi�n de Fibonacci. Por ejemplo,
--    take 10 fibs4  ==  [0,1,1,2,3,5,8,13,21,34]
-- ---------------------------------------------------------------------

fibs4 :: [Integer]
fibs4 = undefined

-- ---------------------------------------------------------------------
-- � El tri�ngulo de Pascal                                           --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 11.1. El tri�ngulo de Pascal es un tri�ngulo de n�meros
--          1
--         1 1
--        1 2 1
--      1  3 3  1
--     1 4  6  4 1
--    1 5 10 10 5 1
--   ...............
-- construido de la siguiente forma
-- * la primera fila est� formada por el n�mero 1;
-- * las filas siguientes se construyen sumando los n�meros adyacentes
--   de la fila superior y a�adiendo un 1 al principio y al final de la
--   fila. 
-- 
-- Definir, con iterate y zipWith, la funci�n
--    pascal1 :: [[Integer]]
-- tal que pascal es la lista de las l�neas del tri�ngulo de Pascal. Por
-- ejemplo, 
--    ghci> take 6 pascal1
--    [[1],[1,1],[1,2,1],[1,3,3,1],[1,4,6,4,1],[1,5,10,10,5,1]]
-- ---------------------------------------------------------------------

pascal1 :: [[Integer]]
pascal1 = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 11.2. Definir, con map y zipWith, la funci�n
--    pascal2 :: [[Integer]]
-- tal que pascal es la lista de las l�neas del tri�ngulo de Pascal. Por
-- ejemplo, 
--    ghci> take 6 pascal2
--    [[1],[1,1],[1,2,1],[1,3,3,1],[1,4,6,4,1],[1,5,10,10,5,1]]
-- ---------------------------------------------------------------------

-- 2� definici�n (con map):
pascal2 :: [[Integer]]
pascal2 = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 11.3. Escribir la traza del c�lculo de la expresi�n
--    take 4 pascal2
-- ---------------------------------------------------------------------

