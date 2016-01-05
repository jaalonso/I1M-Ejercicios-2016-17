-- I1M 2015-16: Relaci�n 27 (12 de diciembre de 2015)
-- Aplicaciones de la programaci�n funcional con listas infinitas.
-- Departamento de Ciencias de la Computaci�n e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducci�n                                                       --
-- ---------------------------------------------------------------------

-- En esta relaci�n se estudia distintas aplicaciones de la programaci�n
-- funcional que usan listas infinitas
-- + la sucesi�n de Hamming,
-- + problemas 10 y 12 del proyecto Euler,
-- + enumeraci�n de los n�meros enteros,
-- + el problema de la bicicleta de Turing,
-- + la sucesi�n de Golomb,
-- + la codificaci�n por longitud,
-- + la sucesi�n de Kolakoski y
-- + el tri�ngulo de Floyd.

-- ---------------------------------------------------------------------
-- Importaci�n de librer�as                                           --
-- ---------------------------------------------------------------------

import Data.Char 
import Data.List
import Data.Numbers.Primes
import Test.QuickCheck

-- ---------------------------------------------------------------------
-- � Sucesi�n de Hamming                                              --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Definir la funci�n
--    divisoresPrimosEn :: Integer -> [Integer] -> Bool
-- tal que (divisoresPrimosEn x ys) se verifica si x puede expresarse
-- como un producto de potencias de elementos de la lista de n�meros
-- primos ys. Por ejemplo, 
--    divisoresPrimosEn 12 [2,3,5]  ==  True
--    divisoresPrimosEn 14 [2,3,5]  ==  False
-- ---------------------------------------------------------------------

divisoresPrimosEn :: Integer -> [Integer] -> Bool
divisoresPrimosEn = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Los n�meros de Hamming forman una sucesi�n 
-- estrictamente creciente de n�meros que cumplen las siguientes 
-- condiciones: 
--    1. El n�mero 1 est� en la sucesi�n.
--    2. Si x est� en la sucesi�n, entonces 2x, 3x y 5x tambi�n est�n.
--    3. Ning�n otro n�mero est� en la sucesi�n.
-- Definir, usando divisoresPrimosEn, la constante
--    hamming :: [Integer]
-- tal que hamming es la sucesi�n de Hamming. Por ejemplo,
--    take 12 hamming  ==  [1,2,3,4,5,6,8,9,10,12,15,16]
-- ---------------------------------------------------------------------

hamming :: [Integer]
hamming = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 1.3. Definir la funci�n
--    cantidadHammingMenores :: Integer -> Int
-- tal que (cantidadHammingMenores x) es la cantidad de n�meros de
-- Hamming menores que x. Por ejemplo,
--    cantidadHammingMenores 6  ==  5
--    cantidadHammingMenores 7  ==  6
--    cantidadHammingMenores 8  ==  6
-- ---------------------------------------------------------------------

cantidadHammingMenores :: Integer -> Int
cantidadHammingMenores x = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 1.4. Definir la funci�n
--    siguienteHamming :: Integer -> Integer
-- tal que (siguienteHamming x) es el menor n�mero de la sucesi�n de
-- Hamming mayor que x. Por ejemplo,
--    siguienteHamming 6  ==  8
--    siguienteHamming 21  ==  24
-- ---------------------------------------------------------------------

siguienteHamming :: Integer -> Integer
siguienteHamming x = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 1.5. Definir la funci�n
--    huecoHamming :: Integer -> [(Integer,Integer)]
-- tal que (huecoHamming n) es la lista de pares de n�meros consecutivos
-- en la sucesi�n de Hamming cuya distancia es mayor que n. Por ejemplo,  
--    take 4 (huecoHamming 2)   ==  [(12,15),(20,24),(27,30),(32,36)]
--    take 3 (huecoHamming 2)   ==  [(12,15),(20,24),(27,30)]
--    take 2 (huecoHamming 3)   ==  [(20,24),(32,36)]
--    head (huecoHamming 10)    ==  (108,120)
--    head (huecoHamming 1000)  ==  (34992,36000)
-- ---------------------------------------------------------------------

huecoHamming :: Integer -> [(Integer,Integer)]
huecoHamming n = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 1.6. Comprobar con QuickCheck que para todo n, existen
-- pares de n�meros consecutivos en la sucesi�n de Hamming cuya
-- distancia es mayor o igual que n.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_Hamming :: Integer -> Bool
prop_Hamming n = undefined

-- La comprobaci�n es

-- ---------------------------------------------------------------------
-- � Problema 10 del Proyecto Euler                                   --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la funci�n 
--    sumaPrimoMenores :: Integer -> Integer
-- tal que (sumaPrimoMenores n) es la suma de los primos menores que
-- n. Por ejemplo,
--    sumaPrimoMenores 10  ==  17
--    sumaPrimoMenores 7   ==  10                       
-- ---------------------------------------------------------------------

sumaPrimoMenores :: Integer -> Integer
sumaPrimoMenores n = undefined


-- ---------------------------------------------------------------------
-- � Problema 12 del Proyecto Euler                                   --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Los n�meros triangulares se forman como sigue
--    *     *      * 
--         * *    * *
--               * * *
--    1     3      6
-- 
-- La sucesi�n de los n�meros triangulares se obtiene sumando los
-- n�meros naturales. As�, los 5 primeros n�meros triangulares son
--     1 = 1
--     3 = 1+2
--     6 = 1+2+3
--    10 = 1+2+3+4
--    15 = 1+2+3+4+5
-- 
-- Definir la funci�n
--    triangulares :: [Integer]
-- tal que triangulares es la lista de los n�meros triangulares. Por
-- ejemplo, 
--    take 10 triangulares  ==  [1,3,6,10,15,21,28,36,45,55]
-- ---------------------------------------------------------------------

triangulares :: [Integer]
triangulares = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Definir la funci�n
--    nDivisores :: Integer -> Integer
-- tal que (nDivisores n) es el n�mero de los divisores de n. Por
-- ejemplo, 
--    nDivisores 28                 ==  6
--    nDivisores (product [1..200]) == 139503973313460993785856000000
-- ---------------------------------------------------------------------

nDivisores :: Integer -> Integer
nDivisores = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 3.3. Los divisores de los primeros 7 n�meros triangulares
-- son: 
--     1: 1
--     3: 1,3
--     6: 1,2,3,6
--    10: 1,2,5,10
--    15: 1,3,5,15
--    21: 1,3,7,21
--    28: 1,2,4,7,14,28
-- Como se puede observar, 28 es el menor n�mero triangular con m�s de 5
-- divisores. 
-- 
-- Definir la funci�n 
--    euler12 :: Int -> Integer
-- tal que (euler12 n) es el menor n�mero triangular con m�s de n
-- divisores. Por ejemplo,
--    euler12 5    ==  28
--    euler12 500  ==  76576500
-- ---------------------------------------------------------------------

euler12 :: Integer -> Integer
euler12 n = undefined

-- ---------------------------------------------------------------------
-- � Enumeraci�n de los n�meros enteros                               --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. Los n�meros enteros se pueden ordenar como sigue 
--    0, -1, 1, -2, 2, -3, 3, -4, 4, -5, 5, -6, 6, -7, 7, ...
-- Definir, por comprensi�n, la constante
--    enteros :: [Int]
-- tal que enteros es la lista de los enteros con la ordenaci�n
-- anterior. Por ejemplo,
--    take 10 enteros  ==  [0,-1,1,-2,2,-3,3,-4,4,-5]
-- ---------------------------------------------------------------------

enteros :: [Int]
enteros = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Definir, por iteraci�n, la constante
--    enteros' :: [Int]
-- tal que enteros' es la lista de los enteros con la ordenaci�n
-- anterior. Por ejemplo,
--    take 10 enteros  ==  [0,-1,1,-2,2,-3,3,-4,4,-5]
-- ---------------------------------------------------------------------

enteros' :: [Int]
enteros' = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 4.3. Definir, por selecci�n con takeWhile, la funci�n
--    posicion :: Int -> Int
-- tal que (posicion x) es la posici�n del entero x en la ordenaci�n
-- anterior. Por ejemplo,
--    posicion 2  ==  4
-- ---------------------------------------------------------------------

posicion :: Int -> Int
posicion x = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 4.4. Definir, por recursi�n, la funci�n
--    posicionR :: Int -> Int
-- tal que (posicionR x) es la posici�n del entero x en la ordenaci�n
-- anterior. Por ejemplo,
--    posicionR 2  ==  4
-- ---------------------------------------------------------------------

posicionR :: Int -> Int
posicionR x = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 4.5. Definir, por comprensi�n, la funci�n
--    posicionC :: Int -> Int
-- tal que (posicionC x) es la posici�n del entero x en la ordenaci�n
-- anterior. Por ejemplo,
--    posicionC 2  ==  4
-- ---------------------------------------------------------------------

posicionC :: Int -> Int
posicionC x = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 4.6. Definir, sin b�squeda, la funci�n
--    posicion2 :: Int -> Int
-- tal que (posicion2 x) es la posici�n del entero x en la ordenaci�n
-- anterior. Por ejemplo,
--    posicion2 2  ==  4
-- ---------------------------------------------------------------------

-- Definici�n directa
posicion2 :: Int -> Int
posicion2 = undefined

-- ---------------------------------------------------------------------
-- � El problema de la bicicleta de Turing                            --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 5.1. Cuentan que Alan Turing ten�a una bicicleta vieja,
-- que ten�a una cadena con un eslab�n d�bil y adem�s uno de los radios
-- de la rueda estaba doblado. Cuando el radio doblado coincid�a con el
-- eslab�n d�bil, entonces la cadena se romp�a.   
--
-- La bicicleta se identifica por los par�metros (i,d,n) donde 
-- - i es el n�mero del eslab�n que coincide con el radio doblado al
--   empezar a andar,
-- - d es el n�mero de eslabones que se desplaza la cadena en cada
--   vuelta de la rueda y  
-- - n es el n�mero de eslabones de la cadena (el n�mero n es el d�bil).
-- Si i=2 y d=7 y n=25, entonces la lista con el n�mero de eslab�n que 
-- toca el radio doblado en cada vuelta es 
--    [2,9,16,23,5,12,19,1,8,15,22,4,11,18,0,7,14,21,3,10,17,24,6,...
-- Con lo que la cadena se rompe en la vuelta n�mero 14.
-- 
-- Definir la funci�n
--    eslabones :: Int -> Int -> Int -> [Int]
-- tal que (eslabones i d n) es la lista con los n�meros de eslabones 
-- que tocan el radio doblado en cada vuelta en una bicicleta de tipo 
-- (i,d,n). Por ejemplo, 
--    take 10 (eslabones 2 7 25)  ==  [2,9,16,23,5,12,19,1,8,15]
-- ---------------------------------------------------------------------

eslabones :: Int -> Int -> Int -> [Int]
eslabones i d n = undefined

-- 2� definici�n (con iterate):
eslabones2 :: Int -> Int -> Int -> [Int]
eslabones2 i d n = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 5.2. Definir la funci�n
--    numeroVueltas :: Int -> Int -> Int -> Int 
-- tal que (numeroVueltas i d n) es el n�mero de vueltas que pasar�n 
-- hasta que la cadena se rompa en una bicicleta de tipo (i,d,n). Por 
-- ejemplo,
--    numeroVueltas 2 7 25  ==  14
-- ---------------------------------------------------------------------

numeroVueltas :: Int -> Int -> Int -> Int
numeroVueltas i d n = undefined

-- ---------------------------------------------------------------------
-- � La sucesi�n de Golomb                                            --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 6.1. [Basado en el problema 341 del proyecto Euler]. La
-- sucesi�n de Golomb {G(n)} es una sucesi�n auto descriptiva: es la
-- �nica sucesi�n no decreciente de n�meros naturales tal que el n�mero
-- n aparece G(n) veces en la sucesi�n. Los valores de G(n) para los
-- primeros n�meros son los siguientes:
--    n       1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 ...
--    G(n)    1 2 2 3 3 4 4 4 5  5  5  6  6  6  6 ...
-- En los apartados de este ejercicio se definir� una funci�n para
-- calcular los t�rminos de la sucesi�n de Golomb. 
-- 
-- Definir la funci�n
--    golomb :: Int -> Int
-- tal que (golomb n) es el n-�simo t�rmino de la sucesi�n de Golomb. 
-- Por ejemplo,
--    golomb 5  ==  3
--    golomb 9  ==  5
-- Indicaci�n: Se puede usar la funci�n sucGolomb del apartado 2.
-- ---------------------------------------------------------------------

golomb :: Int -> Int
golomb n = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 6.2. Definir la funci�n
--    sucGolomb :: [Int]
-- tal que sucGolomb es la lista de los t�rminos de la sucesi�n de
-- Golomb. Por ejemplo,
--    take 15 sucGolomb  ==  [1,2,2,3,3,4,4,4,5,5,5,6,6,6,6]
-- Indicaci�n: Se puede usar la funci�n subSucGolomb del apartado 3.
-- ---------------------------------------------------------------------

sucGolomb :: [Int]
sucGolomb = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 6.3. Definir la funci�n
--    subSucGolomb :: Int -> [Int]
-- tal que (subSucGolomb x) es la lista de los t�rminos de la sucesi�n
-- de Golomb a partir de la primera ocurrencia de x. Por ejemplo,
--    take 10 (subSucGolomb 4)  ==  [4,4,4,5,5,5,6,6,6,6]
-- Indicaci�n: Se puede usar la funci�n golomb del apartado 1.
-- ---------------------------------------------------------------------

subSucGolomb :: Int -> [Int]
subSucGolomb = undefined

-- ---------------------------------------------------------------------
-- � La codificaci�n por longitud                                     --
-- ---------------------------------------------------------------------

-- La codificaci�n por longitud, o comprensi�n RLE (del ingl�s,
-- "Run-length encoding"), es una compresi�n de datos en la que
-- secuencias de datos con el mismo valor consecutivas son almacenadas
-- como un �nico valor m�s su recuento. Por ejemplo, la cadena 
--    BBBBBBBBBBBBNBBBBBBBBBBBBNNNBBBBBBBBBBBBBBBBBBBBBBBBNBBBBBBBBBBBBBB
-- se codifica por 
--    12B1N12B3N24B1N14B
-- Interpretado esto como 12 letras B, 1 letra N , 12 letras B, 3 letras
-- N, etc.
-- 
-- En los siguientes ejercicios se definir�n funciones para codificar y
-- descodificar por longitud.

-- ---------------------------------------------------------------------
-- Ejercicio 7.1. Una lista se puede comprimir indicando el n�mero de
-- veces consecutivas que aparece cada elemento. Por ejemplo, la lista 
-- comprimida de [1,1,7,7,7,5,5,7,7,7,7] es [(2,1),(3,7),(2,5),(4,7)],
-- indicando que comienza con dos 1, seguido de tres 7, dos 5 y cuatro
-- 7. 
-- 
-- Definir la funci�n
--    comprimida :: Eq a => [a] -> [(Int,a)]
-- tal que (comprimida xs) es la lista obtenida al comprimir por
-- longitud la lista xs. Por ejemplo, 
--    ghci> comprimida [1,1,7,7,7,5,5,7,7,7,7]
--    [(2,1),(3,7),(2,5),(4,7)]
--    ghci> comprimida "BBBBBBBBBBBBNBBBBBBBBBBBBNNNBBBBBBBBBBBBBBBBBBB"
--    [(12,'B'),(1,'N'),(12,'B'),(3,'N'),(19,'B')]
-- ---------------------------------------------------------------------

comprimida :: Eq a => [a] -> [(Int,a)]
comprimida xs = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 7.2. Definir la funci�n
--    expandida :: [(Int,a)] -> [a]
-- tal que (expandida ps) es la lista expandida correspondiente a ps (es
-- decir, es la lista xs tal que la comprimida de xs es ps). Por
-- ejemplo, 
--    expandida [(2,1),(3,7),(2,5),(4,7)]  ==  [1,1,7,7,7,5,5,7,7,7,7]
-- ---------------------------------------------------------------------

expandida :: [(Int,a)] -> [a]
expandida ps = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 7.3. Comprobar con QuickCheck que dada una lista de enteros,
-- si se la comprime y despu�s se expande se obtiene la lista inicial. 
-- ---------------------------------------------------------------------

-- La propiedad es
prop_expandida_comprimida :: [Int] -> Bool 
prop_expandida_comprimida xs = undefined

-- La comprobaci�n es

-- ---------------------------------------------------------------------
-- Ejercicio 7.4. Comprobar con QuickCheck que dada una lista de pares
-- de enteros, si se la expande y despu�s se comprime se obtiene la
-- lista inicial.  
-- ---------------------------------------------------------------------

-- La propiedad es
prop_comprimida_expandida :: [(Int,Int)] -> Bool 
prop_comprimida_expandida xs = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 7.5. Definir la funci�n
--    listaAcadena :: [(Int,Char)] -> String
-- tal que (listaAcadena xs) es la cadena correspondiente a la lista de
-- pares de xs. Por ejemplo,
--    ghci> listaAcadena [(12,'B'),(1,'N'),(12,'B'),(3,'N'),(19,'B')]
--    "12B1N12B3N19B"
-- ---------------------------------------------------------------------

listaAcadena :: [(Int,Char)] -> String
listaAcadena xs = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 7.6. Definir la funci�n
--    cadenaComprimida :: String -> String
-- tal que (cadenaComprimida cs) es la cadena obtenida comprimiendo por
-- longitud la cadena cs. Por ejemplo,
--    ghci> cadenaComprimida "BBBBBBBBBBBBNBBBBBBBBBBBBNNNBBBBBBBBBBNNN"
--    "12B1N12B3N10B3N"
-- ---------------------------------------------------------------------

cadenaComprimida :: String -> String
cadenaComprimida = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 7.7. Definir la funci�n 
--    cadenaAlista :: String -> [(Int,Char)]
-- tal que (cadenaAlista cs) es la lista de pares correspondientes a la
-- cadena cs. Por ejemplo,
--    ghci> cadenaAlista "12B1N12B3N10B3N"
--    [(12,'B'),(1,'N'),(12,'B'),(3,'N'),(10,'B'),(3,'N')]
-- ---------------------------------------------------------------------

cadenaAlista :: String -> [(Int,Char)]
cadenaAlista = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 7.8. Definir la funci�n
--    cadenaExpandida :: String -> String
-- tal que (cadenaExpandida cs) es la cadena expandida correspondiente a
-- cs (es decir, es la cadena xs que al comprimirse por longitud da cs). 
-- Por ejemplo, 
--    ghci> cadenaExpandida "12B1N12B3N10B3N"
--    "BBBBBBBBBBBBNBBBBBBBBBBBBNNNBBBBBBBBBBNNN"
-- ---------------------------------------------------------------------

cadenaExpandida :: String -> String
cadenaExpandida = undefined

-- ---------------------------------------------------------------------
-- � La sucesi�n de Kolakoski                                         --
-- ---------------------------------------------------------------------

-- Dada una sucesi�n, su contadora es la sucesi�n de las longitudes de
-- de sus bloque de elementos consecutivos iguales. Por ejemplo, la
-- sucesi�n contadora de abbaaabbba es 12331; es decir; 1 vez la a,
-- 2 la b, 3 la a, 3 la b y 1 la a.
-- 
-- La sucesi�n de Kolakoski es una sucesi�n infinita de los s�mbolos 1 y
-- 2 que es su propia contadora. Los primeros t�rminos de la sucesi�n
-- de Kolakoski son 1221121221221... que coincide con su contadora (es
-- decir, 1 vez el 1, 2 veces el 2, 2 veces el 1, ...). 
-- 
-- En esta secci�n se define la sucesi�n de Kolakoski.

-- ---------------------------------------------------------------------
-- Ejercicio 8.1. Dados los s�mbolos a y b, la sucesi�n contadora de
--    abbaaabbba... =  a bb aaa bbb a ...  
-- es
--    1233...       =  1 2  3   3...
-- es decir; 1 vez la a, 2 la b, 3 la a, 3 la b, 1 la a, ...
-- 
-- Definir la funci�n
--    contadora :: Eq a => [a] -> [Int]
-- tal que (contadora xs) es la sucesi�n contadora de xs. Por ejemplo,
--    contadora "abbaaabbb"        ==  [1,2,3,3]
--    contadora "122112122121121"  ==  [1,2,2,1,1,2,1,1,2,1,1]
-- ---------------------------------------------------------------------

contadora :: Eq a => [a] -> [Int]
contadora xs = undefined


-- ---------------------------------------------------------------------
-- Ejercicio 8.2. Definir la funci�n
--    contada :: [Int] -> [a] -> [a]
-- tal que (contada ns xs) es la sucesi�n formada por los s�mbolos de xs
-- cuya contadora es ns. Por ejemplo,
--    contada [1,2,3,3] "ab"                ==  "abbaaabbb"
--    contada [1,2,3,3] "abc"               ==  "abbcccaaa"
--    contada [1,2,2,1,1,2,1,1,2,1,1] "12"  ==  "122112122121121"
-- ---------------------------------------------------------------------

contada :: [Int] -> [a] -> [a]
contada = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 8.3. La sucesi�n autocontadora (o sucesi�n de  Kolakoski) es
-- la sucesi�n xs formada por 1 y 2 tal que coincide con su contada; es
-- decir (contadora xs) == xs. Los primeros t�rminos de la funci�n
-- autocontadora son
--    1221121221221... = 1 22 11 2 1 22 1 22 11 ...
-- y su contadora es
--    122112122...     = 1 2  2  1 1 2  1 2  2...
-- que coincide con la inicial. 
-- 
-- Definir la funci�n
--    autocontadora :: [Int]
-- tal que autocontadora es la sucesi�n autocondadora con los n�meros 1
-- y 2. Por ejemplo,
--    take 11 autocontadora  ==  [1,2,2,1,1,2,1,2,2,1,2]
--    take 12 autocontadora  ==  [1,2,2,1,1,2,1,2,2,1,2,2]
--    take 18 autocontadora  ==  [1,2,2,1,1,2,1,2,2,1,2,2,1,1,2,1,1,2]
-- ---------------------------------------------------------------------

autocontadora :: [Int]
autocontadora = undefined

-- ---------------------------------------------------------------------
-- � El tri�ngulo de Floyd                                            --
-- ---------------------------------------------------------------------

-- El tri�ngulo de Floyd, llamado as� en honor a Robert Floyd, es un
-- tri�ngulo rect�ngulo formado con n�meros naturales. Para crear un
-- tri�ngulo de Floyd, se comienza con un 1 en la esquina superior
-- izquierda, y se contin�a escribiendo la secuencia de los n�meros
-- naturales de manera que cada l�nea contenga un n�mero m�s que la
-- anterior. Las 5 primeras l�neas del tri�ngulo de Floyd son
--     1
--     2   3
--     4   5   6
--     7   8   9  10
--    11  12  13  14  15
-- 
-- El tri�ngulo de Floyd tiene varias propiedades matem�ticas
-- interesantes. Los n�meros del cateto de la parte izquierda forman la
-- secuencia de los n�meros poligonales centrales, mientras que los de
-- la hipotenusa nos dan el conjunto de los n�meros triangulares.

-- ---------------------------------------------------------------------
-- Ejercicio 9.1. Definir la funci�n
--    siguienteF :: [Integer] -> [Integer]
-- tal que (siguienteF xs) es la lista de los elementos de la l�nea xs en
-- el tri�ngulo de Lloyd. Por ejemplo,
--    siguienteF [2,3]    ==  [4,5,6]
--    siguienteF [4,5,6]  ==  [7,8,9,10]
-- ---------------------------------------------------------------------

siguienteF :: [Integer] -> [Integer]
siguienteF xs = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 9.2. Definir la funci�n        
--    trianguloFloyd :: [[Integer]]
-- tal que trianguloFloyd es el tri�ngulo de Floyd. Por ejemplo,
--    ghci> take 4 trianguloFloyd
--    [[1],
--     [2,3],
--     [4,5,6],
--     [7,8,9,10]]
-- ---------------------------------------------------------------------

trianguloFloyd :: [[Integer]]
trianguloFloyd = undefined

-- Filas del tri�ngulo de Floyd
-- ============================

-- ---------------------------------------------------------------------
-- Ejercicio 9.3. Definir la funci�n
--    filaTrianguloFloyd :: Integer -> [Integer]
-- tal que (filaTrianguloFloyd n) es la fila n-�sima del tri�ngulo de
-- Floyd. Por ejemplo,  
--    filaTrianguloFloyd 3  ==  [4,5,6]
--    filaTrianguloFloyd 4  ==  [7,8,9,10]
-- ---------------------------------------------------------------------

filaTrianguloFloyd :: Integer -> [Integer]
filaTrianguloFloyd n = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 9.4. Definir la funci�n
--    sumaFilaTrianguloFloyd :: Integer -> Integer
-- tal que (sumaFilaTrianguloFloyd n) es la suma de los fila n-�sima del
-- tri�ngulo de Floyd. Por ejemplo,
--    sumaFilaTrianguloFloyd 1  ==  1
--    sumaFilaTrianguloFloyd 2  ==  5
--    sumaFilaTrianguloFloyd 3  ==  15
--    sumaFilaTrianguloFloyd 4  ==  34
--    sumaFilaTrianguloFloyd 5  ==  65
-- ---------------------------------------------------------------------

sumaFilaTrianguloFloyd :: Integer -> Integer
sumaFilaTrianguloFloyd = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 9.5. A partir de los valores de (sumaFilaTrianguloFloyd n)
-- para n entre 1 y 5, conjeturar una f�rmula para calcular
-- (sumaFilaTrianguloFloyd n). 
-- ---------------------------------------------------------------------


-- ---------------------------------------------------------------------
-- Ejecicio 6. Comprobar con QuickCheck la conjetura obtenida en el
-- ejercicio anterior.
-- ---------------------------------------------------------------------

-- La conjetura es
prop_sumaFilaTrianguloFloyd :: Integer -> Property
prop_sumaFilaTrianguloFloyd n = undefined
  
-- La comprobaci�n es

-- Hipotenusa del tri�ngulo de Floyd y n�meros triangulares
-- ========================================================

-- ---------------------------------------------------------------------
-- Ejercicio 9.7. Definir la funci�n
--    hipotenusaFloyd :: [Integer]
-- tal que hipotenusaFloyd es la lista de los elementos de la hipotenusa
-- del tri�ngulo de Floyd. Por ejemplo, 
--    take 5 hipotenusaFloyd  ==  [1,3,6,10,15]
-- ---------------------------------------------------------------------

hipotenusaFloyd :: [Integer]
hipotenusaFloyd = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 9.9. Definir la funci�n 
--    prop_hipotenusaFloyd :: Int -> Bool
-- tal que (prop_hipotenusaFloyd n) se verifica si los n primeros
-- elementos de la hipotenusa del tri�ngulo de Floyd son los primeros n
-- n�meros triangulares. 
-- 
-- Comprobar la propiedad para los 1000 primeros elementos.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_hipotenusaFloyd :: Int -> Bool
prop_hipotenusaFloyd n = undefined

-- La comprobaci�n es