-- I1M 2015-16: Relación 27 (12 de diciembre de 2015)
-- Aplicaciones de la programación funcional con listas infinitas.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- En esta relación se estudia distintas aplicaciones de la programación
-- funcional que usan listas infinitas
-- + la sucesión de Hamming,
-- + problemas 10 y 12 del proyecto Euler,
-- + enumeración de los números enteros,
-- + el problema de la bicicleta de Turing,
-- + la sucesión de Golomb,
-- + la codificación por longitud,
-- + la sucesión de Kolakoski y
-- + el triángulo de Floyd.

-- ---------------------------------------------------------------------
-- Importación de librerías                                           --
-- ---------------------------------------------------------------------

import Data.Char 
import Data.List
import Data.Numbers.Primes
import Test.QuickCheck

-- ---------------------------------------------------------------------
-- § Sucesión de Hamming                                              --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Definir la función
--    divisoresPrimosEn :: Integer -> [Integer] -> Bool
-- tal que (divisoresPrimosEn x ys) se verifica si x puede expresarse
-- como un producto de potencias de elementos de la lista de números
-- primos ys. Por ejemplo, 
--    divisoresPrimosEn 12 [2,3,5]  ==  True
--    divisoresPrimosEn 14 [2,3,5]  ==  False
-- ---------------------------------------------------------------------

divisoresPrimosEn :: Integer -> [Integer] -> Bool
divisoresPrimosEn = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Los números de Hamming forman una sucesión 
-- estrictamente creciente de números que cumplen las siguientes 
-- condiciones: 
--    1. El número 1 está en la sucesión.
--    2. Si x está en la sucesión, entonces 2x, 3x y 5x también están.
--    3. Ningún otro número está en la sucesión.
-- Definir, usando divisoresPrimosEn, la constante
--    hamming :: [Integer]
-- tal que hamming es la sucesión de Hamming. Por ejemplo,
--    take 12 hamming  ==  [1,2,3,4,5,6,8,9,10,12,15,16]
-- ---------------------------------------------------------------------

hamming :: [Integer]
hamming = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 1.3. Definir la función
--    cantidadHammingMenores :: Integer -> Int
-- tal que (cantidadHammingMenores x) es la cantidad de números de
-- Hamming menores que x. Por ejemplo,
--    cantidadHammingMenores 6  ==  5
--    cantidadHammingMenores 7  ==  6
--    cantidadHammingMenores 8  ==  6
-- ---------------------------------------------------------------------

cantidadHammingMenores :: Integer -> Int
cantidadHammingMenores x = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 1.4. Definir la función
--    siguienteHamming :: Integer -> Integer
-- tal que (siguienteHamming x) es el menor número de la sucesión de
-- Hamming mayor que x. Por ejemplo,
--    siguienteHamming 6  ==  8
--    siguienteHamming 21  ==  24
-- ---------------------------------------------------------------------

siguienteHamming :: Integer -> Integer
siguienteHamming x = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 1.5. Definir la función
--    huecoHamming :: Integer -> [(Integer,Integer)]
-- tal que (huecoHamming n) es la lista de pares de números consecutivos
-- en la sucesión de Hamming cuya distancia es mayor que n. Por ejemplo,  
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
-- pares de números consecutivos en la sucesión de Hamming cuya
-- distancia es mayor o igual que n.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_Hamming :: Integer -> Bool
prop_Hamming n = undefined

-- La comprobación es

-- ---------------------------------------------------------------------
-- § Problema 10 del Proyecto Euler                                   --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función 
--    sumaPrimoMenores :: Integer -> Integer
-- tal que (sumaPrimoMenores n) es la suma de los primos menores que
-- n. Por ejemplo,
--    sumaPrimoMenores 10  ==  17
--    sumaPrimoMenores 7   ==  10                       
-- ---------------------------------------------------------------------

sumaPrimoMenores :: Integer -> Integer
sumaPrimoMenores n = undefined


-- ---------------------------------------------------------------------
-- § Problema 12 del Proyecto Euler                                   --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Los números triangulares se forman como sigue
--    *     *      * 
--         * *    * *
--               * * *
--    1     3      6
-- 
-- La sucesión de los números triangulares se obtiene sumando los
-- números naturales. Así, los 5 primeros números triangulares son
--     1 = 1
--     3 = 1+2
--     6 = 1+2+3
--    10 = 1+2+3+4
--    15 = 1+2+3+4+5
-- 
-- Definir la función
--    triangulares :: [Integer]
-- tal que triangulares es la lista de los números triangulares. Por
-- ejemplo, 
--    take 10 triangulares  ==  [1,3,6,10,15,21,28,36,45,55]
-- ---------------------------------------------------------------------

triangulares :: [Integer]
triangulares = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Definir la función
--    nDivisores :: Integer -> Integer
-- tal que (nDivisores n) es el número de los divisores de n. Por
-- ejemplo, 
--    nDivisores 28                 ==  6
--    nDivisores (product [1..200]) == 139503973313460993785856000000
-- ---------------------------------------------------------------------

nDivisores :: Integer -> Integer
nDivisores = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 3.3. Los divisores de los primeros 7 números triangulares
-- son: 
--     1: 1
--     3: 1,3
--     6: 1,2,3,6
--    10: 1,2,5,10
--    15: 1,3,5,15
--    21: 1,3,7,21
--    28: 1,2,4,7,14,28
-- Como se puede observar, 28 es el menor número triangular con más de 5
-- divisores. 
-- 
-- Definir la función 
--    euler12 :: Int -> Integer
-- tal que (euler12 n) es el menor número triangular con más de n
-- divisores. Por ejemplo,
--    euler12 5    ==  28
--    euler12 500  ==  76576500
-- ---------------------------------------------------------------------

euler12 :: Integer -> Integer
euler12 n = undefined

-- ---------------------------------------------------------------------
-- § Enumeración de los números enteros                               --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. Los números enteros se pueden ordenar como sigue 
--    0, -1, 1, -2, 2, -3, 3, -4, 4, -5, 5, -6, 6, -7, 7, ...
-- Definir, por comprensión, la constante
--    enteros :: [Int]
-- tal que enteros es la lista de los enteros con la ordenación
-- anterior. Por ejemplo,
--    take 10 enteros  ==  [0,-1,1,-2,2,-3,3,-4,4,-5]
-- ---------------------------------------------------------------------

enteros :: [Int]
enteros = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Definir, por iteración, la constante
--    enteros' :: [Int]
-- tal que enteros' es la lista de los enteros con la ordenación
-- anterior. Por ejemplo,
--    take 10 enteros  ==  [0,-1,1,-2,2,-3,3,-4,4,-5]
-- ---------------------------------------------------------------------

enteros' :: [Int]
enteros' = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 4.3. Definir, por selección con takeWhile, la función
--    posicion :: Int -> Int
-- tal que (posicion x) es la posición del entero x en la ordenación
-- anterior. Por ejemplo,
--    posicion 2  ==  4
-- ---------------------------------------------------------------------

posicion :: Int -> Int
posicion x = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 4.4. Definir, por recursión, la función
--    posicionR :: Int -> Int
-- tal que (posicionR x) es la posición del entero x en la ordenación
-- anterior. Por ejemplo,
--    posicionR 2  ==  4
-- ---------------------------------------------------------------------

posicionR :: Int -> Int
posicionR x = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 4.5. Definir, por comprensión, la función
--    posicionC :: Int -> Int
-- tal que (posicionC x) es la posición del entero x en la ordenación
-- anterior. Por ejemplo,
--    posicionC 2  ==  4
-- ---------------------------------------------------------------------

posicionC :: Int -> Int
posicionC x = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 4.6. Definir, sin búsqueda, la función
--    posicion2 :: Int -> Int
-- tal que (posicion2 x) es la posición del entero x en la ordenación
-- anterior. Por ejemplo,
--    posicion2 2  ==  4
-- ---------------------------------------------------------------------

-- Definición directa
posicion2 :: Int -> Int
posicion2 = undefined

-- ---------------------------------------------------------------------
-- § El problema de la bicicleta de Turing                            --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 5.1. Cuentan que Alan Turing tenía una bicicleta vieja,
-- que tenía una cadena con un eslabón débil y además uno de los radios
-- de la rueda estaba doblado. Cuando el radio doblado coincidía con el
-- eslabón débil, entonces la cadena se rompía.   
--
-- La bicicleta se identifica por los parámetros (i,d,n) donde 
-- - i es el número del eslabón que coincide con el radio doblado al
--   empezar a andar,
-- - d es el número de eslabones que se desplaza la cadena en cada
--   vuelta de la rueda y  
-- - n es el número de eslabones de la cadena (el número n es el débil).
-- Si i=2 y d=7 y n=25, entonces la lista con el número de eslabón que 
-- toca el radio doblado en cada vuelta es 
--    [2,9,16,23,5,12,19,1,8,15,22,4,11,18,0,7,14,21,3,10,17,24,6,...
-- Con lo que la cadena se rompe en la vuelta número 14.
-- 
-- Definir la función
--    eslabones :: Int -> Int -> Int -> [Int]
-- tal que (eslabones i d n) es la lista con los números de eslabones 
-- que tocan el radio doblado en cada vuelta en una bicicleta de tipo 
-- (i,d,n). Por ejemplo, 
--    take 10 (eslabones 2 7 25)  ==  [2,9,16,23,5,12,19,1,8,15]
-- ---------------------------------------------------------------------

eslabones :: Int -> Int -> Int -> [Int]
eslabones i d n = undefined

-- 2ª definición (con iterate):
eslabones2 :: Int -> Int -> Int -> [Int]
eslabones2 i d n = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 5.2. Definir la función
--    numeroVueltas :: Int -> Int -> Int -> Int 
-- tal que (numeroVueltas i d n) es el número de vueltas que pasarán 
-- hasta que la cadena se rompa en una bicicleta de tipo (i,d,n). Por 
-- ejemplo,
--    numeroVueltas 2 7 25  ==  14
-- ---------------------------------------------------------------------

numeroVueltas :: Int -> Int -> Int -> Int
numeroVueltas i d n = undefined

-- ---------------------------------------------------------------------
-- § La sucesión de Golomb                                            --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 6.1. [Basado en el problema 341 del proyecto Euler]. La
-- sucesión de Golomb {G(n)} es una sucesión auto descriptiva: es la
-- única sucesión no decreciente de números naturales tal que el número
-- n aparece G(n) veces en la sucesión. Los valores de G(n) para los
-- primeros números son los siguientes:
--    n       1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 ...
--    G(n)    1 2 2 3 3 4 4 4 5  5  5  6  6  6  6 ...
-- En los apartados de este ejercicio se definirá una función para
-- calcular los términos de la sucesión de Golomb. 
-- 
-- Definir la función
--    golomb :: Int -> Int
-- tal que (golomb n) es el n-ésimo término de la sucesión de Golomb. 
-- Por ejemplo,
--    golomb 5  ==  3
--    golomb 9  ==  5
-- Indicación: Se puede usar la función sucGolomb del apartado 2.
-- ---------------------------------------------------------------------

golomb :: Int -> Int
golomb n = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 6.2. Definir la función
--    sucGolomb :: [Int]
-- tal que sucGolomb es la lista de los términos de la sucesión de
-- Golomb. Por ejemplo,
--    take 15 sucGolomb  ==  [1,2,2,3,3,4,4,4,5,5,5,6,6,6,6]
-- Indicación: Se puede usar la función subSucGolomb del apartado 3.
-- ---------------------------------------------------------------------

sucGolomb :: [Int]
sucGolomb = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 6.3. Definir la función
--    subSucGolomb :: Int -> [Int]
-- tal que (subSucGolomb x) es la lista de los términos de la sucesión
-- de Golomb a partir de la primera ocurrencia de x. Por ejemplo,
--    take 10 (subSucGolomb 4)  ==  [4,4,4,5,5,5,6,6,6,6]
-- Indicación: Se puede usar la función golomb del apartado 1.
-- ---------------------------------------------------------------------

subSucGolomb :: Int -> [Int]
subSucGolomb = undefined

-- ---------------------------------------------------------------------
-- § La codificación por longitud                                     --
-- ---------------------------------------------------------------------

-- La codificación por longitud, o comprensión RLE (del inglés,
-- "Run-length encoding"), es una compresión de datos en la que
-- secuencias de datos con el mismo valor consecutivas son almacenadas
-- como un único valor más su recuento. Por ejemplo, la cadena 
--    BBBBBBBBBBBBNBBBBBBBBBBBBNNNBBBBBBBBBBBBBBBBBBBBBBBBNBBBBBBBBBBBBBB
-- se codifica por 
--    12B1N12B3N24B1N14B
-- Interpretado esto como 12 letras B, 1 letra N , 12 letras B, 3 letras
-- N, etc.
-- 
-- En los siguientes ejercicios se definirán funciones para codificar y
-- descodificar por longitud.

-- ---------------------------------------------------------------------
-- Ejercicio 7.1. Una lista se puede comprimir indicando el número de
-- veces consecutivas que aparece cada elemento. Por ejemplo, la lista 
-- comprimida de [1,1,7,7,7,5,5,7,7,7,7] es [(2,1),(3,7),(2,5),(4,7)],
-- indicando que comienza con dos 1, seguido de tres 7, dos 5 y cuatro
-- 7. 
-- 
-- Definir la función
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
-- Ejercicio 7.2. Definir la función
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
-- si se la comprime y después se expande se obtiene la lista inicial. 
-- ---------------------------------------------------------------------

-- La propiedad es
prop_expandida_comprimida :: [Int] -> Bool 
prop_expandida_comprimida xs = undefined

-- La comprobación es

-- ---------------------------------------------------------------------
-- Ejercicio 7.4. Comprobar con QuickCheck que dada una lista de pares
-- de enteros, si se la expande y después se comprime se obtiene la
-- lista inicial.  
-- ---------------------------------------------------------------------

-- La propiedad es
prop_comprimida_expandida :: [(Int,Int)] -> Bool 
prop_comprimida_expandida xs = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 7.5. Definir la función
--    listaAcadena :: [(Int,Char)] -> String
-- tal que (listaAcadena xs) es la cadena correspondiente a la lista de
-- pares de xs. Por ejemplo,
--    ghci> listaAcadena [(12,'B'),(1,'N'),(12,'B'),(3,'N'),(19,'B')]
--    "12B1N12B3N19B"
-- ---------------------------------------------------------------------

listaAcadena :: [(Int,Char)] -> String
listaAcadena xs = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 7.6. Definir la función
--    cadenaComprimida :: String -> String
-- tal que (cadenaComprimida cs) es la cadena obtenida comprimiendo por
-- longitud la cadena cs. Por ejemplo,
--    ghci> cadenaComprimida "BBBBBBBBBBBBNBBBBBBBBBBBBNNNBBBBBBBBBBNNN"
--    "12B1N12B3N10B3N"
-- ---------------------------------------------------------------------

cadenaComprimida :: String -> String
cadenaComprimida = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 7.7. Definir la función 
--    cadenaAlista :: String -> [(Int,Char)]
-- tal que (cadenaAlista cs) es la lista de pares correspondientes a la
-- cadena cs. Por ejemplo,
--    ghci> cadenaAlista "12B1N12B3N10B3N"
--    [(12,'B'),(1,'N'),(12,'B'),(3,'N'),(10,'B'),(3,'N')]
-- ---------------------------------------------------------------------

cadenaAlista :: String -> [(Int,Char)]
cadenaAlista = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 7.8. Definir la función
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
-- § La sucesión de Kolakoski                                         --
-- ---------------------------------------------------------------------

-- Dada una sucesión, su contadora es la sucesión de las longitudes de
-- de sus bloque de elementos consecutivos iguales. Por ejemplo, la
-- sucesión contadora de abbaaabbba es 12331; es decir; 1 vez la a,
-- 2 la b, 3 la a, 3 la b y 1 la a.
-- 
-- La sucesión de Kolakoski es una sucesión infinita de los símbolos 1 y
-- 2 que es su propia contadora. Los primeros términos de la sucesión
-- de Kolakoski son 1221121221221... que coincide con su contadora (es
-- decir, 1 vez el 1, 2 veces el 2, 2 veces el 1, ...). 
-- 
-- En esta sección se define la sucesión de Kolakoski.

-- ---------------------------------------------------------------------
-- Ejercicio 8.1. Dados los símbolos a y b, la sucesión contadora de
--    abbaaabbba... =  a bb aaa bbb a ...  
-- es
--    1233...       =  1 2  3   3...
-- es decir; 1 vez la a, 2 la b, 3 la a, 3 la b, 1 la a, ...
-- 
-- Definir la función
--    contadora :: Eq a => [a] -> [Int]
-- tal que (contadora xs) es la sucesión contadora de xs. Por ejemplo,
--    contadora "abbaaabbb"        ==  [1,2,3,3]
--    contadora "122112122121121"  ==  [1,2,2,1,1,2,1,1,2,1,1]
-- ---------------------------------------------------------------------

contadora :: Eq a => [a] -> [Int]
contadora xs = undefined


-- ---------------------------------------------------------------------
-- Ejercicio 8.2. Definir la función
--    contada :: [Int] -> [a] -> [a]
-- tal que (contada ns xs) es la sucesión formada por los símbolos de xs
-- cuya contadora es ns. Por ejemplo,
--    contada [1,2,3,3] "ab"                ==  "abbaaabbb"
--    contada [1,2,3,3] "abc"               ==  "abbcccaaa"
--    contada [1,2,2,1,1,2,1,1,2,1,1] "12"  ==  "122112122121121"
-- ---------------------------------------------------------------------

contada :: [Int] -> [a] -> [a]
contada = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 8.3. La sucesión autocontadora (o sucesión de  Kolakoski) es
-- la sucesión xs formada por 1 y 2 tal que coincide con su contada; es
-- decir (contadora xs) == xs. Los primeros términos de la función
-- autocontadora son
--    1221121221221... = 1 22 11 2 1 22 1 22 11 ...
-- y su contadora es
--    122112122...     = 1 2  2  1 1 2  1 2  2...
-- que coincide con la inicial. 
-- 
-- Definir la función
--    autocontadora :: [Int]
-- tal que autocontadora es la sucesión autocondadora con los números 1
-- y 2. Por ejemplo,
--    take 11 autocontadora  ==  [1,2,2,1,1,2,1,2,2,1,2]
--    take 12 autocontadora  ==  [1,2,2,1,1,2,1,2,2,1,2,2]
--    take 18 autocontadora  ==  [1,2,2,1,1,2,1,2,2,1,2,2,1,1,2,1,1,2]
-- ---------------------------------------------------------------------

autocontadora :: [Int]
autocontadora = undefined

-- ---------------------------------------------------------------------
-- § El triángulo de Floyd                                            --
-- ---------------------------------------------------------------------

-- El triángulo de Floyd, llamado así en honor a Robert Floyd, es un
-- triángulo rectángulo formado con números naturales. Para crear un
-- triángulo de Floyd, se comienza con un 1 en la esquina superior
-- izquierda, y se continúa escribiendo la secuencia de los números
-- naturales de manera que cada línea contenga un número más que la
-- anterior. Las 5 primeras líneas del triángulo de Floyd son
--     1
--     2   3
--     4   5   6
--     7   8   9  10
--    11  12  13  14  15
-- 
-- El triángulo de Floyd tiene varias propiedades matemáticas
-- interesantes. Los números del cateto de la parte izquierda forman la
-- secuencia de los números poligonales centrales, mientras que los de
-- la hipotenusa nos dan el conjunto de los números triangulares.

-- ---------------------------------------------------------------------
-- Ejercicio 9.1. Definir la función
--    siguienteF :: [Integer] -> [Integer]
-- tal que (siguienteF xs) es la lista de los elementos de la línea xs en
-- el triángulo de Lloyd. Por ejemplo,
--    siguienteF [2,3]    ==  [4,5,6]
--    siguienteF [4,5,6]  ==  [7,8,9,10]
-- ---------------------------------------------------------------------

siguienteF :: [Integer] -> [Integer]
siguienteF xs = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 9.2. Definir la función        
--    trianguloFloyd :: [[Integer]]
-- tal que trianguloFloyd es el triángulo de Floyd. Por ejemplo,
--    ghci> take 4 trianguloFloyd
--    [[1],
--     [2,3],
--     [4,5,6],
--     [7,8,9,10]]
-- ---------------------------------------------------------------------

trianguloFloyd :: [[Integer]]
trianguloFloyd = undefined

-- Filas del triángulo de Floyd
-- ============================

-- ---------------------------------------------------------------------
-- Ejercicio 9.3. Definir la función
--    filaTrianguloFloyd :: Integer -> [Integer]
-- tal que (filaTrianguloFloyd n) es la fila n-ésima del triángulo de
-- Floyd. Por ejemplo,  
--    filaTrianguloFloyd 3  ==  [4,5,6]
--    filaTrianguloFloyd 4  ==  [7,8,9,10]
-- ---------------------------------------------------------------------

filaTrianguloFloyd :: Integer -> [Integer]
filaTrianguloFloyd n = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 9.4. Definir la función
--    sumaFilaTrianguloFloyd :: Integer -> Integer
-- tal que (sumaFilaTrianguloFloyd n) es la suma de los fila n-ésima del
-- triángulo de Floyd. Por ejemplo,
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
-- para n entre 1 y 5, conjeturar una fórmula para calcular
-- (sumaFilaTrianguloFloyd n). 
-- ---------------------------------------------------------------------


-- ---------------------------------------------------------------------
-- Ejecicio 6. Comprobar con QuickCheck la conjetura obtenida en el
-- ejercicio anterior.
-- ---------------------------------------------------------------------

-- La conjetura es
prop_sumaFilaTrianguloFloyd :: Integer -> Property
prop_sumaFilaTrianguloFloyd n = undefined
  
-- La comprobación es

-- Hipotenusa del triángulo de Floyd y números triangulares
-- ========================================================

-- ---------------------------------------------------------------------
-- Ejercicio 9.7. Definir la función
--    hipotenusaFloyd :: [Integer]
-- tal que hipotenusaFloyd es la lista de los elementos de la hipotenusa
-- del triángulo de Floyd. Por ejemplo, 
--    take 5 hipotenusaFloyd  ==  [1,3,6,10,15]
-- ---------------------------------------------------------------------

hipotenusaFloyd :: [Integer]
hipotenusaFloyd = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 9.9. Definir la función 
--    prop_hipotenusaFloyd :: Int -> Bool
-- tal que (prop_hipotenusaFloyd n) se verifica si los n primeros
-- elementos de la hipotenusa del triángulo de Floyd son los primeros n
-- números triangulares. 
-- 
-- Comprobar la propiedad para los 1000 primeros elementos.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_hipotenusaFloyd :: Int -> Bool
prop_hipotenusaFloyd n = undefined

-- La comprobación es
