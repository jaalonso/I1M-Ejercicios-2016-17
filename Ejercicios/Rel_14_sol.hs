-- I1M 2015-16: Relación 14 (12 de diciembre de 2015)
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

-- 1ª definición (por recursión)
divisoresPrimosEn1 :: Integer -> [Integer] -> Bool
divisoresPrimosEn1 1 _  = True
divisoresPrimosEn1 x [] = False
divisoresPrimosEn1 x (y:ys) 
    | mod x y == 0 = divisoresPrimosEn1 (div x y) (y:ys)
    | otherwise    = divisoresPrimosEn1 x ys   

-- 2ª definición (por comprensión)
divisoresPrimosEn2 :: Integer -> [Integer] -> Bool
divisoresPrimosEn2 x ys = and [elem y ys | y <- primeFactors x] 

-- 3ª definición (por cuantificación)
divisoresPrimosEn :: Integer -> [Integer] -> Bool
divisoresPrimosEn x ys = all (`elem` ys) (primeFactors x) 

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
hamming = [x | x <- [1..], divisoresPrimosEn x [2,3,5]]

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
cantidadHammingMenores x = length (takeWhile (<x) hamming)

-- ---------------------------------------------------------------------
-- Ejercicio 1.4. Definir la función
--    siguienteHamming :: Integer -> Integer
-- tal que (siguienteHamming x) es el menor número de la sucesión de
-- Hamming mayor que x. Por ejemplo,
--    siguienteHamming 6  ==  8
--    siguienteHamming 21  ==  24
-- ---------------------------------------------------------------------

siguienteHamming :: Integer -> Integer
siguienteHamming x = head (dropWhile (<=x) hamming)

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
huecoHamming n = [(x,y) | x <- hamming, 
                          let y = siguienteHamming x,
                          y-x > n]

-- ---------------------------------------------------------------------
-- Ejercicio 1.6. Comprobar con QuickCheck que para todo n, existen
-- pares de números consecutivos en la sucesión de Hamming cuya
-- distancia es mayor que n.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_Hamming :: Integer -> Bool
prop_Hamming n = huecoHamming n' /= []
    where n' = abs n

-- La comprobación es
--    ghci> quickCheck prop_Hamming
--    OK, passed 100 tests.

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

-- 1ª definición (por recursión y la criba de Erastótenes)
-- =======================================================

sumaPrimoMenores1 :: Integer -> Integer
sumaPrimoMenores1 n = sumaMenores n primos 0
   where sumaMenores n (x:xs) a | n <= x    = a
                                | otherwise = sumaMenores n xs (a+x)

-- primos es la lista de los número primos obtenida mediante la criba de 
-- Erastótenes. Por ejemplo,
--    primos  =>  [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,...
primos :: [Integer]
primos = criba [2..]
         where criba (p:ps) = p : criba [n | n<-ps, mod n p /= 0]

-- 2ª definición (por comprensión y la criba de Erastótenes)
-- =========================================================

sumaPrimoMenores2 :: Integer -> Integer
sumaPrimoMenores2 n = sum (takeWhile (<n) primos)

-- 3ª definición (por comprensión y la librería de primos)
-- =======================================================

sumaPrimoMenores3 :: Integer -> Integer
sumaPrimoMenores3 n = sum (takeWhile (<n) primes)

-- Comparación de eficiencia 
-- =========================

--    λ> sumaPrimoMenores1 20000
--    21171191
--    (5.11 secs, 922,508,496 bytes)
--    
--    λ> sumaPrimoMenores2 20000
--    21171191
--    (5.05 secs, 898,081,952 bytes)
--    
--    λ> sumaPrimoMenores3 20000
--    21171191
--    (0.02 secs, 0 bytes)

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
--    take 10 triangulares     ==  [1,3,6,10,15,21,28,36,45,55]
--    triangulares !! 2000000  ==  2000003000001
-- ---------------------------------------------------------------------

-- 1ª definición
triangulares1 :: [Integer]
triangulares1 = 1 : [x+y | (x,y) <- zip [2..] triangulares]

-- 2ª definición
triangulares2 :: [Integer]
triangulares2 = scanl (+) 1 [2..]

-- 3ª definición (usando la fórmula de la suma de la progresión):
triangulares3 :: [Integer]
triangulares3 = [(n*(n+1)) `div` 2 | n <- [1..]]

-- Comparación de eficiencia
--    λ> triangulares1 !! 1000000
--    500001500001
--    (3.07 secs, 484,321,192 bytes)
--    λ> triangulares2 !! 1000000
--    500001500001
--    (0.04 secs, 0 bytes)
--    λ> triangulares3 !! 1000000
--    500001500001
--    (1.23 secs, 186,249,472 bytes)

-- En lo sucesivo, usaremos como triangulares la segunda definición.
triangulares :: [Integer]
triangulares = triangulares2

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Definir la función
--    nDivisores :: Integer -> Integer
-- tal que (nDivisores n) es el número de los divisores de n. Por
-- ejemplo, 
--    nDivisores 28                 ==  6
--    nDivisores (product [1..200]) == 139503973313460993785856000000
-- ---------------------------------------------------------------------

-- 1ª definición
-- =============

nDivisores1 :: Integer -> Integer
nDivisores1 = genericLength . divisores 

-- (divisores n) es la lista de los divisores de n. Por ejemplo,
--    divisores 28  ==  [1,2,4,7,14,28]
divisores :: Integer -> [Integer]
divisores x = [y | y <- [1..x], mod x y == 0]

-- 2ª definición (con primeFactors y group)
-- ========================================

nDivisores2 :: Integer -> Integer
nDivisores2 n = 
    product [1 + genericLength xs | xs <- group (primeFactors n)]

-- Comparación de eficiencia
-- =========================

--    λ> nDivisores1 (product [1..10])
--    270
--    (5.18 secs, 763,249,336 bytes)
--    λ> nDivisores2 (product [1..10])
--    270
--    (0.01 secs, 0 bytes)

-- En lo sucesivo usaremos la 2ª definición de nDivisores
nDivisores :: Integer -> Integer
nDivisores = nDivisores2

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
euler12 n = head [x | x <- triangulares, nDivisores x > n]

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
enteros = 0 : concat [[-x,x] | x <- [1..]]

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Definir, por iteración, la constante
--    enteros' :: [Int]
-- tal que enteros' es la lista de los enteros con la ordenación
-- anterior. Por ejemplo,
--    take 10 enteros  ==  [0,-1,1,-2,2,-3,3,-4,4,-5]
-- ---------------------------------------------------------------------

enteros' :: [Int]
enteros' = iterate siguiente 0
    where siguiente x | x >= 0    = -x-1
                      | otherwise = -x

-- ---------------------------------------------------------------------
-- Ejercicio 4.3. Definir, por selección con takeWhile, la función
--    posicion :: Int -> Int
-- tal que (posicion x) es la posición del entero x en la ordenación
-- anterior. Por ejemplo,
--    posicion 2  ==  4
-- ---------------------------------------------------------------------

posicion :: Int -> Int
posicion x = length (takeWhile (/=x) enteros)

-- ---------------------------------------------------------------------
-- Ejercicio 4.4. Definir, por recursión, la función
--    posicionR :: Int -> Int
-- tal que (posicionR x) es la posición del entero x en la ordenación
-- anterior. Por ejemplo,
--    posicionR 2  ==  4
-- ---------------------------------------------------------------------

posicionR :: Int -> Int
posicionR x = aux enteros 0
    where aux (y:ys) n | x == y    = n
                       | otherwise = aux ys (n+1)

-- ---------------------------------------------------------------------
-- Ejercicio 4.5. Definir, por comprensión, la función
--    posicionC :: Int -> Int
-- tal que (posicionC x) es la posición del entero x en la ordenación
-- anterior. Por ejemplo,
--    posicionC 2  ==  4
-- ---------------------------------------------------------------------

posicionC :: Int -> Int
posicionC x = head [n | (n,y) <- zip [0..] enteros, y == x]

-- ---------------------------------------------------------------------
-- Ejercicio 4.6. Definir, sin búsqueda, la función
--    posicion2 :: Int -> Int
-- tal que (posicion2 x) es la posición del entero x en la ordenación
-- anterior. Por ejemplo,
--    posicion2 2  ==  4
-- ---------------------------------------------------------------------

-- Definición directa
posicion2 :: Int -> Int
posicion2 x | x >= 0    = 2*x
            | otherwise = 2*(-x)-1

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
eslabones i d n = [(i+d*j) `mod` n | j <- [0..]]

-- 2ª definición (con iterate):
eslabones2 :: Int -> Int -> Int -> [Int]
eslabones2 i d n = map (\x-> mod x n) (iterate (+d) i)

-- ---------------------------------------------------------------------
-- Ejercicio 5.2. Definir la función
--    numeroVueltas :: Int -> Int -> Int -> Int 
-- tal que (numeroVueltas i d n) es el número de vueltas que pasarán 
-- hasta que la cadena se rompa en una bicicleta de tipo (i,d,n). Por 
-- ejemplo,
--    numeroVueltas 2 7 25  ==  14
-- ---------------------------------------------------------------------

numeroVueltas :: Int -> Int -> Int -> Int
numeroVueltas i d n = length (takeWhile (/=0) (eslabones i d n)) 

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
golomb n = sucGolomb !! (n-1)

-- ---------------------------------------------------------------------
-- Ejercicio 6.2. Definir la función
--    sucGolomb :: [Int]
-- tal que sucGolomb es la lista de los términos de la sucesión de
-- Golomb. Por ejemplo,
--    take 15 sucGolomb  ==  [1,2,2,3,3,4,4,4,5,5,5,6,6,6,6]
-- Indicación: Se puede usar la función subSucGolomb del apartado 3.
-- ---------------------------------------------------------------------

sucGolomb :: [Int]
sucGolomb = subSucGolomb 1

-- ---------------------------------------------------------------------
-- Ejercicio 6.3. Definir la función
--    subSucGolomb :: Int -> [Int]
-- tal que (subSucGolomb x) es la lista de los términos de la sucesión
-- de Golomb a partir de la primera ocurrencia de x. Por ejemplo,
--    take 10 (subSucGolomb 4)  ==  [4,4,4,5,5,5,6,6,6,6]
-- Indicación: Se puede usar la función golomb del apartado 1.
-- ---------------------------------------------------------------------

subSucGolomb :: Int -> [Int]
subSucGolomb 1 = [1] ++ subSucGolomb 2
subSucGolomb 2 = [2,2] ++ subSucGolomb 3
subSucGolomb x = (replicate (golomb x) x) ++ subSucGolomb (x+1) 

-- Nota: La sucesión de Golomb puede definirse de forma más compacta
-- como se muestra a continuación.
sucGolomb2 :: [Int]
sucGolomb2 = 1 : 2 : 2 : g 3
    where g x      = replicate (golomb x) x ++ g (x+1) 
          golomb n = sucGolomb !! (n-1)


sucGolomb3 :: [Int]
sucGolomb3 = 1 : 2 : 2 : 
              concat [replicate n k | (n,k) <-zip (drop 2 sucGolomb3) [3..]]

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

-- 1ª definición (por recursión)
comprimida :: Eq a => [a] -> [(Int,a)]
comprimida xs = aux xs 1
    where aux (x:y:zs) n | x == y    = aux (y:zs) (n+1)
                         | otherwise = (n,x) : aux (y:zs) 1
          aux [x]      n             = [(n,x)]

-- 2ª definición (por recursión usando takeWhile):
comprimida2 :: Eq a => [a] -> [(Int,a)]
comprimida2 [] = []
comprimida2 (x:xs) = 
    (1 + length (takeWhile (==x) xs),x) : comprimida2 (dropWhile (==x) xs)

-- 3ª definición (por comprensión usando group):
comprimida3 :: Eq a => [a] -> [(Int,a)]
comprimida3 xs = [(length ys, head ys) | ys <- group xs]

-- 4ª definición (usando map y group):
comprimida4 :: Eq a => [a] -> [(Int,a)]
comprimida4 = map (\xs -> (length xs, head xs)) . group

-- ---------------------------------------------------------------------
-- Ejercicio 7.2. Definir la función
--    expandida :: [(Int,a)] -> [a]
-- tal que (expandida ps) es la lista expandida correspondiente a ps (es
-- decir, es la lista xs tal que la comprimida de xs es ps). Por
-- ejemplo, 
--    expandida [(2,1),(3,7),(2,5),(4,7)]  ==  [1,1,7,7,7,5,5,7,7,7,7]
-- ---------------------------------------------------------------------

-- 1ª definición (por comprensión)
expandida :: [(Int,a)] -> [a]
expandida ps = concat [replicate k x | (k,x) <- ps]

-- 2ª definición (por concatMap)
expandida2 :: [(Int,a)] -> [a]
expandida2 = concatMap (\(k,x) -> replicate k x) 

-- 3ª definición (por recursión)
expandida3 :: [(Int,a)] -> [a]
expandida3 [] = []
expandida3 ((n,x):ps) = replicate n x ++ expandida3 ps

-- ---------------------------------------------------------------------
-- Ejercicio 7.3. Comprobar con QuickCheck que dada una lista de enteros,
-- si se la comprime y después se expande se obtiene la lista inicial. 
-- ---------------------------------------------------------------------

-- La propiedad es
prop_expandida_comprimida :: [Int] -> Bool 
prop_expandida_comprimida xs = expandida (comprimida xs) == xs

-- La comprobación es
--    ghci> quickCheck prop_expandida_comprimida
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 7.4. Comprobar con QuickCheck que dada una lista de pares
-- de enteros, si se la expande y después se comprime se obtiene la
-- lista inicial.  
-- ---------------------------------------------------------------------

-- La propiedad es
prop_comprimida_expandida :: [(Int,Int)] -> Bool 
prop_comprimida_expandida xs = expandida (comprimida xs) == xs

-- La comprobación es
--    ghci> quickCheck prop_comprimida_expandida
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 7.5. Definir la función
--    listaAcadena :: [(Int,Char)] -> String
-- tal que (listaAcadena xs) es la cadena correspondiente a la lista de
-- pares de xs. Por ejemplo,
--    ghci> listaAcadena [(12,'B'),(1,'N'),(12,'B'),(3,'N'),(19,'B')]
--    "12B1N12B3N19B"
-- ---------------------------------------------------------------------

listaAcadena :: [(Int,Char)] -> String
listaAcadena xs = concat [show n ++ [c] | (n,c) <- xs]

-- ---------------------------------------------------------------------
-- Ejercicio 7.6. Definir la función
--    cadenaComprimida :: String -> String
-- tal que (cadenaComprimida cs) es la cadena obtenida comprimiendo por
-- longitud la cadena cs. Por ejemplo,
--    ghci> cadenaComprimida "BBBBBBBBBBBBNBBBBBBBBBBBBNNNBBBBBBBBBBNNN"
--    "12B1N12B3N10B3N"
-- ---------------------------------------------------------------------

cadenaComprimida :: String -> String
cadenaComprimida = listaAcadena . comprimida

-- ---------------------------------------------------------------------
-- Ejercicio 7.7. Definir la función 
--    cadenaAlista :: String -> [(Int,Char)]
-- tal que (cadenaAlista cs) es la lista de pares correspondientes a la
-- cadena cs. Por ejemplo,
--    ghci> cadenaAlista "12B1N12B3N10B3N"
--    [(12,'B'),(1,'N'),(12,'B'),(3,'N'),(10,'B'),(3,'N')]
-- ---------------------------------------------------------------------

cadenaAlista :: String -> [(Int,Char)]
cadenaAlista [] = []
cadenaAlista cs = (read ns,x) : cadenaAlista xs
    where (ns,(x:xs)) = span isNumber cs

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
cadenaExpandida = expandida . cadenaAlista

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

-- 1ª definición (usando group definida en Data.List)
contadora :: Eq a => [a] -> [Int]
contadora xs = map length (group xs)

-- 2ª definición (por recursión sin group):
contadora2 :: Eq a => [a] -> [Int]
contadora2 [] = []
contadora2 ys@(x:xs) = 
    length (takeWhile (==x) ys) : contadora2 (dropWhile (==x) xs)

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
contada (n:ns) (x:xs) = replicate n x ++ contada ns (xs++[x])
contada []     _      = []

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

-- 1ª solución
autocontadora :: [Int]
autocontadora = [1,2] ++ siguiente [2] 2

-- Los pasos lo da la función siguiente. Por ejemplo,
--    take 3 (siguiente [2] 2)            ==  [2,1,1]
--    take 4 (siguiente [2,1,1] 1)        ==  [2,1,1,2]
--    take 6 (siguiente [2,1,1,2] 2)      ==  [2,1,1,2,1,1]
--    take 7 (siguiente [2,1,1,2,1,1] 1)  ==  [2,1,1,2,1,1,2]
siguiente (x:xs) y = x : siguiente (xs ++ (nuevos x)) y'
    where contrario 1 = 2
          contrario 2 = 1
          y'          = contrario y              
          nuevos 1    = [y']
          nuevos 2    = [y',y'] 

-- 2ª solución (usando contada)
autocontadora2 :: [Int]
autocontadora2 = 1 : 2: xs 
    where xs = 2 : contada xs [1,2]

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
siguienteF xs = [a..a+n]
    where a = 1+last xs
          n = genericLength xs

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
trianguloFloyd = iterate siguienteF [1]

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
filaTrianguloFloyd n = trianguloFloyd `genericIndex` (n-1)

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
sumaFilaTrianguloFloyd = sum . filaTrianguloFloyd

-- ---------------------------------------------------------------------
-- Ejercicio 9.5. A partir de los valores de (sumaFilaTrianguloFloyd n)
-- para n entre 1 y 5, conjeturar una fórmula para calcular
-- (sumaFilaTrianguloFloyd n). 
-- ---------------------------------------------------------------------

-- Usando Wolfram Alpha (como se indica en http://wolfr.am/19XAl2X )
-- a partir de 1, 5, 15, 34, 65, ... se obtiene la fórmula
--    (n^3+n)/2

-- ---------------------------------------------------------------------
-- Ejecicio 6. Comprobar con QuickCheck la conjetura obtenida en el
-- ejercicio anterior.
-- ---------------------------------------------------------------------

-- La conjetura es
prop_sumaFilaTrianguloFloyd :: Integer -> Property
prop_sumaFilaTrianguloFloyd n =        
    n > 0 ==> sum (filaTrianguloFloyd n) == (n^3+n) `div` 2
  
-- La comprobación es
--    ghci> quickCheck prop_sumaFilaTrianguloFloyd
--    +++ OK, passed 100 tests.

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
hipotenusaFloyd = map last trianguloFloyd

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
prop_hipotenusaFloyd n = 
    take n hipotenusaFloyd == take n triangulares

-- La comprobación es
--    ghci> prop_hipotenusaFloyd 1000
--    True

-- Cateto del triángulo de Floyd y números poligonales centrales
-- =============================================================

-- ---------------------------------------------------------------------
-- Ejercicio 9.10. Definir la función
--    catetoFloyd :: [Integer]
-- tal que catetoFloyd es la lista de los elementos del cateto izquierdo
-- del triángulo de Floyd. Por ejemplo, 
--    take 5 catetoFloyd  ==  [1,2,4,7,11]
-- ---------------------------------------------------------------------

catetoFloyd :: [Integer]
catetoFloyd = map head trianguloFloyd

-- ---------------------------------------------------------------------
-- Ejercicio 9.11. El n-ésimo número poligonal centrado es el máximo
-- número de piezas que se pueden obtener a partir de un círculo con n
-- líneas rectas. Por ejemplo,
--    poligonales_centrados.jpg
--
-- Definir la función
--    poligonalCentrado :: Integer -> Integer
-- tal que (poligonalCentrado n) es el n-ésimo número poligonal
-- centrado. Por ejemplo, 
--    [poligonalCentrado n | n <- [0..5]]  ==  [1,2,4,7,11,16]
-- ---------------------------------------------------------------------

poligonalCentrado :: Integer -> Integer
poligonalCentrado 0 = 1
poligonalCentrado n = n + poligonalCentrado (n-1)

-- ---------------------------------------------------------------------
-- Ejercicio 9.12. Definir la función
--    poligonalesCentrados :: [Integer]
-- tal que poligonalesCentrados es la lista de los números poligonales
-- centrados. Por ejemplo, 
--    take 10 poligonalesCentrados  ==  [1,3,6,10,15,21,28,36,45,55]
-- ---------------------------------------------------------------------

-- 1ª definición:
poligonalesCentrados1 :: [Integer]
poligonalesCentrados1 = [poligonalCentrado n | n <- [0..]]

-- 2ª definición (usando scanl):
poligonalesCentrados :: [Integer]
poligonalesCentrados = scanl (+) 1 [1..]

-- ---------------------------------------------------------------------
-- Ejercicio 9.13. Definir la función 
--    prop_catetoFloyd :: Int -> Bool
-- tal que (prop_catetoFloyd n) se verifica si los n primeros
-- elementos del cateto izquierdo del triángulo de Floy son los primeros
-- n números poligonales centrados.
-- 
-- Comprobar la propiedad para los 1000 primeros elementos.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_catetoFloyd :: Int -> Bool
prop_catetoFloyd n = 
    take n catetoFloyd == take n poligonalesCentrados

-- La comprobación es
--    ghci> prop_catetoFloyd 1000
--    True
