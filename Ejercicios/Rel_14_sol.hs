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

-- manvermor alvalvdom1 ivaruicam josllagam blaruiher marvilmor
-- abrdelrod alebergon juanarcon javoliher migandben
divisoresPrimosEn :: Integer -> [Integer] -> Bool
divisoresPrimosEn x ys = and [elem y ys | y <- primeFactors x] 

-- carmengar juamorrom1 fracruzam rubvilval manpende erisancha
divisoresPrimosEn2 :: Integer -> [Integer] -> Bool
divisoresPrimosEn2 n ns = all (`elem` ns) p
    where p = primeFactors n

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

-- manvermor alvalvdom1 carmengar juamorrom1 ivaruicam josllagam
-- blaruiher rubvilval manpende erisancha marvilmor abrdelrod alebergon
-- juanarcon javoliher migandben
hamming :: [Integer]
hamming = [x | x <- [1..], divisoresPrimosEn x [2,3,5]]

-- fracruzam
hamming2 :: [Integer]
hamming2 = filter esHamming [1..]
    where esHamming :: Integer -> Bool
          esHamming 1 = True
          esHamming n | n `rem` 2 == 0 = esHamming (n `div` 2)
                      | n `rem` 3 == 0 = esHamming (n `div` 3)
                      | n `rem` 5 == 0 = esHamming (n `div` 5)
                      | otherwise      = False

-- ---------------------------------------------------------------------
-- Ejercicio 1.3. Definir la función
--    cantidadHammingMenores :: Integer -> Int
-- tal que (cantidadHammingMenores x) es la cantidad de números de
-- Hamming menores que x. Por ejemplo,
--    cantidadHammingMenores 6  ==  5
--    cantidadHammingMenores 7  ==  6
--    cantidadHammingMenores 8  ==  6
-- ---------------------------------------------------------------------
 
-- manvermor alvalvdom1 carmengar juamorrom1 fracruzam ivaruicam
-- josllagam blaruiher rubvilval manpende erisancha marvilmor abrdelrod
-- alebergon juanarcon javoliher migandben
cantidadHammingMenores :: Integer -> Int
cantidadHammingMenores x = length $ takeWhile (<x) hamming

-- ---------------------------------------------------------------------
-- Ejercicio 1.4. Definir la función
--    siguienteHamming :: Integer -> Integer
-- tal que (siguienteHamming x) es el menor número de la sucesión de
-- Hamming mayor que x. Por ejemplo,
--    siguienteHamming 6  ==  8
--    siguienteHamming 21  ==  24
-- ---------------------------------------------------------------------

-- manvermor alvalvdom1 fracruzam ivaruicam josllagam blaruiher
-- rubvilval manpende erisancha marvilmor alebergon javoliher
siguienteHamming :: Integer -> Integer
siguienteHamming x = head $ dropWhile (<=x) hamming

-- carmengar
siguienteHamming2 :: Integer -> Integer
siguienteHamming2 x =
    head $ filter (`divisoresPrimosEn` [2,3,5]) [x+1..]

-- juamorrom1 abrdelrod juanarcon migandben
siguienteHamming3 :: Integer -> Integer
siguienteHamming3 x = head $ filter (>x) hamming

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

-- manvermor alvalvdom1 ivaruicam rubvilval alebergon javoliher
-- migandben 
huecoHamming :: Integer -> [(Integer,Integer)]
huecoHamming n = 
    [(x,y) | (x,y) <- zip hamming (tail hamming), y-x > n]

-- Comentario: La definición anterior se puede simplificar.

-- juamorrom1 josllagam blaruiher manpende erisancha abrdelrod
-- juanarcon
huecoHamming2 :: Integer -> [(Integer,Integer)]
huecoHamming2 n = 
    [(x,siguienteHamming x) | x <- hamming, 
                              (siguienteHamming x)-x > n ]

-- fracruzam marvilmor
huecoHamming3 :: Integer -> [(Integer,Integer)]
huecoHamming3 n = 
    filter (\(x,y) -> y - x > n) $ zip hamming (tail hamming)

-- ---------------------------------------------------------------------
-- Ejercicio 1.6. Comprobar con QuickCheck que para todo n, existen
-- pares de números consecutivos en la sucesión de Hamming cuya
-- distancia es mayor o igual que n.
-- ---------------------------------------------------------------------

-- alvalvdom1 carmengar juamorrom1 fracruzam ivaruicam josllagam
-- blaruiher manpende marvilmor  javoliher migandben

-- La propiedad es
huecoHamming' :: Integer -> [(Integer,Integer)]
huecoHamming' n = 
    [(x,y) | (x,y) <- zip hamming (tail hamming), y-x>=n]

prop_Hamming :: Integer -> Bool
prop_Hamming n = not $ null $ huecoHamming' n

-- La comprobación es 
--    λ> quickCheck prop_Hamming
--    +++ OK, passed 100 tests.

-- rubvilval erisancha abrdelrod alebergon juanarcon

-- La propiedad es
prop_Hamming2 :: Integer -> Bool
prop_Hamming2 n = huecoHamming n /= []

-- La comprobación es
--    λ> quickCheck prop_Hamming
--    +++ OK, passed 100 tests.

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

-- manvermor alvalvdom1 carmengar fracruzam josllagam erisancha
-- abrdelrod juanarcon javoliher migandben
sumaPrimoMenores :: Integer -> Integer
sumaPrimoMenores n = sum (takeWhile (< n) primes)

-- ivaruicam
sumaPrimoMenores2 :: Integer -> Integer
sumaPrimoMenores2 n = aux n primes
    where aux n (x:xs) | x < n     = x + aux n xs
                       | otherwise = 0

-- Comentario: La definición anterior se puede simplificar.

-- blaruiher rubvilval manpende marvilmor alebergon
sumaPrimoMenores3 :: Integer -> Integer
sumaPrimoMenores3 n = foldl1 (+) $ takeWhile (<n) primes

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

-- manvermor
triangulares :: [Integer]
triangulares = aux 1
    where aux n = sum [x | x <- [1..n]] : aux (n+1)

-- carmengar josllagam rubvilval alvalvdom1 abrdelrod alebergon 
-- juanarcon javoliher
triangulares2 :: [Integer]
triangulares2 = [sum [1..n] | n <- [1..]] 

-- ivaruicam
triangulares3 :: [Integer]
triangulares3 = aux 1 [0..]
    where aux v (x:xs) = v : aux (v+x) xs 

-- fracruzam
triangulares4 :: [Integer]
triangulares4 = aux 0 1
    where aux :: Integer -> Integer -> [Integer]
          aux n s = (n+s) : aux (n+s) (s+1)

-- blaruiher manpende erisancha marvilmor
triangulares5 :: [Integer]
triangulares5 = [sumh x | x <- [2..]]
    where sumh x = sum [1..x-1]

-- migandben
triangulares6 :: [Integer]
triangulares6 = [1] ++ zipWith (+) triangulares [2..] 

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Definir la función
--    nDivisores :: Integer -> Integer
-- tal que (nDivisores n) es el número de los divisores de n. Por
-- ejemplo, 
--    nDivisores 28                 ==  6
--    nDivisores (product [1..200]) == 139503973313460993785856000000
-- ---------------------------------------------------------------------

-- carmengar
nDivisores :: Integer -> Integer
nDivisores n = product [x+1 | (_,x) <- factorizacionAbreviada n]

factorizacionAbreviada :: Integer -> [(Integer,Integer)]
factorizacionAbreviada n = 
    [(x, contar x (primeFactors n)) | x <- nub (primeFactors n)] 

contar :: Eq a => a -> [a] -> Integer
contar _ [] = 0
contar x (y:ys) | x == y    = 1 + contar x ys
                | otherwise = contar x ys

-- Comentario: La definición anterior se puede mejorar usando la función
-- group de la librería Data.List descrita en http://bit.ly/1mhRPOW

-- carmengar juanarcon
nDivisores2 :: Integer -> Integer
nDivisores2 = 
    product . map (\x -> genericLength x +1) . group . primeFactors

-- fracruzam manpende erisancha marvilmor abrdelrod alvalvdom1 alebergon 
nDivisores3 :: Integer -> Integer
nDivisores3 n = 
    genericLength (filter (\x -> n `rem` x == 0) [2..n `div`2]) + 2

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

-- carmengar manvermor ivaruicam josllagam fracruzam blaruiher rubvilval
-- alvalvdom1 manpende erisancha marvilmor abrdelrod alebergon juanarcon
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

-- alvalvdom1 juanarcon javoliher
enteros :: [Int]
enteros = 0 : mezcla [-1,-2..] [1..]

mezcla :: [Int] -> [Int] -> [Int]
mezcla (x:xs) (y:ys) = x:y:mezcla xs ys

-- carmengar ivaruicam
enteros2 :: [Int]
enteros2 = [f x | x <- [0..]]
    where f n | even n    = div n 2
              | otherwise = -1 - div n 2

-- manvermor blaruiher josllagam manpende erisancha
enteros3 :: [Int]
enteros3 = 0 : aux 1
    where aux n = -n : n : aux (n+1)

-- fracruzam
enteros4 :: [Int]
enteros4 = 0 : (concat $ [[-x,x] | x <- [1..]])

-- Comentario: La definición anterior se puede simplificar.

-- rubvilval
enteros5 :: [Int]
enteros5 = intercala [0..] [-1,-2..]
    where intercala []     ys     = ys
          intercala xs     []     = xs
          intercala (x:xs) (y:ys) = x:y:intercala xs ys

-- Comentario: La definición anterior se puede simplificar.

-- abrdelrod alebergon
enteros6 :: [Int]
enteros6 = 0 : concat [[-n,n] | n <- [1..]]

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Definir, por iteración, la constante
--    enteros' :: [Int]
-- tal que enteros' es la lista de los enteros con la ordenación
-- anterior. Por ejemplo,
--    take 10 enteros  ==  [0,-1,1,-2,2,-3,3,-4,4,-5]
-- ---------------------------------------------------------------------

-- alvalvdom1 erisancha juanarcon
enteros' :: [Int]
enteros' = 0 : mezcla (iterate (1-) (-1)) (iterate (1+) 1)

-- carmengar ivaruicam fracruzam josllagam rubvilval abrdelrod alebergon
-- javoliher
enteros'2 :: [Int]
enteros'2 = iterate f 0
    where f n | n >= 0    = -(n+1)
              | otherwise = -n

-- ---------------------------------------------------------------------
-- Ejercicio 4.3. Definir, por selección con takeWhile, la función
--    posicion :: Int -> Int
-- tal que (posicion x) es la posición del entero x en la ordenación
-- anterior. Por ejemplo,
--    posicion 2  ==  4
-- ---------------------------------------------------------------------

-- alvalvdom1 carmengar manvermor ivaruicam fracruzam blaruiher
-- josllagam rubvilval manpende erisancha abrdelrod alebergon juanarcon
 posicion :: Int -> Int
posicion x = length $ takeWhile (/= x) enteros

-- ---------------------------------------------------------------------
-- Ejercicio 4.4. Definir, por recursión, la función
--    posicionR :: Int -> Int
-- tal que (posicionR x) es la posición del entero x en la ordenación
-- anterior. Por ejemplo,
--    posicionR 2  ==  4
-- ---------------------------------------------------------------------

-- carmengar manvermor ivaruicam alvalvdom1 fracruzam blaruiher
-- josllagam manpende erisancha abrdelrod alebergon juanarcon
posicionR :: Int -> Int
posicionR x = aux x enteros
    where aux x (y:ys) | x == y    = 0
                       | otherwise = 1 + aux x ys

-- rubvilval
posicionR2 :: Int -> Int
posicionR2 0           = 0
posicionR2 x | x>0     = 2+(posicionR2 (x-1))
             | x==(-1) = 1
             | x<0     = 2+(posicionR2 (x+1))

-- Comentario: La definición anterior se puede simplificar.

-- ---------------------------------------------------------------------
-- Ejercicio 4.5. Definir, por comprensión, la función
--    posicionC :: Int -> Int
-- tal que (posicionC x) es la posición del entero x en la ordenación
-- anterior. Por ejemplo,
--    posicionC 2  ==  4
-- ---------------------------------------------------------------------

-- alvalvdom1 blaruiher manpende abrdelrod
posicionC :: Int -> Int
posicionC x = head [y | y <- [0..], enteros !! y == x]

-- Comentario: La definición anterior se puede mejorar eliminando el uso
-- de (!!).

-- carmengar erisancha
posicionC2 :: Int -> Int
posicionC2 x = length [1 | _ <- takeWhile (/=x) enteros] 
 
-- manvermor ivaruicam fracruzam josllagam rubvilval alebergon juanarcon 
posicionC3 :: Int -> Int
posicionC3 x = head [v | (u,v) <- zip enteros [0..], u == x]

-- ---------------------------------------------------------------------
-- Ejercicio 4.6. Definir, sin búsqueda, la función
--    posicion2 :: Int -> Int
-- tal que (posicion2 x) es la posición del entero x en la ordenación
-- anterior. Por ejemplo,
--    posicion2 2  ==  4
-- ---------------------------------------------------------------------

-- Definición directa
-- manvermor carmengar ivaruicam alvalvdom1 fracruzam blaruiher
-- josllagam rubvilval manpende erisancha abrdelrod alebergon juanarcon
-- javoliher 
posicion2 :: Int -> Int
posicion2 x | x >= 0    = 2 * x
            | otherwise = -2 * x - 1

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

-- carmengar ivaruicam 
eslabones :: Int -> Int -> Int -> [Int]
eslabones i d n = aux i n
    where aux i n = mod i n : aux (i+d) n

-- carmengar fracruzam josllagam alebergon
eslabones2 :: Int -> Int -> Int -> [Int]
eslabones2 i d n = iterate ((`mod` n) . (+d)) i

-- fracruzam josllagam erisancha alvalvdom1 alebergon juanarcon 
eslabones3 :: Int -> Int -> Int -> [Int]
eslabones3 i d n = [(i + x*d) `mod` n | x <- [0..]]

-- rubvilval manpende
eslabones4 :: Int -> Int -> Int -> [Int]
eslabones4 i d n = i:esl i d n
    where esl i d n | i+d<n     = i+d:esl (i+d) d n
                    | otherwise = i+d-n:esl (i+d-n) d n

-- 2ª definición (con iterate):
eslabones2' :: Int -> Int -> Int -> [Int]
eslabones2' i d n = iterate (f d n) i
    where f d n x | (x+d)<n   = x+d
                  | otherwise = x+d-n


-- abrdelrod javoliher
eslabones5 :: Int -> Int -> Int -> [Int]
eslabones5 i d n = i : eslabones5 (rem (i+d) n) d n

-- 2ª definición (con iterate):
eslabones5' :: Int -> Int -> Int -> [Int]
eslabones5' i d n = iterate (\k -> rem (k+d) n) i

-- ---------------------------------------------------------------------
-- Ejercicio 5.2. Definir la función
--    numeroVueltas :: Int -> Int -> Int -> Int 
-- tal que (numeroVueltas i d n) es el número de vueltas que pasarán 
-- hasta que la cadena se rompa en una bicicleta de tipo (i,d,n). Por 
-- ejemplo,
--    numeroVueltas 2 7 25  ==  14
-- ---------------------------------------------------------------------

-- carmengar fracruzam josllagam rubvilval manpende erisancha abrdelrod
-- alvalvdom1 alebergon juanarcon 
numeroVueltas :: Int -> Int -> Int -> Int
numeroVueltas i d n = length $ takeWhile (/=0) (eslabones i d n)

-- ivaruicam
numeroVueltas2 :: Int -> Int -> Int -> Int
numeroVueltas2 i d n = vueltas (eslabones2 i d n)
    where vueltas (x:xs) | x /= 0 = 1 + vueltas xs
                         | otherwise = 0

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

-- fracruzam josllagam rubvilval erisancha carmengar abrdelrod alebergon 
-- blaruiher juanarcon 

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

-- fracruzam
sucGolomb :: [Int]
sucGolomb = 1:2:2:3:3: aux 4
    where aux :: Int -> [Int]
          aux n = replicate (golomb n) n ++ aux (n+1)

-- Comentario: La definición anterior se puede simplificar usando la
-- una definición subSucGolomb del apartado 3 que no use sucGolomb. 

-- erisancha carmengar alebergon juanarcon 
sucGolomb2 :: [Int]
sucGolomb2 = 1 : 2 : 2 : subSucGolomb 3

-- abrdelrod
sucGolomb3 :: [Int]
sucGolomb3 = 1 : 2 : 2 : concat [replicate (sucGolomb !! (x-1)) x | x <- [3..]]

-- Pregunta: Alguna de las tres habrá que definirla sin usar las demás,
-- ¿no? 
-- Respuesta: Hay casos en los que no, Por ejemplo, par en función de
-- impar e impar en función de par.

-- ---------------------------------------------------------------------
-- Ejercicio 6.3. Definir la función
--    subSucGolomb :: Int -> [Int]
-- tal que (subSucGolomb x) es la lista de los términos de la sucesión
-- de Golomb a partir de la primera ocurrencia de x. Por ejemplo,
--    take 10 (subSucGolomb 4)  ==  [4,4,4,5,5,5,6,6,6,6]
-- Indicación: Se puede usar la función golomb del apartado 1.
-- ---------------------------------------------------------------------

-- fracruzam josllagam rubvilval blaruiher
subSucGolomb :: Int -> [Int]
subSucGolomb x = filter (>= x) sucGolomb

-- erisancha carmengar abrdelrod alebergon juanarcon 
subSucGolomb2 :: Int -> [Int]
subSucGolomb2 n = replicate (golomb n) n ++ subSucGolomb (n + 1)

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

-- manvermor alvalvdom1 javoliher blaruiher manpende 
comprimida :: Eq a => [a] -> [(Int,a)]
comprimida xs = [(x,y) | (x,y) <- zip (map length ys) (map head ys)]
    where ys = group xs

-- Comentario: La definición anterior se puede mejorar.

-- fracruzam ivaruicam carmengar erisancha juanarcon 
comprimida2 :: Eq a => [a] -> [(Int,a)]
comprimida2 = map (\x -> (length x, head x)) . group

-- rubvilval josllagam
comprimida3 :: Eq a => [a] -> [(Int,a)]
comprimida3 xs = zip (map length (group xs)) (concat (map nub (group xs)))

-- Comentario: La definición anterior se puede mejorar.

-- abrdelrod alebergon
comprimida4 :: Eq a => [a] -> [(Int,a)]
comprimida4 [] = []
comprimida4 (x:xs) = 
    (length ys, x) : comprimida (drop (length ys) (x:xs))
    where ys = takeWhile (==x) (x:xs)

-- ---------------------------------------------------------------------
-- Ejercicio 7.2. Definir la función
--    expandida :: [(Int,a)] -> [a]
-- tal que (expandida ps) es la lista expandida correspondiente a ps (es
-- decir, es la lista xs tal que la comprimida de xs es ps). Por
-- ejemplo, 
--    expandida [(2,1),(3,7),(2,5),(4,7)]  ==  [1,1,7,7,7,5,5,7,7,7,7]
-- ---------------------------------------------------------------------

-- manvermor alvalvdom1 fracruzam ivaruicam rubvilval josllagam
-- carmengar erisancha abrdelrod alebergon manpende juanarcon 
expandida :: [(Int,a)] -> [a]
expandida ps =  concat [replicate k x | (k,x) <- ps]

-- carmengar javoliher
expandida2 :: [(Int,a)] -> [a]
expandida2 [] = []
expandida2 ((k,x):ps) = replicate k x ++ expandida ps

-- ---------------------------------------------------------------------
-- Ejercicio 7.3. Comprobar con QuickCheck que dada una lista de enteros,
-- si se la comprime y después se expande se obtiene la lista inicial. 
-- ---------------------------------------------------------------------

-- manvermor alvalvdom1 fracruzam ivaruicam rubvilval josllagam
-- erisancha abrdelrod javoliher blaruiher alebergon manpende juanarcon 

-- La propiedad es
prop_expandida_comprimida :: [Int] -> Bool 
prop_expandida_comprimida xs = (expandida . comprimida) xs == xs

-- La comprobación es
--    *Main> quickCheck prop_expandida_comprimida
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 7.4. Comprobar con QuickCheck que dada una lista de pares
-- de enteros, si se la expande y después se comprime se obtiene la
-- lista inicial.  
-- ---------------------------------------------------------------------

-- manvermor alvalvdom1 fracruzam ivaruicam rubvilval josllagam
-- carmengar erisancha abrdelrod javoliher blaruiher alebergon
-- manpende juanarcon 

-- La propiedad es
prop_comprimida_expandida :: [(Int,Int)] -> Bool 
prop_comprimida_expandida xs = (comprimida . expandida) xs == xs

-- ---------------------------------------------------------------------
-- Ejercicio 7.5. Definir la función
--    listaAcadena :: [(Int,Char)] -> String
-- tal que (listaAcadena xs) es la cadena correspondiente a la lista de
-- pares de xs. Por ejemplo,
--    ghci> listaAcadena [(12,'B'),(1,'N'),(12,'B'),(3,'N'),(19,'B')]
--    "12B1N12B3N19B"
-- ---------------------------------------------------------------------

-- manvermor rubvilval
listaAcadena :: [(Int,Char)] -> String
listaAcadena xs = concat [ reverse (y : (reverse (show x))) | (x,y) <- xs]

-- Comentario: La definición anterior se puede mejorar.

-- fracruzam ivaruicam alvalvdom1 carmengar erisancha javoliher alebergon
-- juanarcon 
listaAcadena2 :: [(Int,Char)] -> String
listaAcadena2 []         = []
listaAcadena2 ((n,c):xs) = show n ++ [c] ++ listaAcadena2 xs 

-- abrdelrod manpende
listaAcadena3 :: [(Int,Char)] -> String
listaAcadena3 xs = concat [show a ++ [b] | (a,b) <- xs]

-- ---------------------------------------------------------------------
-- Ejercicio 7.6. Definir la función
--    cadenaComprimida :: String -> String
-- tal que (cadenaComprimida cs) es la cadena obtenida comprimiendo por
-- longitud la cadena cs. Por ejemplo,
--    ghci> cadenaComprimida "BBBBBBBBBBBBNBBBBBBBBBBBBNNNBBBBBBBBBBNNN"
--    "12B1N12B3N10B3N"
-- ---------------------------------------------------------------------

-- manvermor ivaruicam manpende
cadenaComprimida :: String -> String
cadenaComprimida cs = (listaAcadena . comprimida) cs

-- Comentario: La definición anterior se puede simplificar.

-- fracruzam
cadenaComprimida2 :: String -> String 
cadenaComprimida2 = 
    foldr1 (++) . map (\x -> show (length x)++[head x]) . group

-- rubvilval alvalvdom1 carmengar erisancha abrdelrod javoliher
-- alebergon juanarcon 
cadenaComprimida3 :: String -> String
cadenaComprimida3 = listaAcadena.comprimida

-- ---------------------------------------------------------------------
-- Ejercicio 7.7. Definir la función 
--    cadenaAlista :: String -> [(Int,Char)]
-- tal que (cadenaAlista cs) es la lista de pares correspondientes a la
-- cadena cs. Por ejemplo,
--    ghci> cadenaAlista "12B1N12B3N10B3N"
--    [(12,'B'),(1,'N'),(12,'B'),(3,'N'),(10,'B'),(3,'N')]
-- ---------------------------------------------------------------------

-- manvermor rubvilval
cadenaAlista :: String -> [(Int,Char)]
cadenaAlista cs = [ (read x, y) | (x,y) <- zip (numeros cs) (letras cs)]

-- Comentario: La definición anterior se puede mejorar usando la función
-- span de la librería Data.List descrita en http://bit.ly/1OqHqXP

numeros :: [Char] -> [[Char]]
numeros [] = []
numeros xs = 
    takeWhile (isDigit) xs : numeros (tail (dropWhile (isDigit) xs))

letras :: [Char] -> [Char]
letras [] =  []
letras (x:xs) | isAlpha x = x : letras xs
              | otherwise = letras xs

-- fracruzam
cadenaAlista2 :: String -> [(Int,Char)]
cadenaAlista2 [] = []
cadenaAlista2 ps = 
    (read (fst par), head sndpar): cadenaAlista2 (tail sndpar)
    where par    = span isDigit ps
          sndpar = snd par

-- Comentario: La definición anterior se puede simplificar.

-- ivaruicam carmengar erisancha alvalvdom1 blaruiher alebergon
-- manpende juanarcon 
cadenaAlista3 :: String -> [(Int,Char)]
cadenaAlista3 [] = []
cadenaAlista3 xs = (read a, head b) : cadenaAlista (tail b) 
    where (a,b) = span isDigit xs

-- abrdelrod
cadenaAlista4 :: String -> [(Int,Char)]
cadenaAlista4 [] = []
cadenaAlista4 xs = (read ts, head ys) : cadenaAlista4 (tail ys)
    where p  = not.isAlpha
          ts = takeWhile p xs 
          ys = drop (length ts) xs

-- javoliher
cadenaAlista5 :: String -> [(Int,Char)]
cadenaAlista5 cs = zip (numeros5 cs) (letras5 cs)

numeros5 [] = []
numeros5 cs = [read (takeWhile (isNumber) cs) :: Int] ++ 
              numeros5 (tail (dropWhile (isNumber) cs))

letras5 [] = []
letras5 cs = filter (not.isNumber) cs

-- ---------------------------------------------------------------------
-- Ejercicio 7.8. Definir la función
--    cadenaExpandida :: String -> String
-- tal que (cadenaExpandida cs) es la cadena expandida correspondiente a
-- cs (es decir, es la cadena xs que al comprimirse por longitud da cs). 
-- Por ejemplo, 
--    ghci> cadenaExpandida "12B1N12B3N10B3N"
--    "BBBBBBBBBBBBNBBBBBBBBBBBBNNNBBBBBBBBBBNNN"
-- ---------------------------------------------------------------------

-- manvermor ivaruicam blaruiher manpende 
cadenaExpandida :: String -> String
cadenaExpandida cs = (expandida . cadenaAlista) cs

-- Comentario: La definición anterior se puede simplificar.

-- fracruzam
cadenaExpandida2 :: String -> String
cadenaExpandida2 [] = []
cadenaExpandida2 ps = replicate (read $ fst par) (head sndpar) ++
                      cadenaExpandida2 (tail sndpar)
    where par    = span isDigit ps
          sndpar = snd par

-- Comentario: La definición anterior se puede simplificar.

-- rubvilval alvalvdom1 carmengar erisancha abrdelrod javoliher
-- alebergon juanarcon 
cadenaExpandida3 :: String -> String
cadenaExpandida3 = expandida . cadenaAlista

-- abrdelrod
cadenaExpandida4 :: String -> String
cadenaExpandida4 xs = concat [replicate x y | (x,y) <- cadenaAlista xs]

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

-- alvalvdom1 fracruzam rubvilval carmengar erisancha ivaruicam
-- javoliher alebergon manvermor manpende juanarcon 
contadora :: Eq a => [a] -> [Int]
contadora xs = map length (group xs)

-- abrdelrod
contadora2 :: Eq a => [a] -> [Int]
contadora2 [] = []
contadora2 xs = [x] ++ contadora2 (drop x xs)
    where x = length (takeWhile (== head xs) xs)

-- ---------------------------------------------------------------------
-- Ejercicio 8.2. Definir la función
--    contada :: [Int] -> [a] -> [a]
-- tal que (contada ns xs) es la sucesión formada por los símbolos de xs
-- cuya contadora es ns. Por ejemplo,
--    contada [1,2,3,3] "ab"                ==  "abbaaabbb"
--    contada [1,2,3,3] "abc"               ==  "abbcccaaa"
--    contada [1,2,2,1,1,2,1,1,2,1,1] "12"  ==  "122112122121121"
-- ---------------------------------------------------------------------

-- fracruzam
contada :: [Int] -> [a] -> [a]
contada ns cs = aux ns cs cs
    where aux :: [Int] -> [a] -> [a] -> [a]
          aux (n:ns) (c:cs) xs = replicate n c ++ aux ns cs xs
          aux ns     []     xs = aux ns xs xs
          aux []     _      _  = []

-- rubvilval erisancha manpende
contada2 :: [Int] -> [a] -> [a]
contada2 ns xs = concat [replicate a b | (a,b) <- pares ns xs]

pares :: [Int] -> [a] -> [(Int,a)]
pares ns xs | length ns <= length xs = zip ns xs
            | otherwise = (zip ns xs)++(pares (drop (length xs) ns) xs)

-- abrdelrod javoliher
contada3 :: [Int] -> [a] -> [a]
contada3 ns xs = 
    concat [replicate a b | (a,b) <- zip ns (concat $ repeat xs)]

-- alebergon ivaruicam blaruiher juanarcon 
contada4 :: [Int] -> [a] -> [a]
contada4 [] xs = []
contada4 (n:ns) (x:xs) = replicate n x ++ contada ns (xs++[x])

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

-- fracruzam erisancha blaruiher alebergon juanarcon 
autocontadora :: [Int]
autocontadora = 1:2:xs
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

-- fracruzam abrdelrod ivaruicam blaruiher manpende juanarcon 
siguienteF :: [Integer] -> [Integer]
siguienteF xs = [lxs .. lxs + genericLength xs]
    where lxs = last xs + 1

-- rubvilval erisancha alebergon
siguienteF2 :: [Integer] -> [Integer]
siguienteF2 xs = dropWhile (/=xs) trianguloFloyd !! 1

-- alvalvdom1
siguienteF3 :: [Integer] -> [Integer]
siguienteF3 xs = take (length xs + 1) (iterate (+1) (last xs + 1))

-- manvermor
siguienteF4 :: [Integer] -> [Integer]
siguienteF4 xs = take (length xs +1) [(last xs +1)..]

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

-- fracruzam manpende
trianguloFloyd :: [[Integer]]
trianguloFloyd = [1] : map siguienteF trianguloFloyd

-- rubvilval erisancha alebergon
trianguloFloyd2 :: [[Integer]]
trianguloFloyd2 = [filaTrianguloFloyd x | x <- [1..]]

-- alvalvdom1 abrdelrod ivaruicam manvermor juanarcon 
trianguloFloyd3 :: [[Integer]]
trianguloFloyd3 = iterate siguienteF [1]

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

-- fracruzam alvalvdom1 ivaruicam blaruiher alebergon manvermor manpende
-- juanarcon 
filaTrianguloFloyd :: Integer -> [Integer]
filaTrianguloFloyd n = trianguloFloyd !! fromIntegral (n-1)

-- rubvilval erisancha abrdelrod
filaTrianguloFloyd2 :: Integer -> [Integer]
filaTrianguloFloyd2 n = [sum[1..n-1]+1..sum[1..n]]

-- Comentario: La definición anterior se puede mejorar.

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

-- fracruzam rubvilval alebergon manvermor manpende 
sumaFilaTrianguloFloyd :: Integer -> Integer
sumaFilaTrianguloFloyd n = sum (filaTrianguloFloyd n)

-- alvalvdom1 erisancha abrdelrod ivaruicam blaruiher juanarcon 
sumaFilaTrianguloFloyd2 :: Integer -> Integer
sumaFilaTrianguloFloyd2 = sum . filaTrianguloFloyd

-- ---------------------------------------------------------------------
-- Ejercicio 9.5. A partir de los valores de (sumaFilaTrianguloFloyd n)
-- para n entre 1 y 5, conjeturar una fórmula para calcular
-- (sumaFilaTrianguloFloyd n). 
-- ---------------------------------------------------------------------

-- abrdelrod

-- Veamos que sumaFilaTrianguloFloyd n = n(n^2+1)/2:
-- 
-- Para ello, hemos de darnos cuenta de que el primer término de la fila
-- es precisamente la suma de los n-1 primeros números más 1. Así, el
-- problema se reduce a hallar la suma de los n primeros términos  (ya que 
-- la fila i tiene i términos) de una progresión aritmética de diferencia 
-- 1 y de la que conocemos la expresión (en función de n) de su primer término.
-- 
-- Sabemos que la suma de los n primeros números naturales viene
-- determinada por la expresión n(n+1)/2, por lo que tenemos que el primer
-- término de la progresión es n(n-1)/2 + 1, es decir a1 = (n^2-n+2)/2.
-- 
-- Luego la sucesión nos queda a(n) = a(1) + (n-1)d = (n^2-n+2)/2 + (n-1) =
-- (n^2+n)/2. La suma de los n primeros términos de una progresión
-- aritmética es (a(1) + a(n))/2 * n, por lo que finalmente tenemos:
-- S(n) = ((n^2-n+2)/2 + (n^2+n)/2) / 2 * n = ((2n^2+2)/2)/2 * n =
-- (n^2+1)/2 * n = n(n^2+1)/2, como queríamos.

-- ---------------------------------------------------------------------
-- Ejercicio 6. Comprobar con QuickCheck la conjetura obtenida en el
-- ejercicio anterior.
-- ---------------------------------------------------------------------

-- abrdelrod

-- La conjetura es
prop_sumaFilaTrianguloFloyd :: Integer -> Property
prop_sumaFilaTrianguloFloyd n = 
    n>=0 ==> sumaFilaTrianguloFloyd n == div (n^3+n) 2
  
-- La comprobación es
--    *Main> quickCheck prop_sumaFilaTrianguloFloyd
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

-- fracruzam rubvilval alvalvdom1 erisancha abrdelrod ivaruicam
-- blaruiher alebergon manvermor manpende juanarcon 

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

-- fracruzam rubvilval alvalvdom1 erisancha abrdelrod ivaruicam
-- blaruiher alebergon manvermor manpende juanarcon 

-- La propiedad es
prop_hipotenusaFloyd :: Int -> Bool
prop_hipotenusaFloyd n = 
    take n hipotenusaFloyd == take n triangulares

-- La comprobación es
--    *Main> prop_hipotenusaFloyd 1000
--    True
--    (0.06 secs, 89,568,624 bytes)
