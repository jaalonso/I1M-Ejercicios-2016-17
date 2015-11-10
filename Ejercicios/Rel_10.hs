-- I1M 2015-16: Rel_10.hs (7 de noviembre de 2015)
-- Funciones de orden superior y definiciones por plegados.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- Esta relación tiene contiene ejercicios con funciones de orden
-- superior y definiciones por plegado correspondientes al tema 7 
-- http://www.cs.us.es/~jalonso/cursos/i1m-15/temas/tema-7.html

-- ---------------------------------------------------------------------
-- Importación de librerías auxiliares                                --
-- ---------------------------------------------------------------------

import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Definir, por recursión, la función
--    takeWhileR :: (a -> Bool) -> [a] -> [a]
-- tal que (takeWhileR p xs) es la lista de los elemento de xs hasta el
-- primero que no cumple la propiedad p. Por ejemplo,
--    takeWhileR (<7) [2,3,9,4,5]  ==  [2,3]
-- ---------------------------------------------------------------------

takeWhileR :: (a -> Bool) -> [a] -> [a]
takeWhileR = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Comprobar con QuickCheck que, para cualquier lista de
-- enteros xs, se verifica que (takeWhileR even xs) es igual que 
-- (takeWhile even xs)
-- ---------------------------------------------------------------------

-- La propiedad es
prop_takeWhileR :: [Int] -> Bool
prop_takeWhileR xs = undefined

-- La comprobación es

-- ---------------------------------------------------------------------
-- Ejercicio 1.3. Comprobar con QuickCheck que, para cualquier lista de
-- enteros xs, se verifica que todos los elementos de (takeWhileR even xs)
-- son pares.
-- ---------------------------------------------------------------------

prop_takeWhileTodos :: [Int] -> Bool
prop_takeWhileTodos xs = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Definir por recursión la función
--    dropWhileR :: (a -> Bool) -> [a] -> [a]
-- tal que (dropWhileR p xs) es la lista de eliminando los elemento de xs
-- hasta el primero que cumple la propiedad p. Por ejemplo,
--    dropWhileR (<7) [2,3,9,4,5]  ==  [9,4,5]
-- ---------------------------------------------------------------------

dropWhileR :: (a -> Bool) -> [a] -> [a]
dropWhileR = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Comprobar con QuickCheck que, para cualquier lista de
-- enteros xs, se verifica que la concatenación de (takeWhileR even xs)
-- y (dropWhileR even xs) es igual a xs.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_takeDrop :: [Int] -> Bool
prop_takeDrop xs = undefined

-- La comprobación es

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Definir, por comprensión, la función
--    divideMediaC :: [Double] -> ([Double],[Double])
-- tal que (divideMediaC xs) es el par (ys,zs), donde ys contiene los
-- elementos de xs estrictamente menores que la media, mientras que zs
-- contiene los elementos de xs estrictamente mayores que la media. Por
-- ejemplo,  
--    divideMediaC [6,7,2,8,6,3,4] ==  ([2.0,3.0,4.0],[6.0,7.0,8.0,6.0])
--    divideMediaC [1,2,3]         ==  ([1.0],[3.0])
-- ---------------------------------------------------------------------

divideMediaC :: [Double] -> ([Double],[Double])
divideMediaC xs = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Definir, con filter, la función
--    divideMediaF :: [Double] -> ([Double],[Double])
-- tal que (divideMediaF xs) es el par (ys,zs), donde ys contiene los
-- elementos de xs estrictamente menores que la media, mientras que zs
-- contiene los elementos de xs estrictamente mayores que la media. Por
-- ejemplo,  
--    divideMediaF [6,7,2,8,6,3,4] ==  ([2.0,3.0,4.0],[6.0,7.0,8.0,6.0])
--    divideMediaF [1,2,3]         ==  ([1.0],[3.0])
-- ---------------------------------------------------------------------

divideMediaF :: [Double] -> ([Double],[Double])
divideMediaF xs = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 3.3. Definir, por recursión, la función
--    divideMediaR :: [Double] -> ([Double],[Double])
-- tal que (divideMediaR xs) es el par (ys,zs), donde ys contiene los
-- elementos de xs estrictamente menores que la media, mientras que zs
-- contiene los elementos de xs estrictamente mayores que la media. Por
-- ejemplo,  
--    divideMediaR [6,7,2,8,6,3,4] ==  ([2.0,3.0,4.0],[6.0,7.0,8.0,6.0])
--    divideMediaR [1,2,3]         ==  ([1.0],[3.0])
-- ---------------------------------------------------------------------

divideMediaR :: [Double] -> ([Double],[Double])
divideMediaR xs = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 3.4. Comprobar con QuickCheck que las tres definiciones
-- anteriores divideMediaF, divideMediaC y divideMediaR son
-- equivalentes.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_divideMedia :: [Double] -> Bool
prop_divideMedia xs = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 3.5. Comprobar con QuickCheck que si (ys,zs) es el par
-- obtenido aplicándole la función divideMediaF a xs, entonces la suma
-- de las longitudes de ys y zs es menor o igual que la longitud de xs.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_longitudDivideMedia :: [Double] -> Bool
prop_longitudDivideMedia xs = undefined

-- La comprobación es

-- ---------------------------------------------------------------------
-- Ejercicio 3.6. Comprobar con QuickCheck que si (ys,zs) es el par
-- obtenido aplicándole la función divideMediaF a xs, entonces todos los
-- elementos de ys son menores que todos los elementos de zs.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_divideMediaMenores :: [Double] -> Bool
prop_divideMediaMenores xs = undefined

-- La comprobación es

-- ---------------------------------------------------------------------
-- Ejercicio 3.7. Comprobar con QuickCheck que si (ys,zs) es el par
-- obtenido aplicándole la función divideMediaF a xs, entonces la
-- media de xs no pertenece a ys ni a zs.
-- 
-- Nota: Usar la función notElem tal que (notElem x ys) se verifica si y
-- no pertenece a ys.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_divideMediaSinMedia :: [Double] -> Bool
prop_divideMediaSinMedia xs = undefined

-- La comprobación es

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función
--    segmentos :: (a -> Bool) -> [a] -> [a]
-- tal que (segmentos p xs) es la lista de los segmentos de xs cuyos
-- elementos verifican la propiedad p. Por ejemplo,
--    segmentos even [1,2,0,4,9,6,4,5,7,2]  ==  [[2,0,4],[6,4],[2]]
--    segmentos odd  [1,2,0,4,9,6,4,5,7,2]  ==  [[1],[9],[5,7]]
-- ---------------------------------------------------------------------

segmentos :: (a -> Bool) -> [a] -> [[a]]
segmentos = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 5.1. Definir, por comprensión, la función
--    relacionadosC :: (a -> a -> Bool) -> [a] -> Bool
-- tal que (relacionadosC r xs) se verifica si para todo par (x,y) de
-- elementos consecutivos de xs se cumple la relación r. Por ejemplo,
--    relacionadosC (<) [2,3,7,9]                ==  True
--    relacionadosC (<) [2,3,1,9]                ==  False
-- ---------------------------------------------------------------------

relacionadosC :: (a -> a -> Bool) -> [a] -> Bool
relacionadosC = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 5.1. Definir, por comprensión, la función
--    relacionadosR :: (a -> a -> Bool) -> [a] -> Bool
-- tal que (relacionadosR r xs) se verifica si para todo par (x,y) de
-- elementos consecutivos de xs se cumple la relación r. Por ejemplo,
--    relacionadosR (<) [2,3,7,9]                ==  True
--    relacionadosR (<) [2,3,1,9]                ==  False
-- ---------------------------------------------------------------------

relacionadosR :: (a -> a -> Bool) -> [a] -> Bool
relacionadosR = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 6.1. Definir la función
--    agrupa :: Eq a => [[a]] -> [[a]]
-- tal que (agrupa xss) es la lista de las listas obtenidas agrupando
-- los primeros elementos, los segundos, ... Por ejemplo, 
--    agrupa [[1..6],[7..9],[10..20]]  ==  [[1,7,10],[2,8,11],[3,9,12]]
--    agrupa []                        ==  []
-- ---------------------------------------------------------------------

agrupa :: Eq a => [[a]] -> [[a]]
agrupa = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 6.2. Comprobar con QuickChek que la longitud de todos los
-- elementos de (agrupa xs) es igual a la longitud de xs.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_agrupa :: [[Int]] -> Bool
prop_agrupa xss = undefined

-- La comprobación es

-- ---------------------------------------------------------------------
-- Ejercicio 7.1. Definir por recursión la función
--    superparR :: Int -> Bool
-- tal que (superparR n) se verifica si n es un número par tal que todos
-- sus dígitos son pares. Por ejemplo,
--    superparR 426  ==  True
--    superparR 456  ==  False
-- ---------------------------------------------------------------------

superparR :: Int -> Bool
superparR = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 7.2. Definir por comprensión la función
--    superparC :: Int -> Bool
-- tal que (superparC n) se verifica si n es un número par tal que todos
-- sus dígitos son pares. Por ejemplo,
--    superparC 426  ==  True
--    superparC 456  ==  False
-- ---------------------------------------------------------------------

superparC :: Int -> Bool
superparC n = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 7.3. Definir, por recursión sobre los dígitos, la función
--    superparRD :: Int -> Bool
-- tal que (superparRD n) se verifica si n es un número par tal que todos
-- sus dígitos son pares. Por ejemplo,
--    superparRD 426  ==  True
--    superparRD 456  ==  False
-- ---------------------------------------------------------------------

superparRD :: Int -> Bool
superparRD n = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 7.4. Definir, usando all, la función
--    superparA :: Int -> Bool
-- tal que (superparA n) se verifica si n es un número par tal que todos
-- sus dígitos son pares. Por ejemplo,
--    superparA 426  ==  True
--    superparA 456  ==  False
-- ---------------------------------------------------------------------

superparA :: Int -> Bool
superparA n = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 7.5. Definir, usando filter, la función
--    superparF :: Int -> Bool
-- tal que (superparF n) se verifica si n es un número par tal que todos
-- sus dígitos son pares. Por ejemplo,
--    superparF 426  ==  True
--    superparF 456  ==  False
-- ---------------------------------------------------------------------

superparF :: Int -> Bool
superparF n = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 8.1. Definir, por recursión, la función 
--    concatR :: [[a]] -> [a]
-- tal que (concatR xss) es la concatenación de las listas de xss. Por
-- ejemplo, 
--    concatR [[1,3],[2,4,6],[1,9]]  ==  [1,3,2,4,6,1,9]
-- ---------------------------------------------------------------------

concatR :: [[a]] -> [a]
concatR = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 8.2. Definir, usando foldr, la función 
--    concatP :: [[a]] -> [a]
-- tal que (concatP xss) es la concatenación de las listas de xss. Por
-- ejemplo, 
--    concatP [[1,3],[2,4,6],[1,9]]  ==  [1,3,2,4,6,1,9]
-- ---------------------------------------------------------------------

concatP :: [[a]] -> [a]
concatP = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 8.3. Comprobar con QuickCheck que la funciones concatR,
-- concatP y concat son equivalentes.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_concat :: [[Int]] -> Bool
prop_concat xss = undefined

-- La comprobación es

-- ---------------------------------------------------------------------
-- Ejercicio 8.4. Comprobar con QuickCheck que la longitud de 
-- (concatP xss) es la suma de las longitudes de los elementos de xss.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_longConcat :: [[Int]] -> Bool
prop_longConcat xss = undefined

-- La comprobación es

-- ---------------------------------------------------------------------
-- Ejercicio 9.1. Definir, por comprensión, la función
--    filtraAplicaC :: (a -> b) -> (a -> Bool) -> [a] -> [b]
-- tal que (filtraAplicaC f p xs) es la lista obtenida aplicándole a los
-- elementos de xs que cumplen el predicado p la función f. Por ejemplo,
--    filtraAplicaC (4+) (<3) [1..7]  =>  [5,6]
-- ---------------------------------------------------------------------

filtraAplicaC :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filtraAplicaC f p xs = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 9.2. Definir, usando map y filter, la función
--    filtraAplicaMF :: (a -> b) -> (a -> Bool) -> [a] -> [b]
-- tal que (filtraAplicaMF f p xs) es la lista obtenida aplicándole a los
-- elementos de xs que cumplen el predicado p la función f. Por ejemplo,
--    filtraAplicaMF (4+) (<3) [1..7]  =>  [5,6]
-- ---------------------------------------------------------------------

filtraAplicaMF :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filtraAplicaMF f p xs = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 9.3. Definir, por recursión, la función
--    filtraAplicaR :: (a -> b) -> (a -> Bool) -> [a] -> [b]
-- tal que (filtraAplicaR f p xs) es la lista obtenida aplicándole a los
-- elementos de xs que cumplen el predicado p la función f. Por ejemplo,
--    filtraAplicaR (4+) (<3) [1..7]  =>  [5,6]
-- ---------------------------------------------------------------------

filtraAplicaR :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filtraAplicaR = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 9.4. Definir, por plegado, la función
--    filtraAplicaP :: (a -> b) -> (a -> Bool) -> [a] -> [b]
-- tal que (filtraAplicaP f p xs) es la lista obtenida aplicándole a los
-- elementos de xs que cumplen el predicado p la función f. Por ejemplo,
--    filtraAplicaP (4+) (<3) [1..7]  =>  [5,6]
-- ---------------------------------------------------------------------

filtraAplicaP :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filtraAplicaP f p = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 10.1. Definir, mediante recursión, la función
--    maximumR :: Ord a => [a] -> a
-- tal que (maximumR xs) es el máximo de la lista xs. Por ejemplo,
--    maximumR [3,7,2,5]                  ==  7
--    maximumR ["todo","es","falso"]      ==  "todo"
--    maximumR ["menos","alguna","cosa"]  ==  "menos"
-- 
-- Nota: La función maximumR es equivalente a la predefinida maximum.
-- ---------------------------------------------------------------------

maximumR :: Ord a => [a] -> a
maximumR = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 10.2. La función de plegado foldr1 está definida por 
--    foldr1 :: (a -> a -> a) -> [a] -> a
--    foldr1 _ [x]    =  x
--    foldr1 f (x:xs) =  f x (foldr1 f xs)
-- 
-- Definir, mediante plegado con foldr1, la función
--    maximumP :: Ord a => [a] -> a
-- tal que (maximumR xs) es el máximo de la lista xs. Por ejemplo,
--    maximumP [3,7,2,5]                  ==  7
--    maximumP ["todo","es","falso"]      ==  "todo"
--    maximumP ["menos","alguna","cosa"]  ==  "menos"
-- 
-- Nota: La función maximumP es equivalente a la predefinida maximum.
-- ---------------------------------------------------------------------

maximumP :: Ord a => [a] -> a
maximumP = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 10.3. Comprobar con QuickCheck que, para cualquier lista no
-- vacía xs, (maximumP xs) es un elemento de xs que es mayor o igual que
-- todos los elementos de xs. 
-- ---------------------------------------------------------------------

-- La propiedad es
prop_maximumP :: [Int] -> Property
prop_maximumP xs = undefined

-- La comprobación es

-- ---------------------------------------------------------------------
-- Ejercicio 11.1. Definir, mediante plegado con foldr1, la función
--    minimunP :: Ord a => [a] -> a
-- tal que (minimunR xs) es el máximo de la lista xs. Por ejemplo,
--    minimunP [3,7,2,5]                  ==  2
--    minimumP ["todo","es","falso"]      ==  "es"
--    minimumP ["menos","alguna","cosa"]  ==  "alguna"
-- 
-- Nota: La función minimunP es equivalente a la predefinida minimun.
-- ---------------------------------------------------------------------

minimumP :: Ord a => [a] -> a
minimumP = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 11.2. Comprobar con QuickCheck que, para cualquier lista no
-- vacía xs, (minimumP xs) es un elemento de xs que es menor o igual que
-- todos los elementos de xs. 
-- ---------------------------------------------------------------------

-- La propiedad es
prop_minimumP :: [Int] -> Property
prop_minimumP xs = undefined

-- La comprobación es


