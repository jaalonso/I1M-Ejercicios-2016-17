-- I1M 2016-17: Rel_8.hs (9 de noviembre de 2016)
-- Funciones de orden superior y definiciones por plegados.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- Esta relación tiene contiene ejercicios con funciones de orden
-- superior y definiciones por plegado correspondientes al tema 7 
-- http://www.cs.us.es/~jalonso/cursos/i1m-16/temas/tema-7.html

-- ---------------------------------------------------------------------
-- Importación de librerías auxiliares                                --
-- ---------------------------------------------------------------------

import Test.QuickCheck
import Data.List

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    segmentos :: (a -> Bool) -> [a] -> [a]
-- tal que (segmentos p xs) es la lista de los segmentos de xs cuyos
-- elementos verifican la propiedad p. Por ejemplo,
--    segmentos even [1,2,0,4,9,6,4,5,7,2]  ==  [[2,0,4],[6,4],[2]]
--    segmentos odd  [1,2,0,4,9,6,4,5,7,2]  ==  [[1],[9],[5,7]]
-- ---------------------------------------------------------------------

-- enrnarbej margarflo5
segmentos :: (a -> Bool) -> [a] -> [[a]]
segmentos _ [] = []
segmentos p (x:xs)
  | p x = takeWhile p (x:xs) : segmentos p (dropWhile p xs)
  | otherwise = segmentos p (dropWhile (\a -> not (p a)) xs)

-- antmorper3 cargonler ignareeva belbenzam pabrabmon joscasgom1
segmentos2 :: (a -> Bool) -> [a] -> [[a]]
segmentos2 p [] = []
segmentos2 p (x:xs)
  | p x = takeWhile p (x:xs) : segmentos2 p (dropWhile p xs)
  | otherwise = segmentos2 p xs

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Definir, por comprensión, la función
--    relacionadosC :: (a -> a -> Bool) -> [a] -> Bool
-- tal que (relacionadosC r xs) se verifica si para todo par (x,y) de
-- elementos consecutivos de xs se cumple la relación r. Por ejemplo,
--    relacionadosC (<) [2,3,7,9]                ==  True
--    relacionadosC (<) [2,3,1,9]                ==  False
-- ---------------------------------------------------------------------

-- enrnarbej antmorper3 cargonler ignareeva belbenzam margarflo5
-- pabrabmon joscasgom1
relacionadosC :: (a -> a -> Bool) -> [a] -> Bool
relacionadosC r xs = and [r x y | (x,y) <- zip xs (tail xs)]

-- juaorture
relacionadosC1 :: (a -> a -> Bool) -> [a] -> Bool
relacionadosC1 r (x:xs) = and (map (\(x,y) -> r x y) (zip (x:xs) xs))

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Definir, por recursión, la función
--    relacionadosR :: (a -> a -> Bool) -> [a] -> Bool
-- tal que (relacionadosR r xs) se verifica si para todo par (x,y) de
-- elementos consecutivos de xs se cumple la relación r. Por ejemplo,
--    relacionadosR (<) [2,3,7,9]                ==  True
--    relacionadosR (<) [2,3,1,9]                ==  False
-- ---------------------------------------------------------------------

-- enrnarbej belbenzam margarflo5
relacionadosR :: (a -> a -> Bool) -> [a] -> Bool
relacionadosR _ [x]      = True
relacionadosR r (x:y:xs) =  r x y && relacionadosR r (y:xs)

-- Comentario: Falta el caso de lista vacía.

-- juaorture  
relacionadosR2 :: (a -> a -> Bool) -> [a] -> Bool
relacionadosR2 _ []       = False
relacionadosR2 r [x]      = True
relacionadosR2 r (x:y:xs) | r x y     = relacionadosR2 r (y:xs)
                          | otherwise = False

-- Comentario: La definición relacionadosR2 se puede simplificar.

-- antmorper3 cargonler pabrabmon joscasgom1
relacionadosR3 :: (a -> a -> Bool) -> [a] -> Bool
relacionadosR3 r [x] = True
relacionadosR3 r (x:xs) | r x (head xs) = relacionadosR3 r xs
                        | otherwise = False

-- Comentario: La definición anterior se puede simplificar y completar
-- con el caso de la lista vacía.

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Definir la función
--    agrupa :: Eq a => [[a]] -> [[a]]
-- tal que (agrupa xss) es la lista de las listas obtenidas agrupando
-- los primeros elementos, los segundos, ... Por ejemplo, 
--    agrupa [[1..6],[7..9],[10..20]]  ==  [[1,7,10],[2,8,11],[3,9,12]]
--    agrupa []                        ==  []
-- ---------------------------------------------------------------------

-- enrnarbej antmorper3 margarflo5 pabrabmon joscasgom1
agrupa :: Eq a => [[a]] -> [[a]]
agrupa xss
  | all ( /= []) xss && xss /= [] = map (head) xss : agrupa (map tail xss)
  | otherwise                     = []

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Comprobar con QuickChek que la longitud de todos los
-- elementos de (agrupa xs) es igual a la longitud de xs.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_agrupa :: [[Int]] -> Bool
prop_agrupa xss = undefined

-- La comprobación es

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. Definir, por recursión, la función 
--    concatR :: [[a]] -> [a]
-- tal que (concatR xss) es la concatenación de las listas de xss. Por
-- ejemplo, 
--    concatR [[1,3],[2,4,6],[1,9]]  ==  [1,3,2,4,6,1,9]
-- ---------------------------------------------------------------------

-- enrnarbej cargonler belbenzam pabrabmon joscasgom1
concatR :: [[a]] -> [a]
concatR []       = []
concatR (xs:xss) = xs ++ concatR xss

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Definir, usando foldr, la función 
--    concatP :: [[a]] -> [a]
-- tal que (concatP xss) es la concatenación de las listas de xss. Por
-- ejemplo, 
--    concatP [[1,3],[2,4,6],[1,9]]  ==  [1,3,2,4,6,1,9]
-- ---------------------------------------------------------------------

--enrnarbej cargonler belbenzam pabrabmon joscasgom1
concatP :: [[a]] -> [a]
concatP xss = foldr (\xs ys -> xs ++ ys) [] xss

-- Comentario: La definición anterior se puede simplificar.

-- ---------------------------------------------------------------------
-- Ejercicio 4.3. Comprobar con QuickCheck que la funciones concatR,
-- concatP y concat son equivalentes.
-- ---------------------------------------------------------------------

-- enrnarbej cargonler belbenzam pabrabmon joscasgom1
-- La propiedad es
prop_concat :: [[Int]] -> Bool
prop_concat xss = concatR xss == concatP xss

-- Comentario: Falta la equivalencia con concat.

-- La comprobación es
-- Prelude> quickCheck prop_concat
-- +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 4.4. Comprobar con QuickCheck que la longitud de 
-- (concatP xss) es la suma de las longitudes de los elementos de xss.
-- ---------------------------------------------------------------------

-- enrnarbej cargonler pabrabmon
-- La propiedad es
prop_longConcat :: [[Int]] -> Bool
prop_longConcat xss = length (concatP xss) == sum (map length xss)

-- La comprobación es
-- Prelude> quickCheck prop_longConcat
-- +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 5.1. Definir, por comprensión, la función
--    filtraAplicaC :: (a -> b) -> (a -> Bool) -> [a] -> [b]
-- tal que (filtraAplicaC f p xs) es la lista obtenida aplicándole a los
-- elementos de xs que cumplen el predicado p la función f. Por ejemplo,
--    filtraAplicaC (4+) (<3) [1..7]  =>  [5,6]
-- ---------------------------------------------------------------------

-- enrnarbej pabrabmon joscasgom1
filtraAplicaC :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filtraAplicaC f p xs = [f x | x <- xs, p x] 

-- ---------------------------------------------------------------------
-- Ejercicio 5.2. Definir, usando map y filter, la función
--    filtraAplicaMF :: (a -> b) -> (a -> Bool) -> [a] -> [b]
-- tal que (filtraAplicaMF f p xs) es la lista obtenida aplicándole a los
-- elementos de xs que cumplen el predicado p la función f. Por ejemplo,
--    filtraAplicaMF (4+) (<3) [1..7]  =>  [5,6]
-- ---------------------------------------------------------------------

-- enrnarbej pabrabmon joscasgom1
filtraAplicaMF :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filtraAplicaMF f p xs = map f (filter p xs)

-- ---------------------------------------------------------------------
-- Ejercicio 5.3. Definir, por recursión, la función
--    filtraAplicaR :: (a -> b) -> (a -> Bool) -> [a] -> [b]
-- tal que (filtraAplicaR f p xs) es la lista obtenida aplicándole a los
-- elementos de xs que cumplen el predicado p la función f. Por ejemplo,
--    filtraAplicaR (4+) (<3) [1..7]  =>  [5,6]
-- ---------------------------------------------------------------------

-- enrnarbej pabrabmon joscasgom1
filtraAplicaR :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filtraAplicaR _ _ [] = []
filtraAplicaR f p (x:xs) | p x = f x : filtraAplicaR f p xs
                         | otherwise = filtraAplicaR f p xs

-- ---------------------------------------------------------------------
-- Ejercicio 5.4. Definir, por plegado, la función
--    filtraAplicaP :: (a -> b) -> (a -> Bool) -> [a] -> [b]
-- tal que (filtraAplicaP f p xs) es la lista obtenida aplicándole a los
-- elementos de xs que cumplen el predicado p la función f. Por ejemplo,
--    filtraAplicaP (4+) (<3) [1..7]  =>  [5,6]
-- ---------------------------------------------------------------------

-- enrnarbej pabrabmon
filtraAplicaP :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filtraAplicaP f p xs = foldr ( \y ys -> (f y):ys) [] (filter p xs)

-- ---------------------------------------------------------------------
-- Ejercicio 6.1. Definir, mediante recursión, la función
--    maximumR :: Ord a => [a] -> a
-- tal que (maximumR xs) es el máximo de la lista xs. Por ejemplo,
--    maximumR [3,7,2,5]                  ==  7
--    maximumR ["todo","es","falso"]      ==  "todo"
--    maximumR ["menos","alguna","cosa"]  ==  "menos"
-- 
-- Nota: La función maximumR es equivalente a la predefinida maximum.
-- ---------------------------------------------------------------------

-- enrnarbej
maximumR :: Ord a => [a] -> a
maximumR (x:xs) = maxAux xs x
  where
    maxAux [] x = x
    maxAux (x:xs) n | x > n     = maxAux xs x
                    | otherwise = maxAux xs n

-- enrnarbej pabrabmon
maximumR2 :: Ord a => [a] -> a
maximumR2 [x]      = x
maximumR2 (x:y:xs) = maximumR2 (maxi x y : xs)

maxi :: Ord a => a -> a -> a
maxi a b | a > b     = a
         | otherwise = b

-- pabrabmon
maximumR3 :: Ord a => [a] -> a
maximumR3 [x] = x
maximumR3 (x:xs) | x > head xs = maximumR3 (x:tail xs)
                 | otherwise   = maximumR3 xs

-- joscasgom1
maximumR4 :: Ord a => [a] -> a
maximumR4 [a] = a
maximumR4 (x:y:xs) | x < y     = maximumR4 (y:xs)
                   | otherwise = maximumR4 (x:xs)

-- ---------------------------------------------------------------------
-- Ejercicio 6.2. La función de plegado foldr1 está definida por 
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


-- enrnarbej pabrabmon
maximumP :: Ord a => [a] -> a
maximumP xs = foldr1 maxi xs

