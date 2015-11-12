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
import Data.List

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Definir, por recursión, la función
--    takeWhileR :: (a -> Bool) -> [a] -> [a]
-- tal que (takeWhileR p xs) es la lista de los elemento de xs hasta el
-- primero que no cumple la propiedad p. Por ejemplo,
--    takeWhileR (<7) [2,3,9,4,5]  ==  [2,3]
-- ---------------------------------------------------------------------

-- guache alvalvdom1 irecasmat juanarcon juamorrom1 manpende blaruiher
-- josllagam fracruzam manvermor silgongal albtorval rubvilval anagusil
-- alebergon erisancha carmengar
takeWhileR :: (a -> Bool) -> [a] -> [a]
takeWhileR _ [] = []
takeWhileR p (x:xs)| p x       = x : takeWhileR p xs
                   | otherwise = []

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Comprobar con QuickCheck que, para cualquier lista de
-- enteros xs, se verifica que (takeWhileR even xs) es igual que 
-- (takeWhile even xs)
-- ---------------------------------------------------------------------

-- guache alvalvdom1 irecasmat juanarcon juamorrom1 manpende blaruiher
-- josllagam fracruzam manvermor silgongal albtorval rubvilval anaagusil
-- alebergon erisancha carmengar

-- La propiedad es
prop_takeWhileR :: [Int] -> Bool
prop_takeWhileR xs = takeWhileR even xs == takeWhile even xs

-- La comprobación es
--    *Main> quickCheck prop_takeWhileR
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 1.3. Comprobar con QuickCheck que, para cualquier lista de
-- enteros xs, se verifica que todos los elementos de (takeWhileR even xs)
-- son pares.
-- ---------------------------------------------------------------------

-- guache alvalvdom1 irecasmat juanarcon juamorrom1 manpende josllagam
-- blaruiher fracruzam manvermor silgongal albtorval rubvilval anaagusil
-- alebergon erisancha carmengar

-- La propiedad es
prop_takeWhileTodos :: [Int] -> Bool
prop_takeWhileTodos xs = all even (takeWhileR even xs)

-- La comprobación es
--    *Main> quickCheck prop_takeWhileTodos
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Definir por recursión la función
--    dropWhileR :: (a -> Bool) -> [a] -> [a]
-- tal que (dropWhileR p xs) es la lista de eliminando los elemento de xs
-- hasta el primero que cumple la propiedad p. Por ejemplo,
--    dropWhileR (<7) [2,3,9,4,5]  ==  [9,4,5]
-- ---------------------------------------------------------------------

-- guache alvalvdom1 irecasmat juanarcon juamorrom1 manpende josllagam
-- blaruiher fracruzam manvermor silgongal albtorval rubvilval anaagusil
-- alebergon erisancha carmengar

dropWhileR :: (a -> Bool) -> [a] -> [a]
dropWhileR _ [] = []
dropWhileR p (x:xs) | p x       = dropWhileR p xs
                    | otherwise = (x:xs)

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Comprobar con QuickCheck que, para cualquier lista de
-- enteros xs, se verifica que la concatenación de (takeWhileR even xs)
-- y (dropWhileR even xs) es igual a xs.
-- ---------------------------------------------------------------------

-- guache alvalvdom1 irecasmat juanarcon juamorrom1 manpende josllagam
-- albtorval anaagusil  

-- La propiedad es
prop_takeDrop :: [Int] -> Bool
prop_takeDrop xs = 
    concat [(takeWhileR even xs),(dropWhileR even xs)] == xs

-- Comentario: La definición anterior se puede simplificar.

-- blaruiher rubvilval

-- La propiedad es
prop_takeDrop2 :: [Int] -> Bool
prop_takeDrop2 xs =
    (takeWhileR even xs) ++ (dropWhileR even xs)== xs

-- Comentario: La definición anterior se puede simplificar.

-- fracruzam manvermor silgongal alebergon erisancha carmengar
prop_takeDrop3 :: [Int] -> Bool 
prop_takeDrop3 xs = takeWhileR even xs ++ dropWhileR even xs == xs 

-- La comprobación es
--    *Main> quickCheck prop_takeDrop 
--    +++ OK, passed 100 tests.

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

-- guache juanarcon juamorrom1 manpende fracruzam albtorval anaagusil
-- alebergon erisancha carmengar
divideMediaC :: [Double] -> ([Double],[Double])
divideMediaC xs = ([x | x <- xs, x < w], [x | x <- xs, x > w])
    where w = sum xs / fromIntegral (length xs)

-- josllagam manvermor silgongal rubvilval
divideMediaC2 :: [Double] -> ([Double],[Double])
divideMediaC2 xs = (ys,zs)
    where ys = [x | x <- xs, x < media xs]
          zs = [x | x <- xs, x > media xs]

-- Comentario: La definición anterior se puede mejorar.

media :: [Double] -> Double 
media xs = (sum xs) / (genericLength xs)

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

-- guache juanarcon juamorrom1 manpende fracruzam josllagam manvermor
-- silgongal albtorval rubvilval anaagusil alebergon erisancha carmengar

divideMediaF :: [Double] -> ([Double],[Double])
divideMediaF xs = (filter (<w) xs,filter (>w) xs)
    where w = sum xs / fromIntegral (length xs)

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

-- alebergon
divideMediaR :: [Double] -> ([Double],[Double])
divideMediaR xs = (menorMayorR (< media xs) xs , menorMayorR (> media xs) xs)
    where media :: [Double] -> Double
          media xs = sum xs / genericLength xs

menorMayorR :: (Double -> Bool) -> [Double] -> [Double]
menorMayorR _ [] = []
menorMayorR f (x:xs) | f x = x : menorMayorR f xs
                     | otherwise = menorMayorR f xs

-- carmengar
-- En realidad, las funciones menoresR y mayoresR son la misma función,
-- la función filter definida por recursión. Por tanto, sólo sería 
-- necesario definir una de las dos.

divideMediaR2 xs = (filtrarR (<m) xs, filtrarR (>m) xs)
    where m = sum xs / genericLength xs

filtrarR :: (Double -> Bool) -> [Double] -> [Double]
filtrarR _ [] = []
filtrarR p (x:xs) | p x = x : filtrarR p xs
                  | otherwise = filtrarR p xs

-- erisancha
divideMediaR3 :: [Double] -> ([Double],[Double])
divideMediaR3 xs = (left m xs, right m xs)
    where m = sum xs / genericLength xs

left m xs | null xs = []
          | head xs < m = [head xs] ++ left m (tail xs)
          | otherwise = left m (tail xs)

right m xs | null xs = []
           | head xs > m = [head xs] ++ right m (tail xs)
           | otherwise = right m (tail xs)

-- ---------------------------------------------------------------------
-- Ejercicio 3.4. Comprobar con QuickCheck que las tres definiciones
-- anteriores divideMediaF, divideMediaC y divideMediaR son
-- equivalentes.
-- ---------------------------------------------------------------------

-- La propiedad es

prop_divideMedia :: [Double] -> Bool
prop_divideMedia xs = a == b && a == c && b == c
    where a = divideMediaC xs
          b = divideMediaF xs
          c = divideMediaR xs

-- albtorval rubvilval 
prop_divideMedia2 :: [Double] -> Bool
prop_divideMedia2 xs = and [a==b,b==c,c==a]
    where a = divideMediaF xs
          b = divideMediaC xs
          c = divideMediaR xs

-- alebergon carmengar
prop_divideMedia3 :: [Double] -> Bool
prop_divideMedia3 xs = a == b && b == c
    where a = divideMediaF xs
          b = divideMediaC xs
          c = divideMediaR xs

-- ---------------------------------------------------------------------
-- Ejercicio 3.5. Comprobar con QuickCheck que si (ys,zs) es el par
-- obtenido aplicándole la función divideMediaF a xs, entonces la suma
-- de las longitudes de ys y zs es menor o igual que la longitud de xs.
-- ---------------------------------------------------------------------

-- alvalvdom1

-- La propiedad es
prop_longitudDivideMedia :: [Double] -> Bool
prop_longitudDivideMedia xs = 
    sum [length (fst d), length (snd d)] <= length xs
    where d = divideMediaF xs

-- Comentario: La definición anterior se puede simplificar.

-- manvermor josllagam silgongal rubvilval anaagusil
prop_longitudDivideMedia2 :: [Double] -> Bool
prop_longitudDivideMedia2 xs = 
    length (fst d) + length (snd d) <= length xs
    where d = divideMediaF xs 

-- Comentario: La definición anterior se puede simplificar.

-- fracruzam alebergon carmengar
prop_longitudDivideMedia3 :: [Double] -> Bool
prop_longitudDivideMedia3 xs = 
    longitudPar (divideMediaF xs) <= length xs 
    where longitudPar (a,b) = length a + length b

-- La comprobación es 
--    ghci> quickCheck prop_longitudDivideMedia
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 3.6. Comprobar con QuickCheck que si (ys,zs) es el par
-- obtenido aplicándole la función divideMediaF a xs, entonces todos los
-- elementos de ys son menores que todos los elementos de zs.
-- ---------------------------------------------------------------------

-- alvalvdom1 manpende

-- La propiedad es
prop_divideMediaMenores :: [Double] -> Bool
prop_divideMediaMenores xs = filter (< minimum (snd d)) (fst d) == fst d
    where d = divideMediaF xs

-- Comentario: La definición anterior se puede simplificar.

-- La comprobación es 
--    ghci> quickCheck prop_divideMediaMenores
--    +++ OK, passed 100 tests.

-- fracruzam manvermor josllagam silgongal rubvilval anaagusil alebergon
prop_divideMediaMenores2 :: [Double] -> Bool
prop_divideMediaMenores2 xs = 
    and [y < z | y <- fst (divideMediaF xs) , 
                 z <- snd (divideMediaF xs)]

-- Comentario: La definición anterior se puede mejorar.

-- carmengar
prop_divideMediaMenores3 :: [Double] -> Bool
prop_divideMediaMenores3 xs = all (<m) (fst d)
    where d = divideMediaF xs
          m = minimum (snd d)

-- Comentario: La definición anterior se puede simplificar.

-- ---------------------------------------------------------------------
-- Ejercicio 3.7. Comprobar con QuickCheck que si (ys,zs) es el par
-- obtenido aplicándole la función divideMediaF a xs, entonces la
-- media de xs no pertenece a ys ni a zs.
-- 
-- Nota: Usar la función notElem tal que (notElem x ys) se verifica si y
-- no pertenece a ys.
-- ---------------------------------------------------------------------

-- alvalvdom1 manvermor josllagam silgongal rubvilval

-- La propiedad es
prop_divideMediaSinMedia :: [Double] -> Bool
prop_divideMediaSinMedia xs = notElem m (fst d) && notElem m (snd d)
    where d = divideMediaF xs
          m = (sum xs) / fromIntegral (length xs)

-- Comentario: La definición anterior se puede simplificar.


-- La comprobación es 
--    ghci> quickCheck prop_divideMediaSinMedia
--    +++ OK, passed 100 tests.

-- fracruzam alebergon
prop_divideMediaSinMedia2 :: [Double] -> Bool
prop_divideMediaSinMedia2 xs = notElemPar m (divideMediaF xs)
   where notElemPar x (a,b) = notElem x a && notElem x b
         m = sum xs / fromIntegral (length xs)

-- Comentario: La definición anterior se puede simplificar.

-- carmengar
prop_divideMediaSinMedia3 :: [Double] -> Bool
prop_divideMediaSinMedia3 xs = notElem m ys
    where m  = sum xs / genericLength xs
          ys = fst d ++ snd d
          d  = divideMediaF xs

-- Comentario: La definición anterior se puede simplificar.

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función
--    segmentos :: (a -> Bool) -> [a] -> [a]
-- tal que (segmentos p xs) es la lista de los segmentos de xs cuyos
-- elementos verifican la propiedad p. Por ejemplo,
--    segmentos even [1,2,0,4,9,6,4,5,7,2]  ==  [[2,0,4],[6,4],[2]]
--    segmentos odd  [1,2,0,4,9,6,4,5,7,2]  ==  [[1],[9],[5,7]]
-- ---------------------------------------------------------------------

-- manpende rubvilval josllagam carmengar alebergon
segmentos :: (a -> Bool) -> [a] -> [[a]]
segmentos p [] = []
segmentos p (x:xs) 
    | p x       = takeWhileR p (x:xs) : segmentos p (dropWhileR p xs)
    | otherwise = segmentos p xs

-- ---------------------------------------------------------------------
-- Ejercicio 5.1. Definir, por comprensión, la función
--    relacionadosC :: (a -> a -> Bool) -> [a] -> Bool
-- tal que (relacionadosC r xs) se verifica si para todo par (x,y) de
-- elementos consecutivos de xs se cumple la relación r. Por ejemplo,
--    relacionadosC (<) [2,3,7,9]                ==  True
--    relacionadosC (<) [2,3,1,9]                ==  False
-- ---------------------------------------------------------------------

-- manpende
relacionadosC :: (a -> a -> Bool) -> [a] -> Bool
relacionadosC r xs = 
    foldr (+) 0 [1^n | n <- [0..(length xs - 2)], 
                       r (xs!!n) (xs!!(n+1))] 
    == length xs - 1

-- Comentario: La definición anterior se puede mejorar.

-- fracruzam 
relacionadosC2 :: (a -> a -> Bool) -> [a] -> Bool
relacionadosC2 r xs = all (\(x,y) -> r x y) (zip xs (tail xs))

-- manvermor rubvilval josllagam carmengar alebergon
relacionadosC3 :: (a -> a -> Bool) -> [a] -> Bool
relacionadosC3 r xs = and [x `r` y | (x,y) <- zip xs (tail xs)]

-- ---------------------------------------------------------------------
-- Ejercicio 5.1. Definir, por comprensión, la función
--    relacionadosR :: (a -> a -> Bool) -> [a] -> Bool
-- tal que (relacionadosR r xs) se verifica si para todo par (x,y) de
-- elementos consecutivos de xs se cumple la relación r. Por ejemplo,
--    relacionadosR (<) [2,3,7,9]                ==  True
--    relacionadosR (<) [2,3,1,9]                ==  False
-- ---------------------------------------------------------------------

-- manpende rubvilval
relacionadosR :: (a -> a -> Bool) -> [a] -> Bool
relacionadosR r []  = True
relacionadosR r [x] = True
relacionadosR r (x:y:xs) | r x y     = relacionadosR r (y:xs)
                         | otherwise = False

-- Comentario: La definición anterior se puede simplificar.

-- fracruzam josllagam
relacionadosR2 :: (a -> a -> Bool) -> [a] -> Bool
relacionadosR2 _ [] = True
relacionadosR2 _ [a] = True
relacionadosR2 r (x:y:xs) = r x y && relacionadosR r (y:xs)

-- Comentario: La definición anterior se puede simplificar.

-- carmengar alebergon
relacionadosR3 :: (a -> a -> Bool) -> [a] -> Bool
relacionadosR3 r (x:y:zs) = r x y && relacionadosR r (y:zs)
relacionadosR3 r xs = True

-- ---------------------------------------------------------------------
-- Ejercicio 6.1. Definir la función
--    agrupa :: Eq a => [[a]] -> [[a]]
-- tal que (agrupa xss) es la lista de las listas obtenidas agrupando
-- los primeros elementos, los segundos, ... Por ejemplo, 
--    agrupa [[1..6],[7..9],[10..20]]  ==  [[1,7,10],[2,8,11],[3,9,12]]
--    agrupa []                        ==  []
-- ---------------------------------------------------------------------

-- manpende
agrupa :: Eq a => [[a]] -> [[a]]
agrupa [] = []
agrupa xss = [lista n xss | n <- [0..(minimum (longitudes xss) - 1)]]
    where lista n [] = []
          lista n (xs:xss) = (xs!!n) : lista n xss
          longitudes xss = [length xs | xs <- xss]

-- Comentario: La definición anterior se puede mejorar.

-- fracruzam rubvilval alebergon
agrupa2 :: Eq a => [[a]] -> [[a]]
agrupa2 [] = []
agrupa2 xss | notElem [] xss = map (!! 0) xss : agrupa (map tail xss)
            | otherwise      = []

-- Comentario: La definición anterior se puede simplificar.

-- carmengar
agrupa3 [] = []
agrupa3 xss =
    [[xs !! n | xs <- xss] | n <- [0..minimum [length xs-1 | xs <- xss]]]


-- ---------------------------------------------------------------------
-- Ejercicio 6.2. Comprobar con QuickChek que la longitud de todos los
-- elementos de (agrupa xs) es igual a la longitud de xs.
-- ---------------------------------------------------------------------

-- manpende alebergon

-- La propiedad es
prop_agrupa :: [[Int]] -> Bool
prop_agrupa xss = null [xs | xs <- agrupa xss, length xs /= length xss]

-- La comprobación es
-- *Main> quickCheck prop_agrupa
-- +++ OK, passed 100 tests.

-- rubvilval carmengar
prop_agrupa2 :: [[Int]] -> Bool
prop_agrupa2 xss = all (== length xss) [length xs|xs<-(agrupa xss)] 

-- La comprobación es
-- *Main> quickCheck prop_agrupa
-- +++ OK, passed 100 tests

-- ---------------------------------------------------------------------
-- Ejercicio 7.1. Definir por recursión la función
--    superparR :: Int -> Bool
-- tal que (superparR n) se verifica si n es un número par tal que todos
-- sus dígitos son pares. Por ejemplo,
--    superparR 426  ==  True
--    superparR 456  ==  False
-- ---------------------------------------------------------------------

-- blaruiher
superparR :: Int -> Bool
superparR 0 = True
superparR n | even n    = superparR (n `div`10)
            | otherwise = False

-- Comentario: La definición anterior se puede simplificar.
  
-- fracruzam rubvilval alebergon carmengar
superparR2 :: Int -> Bool
superparR2 0 = True
superparR2 n = even n && superparR2 (div n 10)

-- ---------------------------------------------------------------------
-- Ejercicio 7.2. Definir por comprensión la función
--    superparC :: Int -> Bool
-- tal que (superparC n) se verifica si n es un número par tal que todos
-- sus dígitos son pares. Por ejemplo,
--    superparC 426  ==  True
--    superparC 456  ==  False
-- ---------------------------------------------------------------------

-- blaruiher manpende alebergon
superparC :: Int -> Bool
superparC n = [x | x <- m, even x] == m
    where m = [read [y] | y <- show n]

-- fracruzam manvermor carmengar
superparC2 :: Int -> Bool
superparC2 n = and [even (read [n]) | n <- show n]

-- rubvilval
superparC3 :: Int -> Bool
superparC3 n = 
    and [even (div n (10^x)) | x <- [0..((length(show n))-1)]]

-- ---------------------------------------------------------------------
-- Ejercicio 7.3. Definir, por recursión sobre los dígitos, la función
--    superparRD :: Int -> Bool
-- tal que (superparRD n) se verifica si n es un número par tal que todos
-- sus dígitos son pares. Por ejemplo,
--    superparRD 426  ==  True
--    superparRD 456  ==  False
-- ---------------------------------------------------------------------

-- fracruzam alebergon
superparRD :: Int -> Bool
superparRD 0 = True
superparRD n = even (last (digitos n)) && superparRD (div n 10)
         where  digitos :: Int -> [Int]
                digitos 0 = []
                digitos n =digitos (div n 10) ++ [mod n 10]

-- Comentario: La definición anterior se puede mejorar.

-- carmengar
digitos :: Int -> [Int] 
digitos n = [read [c] | c <- show n]

superparRD2 :: Int -> Bool
superparRD2 n = aux (digitos n)
    where aux [] = True
          aux (x:xs) | odd x = False
                     | otherwise = aux xs

-- Comentario: La definición anterior se puede simplificar.

-- ---------------------------------------------------------------------
-- Ejercicio 7.4. Definir, usando all, la función
--    superparA :: Int -> Bool
-- tal que (superparA n) se verifica si n es un número par tal que todos
-- sus dígitos son pares. Por ejemplo,
--    superparA 426  ==  True
--    superparA 456  ==  False
-- ---------------------------------------------------------------------

-- blaruiher manpende
superparA :: Int -> Bool
superparA n = all even [x | x <- [read [m] | m <- show n]]

-- Comentario: La definición anterior se puede simplificar.

-- fracruzam manvermor rubvilval alebergon carmengar
superparA2 :: Int -> Bool
superparA2 n = all even [read [n] | n <- show n]

-- ---------------------------------------------------------------------
-- Ejercicio 7.5. Definir, usando filter, la función
--    superparF :: Int -> Bool
-- tal que (superparF n) se verifica si n es un número par tal que todos
-- sus dígitos son pares. Por ejemplo,
--    superparF 426  ==  True
--    superparF 456  ==  False
-- ---------------------------------------------------------------------

-- blaruiher manpende manvermor rubvilval
superparF :: Int -> Bool
superparF n = filter (even) m == m
    where m = [read [x] | x <- show n]

-- Comentario: La definición anterior se puede simplificar.

-- fracruzam alebergon carmengar
-- No son necesarios los paréntesis en even
superparF2 :: Int -> Bool
superparF2 n = filter even m == m
    where m = [read [x] | x <- show n]

-- ---------------------------------------------------------------------
-- Ejercicio 8.1. Definir, por recursión, la función 
--    concatR :: [[a]] -> [a]
-- tal que (concatR xss) es la concatenación de las listas de xss. Por
-- ejemplo, 
--    concatR [[1,3],[2,4,6],[1,9]]  ==  [1,3,2,4,6,1,9]
-- ---------------------------------------------------------------------

-- fracruzam
concatR :: [[a]] -> [a]
concatR [] = []
concatR ([]:xss) = concatR xss
concatR ((x:xs):xss) = x:concatR (xs:xss)

-- Comentario: La definición anterior se puede simplificar.

-- fracruzam manvermor rubvilval alebergon carmengar
concatR2 :: [[a]] -> [a]
concatR2 [] = []
concatR2 (xs:xss) = xs ++ concatR xss

-- ---------------------------------------------------------------------
-- Ejercicio 8.2. Definir, usando foldr, la función 
--    concatP :: [[a]] -> [a]
-- tal que (concatP xss) es la concatenación de las listas de xss. Por
-- ejemplo, 
--    concatP [[1,3],[2,4,6],[1,9]]  ==  [1,3,2,4,6,1,9]
-- ---------------------------------------------------------------------

-- fracruzam rubvilval
concatP :: [[a]] -> [a]
concatP xss = foldr (++) [] xss

-- Comentario: La definición anterior se puede simplificar.

-- juanarcon alebergon carmengar
concatP2 :: [[a]] -> [a]
concatP2  = foldr (++) []

-- ---------------------------------------------------------------------
-- Ejercicio 8.3. Comprobar con QuickCheck que la funciones concatR,
-- concatP y concat son equivalentes.
-- ---------------------------------------------------------------------

-- fracruzam manvermor rubvilval alebergon carmengar

-- La propiedad es
prop_concat :: [[Int]] -> Bool
prop_concat xss = cR == concatP xss && cR == concat xss
    where cR = concatR xss

-- La comprobación es
--   quickCheck prop_concat
--   +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 8.4. Comprobar con QuickCheck que la longitud de 
-- (concatP xss) es la suma de las longitudes de los elementos de xss.
-- ---------------------------------------------------------------------

-- fracruzam manvermor rubvilval alebergon carmengar

-- La propiedad es
prop_longConcat :: [[Int]] -> Bool
prop_longConcat xss = length (concatP xss) == sum (map length xss)

-- La comprobación es
--    quickCheck prop_longConcat
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 9.1. Definir, por comprensión, la función
--    filtraAplicaC :: (a -> b) -> (a -> Bool) -> [a] -> [b]
-- tal que (filtraAplicaC f p xs) es la lista obtenida aplicándole a los
-- elementos de xs que cumplen el predicado p la función f. Por ejemplo,
--    filtraAplicaC (4+) (<3) [1..7]  =>  [5,6]
-- ---------------------------------------------------------------------

-- blaruiher manpende fracruzam manvermor rubvilval alebergon carmengar
filtraAplicaC :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filtraAplicaC f p xs = [f x | x <- xs, p x]

-- ---------------------------------------------------------------------
-- Ejercicio 9.2. Definir, usando map y filter, la función
--    filtraAplicaMF :: (a -> b) -> (a -> Bool) -> [a] -> [b]
-- tal que (filtraAplicaMF f p xs) es la lista obtenida aplicándole a los
-- elementos de xs que cumplen el predicado p la función f. Por ejemplo,
--    filtraAplicaMF (4+) (<3) [1..7]  =>  [5,6]
-- ---------------------------------------------------------------------

-- blaruiher manpende fracruzam manvermor rubvilval carruirui3 alebergon
-- carmengar
filtraAplicaMF :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filtraAplicaMF f p xs = map f (filter p xs)

-- ---------------------------------------------------------------------
-- Ejercicio 9.3. Definir, por recursión, la función
--    filtraAplicaR :: (a -> b) -> (a -> Bool) -> [a] -> [b]
-- tal que (filtraAplicaR f p xs) es la lista obtenida aplicándole a los
-- elementos de xs que cumplen el predicado p la función f. Por ejemplo,
--    filtraAplicaR (4+) (<3) [1..7]  =>  [5,6]
-- ---------------------------------------------------------------------

-- manpende manvermor
filtraAplicaR :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filtraAplicaR f p [] = []
filtraAplicaR f p (x:xs) | p x       = f x : filtraAplicaR f p xs
                         | otherwise = filtraAplicaR f p xs

-- Comentario: La definición anterior se puede simplificar.

-- fracruzam alebergon carmengar
filtraAplicaR2 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filtraAplicaR2 _ _ [] = []
filtraAplicaR2 f p (x:xs) | p x       = f x : filtraAplicaR f p xs
                          | otherwise = filtraAplicaR f p xs

-- ---------------------------------------------------------------------
-- Ejercicio 9.4. Definir, por plegado, la función
--    filtraAplicaP :: (a -> b) -> (a -> Bool) -> [a] -> [b]
-- tal que (filtraAplicaP f p xs) es la lista obtenida aplicándole a los
-- elementos de xs que cumplen el predicado p la función f. Por ejemplo,
--    filtraAplicaP (4+) (<3) [1..7]  =>  [5,6]
-- ---------------------------------------------------------------------

-- carruirui3 alebergon carmengar
filtraAplicaP :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filtraAplicaP f p xs = foldr fn [] xs
    where fn y ys | p y       = f y : ys
                  | otherwise = ys

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

-- fracruzam rubvilval
maximumR :: Ord a => [a] -> a
maximumR (x:xs) | any (>x) xs = maximumR xs
                | otherwise   = x

-- manvermor alebergon
maximumR3 :: Ord a => [a] -> a
maximumR3 [x] = x
maximumR3 (x:y:xs) | x < y     = maximumR3 (y:xs)
                   | otherwise = x

-- carmengar
maximumR4 :: Ord a => [a] -> a
maximumR4 (x:xs) | all (<=x) xs = x
                 | otherwise    = maximumR xs     

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

-- fracruzam rubvilval alebergon carmengar
maximumP :: Ord a => [a] -> a
maximumP = foldr1 max

-- ---------------------------------------------------------------------
-- Ejercicio 10.3. Comprobar con QuickCheck que, para cualquier lista no
-- vacía xs, (maximumP xs) es un elemento de xs que es mayor o igual que
-- todos los elementos de xs. 
-- ---------------------------------------------------------------------

-- fracruzam rubvilval alebergon carmengar
-- La propiedad es
prop_maximumP :: [Int] -> Property
prop_maximumP xs = xs /= [] ==> elem mP xs && all (<= mP) xs
    where mP = maximumP xs 

-- La comprobación es
--    quickCheck prop_maximumP
--    +++ OK, passed 100 tests.

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

-- fracruzam rubvilval alebergon carmengar
minimumP :: Ord a => [a] -> a
minimumP = foldr1 min

-- ---------------------------------------------------------------------
-- Ejercicio 11.2. Comprobar con QuickCheck que, para cualquier lista no
-- vacía xs, (minimumP xs) es un elemento de xs que es menor o igual que
-- todos los elementos de xs. 
-- ---------------------------------------------------------------------

-- fracruzam rubvilval alebergon carmengar

-- La propiedad es
prop_minimumP :: [Int] -> Property
prop_minimumP xs = xs /= [] ==> elem mP xs && all (>= mP) xs
    where mP = minimumP xs

-- La comprobación es
--    quickCheck prop_minimumP
--    +++ OK, passed 100 tests.
