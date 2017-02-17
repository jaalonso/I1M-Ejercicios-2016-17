-- I1M 2016-17: Relación 19 (14 de febrero de 2017)
-- Estadística descriptiva.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- El objetivo de esta relación es definir las principales medidas
-- estadísticas de centralización (medias, mediana y modas) y de
-- dispersión (rango, desviación media, varianza y desviación típica)
-- que se estudian en 3º de ESO (como en http://bit.ly/1yXc7mv ).
--
-- En las soluciones de los ejercicios se pueden usar las siguientes
-- funciones de la librería Data.List fromIntegral, genericLength, sort,
-- y group (cuya descripción se puede consultar en el "Manual de
-- funciones básicas de Haskell" http://bit.ly/1PqHagT ).

-- ---------------------------------------------------------------------
-- Librerías auxiliares                                               --
-- ---------------------------------------------------------------------

import Data.List
import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Medidas de centralización                                          --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    media :: Floating a => [a] -> a
-- tal que (media xs) es la media aritmética de los números de la lista
-- xs. Por ejemplo,
--    media [4,8,4,5,9]  ==  6.0
-- ---------------------------------------------------------------------

-- eledejim2 albcercid roscargar margarvil14 paumacpar enrnarbej antmorper3
-- juaorture natmarmar2 monlagare marlobrip joscasgom1 antlopgom2 carmarcar5
-- marmerzaf pabrabmon fatfervaz manruiber marjimcom belbenzam
media :: Floating a => [a] -> a
media xs = sum xs / genericLength xs

-- ---------------------------------------------------------------------
-- Ejercicio 2. La mediana de una lista de valores es el valor de 
-- la lista que ocupa el lugar central de los valores ordenados de menor
-- a mayor. Si el número de datos es impar se toma como valor de la
-- mediana el valor central. Si el número de datos es par se toma como
-- valor de la mediana la media aritmética de los dos valores
-- centrales.
-- 
-- Definir la función 
--    mediana :: (Floating a, Ord a) => [a] -> a
-- tal que (mediana xs) es la mediana de la lista xs. Por ejemplo, 
--    mediana [2,3,6,8,9]    ==  6.0
--    mediana [2,3,4,6,8,9]  ==  5.0
--    mediana [9,6,8,4,3,2]  ==  5.0
-- ---------------------------------------------------------------------

-- albcercid roscargar margarvil14 enrnarbej antmorper3 natmarmar2
-- joscasgom1 antlopgom2 pabrabmon fatfervaz marjimcom belbenzam
mediana :: (Floating a, Ord a) => [a] -> a
mediana xs | odd a     = ys!!x
           | otherwise = media [ys!!x, ys!! (x-1)]
  where a  = length xs
        x  = div (length xs) 2
        ys = sort xs

-- paumacpar eledejim2 monlagare carmarcar5 manruiber
mediana2 :: (Floating a, Ord a) => [a] -> a
mediana2 xs | odd k     = head (drop l ys)
            | otherwise = media (take 2 (drop (l-1) ys))
  where ys = sort xs
        k  = length xs
        l  = div k 2

-- juaorture marmerzaf
mediana3 :: (Floating a, Ord a) => [a] -> a
mediana3 xs = sort xs !! (length xs `div` 2 )

-- Comentario: La definición anterior es incorrecta. Por ejemplo,
--    λ> mediana3 [0,1]
--    1.0

-- marlobrip
mediana4 :: (Floating a, Ord a) => [a] -> a
mediana4 xs | odd l = xs !! x
            | otherwise = media r
  where x = div l 2
        l = length xs
        r = [(last (take x xs)),(head (drop x xs))]

-- Comentario: La definición anterior es incorrecta. Por ejemplo,
--    λ> mediana4 [0,1,0]
--    1.0

prop_equiv_mediana :: [Int] -> Property
prop_equiv_mediana ys =
  not (null xs) ==>
  all (== mediana xs) [ mediana2 xs
                      -- , mediana3 xs
                      -- , mediana4 xs
                      ]
  where xs = map fromIntegral ys

-- ---------------------------------------------------------------------
-- Ejercicio 3. Comprobar con QuickCheck que para cualquier lista no
-- vacía xs el número de elementos de xs menores que su mediana es menor
-- o igual que la mitad de los elementos de xs y lo mismo pasa con los
-- mayores o iguales que la mediana.
-- --------------------------------------------------------------------- 

-- carmarcar5
-- La propiedad es
prop_mediana :: (Floating a, Ord a) => [a] -> Property
prop_mediana xs =
  (not.null) xs ==>
     genericLength [x | x <- xs, x < mediana xs] <= genericLength xs / 2
  && genericLength [x | x <- xs, x > mediana xs] <= genericLength xs / 2

-- La comprobación es
--    *Main> quickCheck prop_mediana
--    +++ OK, passed 100 tests.

-- juaorture monlagare paumacpar marmerzaf manruiber marjimcom belbenzam
-- La propiedad es
prop_mediana2 :: (Floating a, Ord a) => [a] -> Property
prop_mediana2 xs =
  (not . null) xs ==>
  length [x | x <- xs , x < mediana xs] <= length xs `div` 2

-- La comprobación es
--    *Main> quickCheck prop_mediana2
--    +++ OK, passed 100 tests.

-- albcercid roscargar margarvil14 enrnarbej eledejim2 antmorper3
-- natmarmar2 joscasgom1 antlopgom2 pabrabmon 
-- La propiedad es
prop_mediana3 :: (Floating a, Ord a) => [a] -> Property
prop_mediana3 xs = (not.null) xs ==> aux 0 0 xs (mediana xs)
  where aux a b [] n = a == b
        aux a b (x:xs) n | x > n = aux a (b+1) xs n
                         | x < n = aux (a+1) b xs n
                         | otherwise = aux a b xs n

-- La comprobación es
--    λ> quickCheck prop_mediana3
--    +++ OK, passed 100 tests.

-- fatfervaz
prop_mediana4 :: (Floating a, Ord a) => [a] -> Property
prop_mediana4 xs =
  (not.null) xs ==>
  length [x | x <- xs, x < mediana xs, x > mediana xs] <= length xs `div` 2 

-- Comentario: El enunciado de la propiedad es incorrecto.

-- La comprobación es
--    *Main> quickCheck prop_mediana4
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función
--    frecuencias :: Ord a => [a] -> [(a,Int)]
-- tal que (frecuencias xs) es la lista formada por los elementos de xs
-- junto con el número de veces que aparecen en xs. Por ejemplo,  
--    frecuencias "sosos" ==  [('o',2),('s',3)]
--
-- Nota: El orden de los pares no importa
-- ---------------------------------------------------------------------

-- pabrabmon
frecuencias8 :: Ord a => [a] -> [(a,Int)]
frecuencias8 xs = [(head x, length x) | x <- group (sort xs)]

-- enrnarbej paumacpar
frecuencias5 :: Ord a => [a] -> [(a,Int)]
frecuencias5 = map (\xs -> (head xs, length xs)).group.sort 

-- roscargar marlobrip joscasgom1 carmarcar5 fatfervaz manruiber
-- marjimcom belbenzam 
frecuencias4 :: Ord a => [a] -> [(a,Int)]
frecuencias4 xs = nub [(x,ocurrencia x xs) | x <- xs]
  where ocurrencia _ [] = 0
        ocurrencia y (x:xs) | y == x = 1 + ocurrencia y xs
                            | otherwise = ocurrencia y xs

-- albcercid 
frecuencias :: Ord a => [a] -> [(a,Int)]
frecuencias xs = aux xs []
  where aux [] v = v
        aux (x:xs) v = aux xs (f x v)
        f x [] = [(x,1)]
        f x ((a,b):ys) | x == a = (a,b+1):ys
                       | otherwise = (a,b):f x ys

-- albcercid
frecuencias2 :: Ord a => [a] -> [(a,Int)]
frecuencias2 [] = []
frecuencias2 xs = aux ys (head ys) 0
  where aux [] a b = [(a,b)]
        aux (y:ys) a b | y == a = aux ys a (b+1)
                       | otherwise = (a,b):aux ys y 1
        ys = sort xs

-- juaorture antmorper3 antlopgom2 marmerzaf
frecuencias6 :: Ord a => [a] -> [(a,Int)]
frecuencias6 xs = nub [(a,b) | a <- xs
                             , let b = length [ x | x <- xs , a == x]]
 
-- monlagare
frecuencias7 :: Ord a => [a] -> [(a,Int)]
frecuencias7 xs = zip ts ys
  where ts = sort (nub xs)
        ys = map length zs
        zs = group (sort xs)

prop_equiv_frecuencias :: [Int] -> Bool
prop_equiv_frecuencias xs =
  all (== sort (frecuencias xs)) [ sort (frecuencias2 xs)
                                 , sort (frecuencias4 xs)
                                 , sort (frecuencias5 xs)
                                 , sort (frecuencias6 xs)
                                 , sort (frecuencias7 xs)
                                 , sort (frecuencias8 xs)
                                 ]

-- ---------------------------------------------------------------------
-- Ejercicio 5. Las modas de una lista son los elementos de la lista
-- que más se repiten. 
--
-- Definir la función
--    modas :: Ord a => [a] -> [a]
-- tal que (modas xs) es la lista ordenada de las modas de xs. Por
-- ejemplo 
--    modas [7,3,7,5,3,1,6,9,6]  ==  [3,6,7]
-- ---------------------------------------------------------------------

-- paumacpar 
modas6 :: Ord a => [a] -> [a]
modas6 xs = [a | (a,b) <- frecuencias xs, k == b]
  where k = maximum (map length (group (sort xs))) 

-- albcercid enrnarbej antmorper3 joscasgom1
modas :: Ord a => [a] -> [a]
modas xs = [ b | (a,b) <- q, a == (fst $ last q)]
  where q = sort $ frecuencias3 xs

frecuencias3 [] = []
frecuencias3 xs = aux ys (head ys) 0
          where aux [] a b = [(b,a)]
                aux (y:ys) a b | y == a = aux ys a (b+1)
                               | otherwise = (b,a):aux ys y 1
                ys = sort xs

-- roscargar antlopgom2 manruiber
modas2 xs = [x | (x,s) <- frecuencias4 xs, s == y]
  where y = last (sort (map snd (frecuencias xs)))

-- juaorture
modas3 :: Ord a => [a] -> [a]
modas3 xs =
  sort $ nub [ x | x <- xs
                 , frecuencia x xs >= maximum [frecuencia y xs | y <- xs]]
  where frecuencia :: Ord a => a -> [a] -> Int
        frecuencia n ys = head [b | (a,b) <- frecuencias ys, a == n] 

-- monlagare marmerzaf belbenzam
modas4 xs = [x | (x,a) <- frecuencias xs, a == maximum ys]
  where ys = map length (group (sort xs))

-- marlobrip marjimcom
modas5 :: Ord a => [a] -> [a]
modas5 xs = [fst (x,y) | (x,y) <- frecuencias xs, s == y]
  where s = maximum [snd (x,y) | (x,y) <- frecuencias xs]

-- pabrabmon carmarcar5
modas7 :: Ord a => [a] -> [a]
modas7 xs = [x | (x,y) <- frecuencias xs, y == maximum ys]
  where ys = map snd (frecuencias xs)

-- fatfervaz
modas8 :: Ord a => [a] -> [a]
modas8 xs = [x | (x,y) <- h, y == s]
  where ys = sort xs
        s  = maximum [snd (x,y)|(x,y) <- h]
        h  = frecuencias ys

prop_equiv_modas :: [Int] -> Bool
prop_equiv_modas xs =
  all (== sort(modas xs)) [ sort (modas2 xs)
                          , sort (modas3 xs)
                          , sort (modas4 xs)
                          , sort (modas5 xs)
                          , sort (modas6 xs)
                          , sort (modas7 xs)
                          , sort (modas8 xs)
                          ]

-- La comprobación es
--    λ> quickCheckWith (stdArgs {maxSize=10}) prop_equiv_modas
--    +++ OK, passed 100 tests.
  
-- ---------------------------------------------------------------------
-- Ejercicio 6. La media geométrica de una lista de n números es la
-- raíz n-ésima del producto de todos los números.
-- 
-- Definir la función
--    mediaGeometrica :: Floating a => [a] -> a
-- tal que (mediaGeometrica xs) es la media geométrica de xs. Por
-- ejemplo, 
--    mediaGeometrica [2,18]   ==  6.0
--    mediaGeometrica [3,1,9]  ==  3.0
-- ---------------------------------------------------------------------

-- eledejim2 roscargar margarvil14 enrnarbej juaorture antmorper3 monlagare
-- marlobrip joscasgom1 antlopgom2 marmerzaf pabrabmon manruiber carmarcar5
-- fatfervaz marjimcom belbenzam
mediaGeometrica :: Floating a => [a] -> a
mediaGeometrica xs = (product xs) ** (1 / genericLength xs)

-- albcercid paumacpar
mediaGeometrica2 :: Floating a => [a] -> a
mediaGeometrica2 xs = product [x**(1/y) | x <- xs]
  where y  = genericLength xs

-- ---------------------------------------------------------------------
-- Ejercicio 7. Comprobar con QuickCheck que la media geométrica de
-- cualquier lista no vacía de números no negativos es siempre menor o
-- igual que la media aritmética. 
-- ---------------------------------------------------------------------

-- albcercid roscargar margarvil14 enrnarbej eledejim2 antmorper3 monlagare
-- marlobrip joscasgom1 antlopgom2 paumacpar marmerzaf pabrabmon
-- manruiber carmarcar5 fatfervaz marjimcom belbenzam

-- La propiedad es
prop_mediaGeometrica :: (Floating a, Ord a) => [a] -> Property
prop_mediaGeometrica xs =
  (not.null) xs ==> media ys >= mediaGeometrica ys
  where ys = map abs xs

-- La comprobación es

-- juaorture
-- La propiedad es (probando el contrarrecíproco)

prop_mediaGeometrica2 :: (Floating a, Ord a) => [a] -> Property
prop_mediaGeometrica2 xs =
  not (media xs >= mediaGeometrica xs) ==>
  not ((not . null) xs && and (map (>=0) xs)) 

-- La comprobación es
--    *Main> quickCheck prop_mediaGeometrica
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Medidas de dispersión                                              --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 8. El recorrido (o rango) de una lista de valores es la
-- diferencia entre el mayor y el menor.
-- 
-- Definir la función 
--    rango :: (Num a, Ord a) => [a] -> a
-- tal que (rango xs) es el rango de xs. Por ejemplo,
--    rango [4,2,4,7,3]  ==  5
-- ---------------------------------------------------------------------

-- eledejim2 albcercid roscargar margarvil14 enrnarbej antmorper3
-- monlagare marlobrip joscasgom1 juaorture antlopgom2 paumacpar
-- marmerzaf pabrabmon manruiber carmarcar5 fatfervaz marjimcom belbenzam
rango :: (Num a, Ord a) => [a] -> a
rango xs = maximum xs - minimum xs
 
-- ---------------------------------------------------------------------
-- Ejercicio 9. La desviación media de una lista de datos xs es la
-- media de las distancias de los datos a la media xs, donde la
-- distancia entre dos elementos es el valor absoluto de su
-- diferencia. Por ejemplo, la desviación media de [4,8,4,5,9] es 2 ya
-- que la media de [4,8,4,5,9] es 6 y
--      (|4-6| + |8-6| + |4-6| + |5-6| + |9-6|) / 5
--    = (2 + 2 + 2 + 1 + 3) / 5
--    = 2
-- 
-- Definir la función
--    desviacionMedia :: Floating a => [a] -> a
-- tal que (desviacionMedia xs) es la desviación media de xs. Por
-- ejemplo, 
--    desviacionMedia [4,8,4,5,9]       ==  2.0
--    desviacionMedia (replicate 10 3)  ==  0.0
-- ---------------------------------------------------------------------

-- albcercid roscargar enrnarbej eledejim2 antmorper3 monlagare marlobrip
--joscasgom1 juaorture antlopgom2 paumacpar marmerzaf pabrabmon manruiber
-- carmarcar5 fatfervaz marjimcom belbenzam
desviacionMedia :: Floating a => [a] -> a
desviacionMedia xs = media [abs (x-n) | x <- xs]
  where n = media xs

-- margarvil14
desviacionMedia2 :: Floating a => [a] -> a
desviacionMedia2 xs = media (distancia xs)

distancia xs = [ abs (x - y) | x <- xs]
  where y = media xs

-- ---------------------------------------------------------------------
-- Ejercicio 10. La varianza de una lista datos es la media de los
-- cuadrados de las distancias de los datos a la media. Por ejemplo, la
-- varianza de [4,8,4,5,9] es 4.4 ya que la media de [4,8,4,5,9] es 6 y
--      ((4-6)^2 + (8-6)^2 + (4-6)^2 + (5-6)^2 + (9-6)^2) / 5
--    = (4 + 4 + 4 + 1 + 9) / 5
--    = 4.4
-- 
-- Definir la función
--    varianza :: Floating a => [a] -> a
-- tal que (desviacionMedia xs) es la varianza de xs. Por ejemplo, 
--    varianza [4,8,4,5,9]       ==  4.4
--    varianza (replicate 10 3)  ==  0.0
-- ---------------------------------------------------------------------

-- albcercid roscargar enrnarbej eledejim2 antmorper3 monlagare marlobrip
--joscasgom1 juaorture antlopgom2 paumacpar pabrabmon manruiber carmarcar5
-- fatfervaz marjimcom
varianza :: Floating a => [a] -> a
varianza xs = media [(x-n)^2 | x <- xs]
  where n = media xs

-- albcercid marmerzaf belbenzam
varianza2 :: Floating a => [a] -> a
varianza2 xs = media (map (^2) xs) - (media xs)^2

-- margarvil14
varianza3 :: Floating a => [a] -> a
varianza3 xs = media ys - (media xs)^2
  where ys = map (^2) xs

prop_equiv_varianza :: [Int] -> Property
prop_equiv_varianza xs =
  not (null ys) ==>
  all (~= varianza ys) [ varianza2 ys
                       , varianza3 ys
                       ]
  where ys = map fromIntegral xs
        x ~= y = abs (x - y) < 1e-3
  
-- ---------------------------------------------------------------------
-- Ejercicio 11. La desviación típica de una lista de datos es la raíz
-- cuadrada de su varianza.  
-- 
-- Definir la función 
--    desviacionTipica :: Floating a => [a] -> a
-- tal que (desviacionTipica xs) es la desviación típica de xs. Por
-- ejemplo, 
--    desviacionTipica [4,8,4,5,9]       ==  2.0976176963403033
--    desviacionTipica (replicate 10 3)  ==  0.0
-- ---------------------------------------------------------------------

-- albcercid roscargar margarvil14 enrnarbej eledejim2 antmorper3
-- monlagare marlobrip joscasgom1 juaorture antlopgom2 paumacpar
-- marmerzaf pabrabmon manruiber carmarcar5 fatfervaz marjimcom
-- belbenzam 
desviacionTipica :: Floating a => [a] -> a
desviacionTipica = sqrt.varianza

