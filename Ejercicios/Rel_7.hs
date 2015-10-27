-- I1M 2015-16: Relaci�n 7 (18 de octubre de 2015)
-- Estad�stica descriptiva.
-- Departamento de Ciencias de la Computaci�n e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducci�n                                                       --
-- ---------------------------------------------------------------------

-- El objetivo de esta relaci�n es definir las principales medidas
-- estad�sticas de centralizaci�n (medias, mediana y modas) y de
-- dispersi�n (rango, desviaci�n media, varianza y desviaci�n t�pica)
-- que se estudian en 3� de ESO (como en http://bit.ly/1yXc7mv ).
--
-- En las soluciones de los ejercicios se pueden usar las siguientes
-- funciones de la librer�a Data.List fromIntegral, genericLength, sort,
-- y group (cuya descripci�n se puede consultar en el "Manual de
-- funciones b�sicas de Haskell" http://bit.ly/1PqHagT ).

-- ---------------------------------------------------------------------
-- Librer�as auxiliares                                               --
-- ---------------------------------------------------------------------

import Data.List
import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Medidas de centralizaci�n                                          --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la funci�n
--    media :: Floating a => [a] -> a
-- tal que (media xs) es la media aritm�tica de los n�meros de la lista
-- xs. Por ejemplo,
--    media [4,8,4,5,9]  ==  6.0
-- ---------------------------------------------------------------------

-- ivaruicam juamorrom1 fracruzam carmengar josllagam manpende
media :: Floating a => [a] -> a
media xs = (sum xs) / fromIntegral (length xs)

-- Comentario: La definici�n anterior se puede simplificar.

-- silgongal blaruiher pabmorgar alvalvdom1
media2 :: Floating a => [a] -> a
media2 xs = sum xs / fromIntegral (length xs)

-- Comentario: La definici�n anterior se puede simplificar.

-- carmengar paocabper rubvilval manvermor marvilmor carruirui3
-- juanarcon erisancha enrvalmor  lucgamgal
media3 :: Floating a => [a] -> a 
media3 xs = sum xs / genericLength xs
  
-- ---------------------------------------------------------------------
-- Ejercicio 2. La mediana de una lista de valores es el valor de 
-- la lista que ocupa el lugar central de los valores ordenados de menor
-- a mayor. Si el n�mero de datos es impar se toma como valor de la
-- mediana el valor central. Si el n�mero de datos es par se toma como
-- valor de la mediana la media aritm�tica de los dos valores
-- centrales.
-- 
-- Definir la funci�n 
--    mediana :: (Floating a, Ord a) => [a] -> a
-- tal que (mediana xs) es la mediana de la lista xs. Por ejemplo, 
--    mediana [2,3,6,8,9]    ==  6.0
--    mediana [2,3,4,6,8,9]  ==  5.0
--    mediana [9,6,8,4,3,2]  ==  5.0
-- ---------------------------------------------------------------------

-- carmengar blaruiher silgongal fracruzam paocabper rubvilval pabmorgar
-- marvilmor josllagam carruirui3 alvalvdom1 juanarcon manpende
-- erisancha enrvalmor lucgamgal
mediana :: (Floating a, Ord a) => [a] -> a
mediana xs | odd n     = s !! (div n 2)
           | otherwise = (s !! (div n 2) + s !! ((div n 2) - 1))/2
           where n = length xs
                 s = sort xs

-- ---------------------------------------------------------------------
-- Ejercicio 3. Comprobar con QuickCheck que para cualquier lista no
-- vac�a xs el n�mero de elementos de xs menores que su mediana es menor
-- o igual que la mitad de los elementos de xs y lo mismo pasa con los
-- mayores o iguales que la mediana.
-- --------------------------------------------------------------------- 

-- carmengar silgongal fracruzam paocabper rubvilval blaruiher pabmorgar
-- marvilmor josllagam carruirui3 alvalvdom1 juanarcon manpende
-- erisancha enrvalmor lucgamgal

-- La propiedad es
prop_mediana :: (Floating a, Ord a) => [a] -> Property
prop_mediana xs = 
    xs /= [] ==> length (filter (< m) xs) <= l && 
                 length (filter (>m) xs) <= l
    where l = (length xs) `div` 2
          m = mediana xs

-- La comprobaci�n es
-- *Main> quickCheck prop_mediana
-- +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la funci�n
--    frecuencias :: Ord a => [a] -> [(a,Int)]
-- tal que (frecuencias xs) es la lista formada por los elementos de xs
-- junto con el n�mero de veces que aparecen en xs. Por ejemplo,  
--    frecuencias "sosos" ==  [('o',2),('s',3)]
--
-- Nota: El orden de los pares no importa
-- ---------------------------------------------------------------------

-- carmengar silgongal fracruzam paocabper rubvilval pabmorgar blaruiher
-- manvermor marvilmor josllagam alvalvdom1 juanarcon erisancha
-- enrvalmor lucgamgal 
frecuencias :: Ord a => [a] -> [(a,Int)]
frecuencias xs = [(x,y) | x <- (nub xs), y <- [contar x xs]]

-- Comentario: La definici�n anterior se puede mejorar.

contar :: Eq a => a -> [a] -> Int
contar x xs = length [1 | x' <- xs, x' == x]

-- ---------------------------------------------------------------------
-- Ejercicio 5. Las modas de una lista son los elementos de la lista
-- que m�s se repiten. 
--
-- Definir la funci�n
--    modas :: Ord a => [a] -> [a]
-- tal que (modas xs) es la lista ordenada de las modas de xs. Por
-- ejemplo 
--    modas [7,3,7,5,3,1,6,9,6]  ==  [3,6,7]
-- ---------------------------------------------------------------------

-- carmengar silgongal fracruzam rubvilval pabmorgar blaruiher paocabper
-- marvilmor josllagam alvalvdom1 juanarcon erisancha enrvalmor
-- lucgamgal 
modas :: Ord a => [a] -> [a]
modas xs = sort [x | (x,y) <- frecuencias xs, y == maximum f]
    where f = [y | (_,y) <- frecuencias xs]

-- carruirui3
modas2 :: Ord a => [a] -> [a]
modas2 xs = 
    sort [x | (x,y) <- frecuencias xs, 
              y == maximum (map snd (frecuencias xs))]

-- ---------------------------------------------------------------------
-- Ejercicio 6. La media geom�trica de una lista de n n�meros es la
-- ra�z n-�sima del producto de todos los n�meros.
-- 
-- Definir la funci�n
--    mediaGeometrica :: Floating a => [a] -> a
-- tal que (mediaGeometrica xs) es la media geom�trica de xs. Por
-- ejemplo, 
--    mediaGeometrica [2,18]   ==  6.0
--    mediaGeometrica [3,1,9]  ==  3.0
-- ---------------------------------------------------------------------

-- carmengar blaruiher manpende lucgamgal
mediaGeometrica :: Floating a => [a] -> a
mediaGeometrica xs = (product xs)**(1/ fromIntegral (length xs))

-- Comentario: La definici�n anterior se puede simplificar.

-- guache carmengar silgongal rubvilval pabmorgar manvermor marvilmor
-- josllagam carruirui3 juanarcon erisancha enrvalmor 
mediaGeometrica2 :: Floating a => [a] -> a
mediaGeometrica2 xs = (product xs)**(1/genericLength xs)

-- fracruzam alvalvdom1
mediaGeometrica3 :: Floating a => [a] -> a
mediaGeometrica3 xs = product xs ** (1/ fromIntegral (length xs))

-- La versi�n de carmengar con un par�ntesis menos
-- pero sin aprovechar genericLength

-- Comentario: La definici�n anterior se puede simplificar.

-- ---------------------------------------------------------------------
-- Ejercicio 7. Comprobar con QuickCheck que la media geom�trica de
-- cualquier lista no vac�a de n�meros no negativos es siempre menor o
-- igual que la media aritm�tica. 
-- ---------------------------------------------------------------------

-- guache carmengar silgongal fracruzam rubvilval pabmorgar manvermor
-- blaruiher marvilmor josllagam carruirui3 alvalvdom1 juanarcon
-- lucgamgal 

-- La propiedad es
prop_mediaGeometrica :: (Floating a, Ord a) => [a] -> Property
prop_mediaGeometrica xs = 
    length xs > 0 ==> media z >= mediaGeometrica z
    where z = map abs xs

-- La comprobaci�n es
--    *Main> quickCheck prop_mediaGeometrica
--    +++ OK, passed 100 tests.

-- manpende erisancha enrvalmor
prop_mediaGeometrica2 :: (Floating a, Ord a) => [a] -> Property
prop_mediaGeometrica2 xs = 
    length xs > 0 && minimum xs > 0 ==> mediaGeometrica xs <= media xs

-- La comprobaci�n es
--    *Main> quickCheck prop_mediaGeometrica2
--    +++ Gave up! Passed only 44 tests.

-- ---------------------------------------------------------------------
-- Medidas de dispersi�n                                              --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 8. El recorrido (o rango) de una lista de valores es la
-- diferencia entre el mayor y el menor.
-- 
-- Definir la funci�n 
--    rango :: (Num a, Ord a) => [a] -> a
-- tal que (rango xs) es el rango de xs. Por ejemplo,
--    rango [4,2,4,7,3]  ==  5
-- ---------------------------------------------------------------------

-- carmengar silgongal rubvilval pabmorgar manvermor blaruiher josllagam
-- carruirui3 juanarcon alvalvdom1 manpende erisancha enrvalmor
-- lucgamgal 
rango :: (Num a, Ord a) => [a] -> a
rango xs = maximum xs - minimum xs

-- fracruzam
rango2 :: (Num a, Ord a) => [a] -> a
rango2 xs = last (sort xs) - head (sort xs)
 
-- ---------------------------------------------------------------------
-- Ejercicio 9. La desviaci�n media de una lista de datos xs es la
-- media de las distancias de los datos a la media xs, donde la
-- distancia entre dos elementos es el valor absoluto de su
-- diferencia. Por ejemplo, la desviaci�n media de [4,8,4,5,9] es 2 ya
-- que la media de [4,8,4,5,9] es 6 y
--      (|4-6| + |8-6| + |4-6| + |5-6| + |9-6|) / 5
--    = (2 + 2 + 2 + 1 + 3) / 5
--    = 2
-- 
-- Definir la funci�n
--    desviacionMedia :: Floating a => [a] -> a
-- tal que (desviacionMedia xs) es la desviaci�n media de xs. Por
-- ejemplo, 
--    desviacionMedia [4,8,4,5,9]       ==  2.0
--    desviacionMedia (replicate 10 3)  ==  0.0
-- ---------------------------------------------------------------------

-- carmengar silgongal fracruzam rubvilval pabmorgar manvermor blaruiher
-- josllagam juanarcon alvalvdom1 manpende erisancha enrvalmor lucgamgal 
desviacionMedia :: Floating a => [a] -> a
desviacionMedia xs = media [abs (x-m) | x <- xs]
    where m = media xs

-- carruirui3
desviacionMedia2 :: Floating a => [a] -> a
desviacionMedia2 xs = media [abs (x - media xs) | x <- xs]

-- Comentario: La definici�n anterior se puede mejorar.

-- ---------------------------------------------------------------------
-- Ejercicio 10. La varianza de una lista datos es la media de los
-- cuadrados de las distancias de los datos a la media. Por ejemplo, la
-- varianza de [4,8,4,5,9] es 4.4 ya que la media de [4,8,4,5,9] es 6 y
--      ((4-6)^2 + (8-6)^2 + (4-6)^2 + (5-6)^2 + (9-6)^2) / 5
--    = (4 + 4 + 4 + 1 + 9) / 5
--    = 4.4
-- 
-- Definir la funci�n
--    varianza :: Floating a => [a] -> a
-- tal que (desviacionMedia xs) es la varianza de xs. Por ejemplo, 
--    varianza [4,8,4,5,9]       ==  4.4
--    varianza (replicate 10 3)  ==  0.0
-- ---------------------------------------------------------------------

-- carmengar silgongal fracruzam rubvilval pabmorgar manvermor blaruiher
-- josllagam juanarcon alvalvdom1 manpende erisancha enrvalmor lucgamgal
varianza :: Floating a => [a] -> a
varianza xs =  media [(x-m)^2 | x <- xs]
    where m = media xs

-- carruirui3
varianza2 :: Floating a => [a] -> a
varianza2 xs =  media [(x - media xs)^2 | x <- xs]

-- Comentario: La definici�n anterior se puede mejorar.

-- ---------------------------------------------------------------------
-- Ejercicio 11. La desviaci�n t�pica de una lista de datos es la ra�z
-- cuadrada de su varianza.  
-- 
-- Definir la funci�n 
--    desviacionTipica :: Floating a => [a] -> a
-- tal que (desviacionTipica xs) es la desviaci�n t�pica de xs. Por
-- ejemplo, 
--    desviacionTipica [4,8,4,5,9]       ==  2.0976176963403033
--    desviacionTipica (replicate 10 3)  ==  0.0
-- ---------------------------------------------------------------------

-- carmengar silgongal fracruzam rubvilval pabmorgar manvermor blaruiher
-- josllagam carruirui3 juanarcon alvalvdom1 manpende erisancha
-- enrvalmor lucgamgal 
desviacionTipica :: Floating a => [a] -> a
desviacionTipica xs = sqrt (varianza xs)
