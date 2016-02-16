-- I1M 2015-16: Relación 20 (11 de febrero de 2016)
-- Vectores y matrices con las librerías.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- El objetivo de esta relación es adaptar los ejercicios de las
-- relaciones 18 y 19 (sobre vectores y matrices) usando las librerías
-- Data.Vector y Data.Matrix. 
--
-- El manual, con ejemplos, de la librería de vectores de encuentra en
-- http://bit.ly/17Oq893 y el de matrices en http://bit.ly/17Oq9K5
--
-- Para instalar las librerías basta escribir en la consola
--    cabal update
--    cabal install vector
--    cabal install matrix
 
-- ---------------------------------------------------------------------
-- Importación de librerías                                           --
-- ---------------------------------------------------------------------

import qualified Data.Vector as V 
import Data.Matrix 
import Data.Ratio

-- ---------------------------------------------------------------------
-- Tipos de los vectores y de las matrices                            --
-- ---------------------------------------------------------------------

-- Los vectores (con elementos de tipo a son del tipo (V.Vector a).
-- Los matrices (con elementos de tipo a son del tipo (Matrix a).

-- ---------------------------------------------------------------------
-- Operaciones básicas con matrices                                   --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    listaVector :: Num a => [a] -> V.Vector a
-- tal que (listaVector xs) es el vector correspondiente a la lista
-- xs. Por ejemplo, 
--    ghci> listaVector [3,2,5]
--    fromList [3,2,5]
-- ---------------------------------------------------------------------

-- alvalvdom1 manvermor fracruzam manpende blaruiher erisancha ivaruicam
-- abrdelrod 
listaVector :: Num a => [a] -> V.Vector a
listaVector = V.fromList

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    listaMatriz :: Num a => [[a]] -> Matrix a
-- tal que (listaMatriz xss) es la matriz cuyas filas son los elementos
-- de xss. Por ejemplo,
--    ghci> listaMatriz [[1,3,5],[2,4,7]]
--    ( 1 3 5 )
--    ( 2 4 7 )
-- ---------------------------------------------------------------------

-- alvalvdom1 manvermor fracruzam manpende blaruiher erisancha ivaruicam
-- abrdelrod 
listaMatriz :: Num a => [[a]] -> Matrix a
listaMatriz = fromLists

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--    numFilas :: Num a => Matrix a -> Int
-- tal que (numFilas m) es el número de filas de la matriz m. Por
-- ejemplo,
--    numFilas (listaMatriz [[1,3,5],[2,4,7]])  ==  2
-- ---------------------------------------------------------------------

-- alvalvdom1 manvermor fracruzam manpende blaruiher erisancha ivaruicam
-- abrdelrod 
numFilas :: Num a => Matrix a -> Int
numFilas = nrows

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función
--    numColumnas :: Num a => Matrix a -> Int
-- tal que (numColumnas m) es el número de columnas de la matriz
-- m. Por ejemplo,
--    numColumnas (listaMatriz [[1,3,5],[2,4,7]])  ==  3
-- ---------------------------------------------------------------------

-- alvalvdom1 manvermor fracruzam manpende blaruiher erisancha ivaruicam
-- abrdelrod 
numColumnas:: Num a => Matrix a -> Int
numColumnas = ncols

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la función
--    dimension :: Num a => Matrix a -> (Int,Int)
-- tal que (dimension m) es la dimensión de la matriz m. Por ejemplo, 
--    dimension (listaMatriz [[1,3,5],[2,4,7]])  ==  (2,3)
-- ---------------------------------------------------------------------

-- alvalvdom1 manvermor fracruzam manpende
dimension :: Num a => Matrix a -> (Int,Int)
dimension p = (numFilas p, numColumnas p)

-- Comentario: La definición anterior se puede mejorar.

-- blaruiher erisancha ivaruicam abrdelrod
dimension2 :: Num a => Matrix a -> (Int,Int)
dimension2 p = (nrows p, ncols p)

-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir la función
--    matrizLista :: Num a => Matrix a -> [[a]]
-- tal que (matrizLista x) es la lista de las filas de la matriz x. Por
-- ejemplo, 
--    ghci> let m = listaMatriz [[5,1,0],[3,2,6]]
--    ghci> m
--    ( 5 1 0 )
--    ( 3 2 6 )
--    ghci> matrizLista m
--    [[5,1,0],[3,2,6]]
-- ---------------------------------------------------------------------

-- alvalvdom1 manvermor manpende
matrizLista :: Num a => Matrix a -> [[a]]
matrizLista p = [vectorLista (filaMat i p) | i <- [1..numFilas p]]

-- Comentario: La definición anterior se puede simplificar.

-- fracruzam blaruiher erisancha ivaruicam abrdelrod
matrizLista2 :: Num a => Matrix a -> [[a]]
matrizLista2 = toLists

-- ---------------------------------------------------------------------
-- Ejercicio 8. Definir la función
--    vectorLista :: Num a => V.Vector a -> [a]
-- tal que (vectorLista x) es la lista de los elementos del vector
-- v. Por ejemplo, 
--    ghci> let v = listaVector [3,2,5]
--    ghci> v
--    fromList [3,2,5]
--    ghci> vectorLista v
--    [3,2,5]
-- ---------------------------------------------------------------------

-- alvalvdom1 manvermor fracruzam manpende blaruiher erisancha ivaruicam
-- abrdelrod 
vectorLista :: Num a => V.Vector a -> [a]
vectorLista = V.toList

-- ---------------------------------------------------------------------
-- Suma de matrices                                                   --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 9. Definir la función
--    sumaMatrices:: Num a => Matrix a -> Matrix a -> Matrix a
-- tal que (sumaMatrices x y) es la suma de las matrices x e y. Por
-- ejemplo, 
--    ghci> let m1 = listaMatriz [[5,1,0],[3,2,6]]
--    ghci> let m2 = listaMatriz [[4,6,3],[1,5,2]]
--    ghci> sumaMatrices m1 m2
--    ( 9 7 3 )
--    ( 4 7 8 )
-- ---------------------------------------------------------------------

-- alvalvdom1 manvermor manpende blaruiher
sumaMatrices:: Num a => Matrix a -> Matrix a -> Matrix a
sumaMatrices p q = p+q

-- Comentario: La definición anterior se puede simplificar.

-- fracruzam erisancha ivaruicam abrdelrod
sumaMatrices2 :: Num a => Matrix a -> Matrix a -> Matrix a
sumaMatrices2 = (+)

-- ---------------------------------------------------------------------
-- Ejercicio 10. Definir la función
--    filaMat :: Num a => Int -> Matrix a -> V.Vector a
-- tal que (filaMat i p) es el vector correspondiente a la fila i-ésima
-- de la matriz p. Por ejemplo,
--    ghci> let p = listaMatriz [[5,1,0],[3,2,6],[4,5,7]]
--    ghci> filaMat 2 p
--    fromList [3,2,6]
--    ghci> vectorLista (filaMat 2 p)
--    [3,2,6]
-- ---------------------------------------------------------------------

-- alvalvdom1 manvermor fracruzam blaruiher erisancha ivaruicam abrdelrod
filaMat :: Num a => Int -> Matrix a -> V.Vector a
filaMat = getRow

-- manpende
filaMat2 :: Num a => Int -> Matrix a -> V.Vector a
filaMat2 i p = listaVector [ p ! (i,j) | j <- [1..ncols p]]

-- Comentario: La definición anterior se puede simplificar.

-- ---------------------------------------------------------------------
-- Ejercicio 11. Definir la función
--    columnaMat :: Num a => Int -> Matrix a -> V.Vector a
-- tal que (columnaMat j p) es el vector correspondiente a la columna
-- j-ésima de la matriz p. Por ejemplo,
--    ghci> let p = listaMatriz [[5,1,0],[3,2,6],[4,5,7]]
--    ghci> columnaMat 2 p
--    fromList [1,2,5]
--    ghci> vectorLista (columnaMat 2 p)
--    [1,2,5]
-- ---------------------------------------------------------------------

-- alvalvdom1 manvermor fracruzam blaruiher erisancha ivaruicam abrdelrod
columnaMat :: Num a => Int -> Matrix a -> V.Vector a
columnaMat = getCol

-- manpende
columnaMat2 :: Num a => Int -> Matrix a -> V.Vector a
columnaMat2 i p = listaVector [ p ! (j,i) | j <- [1..nrows p]]

-- Comentario: La definición anterior se puede simplificar.

-- ---------------------------------------------------------------------
-- Producto de matrices                                               --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 12. Definir la función
--    prodEscalar :: Num a => V.Vector a -> V.Vector a -> a
-- tal que (prodEscalar v1 v2) es el producto escalar de los vectores v1
-- y v2. Por ejemplo,
--    ghci> let v = listaVector [3,1,10]
--    ghci> prodEscalar v v
--    110
-- ---------------------------------------------------------------------

-- alvalvdom1 manvermor
prodEscalar :: Num a => V.Vector a -> V.Vector a -> a
prodEscalar v1 v2 = sum $ zipWith (*) (vectorLista v1) (vectorLista v2)

-- Comentario: La definición anterior se puede simplificar.

-- fracruzam blaruiher
prodEscalar2 :: Num a => V.Vector a -> V.Vector a -> a
prodEscalar2 v1 v2 = foldl1 (+) $ V.zipWith (*) v1 v2

-- Comentario: La definición anterior se puede simplificar usando V.sum.

-- manpende
prodEscalar3 :: Num a => V.Vector a -> V.Vector a -> a
prodEscalar3 v1 v2 = 
         foldl (+) 0 [ v1 V.! i * v2 V.! i | i <- [0..length v1-1]]

-- Comentario: La definición anterior se puede simplificar.

-- erisancha ivaruicam abrdelrod
prodEscalar5 :: Num a => V.Vector a -> V.Vector a -> a
prodEscalar5 v1 v2 = V.sum $ V.zipWith (*) v1 v2

-- pedjaecar 
prodEscalar4 :: Num a => V.Vector a -> V.Vector a -> a
prodEscalar4 v1 v2 = V.sum (V.zipWith (*) v1 v2)

-- ---------------------------------------------------------------------
-- Ejercicio 13. Definir la función
--    prodMatrices:: Num a => Matrix a -> Matrix a -> Matrix a
-- tal que (prodMatrices p q) es el producto de las matrices p y q. Por
-- ejemplo, 
--    ghci> let p = listaMatriz [[3,1],[2,4]]
--    ghci> prodMatrices p p
--    ( 11  7 )
--    ( 14 18 )
--    ghci> let q = listaMatriz [[7],[5]]
--    ghci> prodMatrices p q
--    ( 26 )
--    ( 34 )
-- ---------------------------------------------------------------------

-- alvalvdom1 manvermor manpende blaruiher
prodMatrices:: Num a => Matrix a -> Matrix a -> Matrix a
prodMatrices p q = p*q

-- Comentario: La definición anterior se puede simplificar.

-- fracruzam erisancha ivaruicam abrdelrod
prodMatrices2 :: Num a => Matrix a -> Matrix a -> Matrix a
prodMatrices2 = (*)

-- ---------------------------------------------------------------------
-- Traspuestas y simétricas                                           --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 14. Definir la función
--    traspuesta :: Num a => Matrix a -> Matrix a
-- tal que (traspuesta p) es la traspuesta de la matriz p. Por ejemplo,
--    ghci> let p = listaMatriz [[5,1,0],[3,2,6]]
--    ghci> traspuesta p
--    ( 5 3 )
--    ( 1 2 )
--    ( 0 6 )
-- ---------------------------------------------------------------------

-- alvalvdom1 manvermor fracruzam blaruiher erisancha ivaruicam
-- abrdelrod 
traspuesta :: Num a => Matrix a -> Matrix a
traspuesta = transpose

-- manpende
traspuesta2 :: Num a => Matrix a -> Matrix a
traspuesta2 p = listaMatriz (map vectorLista 
             [columnaMat i p | i <- [1..ncols p]])

-- Comentario: La definición anterior se puede simplificar.

-- ---------------------------------------------------------------------
-- Ejercicio 15. Definir la función
--    esCuadrada :: Num a => Matrix a -> Bool
-- tal que (esCuadrada p) se verifica si la matriz p es cuadrada. Por
-- ejemplo, 
--    ghci> let p = listaMatriz [[5,1,0],[3,2,6]]
--    ghci> esCuadrada p
--    False
--    ghci> let q = listaMatriz [[5,1],[3,2]]
--    ghci> esCuadrada q
--    True
-- ---------------------------------------------------------------------

-- alvalvdom1 manvermor 
esCuadrada :: Num a => Matrix a -> Bool
esCuadrada p = numFilas p == numColumnas p

-- Comentario: La definición anterior se puede mejorar.

-- fracruzam manpende blaruiher erisancha ivaruicam abrdelrod
esCuadrada2 :: Num a => Matrix a -> Bool
esCuadrada2 p = nrows p == ncols p

-- ---------------------------------------------------------------------
-- Ejercicio 16. Definir la función
--    esSimetrica :: (Num a, Eq a) => Matrix a -> Bool
-- tal que (esSimetrica p) se verifica si la matriz p es simétrica. Por
-- ejemplo, 
--    ghci> let p = listaMatriz [[5,1,3],[1,4,7],[3,7,2]]
--    ghci> esSimetrica p
--    True
--    ghci> let q = listaMatriz [[5,1,3],[1,4,7],[3,4,2]]
--    ghci> esSimetrica q
--    False
-- ---------------------------------------------------------------------    

-- alvalvdom1 manvermor manpende
esSimetrica :: (Num a, Eq a) => Matrix a -> Bool
esSimetrica x = traspuesta x == x

-- Comentario: La definición anterior se puede mejorar.

-- fracruzam blaruiher erisancha ivaruicam abrdelrod
esSimetrica2 :: (Num a, Eq a) => Matrix a -> Bool
esSimetrica2 x = x == transpose x

-- ---------------------------------------------------------------------
-- Diagonales de una matriz                                           --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 17. Definir la función
--    diagonalPral :: Num a => Matrix a -> V.Vector a
-- tal que (diagonalPral p) es la diagonal principal de la matriz p. Por
-- ejemplo, 
--    ghci> let p = listaMatriz [[5,1,0],[3,2,6]]
--    ghci> diagonalPral p
--    fromList [5,2]
-- ---------------------------------------------------------------------

-- alvalvdom1 manvermor fracruzam erisancha ivaruicam abrdelrod
diagonalPral :: Num a => Matrix a -> V.Vector a
diagonalPral = getDiag


-- manpende
diagonalPral2 :: Num a => Matrix a -> V.Vector a
diagonalPral2 p = listaVector [ p ! (i,i) | i <- [1..m p]]
    where m p = minimum (ncols p, nrows p)

-- Comentario: La definición anterior se puede simplificar.

-- ---------------------------------------------------------------------
-- Ejercicio 18. Definir la función
--    diagonalSec :: Num a => Matrix a -> V.Vector a
-- tal que (diagonalSec p) es la diagonal secundaria de la matriz p. Por
-- ejemplo, 
--    ghci> let p = listaMatriz [[5,1,0],[3,2,6]]
--    ghci> diagonalSec p
--    fromList [1,3]
--    ghci> let q = traspuesta p
--    ghci> matrizLista q
--    [[5,3],[1,2],[0,6]]
--    ghci> diagonalSec q
--    fromList [1,2]
-- ---------------------------------------------------------------------

-- alvalvdom1 manpende abrdelrod
diagonalSec :: Num a => Matrix a -> V.Vector a
diagonalSec p = listaVector [p!(i,n-i+1) | i <- [1..n]]
    where n = min (numFilas p) (numColumnas p)

-- manvermor erisancha
diagonalSec2 :: Num a => Matrix a -> V.Vector a
diagonalSec2 p = V.fromList [ p!(i,m-i+1) | i <- [1..m]]
    where m = min (nrows p) (ncols p)

-- ---------------------------------------------------------------------
-- Submatrices                                                        --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 19. Definir la función
--    submatriz :: Num a => Int -> Int -> Matrix a -> Matrix a
-- tal que (submatriz i j p) es la matriz obtenida a partir de la p
-- eliminando la fila i y la columna j. Por ejemplo,
--    ghci> let p = listaMatriz [[5,1,0],[3,2,6],[4,6,9]]
--    ghci> submatriz 2 3 p
--    ( 5 1 )
--    ( 4 6 )
-- ---------------------------------------------------------------------

-- alvalvdom1 manvermor fracruzam manpende blaruiher erisancha ivaruicam
-- abrdelrod 
submatriz :: Num a => Int -> Int -> Matrix a -> Matrix a
submatriz = minorMatrix

-- ---------------------------------------------------------------------
-- Transformaciones elementales                                       --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 20. Definir la función
--    intercambiaFilas :: Num a => Int -> Int -> Matrix a -> Matrix a
-- tal que (intercambiaFilas k l p) es la matriz obtenida intercambiando
-- las filas k y l de la matriz p. Por ejemplo, 
--    ghci> let p = listaMatriz [[5,1,0],[3,2,6],[4,6,9]]
--    ghci> intercambiaFilas 1 3 p
--    ( 4 6 9 )
--    ( 3 2 6 )
--    ( 5 1 0 )
-- ---------------------------------------------------------------------

-- alvalvdom1 manvermor fracruzam blaruiher ivaruicam abrdelrod
intercambiaFilas :: Num a => Int -> Int -> Matrix a -> Matrix a
intercambiaFilas = switchRows

-- manpende erisancha
-- Sin utilizar las funciones predefinidas, se puede definir como:
intercambiaFilas2 :: Num a => Int -> Int -> Matrix a -> Matrix a
intercambiaFilas2 k l p = listaMatriz $ map vectorLista 
           [filaMat' i k l p | i <- [1..ncols p]]
    where filaMat' i k l p |i == k = filaMat l p
                           |i == l = filaMat k p
                           | otherwise = filaMat i p 

-- ---------------------------------------------------------------------
-- Ejercicio 21. Definir la función
--    intercambiaColumnas :: Num a => Int -> Int -> Matrix a -> Matrix a
-- tal que (intercambiaColumnas k l p) es la matriz obtenida
-- intercambiando las columnas k y l de la matriz p. Por ejemplo, 
--    ghci> let p = listaMatriz [[5,1,0],[3,2,6],[4,6,9]]
--    ghci> intercambiaColumnas 1 3 p
--    ( 0 1 5 )
--    ( 6 2 3 )
--    ( 9 6 4 )
-- ---------------------------------------------------------------------

-- alvalvdom1 manvermor fracruzam blaruiher ivaruicam abrdelrod
intercambiaColumnas :: Num a => Int -> Int -> Matrix a -> Matrix a
intercambiaColumnas = switchCols

-- manpende
intercambiaColumnas2 :: Num a => Int -> Int -> Matrix a -> Matrix a
intercambiaColumnas2 k l p = 
    traspuesta $ listaMatriz (map vectorLista [columnaMat' i k l p 
                                               | i <- [1..nrows p]])
        where columnaMat' i k l p | i == k = columnaMat l p
                                  | i == l = columnaMat k p
                                  | otherwise = columnaMat i p

-- Comentario: La definición anterior se puede simplificar.

-- ---------------------------------------------------------------------
-- Ejercicio 22. Definir la función
--    multFilaPor :: Num a => Int -> a -> Matrix a -> Matrix a
-- tal que (multFilaPor k x p) es la matriz obtenida multiplicando la
-- fila k de la matriz p por el número x. Por ejemplo,
--    ghci> let p = listaMatriz [[5,1,0],[3,2,6],[4,6,9]]
--    ghci> multFilaPor 2 3 p
--    (  5  1  0 )
--    (  9  6 18 )
--    (  4  6  9 )
-- ---------------------------------------------------------------------

-- alvalvdom1 manvermor blaruiher
multFilaPor :: Num a => Int -> a -> Matrix a -> Matrix a
multFilaPor k x p = scaleRow x k p

-- fracruzam ivaruicam abrdelrod
multFilaPor2 :: Num a => Int -> a -> Matrix a -> Matrix a
multFilaPor2 k x = scaleRow x k

-- manpende
multFilaPor3 :: Num a => Int -> a -> Matrix a -> Matrix a
multFilaPor3 k x p = 
    listaMatriz [filaMat' i k x p | i <- [1..ncols p]]
     where filaMat' i k x p | i == k = map (*x) (vectorLista (filaMat i p))
                            | otherwise = vectorLista (filaMat i p)

-- Comentario: La definición anterior se puede simplificar.

-- ---------------------------------------------------------------------
-- Ejercicio 23. Definir la función
--    sumaFilaFila :: Num a => Int -> Int -> Matrix a -> Matrix a
-- tal que (sumaFilaFila k l p) es la matriz obtenida sumando la fila l
-- a la fila k de la matriz p. Por ejemplo,
--    ghci> let p = listaMatriz [[5,1,0],[3,2,6],[4,6,9]]
--    ghci> sumaFilaFila 2 3 p
--    (  5  1  0 )
--    (  7  8 15 )
--    (  4  6  9 )
-- ---------------------------------------------------------------------

-- alvalvdom1 manvermor manpende blaruiher
sumaFilaFila :: Num a => Int -> Int -> Matrix a -> Matrix a
sumaFilaFila k l p = combineRows k 1 l p

-- fracruzam ivaruicam abrdelrod
sumaFilaFila2 :: Num a => Int -> Int -> Matrix a -> Matrix a
sumaFilaFila2 k l = combineRows k 1 l

-- ---------------------------------------------------------------------
-- Ejercicio 24. Definir la función
--    sumaFilaPor :: Num a => Int -> Int -> a -> Matrix a -> Matrix a
-- tal que (sumaFilaPor k l x p) es la matriz obtenida sumando a la fila
-- k de la matriz p la fila l multiplicada por x. Por ejemplo,
--    ghci> let p = listaMatriz [[5,1,0],[3,2,6],[4,6,9]]
--    ghci> sumaFilaPor 2 3 10 p
--    (  5  1  0 )
--    ( 43 62 96 )
--    (  4  6  9 )
-- ---------------------------------------------------------------------

-- alvalvdom1 manvermor manpende
sumaFilaPor :: Num a => Int -> Int -> a -> Matrix a -> Matrix a
sumaFilaPor k l x p = combineRows k x l p

-- fracruzam ivaruicam abrdelrod
sumaFilaPor2 :: Num a => Int -> Int -> a -> Matrix a -> Matrix a
sumaFilaPor2 k l x = combineRows k x l

-- ---------------------------------------------------------------------
-- Triangularización de matrices                                      --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 25. Definir la función
--    buscaIndiceDesde :: (Num a, Eq a) => 
--                        Matrix a -> Int -> Int -> Maybe Int
-- tal que (buscaIndiceDesde p j i) es el menor índice k, mayor o igual
-- que i, tal que el elemento de la matriz p en la posición (k,j) es no
-- nulo. Por ejemplo,
--    ghci> let p = listaMatriz [[5,1,0],[3,2,6],[4,6,9]]
--    ghci> buscaIndiceDesde p 3 2
--    Just 2
--    ghci> let q = listaMatriz [[5,1,1],[3,2,0],[4,6,0]]
--    ghci> buscaIndiceDesde q 3 2
--    Nothing
-- ---------------------------------------------------------------------

-- manvermor
buscaIndiceDesde :: (Num a, Eq a) => Matrix a -> Int -> Int -> Maybe Int
buscaIndiceDesde p j i = if null xs 
                         then Nothing 
                         else Just (head xs)
    where xs = [ k | k <- [i..n], getElem k j p /= 0]
          n = ncols p

-- manpende alvalvdom1
buscaIndiceDesde2 :: (Num a, Eq a) => Matrix a -> Int -> Int -> Maybe Int
buscaIndiceDesde2 p j i 
    | not(null xs) = Just (head xs) 
    | otherwise     = Nothing 
    where xs = [k | k <- [i..nrows p], p ! (k,j) /= 0] 

-- ivaruicam abrdelrod
buscaIndiceDesde3 :: (Num a, Eq a) => Matrix a -> Int -> Int -> Maybe Int
buscaIndiceDesde3 p j i = if null t then Nothing else Just (head t)
    where t = [k | k <- [i..nrows p] , getElem k j p /= 0]

-- ---------------------------------------------------------------------
-- Ejercicio 26. Definir la función
--    buscaPivoteDesde :: (Num a, Eq a) => 
--                        Matrix a -> Int -> Int -> Maybe a
-- tal que (buscaPivoteDesde p j i) es el elemento de la matriz p en la
-- posición (k,j) donde k es (buscaIndiceDesde p j i). Por ejemplo,
--    ghci> let p = listaMatriz [[5,1,0],[3,2,6],[4,6,9]]
--    ghci> buscaPivoteDesde p 3 2
--    Just 6
--    ghci> let q = listaMatriz [[5,1,1],[3,2,0],[4,6,0]]
--    ghci> buscaPivoteDesde q 3 2
--    Nothing
-- ---------------------------------------------------------------------

-- manvermor alvalvdom1
buscaPivoteDesde :: (Num a, Eq a) => Matrix a -> Int -> Int -> Maybe a
buscaPivoteDesde p j i 
    | buscaIndiceDesde p j i == Nothing = Nothing
    | otherwise = Just (getElem (f $ buscaIndiceDesde p j i) j p)
    where f (Just a) = a

-- manpende
buscaPivoteDesde2 :: (Num a, Eq a) => Matrix a -> Int -> Int -> Maybe a
buscaPivoteDesde2 p j i 
    | buscaIndiceDesde p j i == Nothing = Nothing
    | otherwise = Just (p ! (k,j))
    where k = head [l | l <- [i..nrows p], p!(l,j) /= 0]

-- ivaruicam abrdelrod
buscaPivoteDesde3 :: (Num a, Eq a) => Matrix a -> Int -> Int -> Maybe a
buscaPivoteDesde3 p j i = if null t then Nothing else Just (head t)
    where t = [getElem k j p| k <- [i..nrows p] , getElem k j p /= 0]

-- ---------------------------------------------------------------------
-- Ejercicio 27. Definir la función
--    anuladaColumnaDesde :: (Num a, Eq a) => 
--                           Int -> Int -> Matrix a -> Bool
-- tal que (anuladaColumnaDesde j i p) se verifica si todos los
-- elementos de la columna j de la matriz p desde i+1 en adelante son
-- nulos. Por ejemplo,
--    ghci> let q = listaMatriz [[5,1,1],[3,2,0],[4,6,0]]
--    ghci> anuladaColumnaDesde q 3 2
--    True
--    ghci> let p = listaMatriz [[5,1,0],[3,2,6],[4,6,9]]
--    ghci> anuladaColumnaDesde p 3 2
--    False
-- ---------------------------------------------------------------------

-- manvermor alvalvdom1 ivaruicam abrdelrod
anuladaColumnaDesde :: (Num a, Eq a) => Matrix a -> Int -> Int -> Bool
anuladaColumnaDesde p j i = 
    buscaIndiceDesde p j (i+1) == Nothing

-- manpende
anuladaColumnaDesde2 :: (Num a, Eq a) => Matrix a -> Int -> Int -> Bool
anuladaColumnaDesde2 p j i = 
    all (==Nothing) [buscaIndiceDesde p j l | l <- [i+1..nrows p]]

-- ---------------------------------------------------------------------
-- Ejercicio 28. Definir la función
--    anulaEltoColumnaDesde :: (Fractional a, Eq a) => 
--                             Matrix a -> Int -> Int -> Matrix a
-- tal que (anulaEltoColumnaDesde p j i) es la matriz obtenida a partir
-- de p anulando el primer elemento de la columna j por debajo de la
-- fila i usando el elemento de la posición (i,j). Por ejemplo,
--    ghci> let p = listaMatriz [[2,3,1],[5,0,5],[8,6,9]] :: Matrix Double
--    ghci> matrizLista (anulaEltoColumnaDesde p 2 1)
--    [[2.0,3.0,1.0],[5.0,0.0,5.0],[4.0,0.0,7.0]]
-- ---------------------------------------------------------------------

-- manvermor alvalvdom1 abrdelrod
anulaEltoColumnaDesde :: (Fractional a, Eq a) => 
                         Matrix a -> Int -> Int -> Matrix a
anulaEltoColumnaDesde p j i 
    | anuladaColumnaDesde p j i = p
    | otherwise = combineRows k x i p
    where k = f $ buscaIndiceDesde p j (i+1)
          x = - (f $ buscaPivoteDesde p j (i+1)) / (getElem i j p)
          f (Just a) = a

-- manpende
anulaEltoColumnaDesde2 :: (Fractional a, Eq a) =>
                         Matrix a -> Int -> Int -> Matrix a
anulaEltoColumnaDesde2 p j i 
         | anuladaColumnaDesde p j i = p
         | otherwise = anulaEltoColumnaDesde (combineRows k (-x) i p) j i
         where k = head [l | l <- [i+1..nrows p], p!(l,j) /= 0]
               x = (p ! (k,j)) / (p ! (i,j))

-- ivaruicam
anulaEltoColumnaDesde3 :: (Fractional a, Eq a) => 
                         Matrix a -> Int -> Int -> Matrix a
anulaEltoColumnaDesde3 p j i 
    | anuladaColumnaDesde p j i = p
    | otherwise = combineRows k (-x) i p
    where k = head [l | l <- [i+1..nrows p], p!(l,j) /= 0]
          x = (p ! (k,j)) / (p ! (i,j))

-- En mi interpretación he intentado que elimine un elemento de la
-- columna, no la columna entera como ocurre en el siguiente.  

-- ---------------------------------------------------------------------
-- Ejercicio 29. Definir la función
--    anulaColumnaDesde :: (Fractional a, Eq a) => 
--                         Matrix a -> Int -> Int -> Matrix a
-- tal que (anulaColumnaDesde p j i) es la matriz obtenida anulando
-- todos los elementos de la columna j de la matriz p por debajo del la
-- posición (i,j) (se supone que el elemnto p_(i,j) es no nulo). Por
-- ejemplo, 
--    ghci> let p = listaMatriz [[2,2,1],[5,4,5],[10,8,9]] :: Matrix Double
--    ghci> matrizLista (anulaColumnaDesde p 2 1)
--    [[2.0,2.0,1.0],[1.0,0.0,3.0],[2.0,0.0,5.0]]
--    ghci> let p = listaMatriz [[4,5],[2,7%2],[6,10]] 
--    ghci> matrizLista (anulaColumnaDesde p 1 1)
--    [[4 % 1,5 % 1],[0 % 1,1 % 1],[0 % 1,5 % 2]]
-- ---------------------------------------------------------------------

-- manvermor manpende alvalvdom1 abrdelrod
anulaColumnaDesde :: (Fractional a, Eq a) => 
                     Matrix a -> Int -> Int -> Matrix a
anulaColumnaDesde p j i 
    | anuladaColumnaDesde p j i = p
    | otherwise =  anulaColumnaDesde (anulaEltoColumnaDesde p j i)  j i

--ivaruicam 
anulaColumnaDesde2 :: (Fractional a, Eq a) => 
                     Matrix a -> Int -> Int -> Matrix a
anulaColumnaDesde2 p j i 
    | anuladaColumnaDesde p j i = p
    | otherwise = anulaColumnaDesde (anulaEltoColumnaDesde p j i) j i

-- ---------------------------------------------------------------------
-- Algoritmo de Gauss para triangularizar matrices                    --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 30. Definir la función
--    elementosNoNulosColDesde :: (Num a, Eq a) => 
--                                Matrix a -> Int -> Int -> [a]
-- tal que (elementosNoNulosColDesde p j i) es la lista de los elementos
-- no nulos de la columna j a partir de la fila i. Por ejemplo,
--    ghci> let p = listaMatriz [[3,2],[5,1],[0,4]]
--    ghci> elementosNoNulosColDesde p 1 2
--    [5]
-- ---------------------------------------------------------------------

-- manvermor manpende alvalvdom1 ivaruicam
elementosNoNulosColDesde :: (Num a, Eq a) => Matrix a -> Int -> Int -> [a]
elementosNoNulosColDesde p j i = 
  [ getElem k j p | k <- [i..nrows p], getElem k j p /= 0]

-- abrdelrod
elementosNoNulosColDesde2 :: (Num a, Eq a) => Matrix a -> Int -> Int -> [a]
elementosNoNulosColDesde2 p j i = 
    filter (/=0) [getElem k j p | k <- [i..nrows p]]

-- ---------------------------------------------------------------------
-- Ejercicio 31. Definir la función
--    existeColNoNulaDesde :: (Num a, Eq a) => 
--                            Matrix a -> Int -> Int -> Bool
-- tal que (existeColNoNulaDesde p j i) se verifica si la matriz p tiene
-- una columna a partir de la j tal que tiene algún elemento no nulo por
-- debajo de la j; es decir, si la submatriz de p obtenida eliminando
-- las i-1 primeras filas y las j-1 primeras columnas es no nula. Por
-- ejemplo, 
--    ghci> let p = listaMatriz [[3,2,5],[5,0,0],[6,0,0]]
--    ghci> existeColNoNulaDesde p 2 2
--    False
--    ghci> let q = listaMatriz [[3,2,5],[5,7,0],[6,0,0]]
--    ghci> existeColNoNulaDesde q 2 2
-- ---------------------------------------------------------------------
  
-- manvermor abrdelrod
existeColNoNulaDesde :: (Num a, Eq a) => Matrix a -> Int -> Int -> Bool
existeColNoNulaDesde p j i = minorMatrix (i-1) (j-1) p /= zero m n
    where (m,n) = (nrows p -1, ncols p -1)

-- manpende
existeColNoNulaDesde2 :: (Num a, Eq a) => Matrix a -> Int -> Int -> Bool
existeColNoNulaDesde2 p j i = 
    any (/= Nothing)  [buscaIndiceDesde2 p l i | l <- [j..ncols p]]

-- alvalvdom1 ivaruicam
existeColNoNulaDesde3 :: (Num a, Eq a) => Matrix a -> Int -> Int -> Bool
existeColNoNulaDesde3 p j i = 
    any (/= []) [elementosNoNulosColDesde p x i | x <- [j..ncols p]]

-- ---------------------------------------------------------------------
-- Ejercicio 32. Definir la función
--    menorIndiceColNoNulaDesde :: (Num a, Eq a) => 
--                                 Matrix a -> Int -> Int -> Maybe Int
-- tal que (menorIndiceColNoNulaDesde p j i) es el índice de la primera
-- columna, a partir de la j, en el que la matriz p tiene un elemento no
-- nulo a partir de la fila i. Por ejemplo,
--    ghci> let p = listaMatriz [[3,2,5],[5,7,0],[6,0,0]]
--    ghci> menorIndiceColNoNulaDesde p 2 2
--    Just 2
--    ghci> let q = listaMatriz [[3,2,5],[5,0,0],[6,0,2]]
--    ghci> menorIndiceColNoNulaDesde q 2 2
--    Just 3
--    ghci> let r = listaMatriz [[3,2,5],[5,0,0],[6,0,0]]
--    ghci> menorIndiceColNoNulaDesde r 2 2
--    Nothing
-- ---------------------------------------------------------------------

-- manvermor alvalvdom1
menorIndiceColNoNulaDesde :: (Num a, Eq a) => 
                             Matrix a -> Int -> Int -> Maybe Int
menorIndiceColNoNulaDesde p j i = 
    if existeColNoNulaDesde p j i 
    then Just $ head [ l | l <- [j..ncols p], 
                           elementosNoNulosColDesde p l i /= []] 
    else Nothing

-- manpende ivaruicam abrdelrod
menorIndiceColNoNulaDesde2 :: (Num a, Eq a) => 
                             Matrix a -> Int -> Int -> Maybe Int
menorIndiceColNoNulaDesde2 p j i 
     | existeColNoNulaDesde p j i = Just (head xs) 
     | otherwise = Nothing
    where xs = [ l | l <- [j..ncols p], buscaIndiceDesde p l i /= Nothing]

-- ---------------------------------------------------------------------
-- Ejercicio 33. Definir la función
--    gaussAux :: (Fractional a, Eq a) => 
--                Matrix a -> Int -> Int -> Matrix a
-- tal que (gaussAux p i j) es la matriz que en el que las i-1 primeras
-- filas y las j-1 primeras columnas son las de p y las restantes están
-- triangularizadas por el método de Gauss; es decir,
--    1. Si la dimensión de p es (i,j), entonces p.
--    2. Si la submatriz de p sin las i-1 primeras filas y las j-1
--       primeras columnas es nulas, entonces p.
--    3. En caso contrario, (gaussAux p' (i+1) (j+1)) siendo
--    3.1. j' la primera columna a partir de la j donde p tiene
--         algún elemento no nulo a partir de la fila i,
--    3.2. p1 la matriz obtenida intercambiando las columnas j y j'
--         de p,
--    3.3. i' la primera fila a partir de la i donde la columna j de
--         p1 tiene un elemento no nulo,
--    3.4. p2 la matriz obtenida intercambiando las filas i e i' de
--         la matriz p1 y
--    3.5. p' la matriz obtenida anulando todos los elementos de la
--         columna j de p2 por debajo de la fila i.
-- Por ejemplo,
--    ghci> let p = listaMatriz [[1.0,2,3],[1,2,4],[3,2,5]]
--    ghci> gaussAux p 2 2
--    ( 1.0 2.0 3.0 )
--    ( 1.0 2.0 4.0 )
--    ( 2.0 0.0 1.0 )
-- ---------------------------------------------------------------------

-- manvermor alvalvdom1 abrdelrod
gaussAux :: (Fractional a, Eq a) => Matrix a -> Int -> Int -> Matrix a
gaussAux p i j | dimension p == (i,j) = p
               | not (existeColNoNulaDesde p j i) = p
               | otherwise = gaussAux p' (i+1) (j+1) 
                 where j' = g $ menorIndiceColNoNulaDesde p j i
                       p1 = switchCols j j' p
                       i' = g $ buscaIndiceDesde p1 j i
                       p2 = switchRows i i' p1
                       p' = anulaColumnaDesde p2 j i
                       g (Just a) = a

-- ---------------------------------------------------------------------
-- Ejercicio 34. Definir la función
--    gauss :: (Fractional a, Eq a) => Matrix a -> Matrix a
-- tal que (gauss p) es la triangularización de la matriz p por el método
-- de Gauss. Por ejemplo, 
--    ghci> let p = listaMatriz [[1.0,2,3],[1,2,4],[1,2,5]]
--    ghci> gauss p
--    ( 1.0 3.0 2.0 )
--    ( 0.0 1.0 0.0 )
--    ( 0.0 0.0 0.0 )
--    ghci> let p = listaMatriz [[3%1,2,3],[1,2,4],[1,2,5]]
--    ghci> gauss p
--    ( 3 % 1 2 % 1 3 % 1 )
--    ( 0 % 1 4 % 3 3 % 1 )
--    ( 0 % 1 0 % 1 1 % 1 )
--    ghci> let p = listaMatriz [[1.0,0,3],[1,0,4],[3,0,5]]
--    ghci> gauss p
--    ( 1.0 3.0 0.0 )
--    ( 0.0 1.0 0.0 )
--    ( 0.0 0.0 0.0 )
-- ---------------------------------------------------------------------

-- manvermor alvalvdom1 abrdelrod
gauss :: (Fractional a, Eq a) => Matrix a -> Matrix a
gauss p = gaussAux p 1 1

-- ---------------------------------------------------------------------
-- Determinante                                                       --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 35. Definir la función
--    gaussCAux :: (Fractional a, Eq a) => 
--                 Matriz a -> Int -> Int -> Int -> Matriz a
-- tal que (gaussCAux p i j c) es el par (n,q) donde q es la matriz que
-- en el que las i-1 primeras filas y las j-1 primeras columnas son las
-- de p y las restantes están triangularizadas por el método de Gauss;
-- es decir, 
--    1. Si la dimensión de p es (i,j), entonces p.
--    2. Si la submatriz de p sin las i-1 primeras filas y las j-1
--       primeras columnas es nulas, entonces p.
--    3. En caso contrario, (gaussAux p' (i+1) (j+1)) siendo
--    3.1. j' la primera columna a partir de la j donde p tiene
--         algún elemento no nulo a partir de la fila i,
--    3.2. p1 la matriz obtenida intercambiando las columnas j y j'
--         de p,
--    3.3. i' la primera fila a partir de la i donde la columna j de
--         p1 tiene un elemento no nulo,
--    3.4. p2 la matriz obtenida intercambiando las filas i e i' de
--         la matriz p1 y
--    3.5. p' la matriz obtenida anulando todos los elementos de la
--         columna j de p2 por debajo de la fila i.
-- y n es c más el número de intercambios de columnas y filas que se han 
-- producido durante el cálculo. Por ejemplo,
--    ghci> gaussCAux (fromLists [[1.0,2,3],[1,2,4],[1,2,5]]) 1 1 0
--    (1,( 1.0 3.0 2.0 )
--       ( 0.0 1.0 0.0 )
--       ( 0.0 0.0 0.0 ))
-- ---------------------------------------------------------------------

-- manvermor abrdelrod
gaussCAux :: (Fractional a, Eq a) => 
             Matrix a -> Int -> Int -> Int -> (Int,Matrix a)
gaussCAux p i j c  | dimension p == (i,j) = (c,p)
                   | not (existeColNoNulaDesde p j i) = (c,p)
                   | otherwise = gaussCAux (snd par) (i+1) (j+1) (fst par)
                    where j' = g $ menorIndiceColNoNulaDesde p j i
                          parA = (c+l, switchCols j j' p)
                          l = if j == j' then 0 else 1
                          i' = g $ buscaIndiceDesde (snd parA) j i
                          parB = (fst parA +x, switchRows i i' (snd parA))
                          x = if i == i' then 0 else 1
                          par = (fst parB , anulaColumnaDesde (snd parB) j i)
                          g (Just a) = a

-- alvalvdom1
gaussCAux2 :: (Fractional a, Eq a) => 
             Matrix a -> Int -> Int -> Int -> (Int,Matrix a)
gaussCAux2 p i j c 
    | dimension p == (i,j) = (c,p)                     
    | not (existeColNoNulaDesde p j i) = (c,p)                         
    | otherwise = gaussCAux p' (i+1) (j+1) c' 
    where j' = f (menorIndiceColNoNulaDesde p j i)               
          p1 = switchCols j j' p                    
          i' = f (buscaIndiceDesde p1 j i)                          
          p2 = switchRows i i' p1                         
          p' = anulaColumnaDesde p2 j i                          
          c' = c + signum (abs (j-j')) + signum (abs (i-i'))
          f (Just a) = a

-- ---------------------------------------------------------------------
-- Ejercicio 36. Definir la función
--    gaussC :: (Fractional a, Eq a) => Matriz a -> Matriz a
-- tal que (gaussC p) es el par (n,q), donde q es la triangularización
-- de la matriz p por el método de Gauss y n es el número de
-- intercambios de columnas y filas que se han producido durante el
-- cálculo. Por ejemplo,  
--    ghci> gaussC (fromLists [[1.0,2,3],[1,2,4],[1,2,5]])
--    (1, ( 1.0 3.0 2.0 )
--        ( 0.0 1.0 0.0 )
--        ( 0.0 0.0 0.0 )
-- ---------------------------------------------------------------------

-- manvermor alvalvdom1 abrdelrod
gaussC :: (Fractional a, Eq a) => Matrix a -> (Int,Matrix a)
gaussC p = gaussCAux p 1 1 0

-- ---------------------------------------------------------------------
-- Ejercicio 37. Definir la función
--    determinante :: (Fractional a, Eq a) => Matriz a -> a
-- tal que (determinante p) es el determinante de la matriz p. Por
-- ejemplo, 
--    ghci> determinante (fromLists [[1.0,2,3],[1,3,4],[1,2,5]])
--    2.0
-- ---------------------------------------------------------------------

-- manvermor
determinante :: (Fractional a, Eq a) => Matrix a -> a
determinante p = diagProd $ gauss p

determinante2 p = diagProd $ snd $ gaussC p

determinante3 p = detLaplace p

determinante4 p = detLU p

-- alvalvdom1 ivaruicam abrdelrod
determinante5 :: (Fractional a, Eq a) => Matrix a -> a
determinante5 = detLaplace
