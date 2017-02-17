-- I1M 2016-17: Relación 17 (10 de febrero de 2017)
-- Vectores y matrices con las librerías.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- El objetivo de esta relación es adaptar los ejercicios de las
-- relaciones 15 y 16 (sobre vectores y matrices) usando las librerías
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
import Data.Maybe

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

-- albcercid marjimcom marlobrip paumacpar eledejim2 glovizcas enrnarbej
-- cescarde monlagare pabrabmon antmorper3 marmerzaf eliguivil antlopgom2
-- joscasgom1 juaorture roscargar natmarmar2 alvfercen congomgom belbenzam
-- fatfervaz ignareeva felsuacor josdeher margarflo5 manruiber antbeacar
-- fraferpoy beagongon1 cargonler margarvil14
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

-- albcercid marjimcom marlobrip paumacpar eledejim2 glovizcas enrnarbej
-- cescarde monlagare pabrabmon antmorper3 marmerzaf eliguivil antlopgom2
-- joscasgom1 juaorture roscargar natmarmar2 alvfercen congomgom belbenzam
-- fatfervaz ignareeva felsuacor josdeher margarflo5 manruiber antbeacar
-- fraferpoy beagongon1 cargonler margarvil14
listaMatriz :: Num a => [[a]] -> Matrix a
listaMatriz = fromLists

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--    numFilas :: Num a => Matrix a -> Int
-- tal que (numFilas m) es el número de filas de la matriz m. Por
-- ejemplo,
--    numFilas (listaMatriz [[1,3,5],[2,4,7]])  ==  2
-- ---------------------------------------------------------------------

-- albcercid marjimcom marlobrip paumacpar eledejim2 glovizcas enrnarbej
-- cescarde monlagare pabrabmon antmorper3 marmerzaf eliguivil antlopgom2
-- joscasgom1 juaorture roscargar natmarmar2 alvfercen congomgom belbenzam
-- fatfervaz ignareeva felsuacor josdeher margarflo5 manruiber antbeacar
-- fraferpoy beagongon1 cargonler margarvil14
numFilas :: Num a => Matrix a -> Int
numFilas = nrows

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función
--    numColumnas :: Num a => Matrix a -> Int
-- tal que (numColumnas m) es el número de columnas de la matriz
-- m. Por ejemplo,
--    numColumnas (listaMatriz [[1,3,5],[2,4,7]])  ==  3
-- ---------------------------------------------------------------------

-- albcercid marjimcom marlobrip paumacpar eledejim2 glovizcas enrnarbej
-- cescarde monlagare pabrabmon antmorper3 marmerzaf eliguivil antlopgom2
-- joscasgom1 juaorture roscargar natmarmar2 alvfercen congomgom belbenzam
-- fatfervaz ignareeva felsuacor josdeher margarflo5 manruiber antbeacar
-- fraferpoy margarvil14 beagongon1 cargonler
numColumnas :: Num a => Matrix a -> Int
numColumnas = ncols

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la función
--    dimension :: Num a => Matrix a -> (Int,Int)
-- tal que (dimension m) es la dimensión de la matriz m. Por ejemplo, 
--    dimension (listaMatriz [[1,3,5],[2,4,7]])  ==  (2,3)
-- ---------------------------------------------------------------------

-- albcercid marjimcom marlobrip paumacpar eledejim2 glovizcas enrnarbej
-- cescarde monlagare pabrabmon antmorper3 antlopgom2 joscasgom1 roscargar
-- congomgom fatfervaz ignareeva felsuacor margarflo5 manruiber
-- antbeacar fraferpoy beagongon1 cargonler
dimension :: Num a => Matrix a -> (Int,Int)
dimension p = (numFilas p, numColumnas p)

-- marmerzaf juaorture natmarmar2 alvfercen belbenzam josdeher
-- margarvil14 
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

-- cescarde monlagare antmorper3 eliguivil juaorture natmarmar2 alvfercen
-- marmerzaf fatfervaz ignareeva josdeher margarflo5 manruiber
-- beagongon1 cargonler 
matrizLista :: Num a => Matrix a -> [[a]]
matrizLista = toLists

-- albcercid marjimcom marlobrip paumacpar eledejim2 glovizcas enrnarbej
-- pabrabmon antlopgom2 joscasgom1 roscargar congomgom belbenzam
-- felsuacor fraferpoy antbeacar margarvil14
matrizLista1 :: Num a => Matrix a -> [[a]]
matrizLista1 p = toLists p

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

-- albcercid marjimcom marlobrip paumacpar eledejim2 glovizcas enrnarbej
-- cescarde monlagare pabrabmon antmorper3 eliguivil antlopgom2 joscasgom1 
-- juaorture roscargar natmarmar2 alvfercen marmerzaf congomgom belbenzam
-- fatfervaz ignareeva felsuacor josdeher margarflo5 manruiber antbeacar
-- fraferpoy beagongon1 cargonler margarvil14
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

-- albcercid marjimcom marlobrip paumacpar eledejim2 enrnarbej 
-- cescarde monlagare glovizcas pabrabmon antmorper3 eliguivil roscargar
-- antlopgom2 joscasgom1 juaorture natmarmar2 alvfercen marmerzaf congomgom
-- fatfervaz ignareeva felsuacor josdeher margarflo5 manruiber antbeacar
-- fraferpoy 
-- beagongon1 cargonler margarvil14
sumaMatrices :: Num a => Matrix a -> Matrix a -> Matrix a
sumaMatrices p q = p + q

-- Comentario: La definición sumaMatrices se puede simplificar.

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

-- cescarde monlagare glovizcas pabrabmon antmorper3 eliguivil
-- joscasgom1 juaorture natmarmar2 alvfercen marmerzaf fatfervaz
-- ignareeva felsuacor josdeher margarflo5 manruiber antbeacar 
-- beagongon1 cargonler margarvil14
filaMat :: Num a => Int -> Matrix a -> V.Vector a
filaMat = getRow

-- albcercid 
filaMat2 :: Num a => Int -> Matrix a -> V.Vector a
filaMat2 i p = listaVector (toLists p !! (i-1))

-- marjimcom marlobrip paumacpar eledejim2 enrnarbej antlopgom2 roscargar
-- congomgom fraferpoy
filaMat3 :: Num a => Int -> Matrix a -> V.Vector a
filaMat3 i p = getRow i p

-- Comentario de enrnarbej: se puede simplificar

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

-- cescarde monlagare glovizcas pabrabmon antmorper3 eliguivil
-- jocasgom1 juaorture natmarmar2 alvfercen marmerzaf fatfervaz
-- ignareeva felsuacor josdeher margarflo5 manruiber antbeacar
-- beagongon1 cargonler margarvil14
columnaMat :: Num a => Int -> Matrix a -> V.Vector a
columnaMat = getCol

-- albcercid
columnaMat2 :: Num a => Int -> Matrix a -> V.Vector a
columnaMat2 j p = listaVector (map (!!(j-1)) (toLists p))

-- marjimcom marlobrip paumacpar eledejim2 enrnarbej antlopgom2 roscargar
-- congomgom fraferpoy
columnaMat23 :: Num a => Int -> Matrix a -> V.Vector a
columnaMat23 j p = getCol j p

-- Comentario de enrnarbej: Se puede simplificar

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

-- paumacpar eledejim2 glovizcas antmorper3 joscasgom1 alvfercen congomgom
-- felsuacor marlobrip marmerzaf  josdeher margarflo5 manruiber
-- antbeacar cargonler margarvil14 
prodEscalar :: Num a => V.Vector a -> V.Vector a -> a
prodEscalar v1 v2 = sum (V.zipWith (*) v1 v2)

-- albcercid marjimcom cescarde monlagare pabrabmon eliguivil antlopgom2 
-- roscargar natmarmar2 ignareeva fatfervaz fraferpoy beagongon1

prodEscalar3 :: Num a => V.Vector a -> V.Vector a -> a
prodEscalar3 v1 v2 = sum $ zipWith (*) (vectorLista v1) (vectorLista v2)

-- enrnarbej
prodEscalar4 :: Num a => V.Vector a -> V.Vector a -> a
prodEscalar4 v1 v2 = (head.head.toLists) (rowVector v1 * colVector v2)

-- juaorture
prodEscalar5 :: Num a => V.Vector a -> V.Vector a -> a
prodEscalar5 v1 v2 =
  head $ head $ toLists $
  fromLists [V.toList v1] * transpose (fromLists [V.toList v2])

-- juaorture : Comentario, se puede simplificar a la definición 4.

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

-- albcercid marjimcom marlobrip paumacpar eledejim2 enrnarbej monlagare
-- glovizcas pabrabmon antmorper3 eliguivil antlopgom2 joscasgom1 juaorture 
-- roscargar natmarmar2 alvfercen marmerzaf congomgom fatfervaz felsuacor
-- margarflo5 manruiber antbeacar fraferpoy beagongon1 cargonler
-- margarvil14 
prodMatrices :: Num a => Matrix a -> Matrix a -> Matrix a
prodMatrices p q = p*q

-- Comentario: La definición prodMatrices se puede simplificar.

-- cescarde josdeher
prodMatrices2 :: Num a => Matrix a -> Matrix a -> Matrix a
prodMatrices2 = multStd

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

-- albcercid marjimcom marlobrip paumacpar eledejim2 enrnarbej 
-- cescarde monlagare glovizcas pabrabmon antmorper3 eliguivil
-- antlopgom2 joscasgom1 juaorture roscargar natmarmar2 alvfercen
-- marmerzaf congomgom fatfervaz felsuacor josdeher margarflo5 manruiber  
-- antbeacar fraferpoy beagongon1 cargonler margarvil14
traspuesta :: Num a => Matrix a -> Matrix a
traspuesta = transpose

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

-- albcercid marjimcom enrnarbej cescarde antlopgom2 joscasgom1
-- juaorture roscargar fatfervaz margarflo5 manruiber antbeacar fraferpoy
-- beagongon1 cargonler
esCuadrada :: Num a => Matrix a -> Bool
esCuadrada p = n == m
  where (n,m) = dimension p

-- marlobrip paumacpar eledejim2 monlagare glovizcas pabrabmon antmorper3 
-- eliguivil congomgom felsuacor
esCuadrada2 :: Num a => Matrix a -> Bool
esCuadrada2 p = numColumnas p == numFilas p

-- natmarmar2 alvfercen marmerzaf josdeher margarvil14
esCuadrada3 :: Num a => Matrix a -> Bool
esCuadrada3 p = nrows p == ncols p

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

-- albcercid marjimcom marlobrip paumacpar eledejim2 enrnarbej  roscargar
-- cescarde monlagare glovizcas pabrabmon antmorper3 eliguivil
-- antlopgom2 joscasgom1 juaorture natmarmar2 alvfercen marmerzaf fatfervaz
-- congomgom felsuacor josdeher margarflo5 manruiber antbeacar fraferpoy
-- beagongon1 cargonler margarvil14
esSimetrica :: (Num a, Eq a) => Matrix a -> Bool
esSimetrica x = traspuesta x == x

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

-- paumacpar eledejim2 enrnarbej cescarde monlagare glovizcas pabrabmon
-- antmorper3 marlobrip eliguivil joscasgom1 juaorture natmarmar2 alvfercen
-- marmerzaf fatfervaz congomgom felsuacor josdeher margarflo5 manruiber
-- antbeacar fraferpoy beagongon1 antlopgom2 cargonler margarvil14
diagonalPral :: Num a => Matrix a -> V.Vector a
diagonalPral = getDiag

-- albcercid marjimcom roscargar
diagonalPral2 :: Num a => Matrix a -> V.Vector a
diagonalPral2 p = listaVector [ p!(i,i) | i <- [1..n] ]
  where n = min (numFilas p) (numColumnas p)

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

-- albcercid marjimcom monlagare pabrabmon eliguivil joscasgom1 roscargar
-- congomgom josdeher cescarde antlopgom2 cargonler
diagonalSec :: Num a => Matrix a -> V.Vector a
diagonalSec p =  listaVector [p!(i,t+1-i) | i <- [1..t]]
  where t     = min m n
        (m,n) = dimension p

-- paumacpar eledejim2 enrnarbej antmorper3 natmarmar2 fatfervaz
-- margarflo5 manruiber antbeacar beagongon1
diagonalSec2 :: Num a => Matrix a -> V.Vector a
diagonalSec2 p = diagonalPral (switchCols 1 m p)
  where m = min (numFilas p) (numColumnas p)

-- Comentario de Josdeher: Las funciones diagonalSec y diagonalSec2 no
-- son equivalentes 
--    let p = listaMatriz [[0,1,2,3],[4,5,6,7],[8,9,10,11],[12,13,14,15]]
--    diagonalSec p  =   fromList [3,6,9,12]
--    diagonalSec2 p =   fromList [3,5,10,12]
--
--    λ> let p = listaMatriz [[0,1,2,3],[4,5,6,7],[8,9,10,11],[12,13,14,15]]
--    λ> p
--    (  0  1  2  3 )
--    (  4  5  6  7 )
--    (  8  9 10 11 )
--    ( 12 13 14 15 )
--    
--    λ> diagonalSec p
--    [3,6,9,12]
--    λ> diagonalSec2 p
--    [3,5,10,12]

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

-- albcercid marlobrip marjimcom paumacpar eledejim2 enrnarbej 
-- cescarde monlagare glovizcas pabrabmon antmorper3 eliguivil roscargar
-- joscasgom1 juaorture natmarmar2 alvfercen marmerzaf fatfervaz congomgom
-- josdeher margarflo5 manruiber antbeacar fraferpoy beagongon1
-- antlopgom2 cargonler margarvil14 
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

-- albcercid marlobrip marjimcom paumacpar eledejim2 enrnarbej 
-- cescarde monlagare glovizcas pabrabmon antmorper3 eliguivil
-- joscasgom1 juaorture roscargar natmarmar2 alvfercen marmerzaf fatfervaz
-- congomgom josdeher margarflo5 manruiber antbeacar fraferpoy
-- beagongon1 antlopgom2 cargonler margarvil14
intercambiaFilas :: Num a => Int -> Int -> Matrix a -> Matrix a
intercambiaFilas = switchRows

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

-- albcercid marlobrip marjimcom eledejim2 enrnarbej cescarde monlagare
-- glovizcas pabrabmon antmorper3 eliguivil joscasgom1 juaorture roscargar
-- paumacpar natmarmar2 alvfercen marmerzaf fatfervaz congomgom josdeher
-- margarflo5 manruiber antbeacar fraferpoy beagongon1 antlopgom2
-- cargonler margarvil14 
intercambiaColumnas :: Num a => Int -> Int -> Matrix a -> Matrix a
intercambiaColumnas = switchCols

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

-- albcercid marlobrip marjimcom eledejim2 enrnarbej cescarde monnlagare
-- glovizcas pabrabmon antmorper3 eliguivil joscasgom1 juaorture roscargar
-- paumacpar natmarmar2 alvfercen marmerzaf fatfervaz congomgom josdeher
-- margarflo5 manruiber antbeacar beagongon1 antlopgom2 cargonler margarvil14
multFilaPor :: Num a => Int -> a -> Matrix a -> Matrix a
multFilaPor k x p = scaleRow x k p

-- ---------------------------------------------------------------------
-- Ejercicio 23. Definir la función
--    sumaFilaFila :: Num a => Int -> Int -> Matrix a -> Matrix a
-- tal que (sumaFilaFila k l p) es la matriz obtenida sumando la fila l
-- a la fila k d la matriz p. Por ejemplo,
--    ghci> let p = listaMatriz [[5,1,0],[3,2,6],[4,6,9]]
--    ghci> sumaFilaFila 2 3 p
--    (  5  1  0 )
--    (  7  8 15 )
--    (  4  6  9 )
-- ---------------------------------------------------------------------

-- albcercid marlobrip marjimcom eledejim2 enrnarbej cescarde monlagare
-- glovizcas pabrabmon antmorper3 eliguivil joscasgom1 juaorture roscargar
-- paumacpar marmerzaf fatfervaz congomgom josdeher margarflo5 manruiber
-- antbeacar beagongon1 antlopgom2 cargonler margarvil14
sumaFilaFila :: Num a => Int -> Int -> Matrix a -> Matrix a
sumaFilaFila k l p = combineRows k 1 l p

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

-- albcercid marlobrip marjimcom eledejim2 enrnarbej cescarde monlagare
-- roscargar glovizcas pabrabmon antmorper3 eliguivil joscasgom1
-- juaorture paumacpar marmerzaf fatfervaz congomgom josdeher margarflo5
-- manruiber antbeacar beagongon1 antlopgom2 cargonler margarvil14
sumaFilaPor :: Num a => Int -> Int -> a -> Matrix a -> Matrix a
sumaFilaPor k l x p = combineRows k x l p

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

-- albcercid marjimcom cescarde monlagare antmorper3 joscasgom1
-- marlobrip roscargar paumacpar marmerzaf josdeher congomgom
-- margarflo5 manruiber antbeacar beagongon1 antlopgom2 cargonler
-- margarvil14 
buscaIndiceDesde :: (Num a, Eq a) => Matrix a -> Int -> Int -> Maybe Int
buscaIndiceDesde p j i
  | null t    = Nothing
  | otherwise = Just (head t)
  where t = [ k | k <- [i..numFilas p], p!(k,j) /= 0]

-- enrnarbej pabrabmon glovizcas
buscaIndiceDesde2 :: (Num a, Eq a) => Matrix a -> Int -> Int -> Maybe Int
buscaIndiceDesde2 p j i =
  (V.find (i<=) . fmap (+1) . V.findIndices (/=0) . getCol j) p

-- eliguivil
buscaIndiceDesde3 :: (Num a, Eq a) => Matrix a -> Int -> Int -> Maybe Int
buscaIndiceDesde3 p j i
  | null t    = Nothing
  | otherwise = Just (fromJust t + i)
  where
    t = V.findIndex (/=0) (V.drop (i-1) (getCol j p))

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
  
-- albcercid marjimcom cescarde monlagare antmorper3 joscasgom1 roscargar
-- paumacpar marmerzaf josdeher congomgom margarflo5 manruiber antbeacar
-- antlopgom2 cargonler
buscaPivoteDesde :: (Num a, Eq a) => Matrix a -> Int -> Int -> Maybe a
buscaPivoteDesde p j i
  | null t    = Nothing
  | otherwise = Just (head t)
  where t = [ p!(k,j) | k <- [i..numFilas p], p!(k,j) /= 0]

-- enrnarbej pabrabmon glovizcas
buscaPivoteDesde2 :: (Num a, Eq a) => Matrix a -> Int -> Int -> Maybe a
buscaPivoteDesde2 p j i = (\k -> p!(k,j)) <$> buscaIndiceDesde2 p j i

-- eliguivil margarvil14
buscaPivoteDesde3 :: (Num a, Eq a) => Matrix a -> Int -> Int -> Maybe a
buscaPivoteDesde3 p j i
  | null k = Nothing
  | otherwise = Just (getElem (fromJust k) j p)
  where
    k = buscaIndiceDesde p j i

-- marlobrip
buscaPivoteDesde4 :: (Num a, Eq a) => Matrix a -> Int -> Int -> Maybe a
buscaPivoteDesde4 p j i
  | null w    = Nothing
  | otherwise = Just (p! (minimum w,j))
  where w = [k | k <- [i..numFilas p], p ! (k,j) /=0]   

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

-- albcercid cescarde monlagare glovizcas eliguivil joscasgom1 roscargar
-- paumacpar  marmerzaf josdeher congomgom manruiber antbeacar cargonler
anuladaColumnaDesde :: (Num a, Eq a) => Matrix a -> Int -> Int -> Bool
anuladaColumnaDesde p j i = Nothing == buscaIndiceDesde p j (i+1)

-- enrnarbej pabrabmon antmorper3
anuladaColumnaDesde2 :: (Num a, Eq a) => Matrix a -> Int -> Int -> Bool
anuladaColumnaDesde2 p j i = (all (==0) . V.drop i . getCol j) p

-- marjimcom margarflo5
anuladaColumnaDesde3 :: (Num a, Eq a) => Matrix a -> Int -> Int -> Bool
anuladaColumnaDesde3 p j i = and [p!(t,j) == 0 | t <- [i+1..m]]
 where m = nrows p

-- marlobrip
anuladaColumnaDesde4 :: (Num a, Eq a) => Matrix a -> Int -> Int -> Bool
anuladaColumnaDesde4 p j i
  | null w   = True
  | otherwise = False
  where w = [p! (x,j) | x <- [i+1..numFilas p],  (p! (x,j))/= 0]

-- antlopgom2
anuladaColumnaDesde5 :: (Num a, Eq a) => Matrix a -> Int -> Int -> Bool
anuladaColumnaDesde5 p j i =
  all (==0) [p!(a,j) | a <- [i+1..numFilas p]] 

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

-- albcercid marjimcom joscasgom1 roscargar marmerzaf
anulaEltoColumnaDesde :: (Fractional a, Eq a) => 
                         Matrix a -> Int -> Int -> Matrix a
anulaEltoColumnaDesde p j i
  | q == Nothing = p
  | otherwise = sumaFilaPor t i x p
  where t = unJust q
        x = -(p!(t,j))/(p!(i,j))
        unJust (Just a) = a
        q = buscaIndiceDesde p j (i+1)

-- enrnarbej cescarde pabrabmon glovizcas antmorper3 paumacpar josdeher
-- congomgom margarflo5 manruiber cargonler antlopgom2
anulaEltoColumnaDesde2 :: (Fractional a, Eq a) => 
                         Matrix a -> Int -> Int -> Matrix a
anulaEltoColumnaDesde2 p j i
  | anuladaColumnaDesde p j i = p
  | otherwise = combineRows a x i p
  where
    Just a = buscaIndiceDesde p j (i+1)
    Just b = buscaPivoteDesde p j (i+1)
    x = (-1)*b/(p!(i,j))

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

-- albcercid marjimcom cescarde pabrabmon glovizcas antmorper3 eliguivil
-- roscargar josdeher congomgom margarflo5 manruiber cargonler antlopgom2
anulaColumnaDesde :: (Fractional a, Eq a) => 
                     Matrix a -> Int -> Int -> Matrix a
anulaColumnaDesde p j i
  | q == Nothing = p
  | otherwise    = anulaColumnaDesde x j i
  where x = anulaEltoColumnaDesde p j i
        q = buscaIndiceDesde p j (i+1)

-- enrnarbej paumacpar
anulaColumnaDesde2 :: (Fractional a, Eq a) => 
                     Matrix a -> Int -> Int -> Matrix a
anulaColumnaDesde2 p j i =
  until (\k -> anuladaColumnaDesde k j i)
        (\k -> anulaEltoColumnaDesde k j i)
        p

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

-- albcercid cescarde monlagare marjimcom pabrabmon glovizcas antmorper3
-- eliguivil roscargar paumacpar josdeher congomgom manruiber cargonler
elementosNoNulosColDesde :: (Num a, Eq a) => Matrix a -> Int -> Int -> [a]
elementosNoNulosColDesde p j i =
  vectorLista $ V.filter (/=0) (V.drop (i-1) (columnaMat j p))

-- enrnarbej
elementosNoNulosColDesde2 :: (Num a, Eq a) => Matrix a -> Int -> Int -> [a]
elementosNoNulosColDesde2 p j i =
   ( V.toList 
   . fmap (\k -> cv V.! (k-1))
   . V.dropWhile (<i)
   . fmap (+1)
   . V.findIndices (/=0))
   cv
  where cv = getCol j p

-- antlopgom2
elementosNoNulosColDesde3 :: (Num a, Eq a) => Matrix a -> Int -> Int -> [a]
elementosNoNulosColDesde3 p j i =
  [p!(a,j) | a <- [i..numFilas p], p!(a,j) /= 0]

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
  
-- albcercid enrnarbej cescarde monlagare marjimcom pabrabmon glovizcas
-- antmorper3 roscargar paumacpar marmerzaf josdeher congomgom manruiber
-- cargonler 
existeColNoNulaDesde :: (Num a, Eq a) => Matrix a -> Int -> Int -> Bool
existeColNoNulaDesde p j i = submatrix i m j n p /= zero (m-i+1) (n-j+1)
  where (m,n) = dimension p

-- eliguivil

existeColNoNulaDesde2 :: (Num a, Eq a) => Matrix a -> Int -> Int -> Bool
existeColNoNulaDesde2 p j i =
  all (/=0) $ submatrix i (nrows p -1) j (ncols p -1) p

-- Comentario: En lugar de
--    all (/=0)
-- se puede usar
--    notElem 0

-- antlopgom2
existeColNoNulaDesde3 :: (Num a, Eq a) => Matrix a -> Int -> Int -> Bool
existeColNoNulaDesde3 p j i
  | elementosNoNulosColDesde p j i == [] = False
  | otherwise                            = True

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

-- albcercid cescarde monlagare marjimcom pabrabmon glovizcas antmorper3
-- eliguivil roscargar paumacpar marmerzaf congomgom manruiber cargonler
menorIndiceColNoNulaDesde :: (Num a, Eq a) => 
                             Matrix a -> Int -> Int -> Maybe Int
menorIndiceColNoNulaDesde p j i
  | not t = Just j
  | j == numColumnas p = Nothing
  | otherwise = menorIndiceColNoNulaDesde p (j+1) i
  where t = anuladaColumnaDesde p j (i-1)

-- enrnarbej josdeher
menorIndiceColNoNulaDesde2 :: (Num a, Eq a) => 
                             Matrix a -> Int -> Int -> Maybe Int
menorIndiceColNoNulaDesde2 p j i
  | not (existeColNoNulaDesde p j i) = Nothing
  | otherwise                        = Just x
  where x = head [y | y <- [j..ncols p]
                    , not (null (elementosNoNulosColDesde p y i))]

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

-- albcercid enrnarbej cescarde pabrabmon marjimcom glovizcas antmorper3
-- eliguivil roscargar paumacpar marmerzaf josdeher congomgom manruiber
-- cargonler 
gaussAux :: (Fractional a, Eq a) => Matrix a -> Int -> Int -> Matrix a
gaussAux p i j | (m,n) == (i,j) || not (existeColNoNulaDesde p j i) = p
               | otherwise = gaussAux p' (i+1) (j+1)
  where (m,n) = dimension p
        j' = unJust (menorIndiceColNoNulaDesde p j i)
        unJust (Just a) = a
        p1 = switchCols j j' p
        i' = head [ a | a <- [i..m], p1!(a,j) /= 0]
        p2 = switchRows i i' p1
        p' = anulaColumnaDesde p2 j i

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

-- albcercid enrnarbej cescarde pabrabmon marjimcom glovizcas antmorper3
-- eliguivil roscargar paumacpar marmerzaf josdeher congomgom manruiber
-- cargonler 
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

-- albcercid enrnarbej cescarde pabrabmon marjimcom glovizcas antmorper3 
-- eliguivil roscargar paumacpar marmerzaf josdeher congomgom manruiber
-- cargonler 
gaussCAux :: (Fractional a, Eq a) => 
             Matrix a -> Int -> Int -> Int -> (Int,Matrix a)
gaussCAux p i j c
  | (m,n) == (i,j) || not (existeColNoNulaDesde p j i) = (c,p)
  | otherwise = gaussCAux p' (i+1) (j+1) (c + x + y)
  where (m,n) = dimension p
        j' = unJust (menorIndiceColNoNulaDesde p j i)
        unJust (Just a) = a
        p1 = switchCols j j' p
        i' = head [ a | a <- [i..m], p1!(a,j) /= 0]
        p2 = switchRows i i' p1
        p' = anulaColumnaDesde p2 j i
        x | i == i' = 0
          | otherwise = 1
        y | j == j' = 0
          | otherwise = 1

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

-- albcercid enrnarbej cescarde pabrabmon marjimcom glovizcas antmorper3
-- eliguivil roscargar paumacpar josdeher congomgom manruiber cargonler
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

-- albcercid cescarde monlagare pabrabmon marjimcom glovizcas antmorper3
-- eliguivil roscargar paumacpar marmerzaf fatfervaz congomgom manruiber
-- cargonler 
determinante :: (Fractional a, Eq a) => Matrix a -> a
determinante = detLaplace

-- enrnarbej

determinante2 :: (Fractional a, Eq a) => Matrix a -> a
determinante2 p = (-1)^n * (product . diagonalPral) m
  where
    (n,m) = gaussC p

-- josdeher

determinante3 :: (Fractional a, Eq a) => Matrix a -> a
determinante3 p = (-1)^n * diagProd m
   where (n,m) = gaussC p
