-- I1M 2016-17: Relación 15 (8 de febrero de 2017)
-- Vectores y matrices.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------
-- El objetivo de esta relación es hacer ejercicios sobre vectores y
-- matrices con el tipo de las tablas, definido en el módulo
-- Data.Array y explicado en el tema 18 cuyas transparencias se
-- encuentran en 
--    http://www.cs.us.es/~jalonso/cursos/i1m-16/temas/tema-18.html
 
-- ---------------------------------------------------------------------
-- Importación de librerías                                           --
-- ---------------------------------------------------------------------

import Data.Array
import Data.List
import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Tipos de los vectores y de las matrices                            --
-- ---------------------------------------------------------------------

-- Los vectores son tablas cuyos índices son números naturales.
type Vector a = Array Int a
 
-- Las matrices son tablas cuyos índices son pares de números
-- naturales. 
type Matriz a = Array (Int,Int) a

-- ---------------------------------------------------------------------
-- Operaciones básicas con matrices                                   --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    listaVector :: Num a => [a] -> Vector a
-- tal que (listaVector xs) es el vector correspondiente a la lista
-- xs. Por ejemplo, 
--    ghci> listaVector [3,2,5]
--    array (1,3) [(1,3),(2,2),(3,5)]
-- ---------------------------------------------------------------------

-- albcercid antlopgom2 fraferpoy enrnarbej roscargar juacasnie paumacpar
-- eliguivil juaorture antmorper3 josrodgal7 artmorfer eledejim2 josdeher
-- margarflo5 natruipin cescarde
listaVector :: Num a => [a] -> Vector a
listaVector xs = listArray (1,length xs) xs

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    listaMatriz :: Num a => [[a]] -> Matriz a
-- tal que (listaMatriz xss) es la matriz cuyas filas son los elementos
-- de xss. Por ejemplo,
--    ghci> listaMatriz [[1,3,5],[2,4,7]]
--    array ((1,1),(2,3)) [((1,1),1),((1,2),3),((1,3),5),
--                         ((2,1),2),((2,2),4),((2,3),7)]
-- ---------------------------------------------------------------------

-- albcercid enrnarbej roscargar antmorper3 eliguivil
listaMatriz :: Num a => [[a]] -> Matriz a
listaMatriz xss =
  listArray ((1,1),(length xss,length $ head xss)) (concat xss)

-- antlopgom2 fraferpoy juacasnie juaorture paumacpar josrodgal7 artmorfer
-- eledejim2 josdeher margarflo5 natruipin cescarde
listaMatriz2 :: Num a => [[a]] -> Matriz a
listaMatriz2 xss = listArray ((1,1),(m,n)) (concat xss)
  where m = length xss
        n = length (head xss)

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--    numFilas :: Num a => Matriz a -> Int
-- tal que (numFilas m) es el número de filas de la matriz m. Por
-- ejemplo,
--    numFilas (listaMatriz [[1,3,5],[2,4,7]])  ==  2
-- ---------------------------------------------------------------------

-- albcercid antlopgom2 fraferpoy enrnarbej roscargar juacasnie eliguivil
-- juaorture paumacpar antmorper3 eliguivil josrodgal7 eledejim2 josdeher
-- margarflo5 natruipin cescarde
numFilas :: Num a => Matriz a -> Int
numFilas = fst . snd . bounds

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función
--    numColumnas :: Num a => Matriz a -> Int
-- tal que (numColumnas m) es el número de columnas de la matriz
-- m. Por ejemplo,
--    numColumnas (listaMatriz [[1,3,5],[2,4,7]])  ==  3
-- ---------------------------------------------------------------------

-- albcercid antlopgom2 fraferpoy enrnarbej roscargar juacasnie
-- juaorture paumacpar antmorper3 eliguivil josrodgal7 eledejim2 josdeher
-- margarflo5 natruipin cescarde
numColumnas:: Num a => Matriz a -> Int
numColumnas = snd . snd . bounds

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la función
--    dimension :: Num a => Matriz a -> (Int,Int)
-- tal que (dimension m) es la dimensión de la matriz m. Por ejemplo, 
--    dimension (listaMatriz [[1,3,5],[2,4,7]])  ==  (2,3)
-- ---------------------------------------------------------------------

-- albcercid antlopgom2 fraferpoy enrnarbej roscargar juacasnie
-- juaorture paumacpar antmorper3 eliguivil josrodgal7 eledejim2 josdeher
-- margarflo5 natruipin
dimension :: Num a => Matriz a -> (Int,Int)
dimension = snd . bounds

-- cescarde
dimension1 :: Num a => Matriz a -> (Int,Int)
dimension1 p = (numFilas p, numColumnas p)

-- ---------------------------------------------------------------------
-- Ejercicio 6. Definir la función
--    separa :: Int -> [a] -> [[a]]
-- tal que (separa n xs) es la lista obtenida separando los elementos de
-- xs en grupos de n elementos (salvo el último que puede tener menos de
-- n elementos). Por ejemplo, 
--    separa 3 [1..11]  ==  [[1,2,3],[4,5,6],[7,8,9],[10,11]]
-- ---------------------------------------------------------------------

-- albcercid antlopgom2 fraferpoy enrnarbej roscargar juacasnie
-- juaorture paumacpar antmorper3 eliguivil josrodgal7 eledejim2 josdeher
-- natruipin cescarde
separa :: Int -> [a] -> [[a]]
separa n [] = []
separa n xs = take n xs : separa n (drop n xs)

-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir la función
--    matrizLista :: Num a => Matriz a -> [[a]]
-- tal que (matrizLista x) es la lista de las filas de la matriz x. Por
-- ejemplo, 
--    ghci> let m = listaMatriz [[5,1,0],[3,2,6]]
--    ghci> m
--    array ((1,1),(2,3)) [((1,1),5),((1,2),1),((1,3),0),
--                         ((2,1),3),((2,2),2),((2,3),6)]
--    ghci> matrizLista m
--    [[5,1,0],[3,2,6]]
-- ---------------------------------------------------------------------

-- albcercid antlopgom2 fraferpoy enrnarbej roscargar juacasnie
-- juaorture paumacpar antmorper3 eliguivil josrodgal7 eledejim2 josdeher
-- natruipin cescarde
matrizLista :: Num a => Matriz a -> [[a]]
matrizLista p = separa (numColumnas p) (elems p) 

-- ---------------------------------------------------------------------
-- Ejercicio 8. Definir la función
--    vectorLista :: Num a => Vector a -> [a]
-- tal que (vectorLista x) es la lista de los elementos del vector
-- v. Por ejemplo, 
--    ghci> let v = listaVector [3,2,5]
--    ghci> v
--    array (1,3) [(1,3),(2,2),(3,5)]
--    ghci> vectorLista v
--    [3,2,5]
-- ---------------------------------------------------------------------

-- albcercid antlopgom2 fraferpoy enrnarbej roscargar juacasnie
-- juaorture paumacpar antmorper3 eliguivil josrodgal7 eledejim2 josdeher
-- natruipin cescarde
vectorLista :: Num a => Vector a -> [a]
vectorLista = elems

-- ---------------------------------------------------------------------
-- Suma de matrices                                                   --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 9. Definir la función
--    sumaMatrices:: Num a => Matriz a -> Matriz a -> Matriz a
-- tal que (sumaMatrices x y) es la suma de las matrices x e y. Por
-- ejemplo, 
--    ghci> let m1 = listaMatriz [[5,1,0],[3,2,6]]
--    ghci> let m2 = listaMatriz [[4,6,3],[1,5,2]]
--    ghci> matrizLista (sumaMatrices m1 m2)
--    [[9,7,3],[4,7,8]]
-- ---------------------------------------------------------------------

-- albcercid fraferpoy enrnarbej roscargar juacasnie juaorture paumacpar
-- antmorper3 josrodgal7 eledejim2 natruipin 
sumaMatrices :: Num a => Matriz a -> Matriz a -> Matriz a
sumaMatrices p q = listArray ((1,1),(m,n)) 
                   [p!(i,j) + q!(i,j) | i <- [1..m], j <- [1..n]]
  where (m,n) = dimension p

-- eliguivil
sumaMatrices2 :: Num a => Matriz a -> Matriz a -> Matriz a
sumaMatrices2 p q
  | bounds p == bounds q =
      listArray (bounds p)
                [p!(i,j) + q!(i,j) | i <- [1..numFilas p],
                                     j <- [1..numColumnas p]]
  | otherwise = error "insumable"

-- josdeher cescarde
sumaMatrices3 :: Num a => Matriz a -> Matriz a -> Matriz a
sumaMatrices3 p q = array (bounds p) 
                          [((i,j), p ! (i,j) + q ! (i,j)) 
                          | i <- [1..m], j <- [1..n]]
  where m = numFilas p
        n = numColumnas p

-- cescarde
sumaMatrices4 :: Num a => Matriz a -> Matriz a -> Matriz a
sumaMatrices4 p q =
  listArray (bounds p) (zipWith (+) (elems p) (elems q))

-- ---------------------------------------------------------------------
-- Ejercicio 10. Definir la función
--    filaMat :: Num a => Int -> Matriz a -> Vector a
-- tal que (filaMat i p) es el vector correspondiente a la fila i-ésima
-- de la matriz p. Por ejemplo,
--    ghci> let p = listaMatriz [[5,1,0],[3,2,6],[4,5,7]]
--    ghci> filaMat 2 p
--    array (1,3) [(1,3),(2,2),(3,6)]
--    ghci> vectorLista (filaMat 2 p)
--    [3,2,6]
-- ---------------------------------------------------------------------

-- albcercid fraferpoy enrnarbej roscargar juacasnie juaorture paumacpar
-- antmorper3 eliguivil josrodgal7 eledejim2 natruipin
filaMat :: Num a => Int -> Matriz a -> Vector a
filaMat i p = listArray (1,n) [p!(i,j) | j <- [1..n]]
  where n = numColumnas p

-- josdeher cescarde
filaMat2 :: Num a => Int -> Matriz a -> Vector a
filaMat2 i p = array (1,n) [(j, p ! (i,j)) | j <- [1..n]]
  where n = numColumnas p

-- ---------------------------------------------------------------------
-- Ejercicio 11. Definir la función
--    columnaMat :: Num a => Int -> Matriz a -> Vector a
-- tal que (columnaMat j p) es el vector correspondiente a la columna
-- j-ésima de la matriz p. Por ejemplo,
--    ghci> let p = listaMatriz [[5,1,0],[3,2,6],[4,5,7]]
--    ghci> columnaMat 2 p
--    array (1,3) [(1,1),(2,2),(3,5)]
--    ghci> vectorLista (columnaMat 2 p)
--    [1,2,5]
-- ---------------------------------------------------------------------

-- albcercid fraferpoy enrnarbej roscargar juacasnie juaorture paumacpar
-- eliguivil antmorper3 josrodgal7 eledejim2 natruipin
columnaMat :: Num a => Int -> Matriz a -> Vector a
columnaMat j p = listArray (1,n) [p!(i,j) | i <- [1..n]]
  where n = numFilas p

-- josdeher cescarde
columnaMat2 :: Num a => Int -> Matriz a -> Vector a
columnaMat2 j p = array (1,n) [(i, p ! (i,j)) | i <- [1..n]]
  where n = numFilas p

-- ---------------------------------------------------------------------
-- Producto de matrices                                               --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 12. Definir la función
--    prodEscalar :: Num a => Vector a -> Vector a -> a
-- tal que (prodEscalar v1 v2) es el producto escalar de los vectores v1
-- y v2. Por ejemplo,
--    ghci> let v = listaVector [3,1,10]
--    ghci> prodEscalar v v
--    110
-- ---------------------------------------------------------------------

-- albcercid enrnarbej roscargar juaorture paumacpar antmorper3 eliguivil
-- josrodgal7
prodEscalar :: Num a => Vector a -> Vector a -> a
prodEscalar v1 v2 = sum $ zipWith (*) (elems v1) (elems v2)

-- fraferpoy eledejim2 josdeher natruipin cescarde
prodEscalar2 :: Num a => Vector a -> Vector a -> a
prodEscalar2 v1 v2 = sum (zipWith (*) (elems v1) (elems v2))

-- juacasnie
prodEscalar3 :: Num a => Vector a -> Vector a -> a
prodEscalar3 v1 v2 = sum [(v1 ! i ) * (v2 ! i ) | i <- [1..n]]  
  where n = length (vectorLista v1)

-- ---------------------------------------------------------------------
-- Ejercicio 13. Definir la función
--    prodMatrices:: Num a => Matriz a -> Matriz a -> Matriz a
-- tal que (prodMatrices p q) es el producto de las matrices p y q. Por
-- ejemplo, 
--    ghci> let p = listaMatriz [[3,1],[2,4]]
--    ghci> prodMatrices p p
--    array ((1,1),(2,2)) [((1,1),11),((1,2),7),((2,1),14),((2,2),18)]
--    ghci> matrizLista (prodMatrices p p)
--    [[11,7],[14,18]]
--    ghci> let q = listaMatriz [[7],[5]]
--    ghci> prodMatrices p q
--    array ((1,1),(2,1)) [((1,1),26),((2,1),34)]
--    ghci> matrizLista (prodMatrices p q)
--    [[26],[34]]
-- ---------------------------------------------------------------------

-- albcercid enrnarbej fraferpoy roscargar juacasnie juaorture paumacpar
-- antmorper3 josrodgal7 natruipin cescarde
prodMatrices :: Num a => Matriz a -> Matriz a -> Matriz a
prodMatrices p q =
  listArray ((1,1),(m,n)) 
            [prodEscalar (filaMat a p) (columnaMat b q) 
            | a <- [1..m], b <- [1..n]]
  where m = numFilas p
        n = numColumnas q

-- eliguivil
prodMatrices2 :: Num a => Matriz a -> Matriz a -> Matriz a
prodMatrices2 p q
    | numColumnas p == numFilas q =
      listArray ((1,1),(numFilas p,numColumnas q))
                [sum [p!(i,k) * q!(k,j) | k <- [1..numColumnas p]]
                | i <- [1..numFilas p],
                  j <- [1..numColumnas q]]
    | otherwise = error "inmultiplicable"

-- josdeher
prodMatrices3 :: Num a => Matriz a -> Matriz a -> Matriz a
prodMatrices3 p q =
  array ((1,1),(m,n)) 
        [((i,j), prodEscalar (filaMat i p) (columnaMat j q)) 
        | i <- [1..m], j <- [1..n]]
  where m = numFilas p
        n = numColumnas q

-- ---------------------------------------------------------------------
-- Matriz identidad                                                   --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 14. Definir la función
--    identidad :: Num a => Int -> Matriz a
-- tal que (identidad n) es la matriz identidad de orden n. Por ejemplo, 
--    ghci> identidad 3
--    array ((1,1),(3,3)) [((1,1),1),((1,2),0),((1,3),0),
--                         ((2,1),0),((2,2),1),((2,3),0),
--                         ((3,1),0),((3,2),0),((3,3),1)]
-- ---------------------------------------------------------------------

-- albcercid enrnarbej roscargar paumacpar antmorper3 juacasnie
-- cescarde
identidad :: Num a => Int -> Matriz a
identidad n = listArray ((1,1),(n,n)) [ t a b | a <- [1..n], b <- [1..n]]
  where t a b | a == b    = 1
              | otherwise = 0

-- juaorture
identidad2 :: Num a => Int -> Matriz a
identidad2 n =
  array ((1,1),(n,n))
        ([((i,i), 1) | i <- [1..n]] ++
         [((i,j),0) | i <- [1..n], j <- [1..n], i /= j])

-- eliguivil
identidad3 :: Num a => Int -> Matriz a
identidad3 n = listArray ((1,1),(n,n))
               [if i == j
                then 1
                else 0 | i <- [1..n], j <- [1..n]] 

-- josdeher
identidad4 :: Num a => Int -> Matriz a
identidad4 n =
  array ((1,1),(n,n)) 
        [((i,j), if i == j then 1 else 0) | i <- [1..n], j <- [1..n]]

-- natruipin 
identidad5 :: Num a => Int -> Matriz a
identidad5 n = listArray ((1,1),(n,n)) (concat (replicate (n*n) ps))
  where ps = 1 : replicate n 0

-- ---------------------------------------------------------------------
-- Ejercicio 15. Definir la función
--    potencia :: Num a => Matriz a -> Int -> Matriz a
-- tal que (potencia p n) es la potencia n-ésima de la matriz cuadrada
-- p. Por ejemplo, si q es la matriz definida por
--    q :: Matriz Int
--    q = listArray ((1,1),(2,2)) [1,1,1,0] 
-- entonces
--    ghci> potencia q 2
--    array ((1,1),(2,2)) [((1,1),2),((1,2),1),((2,1),1),((2,2),1)]
--    ghci> potencia q 3
--    array ((1,1),(2,2)) [((1,1),3),((1,2),2),((2,1),2),((2,2),1)]
--    ghci> potencia q 4
--    array ((1,1),(2,2)) [((1,1),5),((1,2),3),((2,1),3),((2,2),2)]
-- ¿Qué relación hay entre las potencias de la matriz q y la sucesión de
-- Fibonacci? 
-- ---------------------------------------------------------------------

q :: Matriz Int
q = listArray ((1,1),(2,2)) [1,1,1,0] 

-- juaorture antmorper3 josdeher natruipin
potencia :: Num a => Matriz a -> Int -> Matriz a
potencia p 0 = identidad (numFilas p)
potencia p n = prodMatrices p (potencia p (n-1))

-- enrnarbej
potencia2 :: Num a => Matriz a -> Int -> Matriz a
potencia2 ms n =
  foldl prodMatrices (identidad (numFilas ms)) (replicate n ms)

-- El primer valor de la n-ésima potencia de q coincide con el n-ésimo
-- término de la sucesión de Fibonacci: 
fibs = 1:1:zipWith (+) fibs (tail fibs)

prop_fibs n = n >= 0 ==> (potencia q n)!(1,1) == fibs!!n

-- λ> quickCheck prop_fibs
-- +++ OK, passed 100 tests.

-- albcercid roscargar paumacpar juacasnie
potencia3 :: Num a => Matriz a -> Int -> Matriz a
potencia3 p 0 = identidad (numFilas p)
potencia3 p n = aux p p (n-1)
  where aux p _ 0 = p
        aux p q n = aux (prodMatrices p q) q (n-1)

-- eliguivil
potencia4 :: Num a => Matriz a -> Int -> Matriz a
potencia4 p n = aux p (identidad (numColumnas p)) n
  where aux p q 0 = q
        aux p q n = aux p (p `prodMatrices` q) (n-1)

-- cescarde
potencia5 :: Num a => Matriz a -> Int -> Matriz a
potencia5 p 1 = p
potencia5 p n = prodMatrices (potencia p (n-1)) p

-- Comentario: Falta el caso de exponente 0.

-- ---------------------------------------------------------------------
-- Traspuestas                                                        --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 16. Definir la función
--    traspuesta :: Num a => Matriz a -> Matriz a
-- tal que (traspuesta p) es la traspuesta de la matriz p. Por ejemplo,
--    ghci> let p = listaMatriz [[5,1,0],[3,2,6]]
--    ghci> traspuesta p
--    array ((1,1),(3,2)) [((1,1),5),((1,2),3),
--                         ((2,1),1),((2,2),2),
--                         ((3,1),0),((3,2),6)]
--    ghci> matrizLista (traspuesta p)
--    [[5,3],[1,2],[0,6]]
-- ---------------------------------------------------------------------

-- albcercid enrnarbej fraferpoy roscargar paumacpar antmorper3 eliguivil 
-- juacasnie cescarde natruipin
traspuesta :: Num a => Matriz a -> Matriz a
traspuesta p =
  listArray ((1,1),(n,m)) [ p!(i,j) | j <- [1..n], i <- [1..m]]
  where (m,n) = dimension p

-- juaorture josdeher
traspuesta2 :: Num a => Matriz a -> Matriz a
traspuesta2 p = array ((1,1),(n,m)) [((j,i),x) | j <- [1..n]
                                               , i <- [1..m]
                                               , let x = p!(i,j)]
  where n = numColumnas p
        m = numFilas p

-- Comentario: La definición traspuesta2 se puede simplificar.

-- ---------------------------------------------------------------------
-- Tipos de matrices                                                  --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 17. Definir la función
--    esCuadrada :: Num a => Matriz a -> Bool
-- tal que (esCuadrada p) se verifica si la matriz p es cuadrada. Por
-- ejemplo, 
--    ghci> let p = listaMatriz [[5,1,0],[3,2,6]]
--    ghci> esCuadrada p
--    False
--    ghci> let q = listaMatriz [[5,1],[3,2]]
--    ghci> esCuadrada q
--    True
-- ---------------------------------------------------------------------

-- albcercid enrnarbej fraferpoy roscargar paumacpar antmorper3
-- juacasnie 
esCuadrada :: Num a => Matriz a -> Bool
esCuadrada x = m == n
  where (m,n) = dimension x

-- juaorture natruipin
esCuadrada2 :: Num a => Matriz a -> Bool
esCuadrada2 x = fst y == snd y
  where y = snd $ bounds x

-- eliguivil josdeher cescarde
esCuadrada3 :: Num a => Matriz a -> Bool
esCuadrada3 x = numFilas x == numColumnas x

-- ---------------------------------------------------------------------
-- Ejercicio 18. Definir la función
--    esSimetrica :: (Num a, Eq a) => Matriz a -> Bool
-- tal que (esSimetrica p) se verifica si la matriz p es simétrica. Por
-- ejemplo, 
--    ghci> let p = listaMatriz [[5,1,3],[1,4,7],[3,7,2]]
--    ghci> esSimetrica p
--    True
--    ghci> let q = listaMatriz [[5,1,3],[1,4,7],[3,4,2]]
--    ghci> esSimetrica q
--    False
-- ---------------------------------------------------------------------    

-- albcercid enrnarbej fraferpoy roscargar paumacpar antmorper3 eliguivil
-- juacasnie josdeher cescarde natruipin
esSimetrica :: (Num a, Eq a) => Matriz a -> Bool
esSimetrica x = x == traspuesta x

-- juaorture cescarde
esSimetrica2 :: (Num a, Eq a) => Matriz a -> Bool
esSimetrica2 x = and [x!(i,j) == x!(j,i) | i <- [1..n]
                                         , j <- [1..m]]
  where n = numFilas x
        m = numColumnas x

-- Comentario: La definición esSimetrica2 se puede simplificar.

-- ---------------------------------------------------------------------
-- Diagonales de una matriz                                           --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 19. Definir la función
--    diagonalPral :: Num a => Matriz a -> Vector a
-- tal que (diagonalPral p) es la diagonal principal de la matriz p. Por
-- ejemplo, 
--    ghci> let p = listaMatriz [[5,1,0],[3,2,6]]
--    ghci> diagonalPral p
--    array (1,2) [(1,5),(2,2)]
--    ghci> vectorLista (diagonalPral p)
--    [5,2]
-- ---------------------------------------------------------------------

-- albcercid enrnarbej roscargar paumacpar antmorper3 eliguivil natruipin

diagonalPral :: Num a => Matriz a -> Vector a
diagonalPral p = listArray (1,n) [p!(i,i) | i <- [1..n]]
  where n = min (numFilas p) (numColumnas p)

-- juaorture juacasnie josdeher cescarde
diagonalPral2 :: Num a => Matriz a -> Vector a
diagonalPral2 p = array (1,x) [(i,p!(i,i)) | i <- [1..x]]
  where x = min (numFilas p) (numColumnas p)

-- ---------------------------------------------------------------------
-- Ejercicio 20. Definir la función
--    diagonalSec :: Num a => Matriz a -> Vector a
-- tal que (diagonalSec p) es la diagonal secundaria de la matriz p. Por
-- ejemplo, 
--    ghci> let p = listaMatriz [[5,1,0],[3,2,6]]
--    ghci> diagonalSec p
--    array (1,2) [(1,1),(2,3)]
--    ghci> vectorLista (diagonalSec p)
--    [1,3]
--    ghci> let q = traspuesta p
--    ghci> matrizLista q
--    [[5,3],[1,2],[0,6]]
--    ghci> vectorLista (diagonalSec q)
--    [3,1]
-- ---------------------------------------------------------------------

-- albcercid enrnarbej roscargar antmorper3 eliguivil natruipin
diagonalSec :: Num a => Matriz a -> Vector a
diagonalSec p =  listArray (1,t) [p!(i,t+1-i) | i <- [1..t]]
  where t     = min m n
        (m,n) = dimension p

-- juaorture paumacpar juacasnie josdeher cescarde
diagonalSec2 :: Num a => Matriz a -> Vector a
diagonalSec2 p = array (1,x) [(i, p!(i,j)) | i <- [1..x]
                                           , let j = x - i + 1]
  where x = min (numFilas p) (numColumnas p)

-- Comentario: La definición diagonalSec2 se puede simplificar.

-- ---------------------------------------------------------------------
-- Submatrices                                                        --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 21. Definir la función
--    submatriz :: Num a => Int -> Int -> Matriz a -> Matriz a
-- tal que (submatriz i j p) es la matriz obtenida a partir de la p
-- eliminando la fila i y la columna j. Por ejemplo,
--    ghci> let p = listaMatriz [[5,1,0],[3,2,6],[4,6,9]]
--    ghci> submatriz 2 3 p
--    array ((1,1),(2,2)) [((1,1),5),((1,2),1),((2,1),4),((2,2),6)]
--    ghci> matrizLista (submatriz 2 3 p)
--    [[5,1],[4,6]]
-- ---------------------------------------------------------------------

-- albcercid enrnarbej roscargar juaorture antmorper3 cescarde
submatriz :: Num a => Int -> Int -> Matriz a -> Matriz a
submatriz i j p =
  listArray ((1,1),(m-1,n-1)) [ p!(a,b) | a <- [1..m],
                                          b <- [1..n],
                                          a /= i,
                                          b /= j]
  where (m,n) = dimension p

-- eliguivil
submatriz2 :: Num a => Int -> Int -> Matriz a -> Matriz a
submatriz2 i j p = (elimCol j.elimFil i) p
  where
    elimFil k p = listArray ((1,1),(numFilas p -1,numColumnas p))
                  (map snd $ filter (\((i,j),n) -> i /= k) (assocs p))
    elimCol k p = listArray ((1,1),(numFilas p   ,numColumnas p -1))
                  (map snd $ filter (\((i,j),n) -> j /= k) (assocs p))

-- juacasnie natruipin
submatriz3 :: Num a => Int -> Int -> Matriz a -> Matriz a
submatriz3 i j p = listArray ((1,1),(m-1,n-1)) 
                   [p ! (k,l) | k <- [1..m]\\[i], l <- [1..n]\\[j]]
  where m = numFilas p
        n = numColumnas p

-- josdeher
submatriz4 :: Num a => Int -> Int -> Matriz a -> Matriz a
submatriz4 a b p =
  array ((1,1),(m,n)) 
        [((i,j), p! ((if i < a then i else i+1),(if j < b then j else j+1))) 
        | i <- [1..m], j <- [1..n]]
  where m = numFilas p - 1
        n = numColumnas p - 1

-- ---------------------------------------------------------------------
-- Determinante                                                       --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 22. Definir la función
--    determinante:: Matriz Double -> Double
-- tal que (determinante p) es el determinante de la matriz p. Por
-- ejemplo, 
--    ghci> determinante (listArray ((1,1),(3,3)) [2,0,0,0,3,0,0,0,1])
--    6.0
--    ghci> determinante (listArray ((1,1),(3,3)) [1..9])
--    0.0
--    ghci> determinante (listArray ((1,1),(3,3)) [2,1,5,1,2,3,5,4,2])
--    -33.0
-- ---------------------------------------------------------------------

-- albcercid enrnarbej roscargar juaorture antmorper3 juacasnie josdeher
-- cescarde
determinante :: Matriz Double -> Double
determinante p
  | n == 1    = head $ elems p
  | otherwise = sum [(-1)^(i+1) * p!(i,1) * (f i 1 p) | i <- [1..n]]
  where n       = numFilas p
        f i j p = determinante $ submatriz i j p

-- eliguivil
determinante2 :: Matriz Double -> Double
determinante2 p
  | esCuadrada p = det p
  | otherwise    = error "Aún no me han enseñado a hacer determinantes de matrices no cuadradas"
  where
  det p 
    | dimension p == (1,1) = head $ elems p
    | otherwise = sum [p!(i,1) * det (submatriz i 1 p) * (-1.0)^(i+1) |
                       i <- [1..numColumnas p]]
