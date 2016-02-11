-- I1M 2015-16: Relación 18 (11 de enero de 2016)
-- Vectores y matrices.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla 
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- El objetivo de esta relación es hacer ejercicios sobre vectores y
-- matrices con el tipo de tablas de las tablas, definido en el módulo
-- Data.Array y explicado en el tema 18 cuyas transparencias se
-- encuentran en 
--    http://www.cs.us.es/~jalonso/cursos/i1m-15/temas/tema-18.html
 
-- ---------------------------------------------------------------------
-- Importación de librerías                                           --
-- ---------------------------------------------------------------------

import Data.Array

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

-- isrbelnun blaruiher alvalvdom1 manvermor josllagam juamorrom1
listaVector :: Num a => [a] -> Vector a
listaVector xs = array (1,length xs) (zip [1..] xs)

-- fracruzam abrdelrod manpende jespergue ivaruicam javoliher guache
listaVector2 :: Num a => [a] -> Vector a
listaVector2 xs = listArray (1,length xs) xs

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    listaMatriz :: Num a => [[a]] -> Matriz a
-- tal que (listaMatriz xss) es la matriz cuyas filas son los elementos
-- de xss. Por ejemplo,
--    ghci> listaMatriz [[1,3,5],[2,4,7]]
--    array ((1,1),(2,3)) [((1,1),1),((1,2),3),((1,3),5),
--                         ((2,1),2),((2,2),4),((2,3),7)]
-- ---------------------------------------------------------------------

-- fracruzam abrdelrod alvalvdom1 manvermor manpende jespergue blaruiher
-- josllagam ivaruicam juamorrom1 javioliher
listaMatriz :: Num a => [[a]] -> Matriz a
listaMatriz ys@(xs:xss) = listArray ((1,1),(a,b)) (concat ys)
    where a = length ys
          b = length xs

-- guache
listaMatriz2 :: Num a => [[a]] -> Matriz a
listaMatriz2 xss = 
    listArray ((1,1),(length xss,length $ head xss)) (concat xss)

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--    numFilas :: Num a => Matriz a -> Int
-- tal que (numFilas m) es el número de filas de la matriz m. Por
-- ejemplo,
--    numFilas (listaMatriz [[1,3,5],[2,4,7]])  ==  2
-- ---------------------------------------------------------------------

-- fracruzam abrdelrod alvalvdom1 manvermor manpende jespergue blaruiher
-- josllagam ivaruicam juamorrom1 javoliher
numFilas :: Num a => Matriz a -> Int
numFilas = fst . snd . bounds

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función
--    numColumnas :: Num a => Matriz a -> Int
-- tal que (numColumnas m) es el número de columnas de la matriz
-- m. Por ejemplo,
--    numColumnas (listaMatriz [[1,3,5],[2,4,7]])  ==  3
-- ---------------------------------------------------------------------

-- fracruzam abrdelrod alvalvdom1 manvermor manpende jespergue blaruiher
-- josllagam ivaruicam juamorrom1 javoliher
numColumnas:: Num a => Matriz a -> Int
numColumnas = snd . snd . bounds

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la función
--    dimension :: Num a => Matriz a -> (Int,Int)
-- tal que (dimension m) es la dimensión de la matriz m. Por ejemplo, 
--    dimension (listaMatriz [[1,3,5],[2,4,7]])  ==  (2,3)
-- ---------------------------------------------------------------------

-- fracruzam alvalvdom1 manpende jespergue blaruiher josllagam ivaruicam
-- juamorrom1 javoliher
dimension :: Num a => Matriz a -> (Int,Int)
dimension = snd . bounds

-- abrdelrod manvermor
dimension2 :: Num a => Matriz a -> (Int,Int)
dimension2 p = (numFilas p,numColumnas p)

-- ---------------------------------------------------------------------
-- Ejercicio 6. Definir la función
--    separa :: Int -> [a] -> [[a]]
-- tal que (separa n xs) es la lista obtenida separando los elementos de
-- xs en grupos de n elementos (salvo el último que puede tener menos de
-- n elementos). Por ejemplo, 
--    separa 3 [1..11]  ==  [[1,2,3],[4,5,6],[7,8,9],[10,11]]
-- ---------------------------------------------------------------------

-- alvalvdom1 manvermor manpende blaruiher jespergue
separa :: Int -> [a] -> [[a]]
separa n xs | length xs > n = [take n xs] ++ separa n (drop n xs)
            | otherwise     = [xs]

-- Comentario: La definición anterior se puede simplificar.

-- fracruzam abrdelrod jespergue josllagam ivaruicam juamorrom1 javoliher
separa2 :: Int -> [a] -> [[a]]
separa2 _ [] = []
separa2 n xs = take n xs : separa n (drop n xs)

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

-- alvalvdom1 fracruzam abrdelrod manvermor manpende jespergue blaruiher
-- josllagam ivaruicam juamorrom1 javoliher
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

-- alvalvdom1 fracruzam abrdelrod manvermor manpende jespergue blaruiher
-- josllagam ivaruicam juamorrom1 javoliher
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

-- fracruzam abrdelrod alvalvdom1 manvermor manpende ivaruicam juamorrom1 
-- javoliher jespergue
sumaMatrices:: Num a => Matriz a -> Matriz a -> Matriz a
sumaMatrices p q = 
    array ((1,1),dim) 
          [((i,j), p!(i,j) + q!(i,j))
          | i <- [1..fst dim], j <- [1..snd dim]]
    where dim = dimension p

-- josllagam
sumaMatrices2:: Num a => Matriz a -> Matriz a -> Matriz a
sumaMatrices2 p q = 
    listArray ((1,1),(dimension p)) (zipWith (+) (elems p) (elems q))

-- Comentario: La definición anterior se puede simplificar usando
-- bounds. 

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

-- alvalvdom1 abrdelrod manpende josllagam jespergue
filaMat :: Num a => Int -> Matriz a -> Vector a
filaMat i p = listArray (1,numColumnas p) (matrizLista p !! (i-1))

-- Comentario: La definición anterior se puede simplificar usando listaVector.

-- fracruzam
filaMat2 :: Num a => Int -> Matriz a -> Vector a
filaMat2 i p = 
    array (1,d)
    $ map (\((_,c),v) -> (c,v))
    $ filter (\((f,_),_) -> f == i) 
    $ assocs p
    where d = numColumnas p

-- Comentario: La definición anterior se puede simplificar.

-- manvermor blaruiher ivaruicam juamorrom1 javoliher
filaMat3 :: Num a => Int -> Matriz a -> Vector a
filaMat3 i p = listaVector $ (matrizLista p) !! (i-1)

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

-- alvalvdom1 abrdelrod manpende josllagam jespergue
columnaMat :: Num a => Int -> Matriz a -> Vector a
columnaMat j p = listArray (1,numFilas p) (map (!! (j-1)) (matrizLista p))

-- Comentario: La definición anterior se puede simplificar usando
-- listaVector.

-- fracruzam
columnaMat2 :: Num a => Int -> Matriz a -> Vector a
columnaMat2 i p = 
    array (1,d) 
    $ map (\((f,_),v) -> (f,v)) 
    $ filter (\((_,c),_) -> c == i) 
    $ assocs p
  where d = numFilas p

-- Comentario: La definición anterior se puede simplificar.

-- manvermor blaruiher juamorrom1
columnaMat3 :: Num a => Int -> Matriz a -> Vector a
columnaMat3 j p = listaVector [xs !! (j-1) | xs <- matrizLista p]

-- ivaruicam 
columnaMat4 :: Num a => Int -> Matriz a -> Vector a
columnaMat4 j p = listaVector [i | ((_,n),i) <- assocs p , n == j]

-- javoliher
columnaMat5 :: Num a => Int -> Matriz a -> Vector a
columnaMat5 j p = listaVector (map f (matrizLista p))
    where f xs = xs !! (j-1)

-- paocabper
columnaMat6 :: Num a => Int -> Matriz a -> Vector a
columnaMat6 j p = listArray (1,numFilas p) (matrizLista p!!(j-1))

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

-- alvalvdom1 abrdelrod manvermor manpende blaruiher
prodEscalar :: Num a => Vector a -> Vector a -> a
prodEscalar v1 v2 = sum [x*y | (x,y) <- zip (elems v1) (elems v2)]

-- Comentario: La definición anterior se puede simplificar usando zipWith.

-- fracruzam juamorrom1 javoliher josllagam
prodEscalar2 :: Num a => Vector a -> Vector a -> a
prodEscalar2 v1 v2 = sum $ zipWith (*) (elems v1) (elems v2)

-- ivaruicam
prodEscalar3 :: Num a => Vector a -> Vector a -> a
prodEscalar3 v1 v2 = sum [v1!i * v2!i | i <- indices v1]

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

-- alvalvdom1 abrdelrod manvermor manpende blaruiher ivaruicam javoliher
-- josllagam
prodMatrices:: Num a => Matriz a -> Matriz a -> Matriz a
prodMatrices p q = array ((1,1),(m,n))
                         [((i,j),f i j) | i <- [1..m], j <- [1..n]]
    where f i j = prodEscalar (filaMat i p) (columnaMat j q)
          (m,n) = (numFilas p,numColumnas q)

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

-- fracruzam manvermor
identidad :: Num a => Int -> Matriz a
identidad n = 
    listArray ((1,1),(n,n)) 
              [(\i j -> if i == j then 1 else 0) i j | i <- xs, j <- xs]
    where xs = [1..n]    

-- abrdelrod alvalvdom1 manpende ivaruicam blaruiher javoliher josllagam
identidad2 :: Num a => Int -> Matriz a
identidad2 n = 
    array ((1,1), (n,n)) [((i,j), f i j) | i <- [1..n], j <- [1..n]]
    where f i j = if i == j then 1 else 0

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

-- alvalvdom1
potencia :: Num a => Matriz a -> Int -> Matriz a
potencia p n | n > 0  = prodMatrices (potencia p (n-1)) p
             | n == 0 = identidad (numFilas p)

-- abrdelrod manvermor ivaruicam josllagam manpende
potencia2 :: Num a => Matriz a -> Int -> Matriz a
potencia2 p 0 = identidad (numFilas p)
potencia2 p n = prodMatrices (potencia p (n-1)) p

-- javoliher
potencia3 :: Num a => Matriz a -> Int -> Matriz a
potencia3 q n = (iterate (prodMatrices q) q) !! (n-1)

-- La definición anterior está incompleta: le falta el caso de exponente
-- cero. 

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

-- alvalvdom1 abrdelrod manvermor manpende
traspuesta :: Num a => Matriz a -> Matriz a
traspuesta p = array ((1,1),(n,m)) [((i,j),f i j) | i <- [1..n], j <- [1..m]]
    where f i j = p!(j,i)
          (m,n) = dimension p

-- Comentario: La definición anterior se puede simplificar eliminando la
-- función f.

-- fracruzam
traspuesta2 :: Num a => Matriz a -> Matriz a
traspuesta2 p = 
    array ((1,1),dim) [((j,i),v) | ((i,j),v) <- assocs p]
        where (i,j) = dimension p
              dim   = (j,i)

-- ivaruicam blaruiher josllagam
traspuesta3 :: Num a => Matriz a -> Matriz a
traspuesta3 p = 
    array ((1,1), (m,n)) [((i,j), p!(j,i))| i <- [1..m], j <- [1..n]]
    where n = numFilas p
          m = numColumnas p 

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
 
-- alvalvdom1 fracruzam abrdelrod manvermor ivaruicam blaruiher
-- josllagam manpende
esCuadrada :: Num a => Matriz a -> Bool
esCuadrada x = numFilas x == numColumnas x

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

-- alvalvdom1 fracruzam abrdelrod manvermor ivaruicam blaruiher
-- josllagam manpende
esSimetrica :: (Num a, Eq a) => Matriz a -> Bool
esSimetrica x = x == traspuesta x

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

-- fracruzam manpende alvalvdom1
diagonalPral :: Num a => Matriz a -> Vector a
diagonalPral p = array (1,q) [(i,v) | ((i,j),v) <- assocs p, i == j]
    where q = min (numColumnas p) (numFilas p)

-- manvermor
diagonalPral2 :: Num a => Matriz a -> Vector a
diagonalPral2 p = 
    listaVector [ p!(i,j) | i <- [1..numFilas p], 
                            j <- [1..numColumnas p],
                            i == j]
-- abrdelrod ivaruicam josllagam
diagonalPral3 :: Num a => Matriz a -> Vector a
diagonalPral3 p = listArray (1, m) [x | ((i,j),x) <- assocs p, i==j]
    where m = min (numFilas p) (numColumnas p)

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

-- abrdelrod
diagonalSec :: Num a => Matriz a -> Vector a
diagonalSec p = listArray (1,m) (f (matrizLista p) m)
    where f _ 0        = []
          f (xs:xss) n = (xs!!(n-1)) : f xss (n-1)
          m = min (numFilas p) (numColumnas p)


-- carmengar josllagam
diagonalSec2 :: Num a => Matriz a -> Vector a 
diagonalSec2 p = 
    array (1,q) [(i,v) | ((i,j),v) <- assocs p, i+j == q+1]
    where q = min (numFilas p) (numColumnas p)

-- manpende
diagonalSec3 :: Num a => Matriz a -> Vector a
diagonalSec3 p = listaVector [p!(i,n-i+1) | i <- [1..n]]
    where n = min (numFilas p) (numColumnas p) 

-- --------------------------------------------------------------------
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

-- fracruzam
submatriz :: Num a => Int -> Int -> Matriz a -> Matriz a
submatriz i j p = 
    array ((1,1),dim) 
          [((aux f i,aux c j), v) | ((f,c),v) <- assocs p, f /= i, c /= j]
    where dim = (numFilas p - 1, numColumnas p - 1)
          aux :: Int -> Int -> Int
          aux f i | f < i     = f
                  | otherwise = f-1

-- abrdelrod
submatriz2 :: Num a => Int -> Int -> Matriz a -> Matriz a
submatriz2 i j p = listaMatriz (borraFil i $ borraCol j (matrizLista p))
      where borraFil i xss =  take (i-1) xss ++ drop i xss
            borraCol j xss = [borraFil j xs | xs <- xss]

-- manpende alvalvdom1
submatriz3 :: Num a => Int -> Int -> Matriz a -> Matriz a
submatriz3 a b p = 
    array ((1,1),(m-1,n-1)) [((i,j), f i j) | i <- [1..m-1], j <- [1..n-1]]
    where n = numColumnas p
          m = numFilas p
          f i j | i >= a && j >= b = p ! (i+1,j+1)
                | i < a  && j >= b = p ! (i,j+1)
                | i < a  && j <  b = p ! (i,j)
                | i >= a && j <  b = p ! (i+1,j)

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

-- abrdelrod manpende
determinante:: Matriz Double -> Double
determinante p = aux p (numFilas p)
    where aux p 1 = head (elems p)
          aux p n = sum [x * determinante (submatriz i 1 p) * (-1)^(i+1) 
                        | ((i,1),x) <- assocs p]
