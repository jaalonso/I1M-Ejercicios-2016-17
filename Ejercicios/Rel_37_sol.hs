-- I1M 2015-16: Relación 37 (8 de mayo de 2016)
-- Vectores y matrices. (Ejercicios de exámenes).
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- En esta relación se presenta una recopilación de ejercicios vectores
-- y matrices propuestos en exámenes de la asignatura.

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.Array
import Data.List

-- Nota. En la relación usaremos los tipos de los vectores y las matrices 
-- definidos por 

type Vector a = Array Int a
type Matriz a = Array (Int,Int) a

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función 
--    esTriangularS :: Num a => Matriz a -> Bool
-- tal que (esTriangularS p) se verifica si p es una matriz triangular
-- superior. Por ejemplo, 
--    ghci> esTriangularS (listArray ((1,1),(3,3)) [1,2,1,0,4,7,0,0,5])
--    True
--    ghci> esTriangularS (listArray ((1,1),(3,3)) [1,2,3,1,2,4,1,2,5])
--    False
-- ---------------------------------------------------------------------

-- 1ª definición
esTriangularS:: (Num a, Eq a) => Matriz a -> Bool
esTriangularS p = and [p!(i,j) == 0 | i <- [1..m], j <- [1..n], i > j]
    where (_,(m,n)) = bounds p

-- 2ª definición                      
esTriangular2 :: (Num a, Eq a) => Matriz a -> Bool
esTriangular2 p = and [x == 0 | ((i,j),x) <- assocs p, i > j]
                      
-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
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

potencia :: Num a => Matriz a -> Int -> Matriz a
potencia p 0 = identidad n
    where (_,(n,_)) = bounds p
potencia p n = prodMatrices p (potencia p (n-1))

-- (identidad n) es la matriz identidad de orden n. Por ejemplo,
--    ghci> identidad 3
--    array ((1,1),(3,3)) [((1,1),1),((1,2),0),((1,3),0),
--                         ((2,1),0),((2,2),1),((2,3),0),
--                         ((3,1),0),((3,2),0),((3,3),1)]
identidad :: Num a => Int -> Matriz a
identidad n =     
    array ((1,1),(n,n))
          [((i,j),f i j) | i <- [1..n], j <- [1..n]]
    where f i j | i == j    = 1
                | otherwise = 0

-- (prodEscalar v1 v2) es el producto escalar de los vectores v1
-- y v2. Por ejemplo,
--    ghci> prodEscalar (listArray (1,3) [3,2,5]) (listArray (1,3) [4,1,2])
--    24
prodEscalar :: Num a => Vector a -> Vector a -> a
prodEscalar v1 v2 = 
    sum [i*j | (i,j) <- zip (elems v1) (elems v2)]

-- (filaMat i p) es el vector correspondiente a la fila i-ésima
-- de la matriz p. Por ejemplo,
--    filaMat 2 q  ==  array (1,2) [(1,1),(2,0)]
filaMat :: Num a => Int -> Matriz a -> Vector a
filaMat i p = array (1,n) [(j,p!(i,j)) | j <- [1..n]]
    where (_,(_,n)) = bounds p

-- (columnaMat j p) es el vector correspondiente a la columna
-- j-ésima de la matriz p. Por ejemplo,
--    columnaMat 2 q  ==  array (1,2) [(1,1),(2,0)]
columnaMat :: Num a => Int -> Matriz a -> Vector a
columnaMat j p = array (1,m) [(i,p!(i,j)) | i <- [1..m]]
    where (_,(m,_)) = bounds p

-- (prodMatrices p q) es el producto de las matrices p y q. Por ejemplo,
--    ghci> prodMatrices q q
--    array ((1,1),(2,2)) [((1,1),2),((1,2),1),((2,1),1),((2,2),1)]
prodMatrices :: Num a => Matriz a -> Matriz a -> Matriz a
prodMatrices p q = 
    array ((1,1),(m,n))
          [((i,j), prodEscalar (filaMat i p) (columnaMat j q)) |
           i <- [1..m], j <- [1..n]]
    where (_,(m,n)) = bounds p

-- Los sucesión de Fibonacci es 0,1,1,2,3,5,8,13,... Se observa que los
-- elementos de (potencia q n) son los términos de la sucesión en los
-- lugares n+1, n, n y n-1.

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--    indicesMaximo :: (Num a, Ord a) => Matriz a -> [(Int,Int)]
-- tal que (indicesMaximo p) es la lista de los índices del elemento
-- máximo de la matriz p. Por ejemplo,
--    ghci> indicesMaximo (listArray ((1,1),(2,2)) [3,2,3,1])
--    [(1,1),(2,1)]
-- ---------------------------------------------------------------------

indicesMaximo :: (Num a, Ord a) => Matriz a -> [(Int,Int)]
indicesMaximo p = [x | x <- indices p, p!x == m]
    where m = maximum (elems p)
              
-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función
--    antidiagonal :: (Num a, Eq a) => Matriz a -> Bool
-- tal que (antidiagonal m) se verifica si es cuadrada y todos los
-- elementos de m que no están en su diagonal secundaria son nulos. Por
-- ejemplo,   
--    ghci> antidiagonal (listArray ((1,1),(3,3)) [0,0,4, 0,6,0, 0,0,0])
--    True
--    ghci> antidiagonal (listArray ((1,1),(3,3)) [7,0,4, 0,6,0, 0,0,5])
--    False
-- ---------------------------------------------------------------------

-- m1, m2 :: Matriz Int
-- m1 = listArray ((1,1),(3,3)) [0,0,4, 0,6,0, 0,0,0]
-- m2 = listArray ((1,1),(3,3)) [7,0,4, 0,6,0, 0,0,5]

antidiagonal :: (Num a, Eq a) => Matriz a -> Bool 
antidiagonal p = 
    m == n && nula [p!(i,j) | i <- [1..n], j <- [1..n], i + j /= n + 1]
    where (_,(m,n)) = bounds p

nula :: (Num a, Eq a) => [a] -> Bool
nula xs = xs == [0 | x <- xs]

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la función 
--    posiciones :: Int -> Matriz Int -> [(Int,Int)]
-- tal que (posiciones x p) es la lista de las posiciones de la matriz p
-- cuyo valor es x. Por ejemplo,
--    ghci> let p = listArray ((1,1),(2,3)) [1,2,3,2,4,6] :: Matriz Int
--    ghci> p
--    array ((1,1),(2,3)) [((1,1),1),((1,2),2),((1,3),3),
--                         ((2,1),2),((2,2),4),((2,3),6)]
--    ghci> posiciones 2 p
--    [(1,2),(2,1)]
--    ghci> posiciones 6 p
--    [(2,3)]
--    ghci> posiciones 7 p
--    []
-- ---------------------------------------------------------------------

posiciones :: Int -> Matriz Int -> [(Int,Int)]
posiciones x p = [a | a <- indices p, p!a == x]

-- ---------------------------------------------------------------------
-- Ejercicio 6. Definir la función
--    esEscalar:: Num a => Matriz a -> Bool
-- tal que (esEscalar p) se verifica si p es una matriz es escalar; es
-- decir, diagonal con todos los elementos de la diagonal principal
-- iguales. Por ejemplo,
--    esEscalar (listArray ((1,1),(3,3)) [5,0,0,0,5,0,0,0,5])  ==  True
--    esEscalar (listArray ((1,1),(3,3)) [5,0,0,1,5,0,0,0,5])  ==  False
--    esEscalar (listArray ((1,1),(3,3)) [5,0,0,0,6,0,0,0,5])  ==  False
-- ---------------------------------------------------------------------

esEscalar:: (Num a, Eq a) => Matriz a -> Bool
esEscalar p = esDiagonal p && todosIguales (elems (diagonalPral p))

-- (esDiagonal p) se verifica si la matriz p es diagonal. Por ejemplo.
--    esDiagonal (listArray ((1,1),(3,3)) [5,0,0,0,6,0,0,0,5])  ==  True
--    esDiagonal (listArray ((1,1),(3,3)) [5,0,0,1,5,0,0,0,5])  ==  False
esDiagonal:: (Num a, Eq a) => Matriz a -> Bool
esDiagonal p = all (==0) [p!(i,j) | i<-[1..m],j<-[1..n], i/=j]
    where (_,(m,n)) = bounds p

-- (todosIguales xs) se verifica si todos los elementos de xs son
-- iguales. Por ejemplo,
--    todosIguales [5,5,5]  ==  True
--    todosIguales [5,6,5]  ==  False
todosIguales :: Eq a => [a] -> Bool
todosIguales (x:y:ys) = x == y && todosIguales (y:ys) 
todosIguales _ = True 

-- (diagonalPral p) es la diagonal principal de la matriz p. Por
-- ejemplo, 
--    ghci> diagonalPral (listArray ((1,1),(3,3)) [5,0,0,1,6,0,0,2,4])
--    array (1,3) [(1,5),(2,6),(3,4)]
diagonalPral :: Num a => Matriz a -> Vector a
diagonalPral p = array (1,n) [(i,p!(i,i)) | i <- [1..n]]
    where n         = min k l
          (_,(k,l)) = bounds p

-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir la función
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

determinante:: Matriz Double -> Double
determinante p 
    | (m,n) == (1,1) = p!(1,1)
    | otherwise = 
        sum [((-1)^(i+1))*(p!(i,1))*determinante (submatriz i 1 p)
             | i <- [1..m]]
    where (_,(m,n)) = bounds p

-- (submatriz i j p) es la submatriz de p obtenida eliminado la fila i y
-- la columna j. Está definida en el ejercicio 12.

-- ---------------------------------------------------------------------
-- Ejercicio 8. Definir la función 
--    aplicaT :: (Ix a, Num b) => Array a b -> (b -> c) -> Array a c
-- tal que (aplicaT t f) es la tabla obtenida aplicado la función f a
-- los elementos de la tabla t. Por ejemplo,
--    ghci> aplicaT (array (1,5) [(1,6),(2,3),(3,-1),(4,9),(5,20)]) (+1)
--    array (1,5) [(1,7),(2,4),(3,0),(4,10),(5,21)]
--    ghci> :{
--    *Main| aplicaT (array ((1,1),(2,3)) [((1,1),3),((1,2),-1),((1,3),0),
--    *Main|                               ((2,1),0),((2,2),0),((2,3),-1)])
--    *Main|         (*2)
--    *Main| :}
--    array ((1,1),(2,3)) [((1,1),6),((1,2),-2),((1,3),0),
--                         ((2,1),0),((2,2),0),((2,3),-2)]
-- ---------------------------------------------------------------------

aplicaT :: (Ix a, Num b) => Array a b -> (b -> c) -> Array a c
aplicaT t f = listArray (bounds t) [f e | e <- elems t]

-- ---------------------------------------------------------------------
-- Ejercicio 9. Diremos que una matriz es creciente si para toda
-- posición (i,j), el valor de dicha posición es menor o igual que los
-- valores en las posiciones adyacentes de índice superior; es decir,
-- (i+1,j), (i,j+1) e (i+1,j+1) siempre y cuando dichas posiciones
-- existan en la matriz.  
--
-- Definir la función 
--    matrizCreciente :: (Num a,Ord a) =>  Matriz a -> Bool
-- tal que (matrizCreciente p) se verifica si la matriz p es
-- creciente. Por ejemplo, 
--    ghci> matrizCreciente (listArray ((1,1),(3,3)) [1,2,3, 2,3,4, 3,4,5])
--    True
--    ghci> matrizCreciente (listArray ((1,1),(3,3)) [1,2,3, 2,1,4, 3,4,5])
--    False
-- ---------------------------------------------------------------------

matrizCreciente :: (Num a, Ord a) => Matriz a -> Bool
matrizCreciente p = 
    and ([p!(i,j) <= p!(i,j+1)   | i <- [1..m],   j <- [1..n-1]] ++
         [p!(i,j) <= p!(i+1,j)   | i <- [1..m-1], j <- [1..n]] ++
         [p!(i,j) <= p!(i+1,j+1) | i <- [1..m-1], j <- [1..n-1]])
    where (m,n) = snd (bounds p)

-- ---------------------------------------------------------------------
-- Ejercicio 10. Dada una matriz numérica A de dimensiones (m,n) y una
-- matriz booleana B de las mismas dimensiones, y dos funciones f y g,
-- la transformada de A respecto de B, f y g es la matriz C (de las
-- mismas dimensiones), tal que, para cada celda (i,j):   
--    C(i,j) = f(A(i,j)) si B(i,j) es verdadero
--    C(i,j) = f(A(i,j)) si B(i,j) es falso
-- Por ejemplo, si A y B son las matrices
--    |1 2|   |True  False|  
--    |3 4|   |False True |
-- respectivamente, y f y g son dos funciones tales que f(x) = x+1 y
-- g(x) = 2*x, entonces la transformada de A respecto de B, f y g es
--    |2 4|
--    |6 5|
--     
-- Definir la función
--    transformada :: Matriz a -> Matriz Bool -> (a -> b) -> (a -> b) -> Matriz b
-- tal que (transformada a b f g) es la transformada de A respecto de B,
-- f y g. Por ejemplo,
--  ghci> let a = listArray ((1,1),(2,2)) [1,2,3,4] :: Matriz Int
--  ghci> let b = listArray ((1,1),(2,2)) [True,False,False,True] :: Matriz Bool
--  ghci> transformada a b (+1) (*2)
--  array ((1,1),(2,2)) [((1,1),2),((1,2),4),((2,1),6),((2,2),5)]
-- ---------------------------------------------------------------------

transformada :: Matriz a -> Matriz Bool -> (a -> b) -> (a -> b) -> Matriz b
transformada a b f g = 
    array ((1,1),(m,n)) [((i,j),aplica i j) | i <- [1..m], j <- [1..m]]
    where (m,n) = snd (bounds a)
          aplica i j | b!(i,j)   = f (a!(i,j))
                     | otherwise = g (a!(i,j))

-- ---------------------------------------------------------------------
-- Ejercicio 11.1. Un vector se denomina estocástico si todos sus
-- elementos son mayores o iguales que 0 y suman 1.  
-- 
-- Definir la función 
--    vectorEstocastico :: Vector Float -> Bool
-- tal que (vectorEstocastico v) se verifica si v es estocástico. Por
-- ejemplo,  
--    vectorEstocastico (listArray (1,5) [0.1, 0.2, 0, 0, 0.7]) == True
--    vectorEstocastico (listArray (1,5) [0.1, 0.2, 0, 0, 0.9]) == False
-- ---------------------------------------------------------------------

vectorEstocastico :: Vector Float -> Bool
vectorEstocastico v = all (>=0) xs && sum xs == 1
    where xs = elems v

-- ---------------------------------------------------------------------
-- Ejercicio 11.2. Una matriz se denomina estocástica si sus columnas
-- son vectores estocásticos.  
-- 
-- Definir la función 
--    matrizEstocastica :: Matriz Float -> Bool
-- tal que (matrizEstocastico p) se verifica si p es estocástica. Por
-- ejemplo,  
--    matrizEstocastica (listArray ((1,1),(2,2)) [0.1,0.2,0.9,0.8]) == True
--    matrizEstocastica (listArray ((1,1),(2,2)) [0.1,0.2,0.3,0.8]) == False
-- ---------------------------------------------------------------------

matrizEstocastica :: Matriz Float -> Bool        
matrizEstocastica p = all vectorEstocastico (columnas p)

-- (columnas p) es la lista de las columnas de la matriz p. Por ejemplo, 
--    ghci> columnas (listArray ((1,1),(2,3)) [1..6])
--    [array (1,2) [(1,1.0),(2,4.0)],
--     array (1,2) [(1,2.0),(2,5.0)],
--     array (1,2) [(1,3.0),(2,6.0)]]
--    ghci> columnas (listArray ((1,1),(3,2)) [1..6])
--    [array (1,3) [(1,1.0),(2,3.0),(3,5.0)],
--     array (1,3) [(1,2.0),(2,4.0),(3,6.0)]]
columnas :: Matriz Float -> [Vector Float]
columnas p = 
    [array (1,m) [(i,p!(i,j)) | i <- [1..m]] | j <- [1..n]]
    where (_,(m,n)) = bounds p

-- ---------------------------------------------------------------------
-- Ejercicio 12. Definir la función 
--    maximaSuma :: Matriz Int -> Int
-- tal que (maximaSuma p) es el máximo de las sumas de las listas de
-- elementos de la matriz p tales que cada elemento pertenece sólo a una
-- fila y a una columna. Por ejemplo, 
--    ghci> maximaSuma (listArray ((1,1),(3,3)) [1,2,3,8,4,9,5,6,7])
--    17
-- ya que las selecciones, y sus sumas, de la matriz
--    |1 2 3|
--    |8 4 9|
--    |5 6 7|
-- son
--    [1,4,7] --> 12
--    [1,9,6] --> 16
--    [2,8,7] --> 17
--    [2,9,5] --> 16
--    [3,8,6] --> 17
--    [3,4,5] --> 12
-- Hay dos selecciones con máxima suma: [2,8,7] y [3,8,6].
-- ---------------------------------------------------------------------

maximaSuma :: Matriz Int -> Int
maximaSuma p = maximum [sum xs | xs <- selecciones p]

-- (selecciones p) es la lista de las selecciones en las que cada
-- elemento pertenece a un única fila y a una única columna de la matriz
-- p. Por ejemplo,
--    ghci> selecciones (listArray ((1,1),(3,3)) [1,2,3,8,4,9,5,6,7])
--    [[1,4,7],[2,8,7],[3,4,5],[2,9,5],[3,8,6],[1,9,6]]
selecciones :: Matriz Int -> [[Int]]
selecciones p = 
    [[p!(i,j) | (i,j) <- ijs] | 
     ijs <- [zip [1..n] xs | xs <- permutations [1..n]]] 
    where (_,(m,n)) = bounds p

-- Nota: En la anterior definición se ha usado la función pernutations
-- de Data.List. También se puede definir mediante
permutaciones :: [a] -> [[a]]
permutaciones []     = [[]]
permutaciones (x:xs) = 
    concat [intercala x ys | ys <- permutaciones xs]

-- (intercala x ys) es la lista de las listas obtenidas intercalando x
-- entre los elementos de ys. Por ejemplo, 
--    intercala 1 [2,3]  ==  [[1,2,3],[2,1,3],[2,3,1]]
intercala :: a -> [a] -> [[a]]
intercala x [] = [[x]]
intercala x (y:ys) = (x:y:ys) : [y:zs | zs <- intercala x ys]

-- 2ª solución (mediante submatrices):
maximaSuma2 :: Matriz Int -> Int
maximaSuma2 p 
    | (m,n) == (1,1) = p!(1,1)
    | otherwise = maximum [p!(1,j) 
                  + maximaSuma2 (submatriz 1 j p) | j <- [1..n]]
    where (_,(m,n)) = bounds p

-- (submatriz i j p) es la matriz obtenida a partir de la p eliminando
-- la fila i y la columna j. Por ejemplo, 
--    ghci> submatriz 2 3 (listArray ((1,1),(3,3)) [1,2,3,8,4,9,5,6,7])
--    array ((1,1),(2,2)) [((1,1),1),((1,2),2),((2,1),5),((2,2),6)]
submatriz :: Num a => Int -> Int -> Matriz a -> Matriz a
submatriz i j p = 
    array ((1,1), (m-1,n -1))
          [((k,l), p ! f k l) | k <- [1..m-1], l <- [1..n-1]]
    where (_,(m,n)) = bounds p
          f k l | k < i  && l < j  = (k,l)
                | k >= i && l < j  = (k+1,l)
                | k < i  && l >= j = (k,l+1)
                | otherwise        = (k+1,l+1)

-- ---------------------------------------------------------------------
-- Ejercicio 13. Definir la función 
--    maximos :: Matriz Int -> [Int]
-- tal que (maximos p) es la lista de los máximos locales de la matriz
-- p; es decir de los elementos de p que son mayores que todos sus
-- vecinos. Por ejemplo,  
--    ghci> maximos (listArray ((1,1),(3,4)) [9,4,6,5,8,1,7,3,0,2,5,4])
--    [9,7]
-- ya que los máximos locales de la matriz
--    |9 4 6 5|
--    |8 1 7 3|
--    |0 2 5 4|
-- son 9 y 7.
-- ---------------------------------------------------------------------

maximos :: Matriz Int -> [Int]
maximos p = 
    [p!(i,j) | (i,j) <- indices p,
               and [p!(a,b) < p!(i,j) | (a,b) <- vecinos (i,j)]] 
    where (_,(m,n)) = bounds p
          vecinos (i,j) = [(a,b) | a <- [max 1 (i-1)..min m (i+1)],
                                   b <- [max 1 (j-1)..min n (j+1)],
                                   (a,b) /= (i,j)]

-- ----------------------------------------------------------------------
-- Ejercicio 14. Entre dos matrices de la misma dimensión se puede
-- aplicar distintas operaciones binarias entre los elementos en la
-- misma posición. Por ejemplo, si a y b son las matrices 
--    |3 4 6|     |1 4 2|
--    |5 6 7|     |2 1 2|
-- entonces a+b y a-b son, respectivamente
--    |4 8 8|     |2 0 4|
--    |7 7 9|     |3 5 5|
-- 
-- Definir la función
--    opMatriz :: (Int -> Int -> Int) -> 
--                Matriz Int -> Matriz Int -> Matriz Int
-- tal que (opMatriz f p q) es la matriz obtenida aplicando la operación
-- f entre los elementos de p y q de la misma posición. Por ejemplo,
--    ghci> let a = listArray ((1,1),(2,3)) [3,4,6,5,6,7] :: Matriz Int 
--    ghci> let b = listArray ((1,1),(2,3)) [1,4,2,2,1,2] :: Matriz Int
--    ghci> opMatriz (+) a b
--    array ((1,1),(2,3)) [((1,1),4),((1,2),8),((1,3),8),
--                         ((2,1),7),((2,2),7),((2,3),9)]
--    ghci> opMatriz (-) a b
--    array ((1,1),(2,3)) [((1,1),2),((1,2),0),((1,3),4),
--                         ((2,1),3),((2,2),5),((2,3),5)]
-- --------------------------------------------------------------------- 

-- 1ª definición
opMatriz :: (Int -> Int -> Int) -> 
            Matriz Int -> Matriz Int -> Matriz Int
opMatriz f p q = 
    array ((1,1),(m,n)) [((i,j), f (p!(i,j)) (q!(i,j)))
                      | i <- [1..m], j <- [1..n]] 
    where (_,(m,n)) = bounds p

-- 2ª definición
opMatriz2 :: (Int -> Int -> Int) -> 
            Matriz Int -> Matriz Int -> Matriz Int
opMatriz2 f p q = 
    listArray (bounds p) [f x y | (x,y) <- zip (elems p) (elems q)]

-- ---------------------------------------------------------------------
-- Ejercicio 15. Definir la función 
--    algunMenor :: Matriz Int -> [Int]
-- tal que (algunMenor p) es la lista de los elementos de p que tienen
-- algún vecino menor que él. Por ejemplo,  
--    algunMenor (listArray ((1,1),(3,4)) [9,4,6,5,8,1,7,3,4,2,5,4])
--    [9,4,6,5,8,7,4,2,5,4]          
-- pues sólo el 1 y el 3 no tienen ningún vecino menor en la matriz
--    |9 4 6 5|
--    |8 1 7 3|
--    |4 2 5 4|
-- ---------------------------------------------------------------------        

algunMenor :: Matriz Int -> [Int]
algunMenor p = 
    [p!(i,j) | (i,j) <- indices p,
               or [p!(a,b) < p!(i,j) | (a,b) <- vecinos (i,j)]] 
    where (_,(m,n)) = bounds p
          vecinos (i,j) = [(a,b) | a <- [max 1 (i-1)..min m (i+1)],
                                   b <- [max 1 (j-1)..min n (j+1)],
                                   (a,b) /= (i,j)]

-- ---------------------------------------------------------------------
-- Ejercicio 16.1. Definir la función
--    esAutovector :: (Fractional a, Eq a) => 
--                    Vector a -> Matriz a -> Bool
-- tal que (esAutovector v p) compruebe si v es un autovector de p
-- (es decir, el producto de v por p es un vector proporcional a
-- v). Por ejemplo, 
--    ghci> let p1 = listArray ((1,1),(3,3)) [1,0,0,0,0,1,0,1,0] :: Matriz Float
--    ghci> let v1 = listArray (1,3) [0,-1,1] :: Vector Float
--    ghci> let v2 = listArray (1,3) [1,2,1] :: Vector Float
--    ghci> esAutovector v1 p1 
--    True
--    ghci> esAutovector v2 p1 
--    False
-- ---------------------------------------------------------------------

esAutovector :: (Fractional a, Eq a) => Vector a -> Matriz a -> Bool
esAutovector v p = proporcional (producto p v) v

-- (producto p v) es el producto de la matriz p por el vector v. Por
-- ejemplo, 
--    producto p1 v1  = array (1,3) [(1,0.0),(2,1.0),(3,-1.0)]
--    producto p1 v2  = array (1,3) [(1,1.0),(2,1.0),(3,2.0)]
producto :: (Fractional a, Eq a) => Matriz a -> Vector a -> Vector a 
producto p v =
    array (1,n) [(i, sum [p!(i,j)*v!j | j <- [1..n]]) | i <- [1..m]]
    where (_,n)     = bounds v
          (_,(m,_)) = bounds p

-- (proporcional v1 v2) se verifica si los vectores v1 y v2 son
-- proporcionales. Por ejemplo,
--    proporcional v1 v1                           = True
--    proporcional v1 v2                           = False
--    proporcional v1 (listArray (1,3) [0,-5,5])   = True
--    proporcional v1 (listArray (1,3) [0,-5,4])   = False
--    proporcional (listArray (1,3) [0,-5,5]) v1   = True
--    proporcional v1 (listArray (1,3) [0,0,0])    = True
--    proporcional (listArray (1,3) [0,0,0]) v1    = False
proporcional :: (Fractional a, Eq a) => Vector a -> Vector a -> Bool
proporcional v1 v2 
    | esCero v1 = esCero v2
    | otherwise = and [v2!i == k*(v1!i) | i <- [1..n]]
    where (_,n) = bounds v1
          j     = minimum [i | i <- [1..n], v1!i /= 0]
          k     = (v2!j) / (v1!j)

-- (esCero v) se verifica si v es el vector 0.
esCero :: (Fractional a, Eq a) => Vector a -> Bool 
esCero v = null [x | x <- elems v, x /= 0]

-- ---------------------------------------------------------------------
-- Ejercicio 16.2. Definir la función
--    autovalorAsociado :: (Fractional a, Eq a) => 
--                         Matriz a -> Vector a -> Maybe a
-- tal que si v es un autovector de p, calcule el autovalor asociado.
-- Por ejemplo,
--    ghci> let p1 = listArray ((1,1),(3,3)) [1,0,0,0,0,1,0,1,0] :: Matriz Float
--    ghci> let v1 = listArray (1,3) [0,-1,1] :: Vector Float
--    ghci> let v2 = listArray (1,3) [1,2,1] :: Vector Float
--    autovalorAsociado p1 v1 == Just (-1.0)
--    autovalorAsociado p1 v2 == Nothing
-- ---------------------------------------------------------------------

autovalorAsociado :: (Fractional a, Eq a) => 
                     Matriz a -> Vector a -> Maybe a
autovalorAsociado p v
    | esAutovector v p = Just (producto p v ! j / v ! j)
    | otherwise        = Nothing
    where (_,n) = bounds v
          j     = minimum [i | i <- [1..n], v!i /= 0]

-- ---------------------------------------------------------------------
-- Ejercicio 17. Definir la función 
--    borraCols :: Int -> Int -> Matriz Int -> Matriz Int
-- tal que (borraCols j1 j2 p) es la matriz obtenida borrando las
-- columnas j1 y j2 (con j1 < j2) de la matriz p. Por ejemplo,
--    ghci> let p = listArray ((1,1),(2,4)) [1..8] :: Matriz Int
--    ghci> p
--    array ((1,1),(2,4)) [((1,1),1),((1,2),2),((1,3),3),((1,4),4),
--                         ((2,1),5),((2,2),6),((2,3),7),((2,4),8)]
--    ghci> borraCols 1 3 p
--    array ((1,1),(2,2)) [((1,1),2),((1,2),4),((2,1),6),((2,2),8)]
--    ghci> borraCols 2 3 p
--    array ((1,1),(2,2)) [((1,1),1),((1,2),4),((2,1),5),((2,2),8)]
-- ---------------------------------------------------------------------
        
-- 1ª definición: 
borraCols :: Int -> Int -> Matriz Int -> Matriz Int
borraCols j1 j2 p = 
  borraCol (j2-1) (borraCol j1 p)

-- (borraCol j1 p) es la matriz obtenida borrando la columna j1 de la
-- matriz p. Por ejemplo,
--    ghci> let p = listArray ((1,1),(2,4)) [1..8]
--    ghci> borraCol 2 p
--    array ((1,1),(2,3)) [((1,1),1),((1,2),3),((1,3),4),
--                         ((2,1),5),((2,2),7),((2,3),8)]
--    ghci> borraCol 3 p
--    array ((1,1),(2,3)) [((1,1),1),((1,2),2),((1,3),4),
--                         ((2,1),5),((2,2),6),((2,3),8)]
borraCol :: Int -> Matriz Int -> Matriz Int
borraCol j1 p = 
  array ((1,1),(m,n-1))
        [((i,j), f i j)| i <- [1..m], j <- [1..n-1]]
  where (_,(m,n)) = bounds p
        f i j | j < j1    = p!(i,j)
              | otherwise = p!(i,j+1)

-- 2ª definición: 
borraCols2 :: Int -> Int -> Matriz Int -> Matriz Int
borraCols2 j1 j2 p = 
  array ((1,1),(m,n-2))
        [((i,j), f i j)| i <- [1..m], j <- [1..n-2]]
  where (_,(m,n)) = bounds p
        f i j | j < j1    = p!(i,j)
              | j < j2-1  = p!(i,j+1)
              | otherwise = p!(i,j+2)

-- 3ª definición: 
borraCols3 :: Int -> Int -> Matriz Int -> Matriz Int
borraCols3 j1 j2 p = 
  listArray ((1,1),(n,m-2)) [p!(i,j) | i <- [1..n], j <- [1..m], j/=j1 && j/=j2]
  where (_,(n,m)) = bounds p

-- ---------------------------------------------------------------------
-- Ejercicio 18.1. Definir la función 
--    cambiaM :: (Int, Int) -> Matriz Int -> Matriz Int
-- tal que (cambiaM i p) es la matriz obtenida cambiando en p los
-- elementos de la fila y la columna en i transformando los 0 en 1 y
-- viceversa. El valor en i cambia solo una vez. Por ejemplo,
--    ghci> cambiaM (2,3) (listArray ((1,1),(3,3)) [1,0,1, 0,7,1, 1,1,1])
--    array ((1,1),(3,3)) [((1,1),1),((1,2),0),((1,3),0),
--                         ((2,1),1),((2,2),7),((2,3),0),
--                         ((3,1),1),((3,2),1),((3,3),0)]
-- ---------------------------------------------------------------------

cambiaM :: (Int, Int) -> Matriz Int -> Matriz Int
cambiaM (a,b) p = array (bounds p) [((i,j),f i j) | (i,j) <- indices p]
       where f i j  | i == a || j == b = cambia (p!(i,j))
                    | otherwise = p!(i,j)
             cambia x | x == 0    = 1
                      | x == 1    = 0
                      | otherwise = x

-- ---------------------------------------------------------------------
-- Ejercicio 18.2. Definir la función 
--    quitaRepetidosFila :: Int -> Matriz Int -> Matriz Int
-- tal que (quitaRepetidosFila i p) es la matriz obtenida a partir de p
-- eliminando los elementos repetidos de la fila i y rellenando con
-- ceros al final hasta completar la fila. Por ejemplo,
--    ghci> let m1 = listArray ((1,1),(3,3)) [1,0,1, 0,7,1, 1,1,1] :: Matriz Int
--    ghci> quitaRepetidosFila 1 m1
--    array ((1,1),(3,3)) [((1,1),1),((1,2),0),((1,3),0),
--                         ((2,1),0),((2,2),7),((2,3),1),
--                         ((3,1),1),((3,2),1),((3,3),1)]
--    ghci> quitaRepetidosFila 2 m1
--    array ((1,1),(3,3)) [((1,1),1),((1,2),0),((1,3),1),
--                         ((2,1),0),((2,2),7),((2,3),1),
--                         ((3,1),1),((3,2),1),((3,3),1)]
--    ghci> quitaRepetidosFila 3 m1
--    array ((1,1),(3,3)) [((1,1),1),((1,2),0),((1,3),1),
--                         ((2,1),0),((2,2),7),((2,3),1),
--                         ((3,1),1),((3,2),0),((3,3),0)]
-- ---------------------------------------------------------------------

quitaRepetidosFila :: Int -> Matriz Int -> Matriz Int
quitaRepetidosFila x p = 
    array (bounds p) [((i,j),f i j) | (i,j) <- indices p]
        where f i j | i == x    = (cambia (fila i p)) !! (j-1)
                    | otherwise = p!(i,j)

-- (fila i p) es la fila i-ésima de la matriz p. Por ejemplo,
--    ghci> m1
--    array ((1,1),(3,3)) [((1,1),1),((1,2),0),((1,3),1),
--                         ((2,1),0),((2,2),7),((2,3),1),
--                         ((3,1),1),((3,2),1),((3,3),1)]
--    ghci> fila 2 m1
--    [0,7,1]
fila :: Int -> Matriz Int -> [Int]
fila i p = [p!(i,j) | j <- [1..n]]
    where (_,(_,n)) = bounds p

-- (cambia xs) es la lista obtenida eliminando los elementos repetidos
-- de xs y completando con ceros al final para que tenga la misma
-- longitud que xs. Por ejemplo,
--   cambia [2,3,2,5,3,2]  ==  [2,3,5,0,0,0]
cambia :: [Int] -> [Int]
cambia xs = ys ++ replicate (n-m) 0
    where ys = nub xs
          n  = length xs
          m  = length ys

-- ------------------------------------------------------------------
-- Ejercicio 19. Definir la función
--    sumaVecinos :: Matriz Int -> Matriz Int
-- tal que (sumaVecinos p) es la matriz obtenida al escribir en la 
-- posicion (i,j) la suma de los todos vecinos del elemento que ocupa 
-- el lugar (i,j) en la matriz p. Por ejemplo,
--    ghci> sumaVecinos (listArray ((1,1),(3,3)) [0,1,3, 1,2,0, 0,5,7])
--    array ((1,1),(3,3)) [((1,1),4),((1,2), 6),((1,3), 3),
--                         ((2,1),8),((2,2),17),((2,3),18),
--                         ((3,1),8),((3,2),10),((3,3), 7)]
-- ------------------------------------------------------------------

sumaVecinos :: Matriz Int -> Matriz Int
sumaVecinos p =  
    array ((1,1),(m,n)) 
          [((i,j), f i j) | i <- [1..m], j <- [1..n]] 
    where (_,(m,n)) = bounds p
          f i j = sum [p!(i+a,j+b) | a <- [-1..1], b <- [-1..1], 
                                     a /= 0 || b /= 0,
                                     inRange (bounds p) (i+a,j+b)]

-- ----------------------------------------------------------------------
-- Ejercicio 20. Definir la función
--    ampliaColumnas :: Matriz -> Matriz -> Matriz
-- tal que (ampliaColumnas p q) es la matriz construida añadiendo las
-- columnas de la matriz q a continuación de las de p (se supone que
-- tienen el mismo número de filas). Por ejemplo, si p y q representa
-- las dos primeras matrices, entonces (ampliaColumnas p q) es la
-- tercera  
--    |0 1|    |4 5 6|    |0 1 4 5 6| 
--    |2 3|    |7 8 9|    |2 3 7 8 9|
-- En Haskell,
--    ghci> let p = listArray ((1,1),(2,2)) [0..3] :: Matriz Int
--    ghci> let q = listArray ((1,1),(2,3)) [4..9] :: Matriz Int
--    ghci> ampliaColumnas p q
--    array ((1,1),(2,5)) 
--          [((1,1),0),((1,2),1),((1,3),4),((1,4),5),((1,5),6),
--           ((2,1),2),((2,2),3),((2,3),7),((2,4),8),((2,5),9)]
-- --------------------------------------------------------------------- 

ampliaColumnas :: Matriz a -> Matriz a -> Matriz a
ampliaColumnas p1 p2 =
  array ((1,1),(m,n1+n2)) [((i,j), f i j) | i <- [1..m], j <- [1..n1+n2]]
    where ((_,_),(m,n1)) = bounds p1
          ((_,_),(_,n2)) = bounds p2
          f i j | j <= n1   = p1!(i,j)
                | otherwise = p2!(i,j-n1) 

-- ---------------------------------------------------------------------
-- Ejercicio 21. Una matriz cuadrada es bisimétrica si es simétrica
-- respecto de su diagonal principal y de su diagonal secundaria. 
-- 
-- Definir la función
--    esBisimetrica :: Eq a => Matriz a -> Bool
-- tal que (esBisimetrica p) se verifica si p es bisimétrica. Por
-- ejemplo,           
--    esBisimetrica ejM1  ==  True
--    esBisimetrica ejM2  ==  False
-- donde las matrices ejM1 y ejM2 están definidas por
--    ejM1, ejM2 :: Matriz Int
--    ejM1 = listArray ((1,1),(5,5)) [1,2,3,4,5,
--                                    2,6,8,9,4,
--                                    3,8,0,8,3,
--                                    4,9,8,6,2,
--                                    5,4,3,2,1]
--    
--    ejM2 = listArray ((1,1),(3,3)) [1,2,3,
--                                    2,6,8,
--                                    3,8,0]
-- ---------------------------------------------------------------------

ejM1, ejM2 :: Matriz Int
ejM1 = listArray ((1,1),(5,5)) [1,2,3,4,5,
                                2,6,8,9,4,
                                3,8,0,8,3,
                                4,9,8,6,2,
                                5,4,3,2,1]

ejM2 = listArray ((1,1),(3,3)) [1,2,3,
                                2,6,8,
                                3,8,0]

-- 1ª definición:
esBisimetrica :: Eq a => Matriz a -> Bool
esBisimetrica p =
    and [p!(i,j) == p!(j,i) | i <- [1..n], j <- [1..n]] &&
    and [p!(i,j) == p!(n+1-j,n+1-i) | i <- [1..n], j <- [1..n]]
    where ((_,_),(n,_)) = bounds p

-- 2ª definición:
esBisimetrica2 :: Eq a => Matriz a -> Bool
esBisimetrica2 p = p == simetrica p && p == simetricaS p
        
-- (simetrica p) es la simétrica de la matriz p respecto de la diagonal
-- principal. Por ejemplo,
--    ghci> simetrica (listArray ((1,1),(4,4)) [1..16])
--    array ((1,1),(4,4)) [((1,1),1),((1,2),5),((1,3), 9),((1,4),13),
--                         ((2,1),2),((2,2),6),((2,3),10),((2,4),14),
--                         ((3,1),3),((3,2),7),((3,3),11),((3,4),15),
--                         ((4,1),4),((4,2),8),((4,3),12),((4,4),16)]
simetrica :: Eq a => Matriz a -> Matriz a
simetrica p =
    array ((1,1),(n,n)) [((i,j),p!(j,i)) | i <- [1..n], j <- [1..n]]
    where ((_,_),(n,_)) = bounds p

-- (simetricaS p) es la simétrica de la matriz p respecto de la diagonal
-- secundaria. Por ejemplo,
--    ghci> simetricaS (listArray ((1,1),(4,4)) [1..16])
--    array ((1,1),(4,4)) [((1,1),16),((1,2),12),((1,3),8),((1,4),4),
--                         ((2,1),15),((2,2),11),((2,3),7),((2,4),3),
--                         ((3,1),14),((3,2),10),((3,3),6),((3,4),2),
--                         ((4,1),13),((4,2), 9),((4,3),5),((4,4),1)]
simetricaS :: Matriz a -> Matriz a
simetricaS p =
    array ((1,1),(n,n)) [((i,j),p!(n+1-j,n+1-i)) | i <- [1..n], j <- [1..n]]
    where ((_,_),(n,_)) = bounds p

-- ----------------------------------------------------------------------
-- Ejercicio 22. Definir la función
--    matrizPorBloques :: Matriz -> Matriz -> Matriz -> Matriz -> Matriz
-- tal que (matrizPorBloques p1 p2 p3 p4) es la matriz cuadrada de orden
-- 2nx2n construida con las matrices cuadradas de orden nxn p1, p2 p3 y
-- p4 de forma que p1 es su bloque superior izquierda, p2 es su bloque
-- superior derecha, p3 es su bloque inferior izquierda y p4 es su bloque
-- inferior derecha. Por ejemplo, 
--    ghci> let p1 = listArray ((1,1),(2,2)) [1,2,3,4] :: Matriz Int
--    ghci> let p2 = listArray ((1,1),(2,2)) [6,5,7,8] :: Matriz Int
--    ghci> let p3 = listArray ((1,1),(2,2)) [0,6,7,1] :: Matriz Int
--    ghci> let p4 = listArray ((1,1),(2,2)) [5,2,8,3] :: Matriz Int
--    ghci> matrizPorBloques p1 p2 p3 p4
--    array ((1,1),(4,4)) [((1,1),1),((1,2),2),((1,3),6),((1,4),5),
--                         ((2,1),3),((2,2),4),((2,3),7),((2,4),8),
--                         ((3,1),0),((3,2),6),((3,3),5),((3,4),2),
--                         ((4,1),7),((4,2),1),((4,3),8),((4,4),3)]
-- --------------------------------------------------------------------- 

matrizPorBloques :: Matriz a -> Matriz a -> Matriz a -> Matriz a ->
                    Matriz a
matrizPorBloques p1 p2 p3 p4 =
  array ((1,1),(m,m)) [((i,j), f i j) | i <- [1..m], j <- [1..m]]
  where ((_,_),(n,_)) = bounds p1
        m = 2*n
        f i j | i <= n && j <= n = p1!(i,j)
              | i <= n && j >  n = p2!(i,j-n)
              | i >  n && j <= n = p3!(i-n,j)
              | i >  n && j >  n = p4!(i-n,j-n)                             

-- ---------------------------------------------------------------------
-- Ejercicio 23. Definir la función
--    sumaColumnas :: Matriz Int -> Matriz Int
-- tal que (sumaColumnas p) es la matriz obtenida sumando a cada columna
-- la anterior salvo a la primera que le suma la última columna. Por
-- ejemplo, 
--    ghci> sumaColumnas (listArray ((1,1),(3,3)) [4,1,3, 1,2,8, 6,5,7])
--    array ((1,1),(3,3)) [((1,1),7), ((1,2),5), ((1,3),4),
--                         ((2,1),9), ((2,2),3), ((2,3),10),
--                         ((3,1),13),((3,2),11),((3,3),12)]
-- es decir, el resultado es la matriz
--    | 7  5  4|
--    | 9  3 10|
--    |13 11 12|
-- ------------------------------------------------------------------

sumaColumnas :: Matriz Int -> Matriz Int
sumaColumnas p =  
    array ((1,1),(m,n)) 
          [((i,j), f i j) | i <- [1..m], j <- [1..n]] 
    where (_,(m,n)) = bounds p
          f i 1 = p!(i,1) + p!(i,m)
          f i j = p!(i,j) + p!(i,j-1)

-- ---------------------------------------------------------------------
-- Ejercicio 24. La matrices piramidales son las formadas por unos y
-- ceros de forma que los unos forman una pirámide. Por ejemplo,  
--   |1|   |0 1 0|   |0 0 1 0 0|   |0 0 0 1 0 0 0|
--         |1 1 1|   |0 1 1 1 0|   |0 0 1 1 1 0 0|
--                   |1 1 1 1 1|   |0 1 1 1 1 1 0|
--                                 |1 1 1 1 1 1 1|
-- 
-- En Haskell, las matrices anteriores se definen por
--    p1, p2, p3 :: Matriz Int
--    p1 = listArray ((1,1),(1,1)) [1]
--    p2 = listArray ((1,1),(2,3)) [0,1,0,
--                                  1,1,1]
--    p3 = listArray ((1,1),(3,5)) [0,0,1,0,0,
--                                  0,1,1,1,0,
--                                  1,1,1,1,1]
--
-- Definir la función
--    esPiramidal :: (Eq a, Num a) => Matriz a -> Bool
-- tal que (esPiramidal p) se verifica si la matriz p es piramidal. Por
-- ejemplo, 
--    esPiramidal p3                                        ==  True
--    esPiramidal (listArray ((1,1),(2,3)) [0,1,0, 1,5,1])  ==  False
--    esPiramidal (listArray ((1,1),(2,3)) [0,1,1, 1,1,1])  ==  False
--    esPiramidal (listArray ((1,1),(2,3)) [0,1,0, 1,0,1])  ==  False
-- ---------------------------------------------------------------------

p1, p2, p3 :: Matriz Int
p1 = listArray ((1,1),(1,1)) [1]
p2 = listArray ((1,1),(2,3)) [0,1,0,
                              1,1,1]
p3 = listArray ((1,1),(3,5)) [0,0,1,0,0,
                              0,1,1,1,0,
                              1,1,1,1,1]

esPiramidal :: (Eq a, Num a) => Matriz a -> Bool
esPiramidal p =
    p == listArray ((1,1),(n,m)) (concat (filasPiramidal n))
    where (_,(n,m)) = bounds p 

-- (filasPiramidal n) es la lista dela filas de la matriz piramidal de n
-- filas. Por ejemplo,
--    filasPiramidal 1  ==  [[1]]
--    filasPiramidal 2  ==  [[0,1,0],[1,1,1]]
--    filasPiramidal 3  ==  [[0,0,1,0,0],[0,1,1,1,0],[1,1,1,1,1]]
filasPiramidal 1 = [[1]]
filasPiramidal n = [0:xs++[0] | xs <- filasPiramidal (n-1)] ++ 
                   [replicate (2*n-1) 1]

-- 2ª definición
-- =============

esPiramidal2 :: (Eq a, Num a) => Matriz a -> Bool
esPiramidal2 p =
    p == piramidal n
    where (_,(n,_)) = bounds p 

-- (piramidal n) es la matriz piramidal con n filas. Por ejemplo,
--    ghci> piramidal 3
--    array ((1,1),(3,5)) [((1,1),0),((1,2),0),((1,3),1),((1,4),0),((1,5),0),
--                         ((2,1),0),((2,2),1),((2,3),1),((2,4),1),((2,5),0),
--                         ((3,1),1),((3,2),1),((3,3),1),((3,4),1),((3,5),1)]
piramidal :: (Eq a, Num a) => Int -> Matriz a
piramidal n =
    array ((1,1),(n,2*n-1)) [((i,j),f i j) | i <- [1..n], j <- [1..2*n-1]]
    where f i j | j <= n-i  = 0
                | j <  n+i  = 1
                | otherwise = 0

-- ----------------------------------------------------------------------
-- Ejercicio 25. El algoritmo de Jacobi se utiliza para calcular el
-- gradiente de temperatura en una malla de cuerpos dispuestos en dos
-- dimensiones. Se emplea para ello una matriz con el siguiente
-- contenido:  
--    a) Se define una frontera, que son los elementos de la primera fila,
--       primera columna, última fila y última columna. Estos elementos
--       indican la temperatura exterior, y su valor es siempre constante.
--    b) Los elementos del interior indican la temperatura de cada
--       cuerpo.
-- En cada iteración del algoritmo la matriz p se transforma en otra q,
-- de la misma dimensión, cuyos elementos son: 
--    a) Elementos de la frontera: 
--          q(i,j)=p(i,j).
--    b) Elementos del interior:
--          q(i,j)=0.2*(p(i,j)+p(i+1,j)+p(i-1,j)+p(i,j+1)+p(i,j-1))
-- Por ejemplo, la transformada de la matriz de la izquierda es la de la
-- derecha  
--    |2, 2, 2, 2, 2|          |2.0, 2.0, 2.0, 2.0, 2.0|
--    |2, 0, 0, 0, 2|          |2.0, 0.8, 0.4, 0.8, 2.0|
--    |2, 0, 0, 0, 2|          |2.0, 0.4, 0.0, 0.4, 2.0|
--    |2, 0, 0, 0, 2|          |2.0, 0.8, 0.4, 0.8, 2.0|
--    |2, 2, 2, 2, 2|          |2.0, 2.0, 2.0, 2.0, 2.0|
-- 
-- En Haskell, las dos matrices anteriores se representan por
--    matriz1, matriz2 :: Matriz Float
--    matriz1 = listArray ((1,1),(5,5)) ([2, 2, 2, 2, 2, 
--                                        2, 0, 0, 0, 2, 
--                                        2, 0, 0, 0, 2, 
--                                        2, 0, 0, 0, 2, 
--                                        2, 2, 2, 2, 2])       
--    matriz2 = listArray ((1,1),(5,5)) ([2.0, 2.0, 2.0, 2.0, 2.0, 
--                                        2.0, 0.8, 0.4, 0.8, 2.0, 
--                                        2.0, 0.4, 0.0, 0.4, 2.0, 
--                                        2.0, 0.8, 0.4, 0.8, 2.0, 
--                                        2.0, 2.0, 2.0, 2.0, 2.0])
-- 
-- Definir la función 
--    iteracion_jacobi:: Matriz Float -> Matriz Float
-- tal que (iteracion_jacobi p) es la matriz obtenida aplicándole una
-- transformación de Jacobi a la matriz p. Por ejemplo,
--    iteracion_jacobi matriz1  ==  matriz2
-- ---------------------------------------------------------------------

matriz1 :: Matriz Float
matriz1 = listArray ((1,1),(5,5)) ([2, 2, 2, 2, 2, 
                                    2, 0, 0, 0, 2, 
                                    2, 0, 0, 0, 2, 
                                    2, 0, 0, 0, 2, 
                                    2, 2, 2, 2, 2])       

matriz2 :: Matriz Float                                 
matriz2 = listArray ((1,1),(5,5)) ([2.0, 2.0, 2.0, 2.0, 2.0, 
                                    2.0, 0.8, 0.4, 0.8, 2.0, 
                                    2.0, 0.4, 0.0, 0.4, 2.0, 
                                    2.0, 0.8, 0.4, 0.8, 2.0, 
                                    2.0, 2.0, 2.0, 2.0, 2.0])

-- 1ª definición:
iteracion_jacobi :: Matriz Float -> Matriz Float
iteracion_jacobi p = array ((1,1),(n,m)) [((i,j), f i j) | i <- [1..n], j<-[1..m]]
    where (_,(n,m)) = bounds p
          f i j | frontera (i,j) = p!(i,j)
                | otherwise      = 0.2*(p!(i,j)+p!(i+1,j)+p!(i-1,j)+p!(i,j+1)+p!(i,j-1))
          frontera (i,j) = i == 1 || i == n || j == 1 || j == m

-- 2ª definición:
iteracion_jacobi2 :: Matriz Float -> Matriz Float              
iteracion_jacobi2 p = 
    array ((1,1),(n,m)) 
          ([((i,j), 0.2*(p!(i,j)+p!(i+1,j)+p!(i-1,j)+p!(i,j+1)+p!(i,j-1))) | 
            i <- [2..n-1], j <- [2..m-1]] ++
           [((i,j),p!(i,j)) | i <- [1,n],  j <- [1..m]]++ 
           [((i,j),p!(i,j)) | i <- [1..n], j <- [1,m]])  
    where (_,(n,m)) = bounds p

-- ---------------------------------------------------------------------
-- Ejercicio 26.1. Una matriz tridiagonal es aquella en la que sólo hay
-- elementos distintos de 0 en la diagonal principal o en las diagonales
-- por encima y por debajo de la diagonal principal. Por ejemplo, 
--    ( 1 2 0 0 0 0 )
--    ( 3 4 5 0 0 0 )
--    ( 0 6 7 8 0 0 )
--    ( 0 0 9 1 2 0 )
--    ( 0 0 0 3 4 5 )
--    ( 0 0 0 0 6 7 )
-- 
-- Definir la función 
--    creaTridiagonal :: Int -> Matriz Int
-- tal que (creaTridiagonal n) es la siguiente matriz tridiagonal
-- cuadrada con n filas y n columnas:
--    ( 1 1 0 0 0 0 ... 0  0  )
--    ( 1 2 2 0 0 0 ... 0  0  )
--    ( 0 2 3 3 0 0 ... 0  0  )
--    ( 0 0 3 4 4 0 ... 0  0  )
--    ( 0 0 0 4 5 5 ... 0  0  )
--    ( 0 0 0 0 5 6 ... 0  0  )
--    ( ..................... )
--    ( 0 0 0 0 0 0 ... n  n  )
--    ( 0 0 0 0 0 0 ... n n+1 )
-- Por ejemplo,
--    ghci> creaTridiagonal 4
--    array ((1,1),(4,4)) [((1,1),1),((1,2),1),((1,3),0),((1,4),0),
--                         ((2,1),1),((2,2),2),((2,3),2),((2,4),0),
--                         ((3,1),0),((3,2),2),((3,3),3),((3,4),3),
--                         ((4,1),0),((4,2),0),((4,3),3),((4,4),4)]
-- ----------------------------------------------------------------------------

creaTridiagonal :: Int -> Matriz Int
creaTridiagonal n =
    array ((1,1),(n,n))
          [((i,j),valores i j) | i <- [1..n], j <- [1..n]]
    where valores i j | i == j     = i
                      | i == j+1   = j
                      | i+1 == j   = i
                      | otherwise  = 0

-- ----------------------------------------------------------------------------
-- Ejercicio 26.2. Definir la función 
--    esTridiagonal :: Matriz Int -> Bool
-- tal que (esTridiagonal p) se verifica si la matriz p es tridiagonal. Por 
-- ejemplo,
--    esTridiagonal (creaTridiagonal 5)               ==  True
--    esTridiagonal (listArray ((1,1),(3,3)) [1..9])  ==  False
-- ----------------------------------------------------------------------------

esTridiagonal :: Matriz Int -> Bool
esTridiagonal p =
    and [p!(i,j) == 0 | i <- [1..m], j <- [1..n], (j < i-1 || j > i+1)]
    where (_,(m,n)) = bounds p

-- ---------------------------------------------------------------------
-- Ejercicio 27. La matriz de Vandermonde generada por
-- [a(1),a(2),a(3),...,a(n)] es la siguiente 
--    |1  a(1)  a(1)^2 ... a(1)^{n-1}|
--    |1  a(2)  a(2)^2 ... a(2)^{n-1}|
--    |1  a(3)  a(3)^2 ... a(3)^{n-1}|
--    |.  .     .          .         |
--    |.  .     .          .         |
--    |.  .     .          .         |
--    |1  a(n)  a(n)^2 ... a(n)^{n-1}|
--
-- Definir la función 
--    vandermonde:: [Integer] -> Matriz Integer
-- tal que (vandermonde xs) es la matriz de Vandermonde cuyos
-- generadores son los elementos de xs. Por ejemplo,
--    ghci> vandermonde [5,2,3,4]
--    array ((1,1),(4,4)) [((1,1),1),((1,2),5),((1,3),25),((1,4),125),
--                         ((2,1),1),((2,2),2),((2,3), 4),((2,4),  8),
--                         ((3,1),1),((3,2),3),((3,3), 9),((3,4), 27),
--                         ((4,1),1),((4,2),4),((4,3),16),((4,4), 64)]
-- ---------------------------------------------------------------------

-- 1ª solución
-- ===========

vandermonde1 :: [Integer] -> Matriz Integer
vandermonde1 xs = array ((1,1), (n,n)) 
                  [((i,j), f i j) | i <- [1..n], j <- [1..n]]
      where n     = length xs
            f i j = (xs!!(i-1))^(j-1)

-- 2ª solución
-- ===========

vandermonde2 :: [Integer] -> Matriz Integer
vandermonde2 xs = listArray ((1,1),(n,n)) (concat (listaVandermonde xs))
    where n = length xs

-- (listaVandermonde xs) es la lista correspondiente a la matriz de
-- Vandermonde generada por xs. Por ejemplo,
--    ghci> listaVandermonde [5,2,3,4]
--    [[1,5,25,125],[1,2,4,8],[1,3,9,27],[1,4,16,64]]
listaVandermonde :: [Integer] -> [[Integer]]
listaVandermonde xs = [[x^i | i <- [0..n-1]] | x <- xs]
    where n = length xs

-- ---------------------------------------------------------------------
-- Ejercicio 28. Una matriz es monomial si en cada una de sus filas y
-- columnas todos los elementos son nulos excepto 1. Por ejemplo, de las
-- matrices  
--    |0  0 3 0|     |0  0 3 0|
--    |0 -2 0 0|     |0 -2 0 0|
--    |1  0 0 0|     |1  0 0 0|
--    |0  0 0 1|     |0  1 0 1|
-- la primera es monomial y la segunda no lo es.
--
-- En Haskell, las matrices anteriores se definen por
--    ej1, ej2 :: Matriz Int
--    ej1 = listArray ((1,1),(4,4)) [0,  0, 3, 0,
--                                   0, -2, 0, 0,
--                                   1,  0, 0, 0,
--                                   0,  0, 0, 1]
--    ej2 = listArray ((1,1),(4,4)) [0,  0, 3, 0,
--                                   0, -2, 0, 0,
--                                   1,  0, 0, 0,
--                                   0,  1, 0, 1]
-- Definir la función 
--    esMonomial :: Matriz Int -> Bool
-- tal que (esMonomial p) se verifica si la matriz p es monomial. Por
-- ejemplo, 
--    esMonomial ej1  ==  True
--    esMonomial ej2  ==  False
-- ---------------------------------------------------------------------

ej1, ej2 :: Matriz Int
ej1 = listArray ((1,1),(4,4)) [0,  0, 3, 0,
                               0, -2, 0, 0,
                               1,  0, 0, 0,
                               0,  0, 0, 1]
ej2 = listArray ((1,1),(4,4)) [0,  0, 3, 0,
                               0, -2, 0, 0,
                               1,  0, 0, 0,
                               0,  1, 0, 1]

esMonomial :: Matriz Int -> Bool
esMonomial p = all esListaMonomial (filas ++ columnas)
    where filas     = [[p!(i,j) | j <- [1..n]] | i <- [1..m]]
          columnas  = [[p!(i,j) | i <- [1..m]] | j <- [1..n]]
          (_,(m,n)) = bounds p

-- (esListaMonomial xs) se verifica si todos los elementos de xs excepto
-- uno son nulos. Por ejemplo,
--    esListaMonomial [0,3,0,0]  ==  True
--    esListaMonomial [0,3,0,2]  ==  False
--    esListaMonomial [0,0,0,0]  ==  False
esListaMonomial :: [Int] -> Bool
esListaMonomial xs = length (filter (/=0) xs) == 1

-- ---------------------------------------------------------------------
-- Ejercicio 29. El triángulo de Pascal es un triángulo de números 
--          1
--         1 1
--        1 2 1
--      1  3 3  1
--     1 4  6  4 1
--    1 5 10 10 5 1
--   ...............
-- construido de la siguiente forma
-- * la primera fila está formada por el número 1;
-- * las filas siguientes se construyen sumando los números adyacentes
--   de la fila superior y añadiendo un 1 al principio y al final de la
--   fila. 
--
-- La matriz de Pascal es la matriz cuyas filas son los elementos de la
-- correspondiente fila del triángulo de Pascal completadas con
-- ceros. Por ejemplo, la matriz de Pascal de orden 6 es
--    |1 0  0  0 0 0|
--    |1 1  0  0 0 0|
--    |1 2  1  0 0 0|
--    |1 3  3  1 0 0|
--    |1 4  6  4 1 0|
--    |1 5 10 10 5 1|
-- 
-- Definir la función
--    matrizPascal :: Int -> Matriz Int 
-- tal que (matrizPascal n) es la matriz de Pascal de orden n. Por
-- ejemplo, 
--    ghci> matrizPascal 5
--    array ((1,1),(5,5)) 
--          [((1,1),1),((1,2),0),((1,3),0),((1,4),0),((1,5),0),
--           ((2,1),1),((2,2),1),((2,3),0),((2,4),0),((2,5),0),
--           ((3,1),1),((3,2),2),((3,3),1),((3,4),0),((3,5),0),
--           ((4,1),1),((4,2),3),((4,3),3),((4,4),1),((4,5),0),
--           ((5,1),1),((5,2),4),((5,3),6),((5,4),4),((5,5),1)]
-- ---------------------------------------------------------------------

-- 1ª solución
-- ===========

matrizPascal1 :: Int -> Matriz Int 
matrizPascal1 1 = array ((1,1),(1,1)) [((1,1),1)]
matrizPascal1 n = 
    array ((1,1),(n,n)) [((i,j), f i j) | i <- [1..n], j <- [1..n]]
    where f i j | i < n && j <  n  = p!(i,j)
                | i < n && j == n  = 0
                | j == 1 || j == n = 1
                | otherwise        = p!(i-1,j-1) + p!(i-1,j)
          p = matrizPascal2 (n-1)

-- 2ª solución
-- ===========

matrizPascal2 :: Int -> Matriz Int
matrizPascal2 n = listArray ((1,1),(n,n)) (concat xss)
    where yss = take n pascal
          xss = map (take n) (map (++ (repeat 0)) yss)
        
pascal :: [[Int]]
pascal = [1] : map f pascal
    where f xs = zipWith (+) (0:xs) (xs++[0])

-- 3ª solución
-- ===========

matrizPascal3 :: Int -> Matriz Int
matrizPascal3 n = 
    array ((1,1),(n,n)) [((i,j), f i j) | i <- [1..n], j <- [1..n]]
    where f i j | i >=  j   = comb (i-1) (j-1)
                | otherwise = 0

-- (comb n k) es el número de combinaciones (o coeficiente binomial) de
-- n sobre k. Por ejemplo,
comb :: Int -> Int -> Int
comb n k = product [n,n-1..n-k+1] `div` product [1..k]

-- ---------------------------------------------------------------------
-- Ejercicio 30. Para cada número n la matriz completa de orden n es la
-- matriz cuadrada de orden n formada por los números enteros
-- consecutivos. Por ejemplo, la matriz completa de orden 3 es 
--    |1 2 3|
--    |4 5 6|
--    |7 8 9|
-- las ternas primas de orden n son los listas formadas por un
-- elemento de la matriz junto con dos de sus vecinos de manera que los
-- tres son primos. Por ejemplo, en la matriz anterior una terna prima
-- es [2,3,5] (formada por el elemento 2, su vecino derecho 3 y su
-- vecino inferior 5), otra es [5,2,7] (formada por el elemento 5, su
-- vecino superior 2 y su vecino inferior-izquierda 7) y otra es [5,3,7]
-- (formada por el elemento 5, su vecino superior-derecha 3 y y su
-- vecino inferior-izquierda 7).
-- 
-- Definir la función 
--    ternasPrimasOrden :: Int -> [[Int]]
-- tal que (ternasPrimasOrden n) es el conjunto de las ternas primas de
-- la matriz completa de orden n. Por ejemplo,
--    ghci> ternasPrimasOrden 3
--    [[2,3,5],[3,2,5],[5,2,3],[5,2,7],[5,3,7]]
--    ghci> ternasPrimasOrden 4
--    [[2,3,5],[2,3,7],[2,5,7],[3,2,7],[7,2,3],[7,2,11],[7,3,11]]
-- ---------------------------------------------------------------------

ternasPrimasOrden :: Int -> [[Int]]
ternasPrimasOrden = ternasPrimas . matrizCompleta

-- (ternasPrimas p) es la lista de las ternas primas de p. Por ejemplo,  
--    ghci> ternasPrimas (listArray ((1,1),(3,3)) [2,3,7,5,4,1,6,8,9])
--    [[2,3,5],[3,2,7],[3,2,5],[3,7,5],[5,2,3]]
ternasPrimas :: Matriz Int -> [[Int]]
ternasPrimas p = 
    [xs | xs <- ternas p, all esPrimo xs]

-- (ternas p) es la lista de las ternas de p formadas por un elemento de
-- p junto con dos vecinos. Por ejemplo,  
--    ghci>  ternas (listArray ((1,1),(3,3)) [2,3,7,5,4,0,6,8,9])
--     [[2,3,5],[2,3,4],[2,5,4],[3,2,7],[3,2,5],[3,2,4],[3,2,0],[3,7,5],
--      [3,7,4],[3,7,0],[3,5,4],[3,5,0],[3,4,0],[7,3,4],[7,3,0],[7,4,0],
--      [5,2,3],[5,2,4],[5,2,6],[5,2,8],[5,3,4],[5,3,6],[5,3,8],[5,4,6],
--      [5,4,8],[5,6,8],[4,2,3],[4,2,7],[4,2,5],[4,2,0],[4,2,6],[4,2,8],
--      [4,2,9],[4,3,7],[4,3,5],[4,3,0],[4,3,6],[4,3,8],[4,3,9],[4,7,5],
--      [4,7,0],[4,7,6],[4,7,8],[4,7,9],[4,5,0],[4,5,6],[4,5,8],[4,5,9],
--      [4,0,6],[4,0,8],[4,0,9],[4,6,8],[4,6,9],[4,8,9],[0,3,7],[0,3,4],
--      [0,3,8],[0,3,9],[0,7,4],[0,7,8],[0,7,9],[0,4,8],[0,4,9],[0,8,9],
--      [6,5,4],[6,5,8],[6,4,8],[8,5,4],[8,5,0],[8,5,6],[8,5,9],[8,4,0],
--      [8,4,6],[8,4,9],[8,0,6],[8,0,9],[8,6,9],[9,4,0],[9,4,8],[9,0,8]]
ternas :: Matriz Int -> [[Int]]
ternas p = 
    [[p!(i1,j1),p!(i2,j2),p!(i3,j3)] | 
     (i1,j1) <- indices p,
     ((i2,j2):ps) <- tails (vecinos (i1,j1) n),
     (i3,j3) <- ps]
    where (_,(n,_)) = bounds p

-- (vecinos (i,j) n) es la lista de las posiciones vecinas de la (i,j)
-- en una matriz cuadrada de orden n. Por ejemplo,
--    vecinos (2,3) 4  ==  [(1,2),(1,3),(1,4),(2,2),(2,4),(3,2),(3,3),(3,4)]
--    vecinos (2,4) 4  ==  [(1,3),(1,4),(2,3),(3,3),(3,4)]
--    vecinos (1,4) 4  ==  [(1,3),(2,3),(2,4)]
vecinos :: (Int,Int) -> Int -> [(Int,Int)]
vecinos (i,j) n = [(a,b) | a <- [max 1 (i-1)..min n (i+1)],
                           b <- [max 1 (j-1)..min n (j+1)],
                           (a,b) /= (i,j)]

-- (esPrimo n) se verifica si n es primo. Por ejemplo,
--    esPrimo  7  ==  True
--    esPrimo 15  ==  False
esPrimo :: Int -> Bool
esPrimo n = [x | x <- [1..n], n `rem` x == 0] == [1,n]

-- (matrizCompleta n) es la matriz completa de orden n. Por ejemplo,
--    ghci> matrizCompleta 3
--    array ((1,1),(3,3)) [((1,1),1),((1,2),2),((1,3),3),
--                         ((2,1),4),((2,2),5),((2,3),6),
--                         ((3,1),7),((3,2),8),((3,3),9)]
matrizCompleta :: Int -> Matriz Int
matrizCompleta n =
    listArray ((1,1),(n,n)) [1..n*n]

-- 2ª definición
-- =============

ternasPrimasOrden2 :: Int -> [[Int]]
ternasPrimasOrden2 = ternasPrimas2 . matrizCompleta

ternasPrimas2 :: Matriz Int -> [[Int]]
ternasPrimas2 p = 
    [[p!(i1,j1),p!(i2,j2),p!(i3,j3)] | 
     (i1,j1) <- indices p,
     esPrimo (p!(i1,j1)),
     ((i2,j2):ps) <- tails (vecinos (i1,j1) n),
     esPrimo (p!(i2,j2)),
     (i3,j3) <- ps,
     esPrimo (p!(i3,j3))]
    where (_,(n,_)) = bounds p

-- Comparación:
--    ghci> length (ternasPrimasOrden 30)
--    51
--    (5.52 secs, 211095116 bytes)
--    ghci> length (ternasPrimasOrden2 30)
--    51
--    (0.46 secs, 18091148 bytes)

