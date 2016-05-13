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

esTriangularS:: (Num a, Eq a) => Matriz a -> Bool
esTriangularS p = undefined

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
potencia p 0 = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--    indicesMaximo :: (Num a, Ord a) => Matriz a -> [(Int,Int)]
-- tal que (indicesMaximo p) es la lista de los índices del elemento
-- máximo de la matriz p. Por ejemplo,
--    ghci> indicesMaximo (listArray ((1,1),(2,2)) [3,2,3,1])
--    [(1,1),(2,1)]
-- ---------------------------------------------------------------------

indicesMaximo :: (Num a, Ord a) => Matriz a -> [(Int,Int)]
indicesMaximo p = undefined

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
antidiagonal p = undefined

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
posiciones x p = undefined

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
esEscalar p = undefined

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
determinante p = undefined

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
aplicaT t f = undefined

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
matrizCreciente p = undefined

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
transformada a b f g = undefined

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
vectorEstocastico v = undefined

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
matrizEstocastica p = undefined

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
maximaSuma p = undefined

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
maximos p = undefined

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

opMatriz :: (Int -> Int -> Int) -> 
            Matriz Int -> Matriz Int -> Matriz Int
opMatriz f p q = undefined

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
algunMenor p = undefined

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
esAutovector v p = undefined

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
autovalorAsociado p v = undefined

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
        
borraCols :: Int -> Int -> Matriz Int -> Matriz Int
borraCols j1 j2 p = undefined

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
cambiaM (a,b) p = undefined

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
quitaRepetidosFila x p = undefined

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
sumaVecinos p = undefined

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
ampliaColumnas p1 p2 = undefined

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

esBisimetrica :: Eq a => Matriz a -> Bool
esBisimetrica p = undefined

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
matrizPorBloques p1 p2 p3 p4 = undefined

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
sumaColumnas p = undefined

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
esPiramidal p = undefined

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

iteracion_jacobi :: Matriz Float -> Matriz Float
iteracion_jacobi p = undefined

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
creaTridiagonal n = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 26.2. Definir la función 
--    esTridiagonal :: Matriz Int -> Bool
-- tal que (esTridiagonal p) se verifica si la matriz p es tridiagonal. Por 
-- ejemplo,
--    esTridiagonal (creaTridiagonal 5)               ==  True
--    esTridiagonal (listArray ((1,1),(3,3)) [1..9])  ==  False
-- ----------------------------------------------------------------------------

esTridiagonal :: Matriz Int -> Bool
esTridiagonal p = undefined

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

vandermonde :: [Integer] -> Matriz Integer
vandermonde xs = undefined

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
esMonomial p = undefined

-- (esListaMonomial xs) se verifica si todos los elementos de xs excepto
-- uno son nulos. Por ejemplo,
--    esListaMonomial [0,3,0,0]  ==  True
--    esListaMonomial [0,3,0,2]  ==  False
--    esListaMonomial [0,0,0,0]  ==  False
esListaMonomial :: [Int] -> Bool
esListaMonomial xs = undefined

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
-- + la primera fila está formada por el número 1;
-- + las filas siguientes se construyen sumando los números adyacentes
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

matrizPascal :: Int -> Matriz Int 
matrizPascal = undefined

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
ternasPrimasOrden = undefined

