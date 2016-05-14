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
import Data.Function
import Data.Numbers.Primes

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

-- carruirui3 abrdelrod
esTriangularS:: (Num a, Eq a) => Matriz a -> Bool
esTriangularS = all ((==0) . snd) . filter ts . assocs
    where ts ((i,j),_) = i > j

-- erisancha jespergue
esTriangularS2 :: (Num a, Eq a) => Matriz a -> Bool
esTriangularS2 p =
    and [p ! x == 0| x <- elementosNulos (filasYcolumnas p)]

elementosNulos n = [(a,b) | a <- [2..n], b <- take (a-1) [1..]]

filasYcolumnas p = snd $ snd $ bounds p

-- fracruzam marvilmor juanarcon ivaruicam
esTriangularS3 :: (Num a, Eq a) => Matriz a -> Bool
esTriangularS3 p = and [x == 0 | ((i,j),x) <- assocs p, i > j]

-- alvalvdom1 manpende
esTriangularS4 p =
    all (==0) [p ! (i,j) | i <- [2..m], j <- [1..n], i > j]
    where (_,(m,n)) = bounds p

-- manvermor rubvilval
esTriangularS5:: (Num a, Eq a) => Matriz a -> Bool
esTriangularS5 p = null [x | ((i,j),x) <- assocs p, i > j, x /= 0]

--fatvilpiz
esTriangularS6:: (Num a, Eq a) => Matriz a -> Bool
esTriangularS6 p = and [p!(i,j)==0 | i <- [1..m],j<-[1..n],i>j]
                  where (_,(m,n))= bounds p
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

-- erisancha alvalvdom1 jespergue marvilmor juanarcon fracruzam
-- manvermor abrdelrod rubvilval manpende ivaruicam
potencia :: Num a => Matriz a -> Int -> Matriz a
potencia p 0 = identidad (snd (snd (bounds p)))
potencia p n = prodMatrices (potencia p (n-1)) p

identidad :: Num a => Int -> Matriz a
identidad n =     
    array ((1,1),(n,n))
          [((i,j),f i j) | i <- [1..n], j <- [1..n]]
    where f i j | i == j    = 1
                | otherwise = 0

prodEscalar :: Num a => Vector a -> Vector a -> a
prodEscalar v1 v2 = 
    sum (zipWith (*) (elems v1) (elems v2))

prodMatrices:: Num a => Matriz a -> Matriz a -> Matriz a
prodMatrices p q = 
    array ((1,1),(m,n))
          [((i,j), prodEscalar (filaMat i p) (columnaMat j q)) |
           i <- [1..m], j <- [1..n]]
    where (m,n) = snd (bounds p)
          
filaMat :: Num a => Int -> Matriz a -> Vector a
filaMat i p = array (1,n) [(j,p!(i,j)) | j <- [1..n]]
    where n = snd (snd (bounds p))

columnaMat :: Num a => Int -> Matriz a -> Vector a
columnaMat j p = array (1,m) [(i,p!(i,j)) | i <- [1..m]]
    where m = fst (snd (bounds p))

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--    indicesMaximo :: (Num a, Ord a) => Matriz a -> [(Int,Int)]
-- tal que (indicesMaximo p) es la lista de los índices del elemento
-- máximo de la matriz p. Por ejemplo,
--    ghci> indicesMaximo (listArray ((1,1),(2,2)) [3,2,3,1])
--    [(1,1),(2,1)]
-- ---------------------------------------------------------------------

-- carruirui3 marvilmor
indicesMaximo :: (Num a, Ord a) => Matriz a -> [(Int,Int)]
indicesMaximo p = map fst . filter ((== maximo) . snd) $ assocs p
    where maximo = maximum $ elems p

-- erisancha jespergue juanarcon manvermor rubvilval fatvilpiz
indicesMaximo2 :: (Num a, Ord a) => Matriz a -> [(Int,Int)]
indicesMaximo2 p = [(a,b) | ((a,b),x) <- assocs p, x == maximum (elems p)]

-- Comentario: La definición anterior se puede mejorar usando variables
-- locales. 

-- fracruzam
indicesMaximo3 :: (Num a, Ord a) => Matriz a -> [(Int,Int)]
indicesMaximo3 p = map fst $ takeWhile (\((_,_),y) -> x == y) xs
    where xs@(((_,_),x):_) = sortBy (flip compare `on` snd) (assocs p)

-- fracruzam: `on` está definido en Data.Function

-- alvalvdom1 abrdelrod manpende ivaruicam
indicesMaximo4 :: (Num a, Ord a) => Matriz a -> [(Int,Int)]
indicesMaximo4 p = [i | i <- indices p, p ! i == maximum (elems p)]

-- Comentario: La definición anterior se puede mejorar usando variables
-- locales. 

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

-- carruirui3 abrdelrod
antidiagonal :: (Num a, Eq a) => Matriz a -> Bool 
antidiagonal p
    | m /= n = False
    | otherwise =
        all (==0) . map snd . filter (\((i,j),_) -> i+j/=n+1) $ assocs p
  where (m,n) = snd $ bounds p

-- erisancha jespergue  rubvilval
antidiagonal2 :: (Num a, Eq a) => Matriz a -> Bool 
antidiagonal2 p = and [p ! x == 0| x <- elementos p]

elementos p =
    [(a,b) | a <- [1..n], b <- [1..(n-a)]] ++ 
    [(a,b) | a <- [2..n], b <- drop (n-a+1) [1..n]]
    where n = snd (snd (bounds p))

-- fracruzam marvilmor juanarcon manvermor manpende ivaruicam
antidiagonal3 :: (Num a, Eq a) => Matriz a -> Bool 
antidiagonal3 p =
    n == m && and [p!(i,j) == 0 | i <- [1..n], j <- [1..n], i + j /= n + 1]
    where (_,(n,m)) = bounds p

-------------------------------
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

-- carruirui3 abrdelrod
posiciones :: Int -> Matriz Int -> [(Int,Int)]
posiciones x = map fst . filter ((==x) . snd) . assocs

-- erisancha fracruzam alvalvdom1 jespergue marvilmor juanarcon manvermor
-- rubvilval manpende ivaruicam
posiciones2 :: Int -> Matriz Int -> [(Int,Int)]
posiciones2 x p = [(a,b) | ((a,b),z) <- assocs p, z == x] 

-- fatvilpiz
posiciones3 :: Int -> Matriz Int -> [(Int,Int)]
posiciones3 x p = [(m,n) | m <- [1..j], n <- [1..i], (p! (m,n)) == x]
    where (_,(j,i)) = bounds p

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

-- carruirui3
esEscalar:: (Num a, Eq a) => Matriz a -> Bool
esEscalar p | esDiagonal p = all (==p!(1,1)) . filter (/=0) $ elems p
            | otherwise    = False
    where esDiagonal =
              all (==0) . map snd . filter (\((i,j),_) -> i/=j) . assocs

-- erisancha
esEscalar2 :: (Num a, Eq a) => Matriz a -> Bool
esEscalar2 p = 
    and [p ! x == 0| x <- elementos6 p] 
    && length (nub [p ! (x,x)| x <- [1..n]]) == 1
    where n = snd (snd(bounds p))

elementos6 p = [(a,b)| a <- [1..n] , b <- [1..n], a /=  b]
    where n = snd (snd (bounds p))

-- fracruzam manpende ivaruicam
esEscalar3 :: (Num a, Eq a) => Matriz a -> Bool
esEscalar3 p = all escalar (range (bounds p))
    where escalar :: (Int,Int) -> Bool
          escalar (i,j) | i == j    = p!(i,j) == p!(1,1)
                        | otherwise = p!(i,j) == 0
                                      
-- Comentario: La definición anterior se puede simplificar usando
-- indices.

-- alvalvdom1 marvilmor juanarcon rubvilval
esEscalar4:: (Num a, Eq a) => Matriz a -> Bool
esEscalar4 p =
    length (nub [p!(i,i) | i <- [1..n]]) == 1
    && all (==0) [p!(i,j) | i <- [1..m], j <- [1..n], i/=j]
    where (_,(m,n)) = bounds p

-- manvermor
esEscalar5 :: (Num a, Eq a) => Matriz a -> Bool
esEscalar5 p = elems p == [aux i j | (i,j) <- indices p]
   where aux i j | i == j    = head (elems p)
                 | otherwise = 0

-- Comentario: La definición anterior se puede mejorar usando variables
-- locales. 

-- abrdelrod
esEscalar6 :: (Num a, Eq a) => Matriz a -> Bool
esEscalar6 p = take (n^2) (concat (repeat xs)) == elems p
    where xs = p!(1,1) : replicate n 0
          n = snd (snd $ bounds p)

--fatvilpiz
esEscalar7:: (Num a, Eq a) => Matriz a -> Bool
esEscalar7 p = diagonal p && iguales p
diagonal p = and [p!(i,j)== 0 |i<-[1..m],j<- [1..n], i /=j]
             where (_,(m,n)) = bounds p
iguales p = length (nub xs) == 1
    where  xs = filter (/=0) (elems p)

---------------------------------------------------------------------
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

-- erisancha marvilmor juanarcon fracruzam manvermor abrdelrod rubvilval
-- manpende
determinante:: Matriz Double -> Double
determinante p
    | (m,n) == (1,1) = p!(1,1) 
    | otherwise =
        sum [((-1)^(i+1))*(p!(i,1))*determinante (submatriz i 1 p) 
            | i <- [1..m]] 
    where (_,(m,n)) = bounds p

submatriz :: Num a => Int -> Int -> Matriz a -> Matriz a 
submatriz i j p = 
    array ((1,1), (m-1,n -1))
          [((k,l), p ! f k l) | k <- [1..m-1], l <- [1..n-1]]   
   where (_,(m,n)) = bounds p 
         f k l | k < i && l < j = (k,l) 
               | k >= i && l < j = (k+1,l) 
               | k < i && l >= j = (k,l+1) 
               | otherwise = (k+1,l+1)
--ivaruicam (otra forma sería determinando el adjunto de la siguiente manera)
submatriz2 :: Num a => Int -> Int -> Matriz a -> Matriz a 
submatriz2 i j p = listArray ((1,1),(m-1,n-1)) [p!(a,b) | (a,b) <- indices p ,i/=a,j/=b]
                     where (_,(m,n)) = bounds p
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

-- erisancha fracruzam alvalvdom1 jespergue marvilmor juanarcon
-- manvermor abrdelrod rubvilval manpende fatvilpiz ivaruicam
aplicaT :: (Ix a, Num b) => Array a b -> (b -> c) -> Array a c
aplicaT t f = listArray (bounds t) [f x | x <- elems t]

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

-- erisancha alvalvdom1 manvermor rubvilval
matrizCreciente :: (Num a, Ord a) => Matriz a -> Bool
matrizCreciente p =  
    and ([p!(a,b) <= p!(a,b+1)   | a <- [1..m],   b <- [1..n-1]] ++
         [p!(a,b) <= p!(a+1,b)   | a <- [1..m-1], b <- [1..n]] ++
         [p!(a,b) <= p!(a+1,b+1) | a <- [1..m-1], b <- [1..n-1]])
    where (m,n) = snd (bounds p)

-- fracruzam marvilmor
matrizCreciente3 :: (Num a, Ord a) => Matriz a -> Bool
matrizCreciente3 p = all creciente (range b)
  where b@(_,(n,m)) = bounds p
        creciente :: (Int,Int) -> Bool
        creciente (i,j)
            | i == n && j == m = True
            | i == n           = x <= j1
            |           j == m = x <= i1
            | otherwise        = x <= i1 && x <= j1 && x <= ij
          where x  = p!(i  ,j)
                i1 = p!(i+1,j)
                j1 = p!(i  ,j+1)
                ij = p!(i+1,j+1)

-- juanarcon
matrizCreciente4 :: (Num a, Ord a) => Matriz a -> Bool
matrizCreciente4 p =
    and (elems (array ((1,1),(m,n)) [((i,j),f i j)| i<-[1..m], j<-[1..n]]))
    where (m,n) = snd (bounds p) 
          f i j | i==m && j==n = True
                | i==m = all (>= p!(i,j)) [x]
                | j == n = all (>= p!(i,j)) [y]
                | otherwise = all (>= p!(i,j)) [x,y,z]            
                              where x = p!(i,j+1)
                                    y = p!(i+1,j)
                                    z = p!(i+1,j+1)

-- abrdelrod manpende
matrizCreciente5 :: (Num a, Ord a) => Matriz a -> Bool
matrizCreciente5 p = all p' (indices p)
    where p' (i,j) | i == m || j == n = True
                   | otherwise = all (>p!(i,j))
                                     [p!(i+1,j),p!(i,j+1),p!(i+1,j+1)]
          (m,n) = snd $ bounds p

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

-- fracruzam
transformada :: Matriz a -> Matriz Bool -> (a -> b) -> (a -> b) -> Matriz b
transformada a b f g = listArray bo (map segunB $ range bo)
  where bo@(_,(n,m)) = bounds a
        --segunB :: (Int,Int) -> b
        segunB ij | b!ij      = f x
                  | otherwise = g x
          where x = a!ij

-- fracruzam : ¿Cuál sería el tipo correcto de segunB? Ese reporta un error.

-- juanarcon alvalvdom1 manvermor rubvilval
transformada1 :: Matriz a -> Matriz Bool -> (a -> b) -> (a -> b) -> Matriz b
transformada1 a b f g = 
    array ((1,1),(m,n)) [((i,j),h i j)| i<- [1..m], j<- [1..n]]
          where (m,n) = fst (bounds a)
                h i j | b!(i,j) = f (a!(i,j))
                      | otherwise = g (a!(i,j))

-- abrdelrod manpende ivaruicam erisancha
transformada2 :: Matriz a -> Matriz Bool -> (a -> b) -> (a -> b) -> Matriz b
transformada2 a b f g = listArray (bounds a) (map h (assocs a))
    where h ((i,j),x) | b!(i,j) = f x
                      | otherwise = g x

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

-- fracruzam erisancha
vectorEstocastico :: Vector Float -> Bool
vectorEstocastico v = estocastico (elems v) 0
  where estocastico :: [Float] -> Float -> Bool
        estocastico (x:xs) n | x >= 0 = estocastico xs (n+x)
                             | otherwise = False
        estocastico  _     n = n == 1

-- juanarcon alvalvdom1 manvermor abrdelrod rubvilval manpende ivaruicam
vectorEstocastico1 :: Vector Float -> Bool
vectorEstocastico1 v = all (>= 0) xs && sum xs == 1
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

-- fracruzam
matrizEstocastica :: Matriz Float -> Bool        
matrizEstocastica p =
    all (flip estocastico 0) [[p!(i,j) | i <- [1..n]] | j <- [1..m]]
    where (_,(n,m)) = bounds p
          estocastico :: [Float] -> Float -> Bool
          estocastico (x:xs) n | x >= 0 = estocastico xs (n+x)
                               | otherwise = False
          estocastico  _     n = n == 1

-- Comentario: Se puede simplificar usando secciones en lugar de flip.

-- juanarcon 
matrizEstocastica1 :: Matriz Float -> Bool        
matrizEstocastica1 p = 
    and (map estocastico (columnas (matrizLista p)))
    where estocastico xs = all (>= 0) xs && sum xs == 1
          columnas xss = [[xs!!i| xs <- xss] | i <- [0..numCol-1]]
          numCol = (snd . snd . bounds) p

-- Comentario: La definición anterior se puede simplificar usando all.

matrizLista :: Matriz a -> [[a]]
matrizLista p = aux (elems p)
    where aux [] = []
          aux xs = take n xs : aux (drop n xs)
          n      = (snd . snd . bounds) p

-- abrdelrod rubvilval manvermor manpende ivaruicam erisancha
matrizEstocastica2 :: Matriz Float -> Bool        
matrizEstocastica2 p = all vectorEstocastico (columnas p)
    where columnas p = [listArray (1,m) [p!(i,k) | i <- [1..m]] 
                       | k <- [1..n]]
          (m,n) = snd $ bounds p

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

-- fracruzam abrdelrod rubvilval
maximaSuma :: Matriz Int -> Int
maximaSuma p 
    | (m,n) == (1,1) = p!(1,1)
    | otherwise = maximum [p!(1,j) 
                  + maximaSuma (submatriz 1 j p) | j <- [1..n]]
    where (_,(m,n)) = bounds p
          submatriz :: Int -> Int -> Matriz Int -> Matriz Int
          submatriz i j p = array ((1,1), (m-1,n -1))
                            [((k,l), p ! f k l) 
                            | k <- [1..m-1], l <- [1.. n-1]]
              where (_,(m,n)) = bounds p
                    f k l | k < i  && l < j  = (k,l)
                          | k >= i && l < j  = (k+1,l)
                          | k < i  && l >= j = (k,l+1)
                          | otherwise        = (k+1,l+1)

-- juanarcon manvermor erisancha
maximaSuma1 :: Matriz Int -> Int
maximaSuma1 p = maximum $ map sum (posiblesListas p)

posiblesListas :: Matriz Int  -> [[Int]]
posiblesListas p = 
    [[p!(i,j) | (i,j) <- seleccionIndices] | 
     seleccionIndices <- [zip [1..m] xs | xs <- permutations [1..n]]] 
    where (m,n) = (snd . bounds) p

-- manpende
maximaSuma2 :: Matriz Int -> Int
maximaSuma2 p | m == 1 = m1
              | n == 1 = m2
              | otherwise = maximo + maximaSuma (submatriz i j p)
    where m1 = maximoCol 1 p
          m2 = maximoFila 1 p
          maximo = max m1 m2 
          (i,j) = head [(x,y) | x <- [1..n], y <- [1..m], p!(x,y) == maximo]
          m = fst $ snd $ bounds p
          n = snd $ snd $ bounds p
                    
maximoFila :: Int -> Matriz Int -> Int
maximoFila x p = maximum [p!(x,j) | j <- [1..n]]
    where (_,(_,n)) = bounds p

maximoCol :: Int -> Matriz Int -> Int
maximoCol x p = maximum [p!(i,x) | i <- [1..m]]
    where (_,(m,_)) = bounds p

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

-- fracruzam juanarcon erisancha
maximos :: Matriz Int -> [Int]
maximos p = [x | y@(_,x) <- assocs p, maximo y]
    where (_,(n,m)) = bounds p
          maximo :: ((Int,Int),Int) -> Bool
          maximo ((i,j),x) =
              and [p!(a,b) < x | r <- [-1..1],
                                 t <- [-1..1],
                                 let a = i + r,
                                 let b = j + t, a > 0, a <= n,
                                 b > 0,
                                 b <= n,
                                 not (r == 0 && t == 0)]

-- abrdelrod rubvilval
maximos2 :: Matriz Int -> [Int]
maximos2 p = map snd (filter q (assocs p))
    where f i j x | i < 1 || i > m || j < 1 || j > n = x-1
                  | otherwise                        = p!(i,j)
          q ((i,j),x) = all (<x) [f k l x | k <- [i-1..i+1],
                                            l <- [j-1..j+1],
                                            (i,j) /= (k,l)]
          (m,n) = snd $ bounds p

-- manvermor
maximos3 :: Matriz Int -> [Int]
maximos3 p = 
    [ p!(i,j) | (i,j) <- indices p,
                and [ p!(a,b) < p!(i,j) | (a,b) <- vecinos (i,j)]]
    where (m,n) = snd $ bounds p
          vecinos (i,j) = [(a,b) | a <- [max 1 (i-1)..min m (i+1)],
                                   b <- [max 1 (j-1)..min n (j+1)],
                                   (a,b) /= (i,j)]

-- manpende
maximos4 :: Matriz Int -> [Int]
maximos4 p = nub $ [p!(i,j)| i <- [1..n], j <- [1..m], esMaximo i j p]
     where (_,(n,m)) = bounds p

-- Comentario: La definición anterior se puede simplificar eliminando $.

esMaximo :: Int -> Int -> Matriz Int -> Bool
esMaximo i j p = 
    all (<= p!(i,j)) [p!(x,y) | x <- [a1..a2], y <- [b1..b2]]
    where a1 = max (i-1) 1
          b1 = max (j-1) 1
          a2 = min (i+1) n
          b2 = min (j+1) m
          (_,(n,m)) = bounds p

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

-- fracruzam
opMatriz :: (Int -> Int -> Int) -> 
            Matriz Int -> Matriz Int -> Matriz Int
opMatriz f p q = listArray b [f (p!ij) (q!ij) | ij <- range b]
  where b = bounds p

-- alvalvdom1 juanarcon manvermor
opMatriz2 :: (Int -> Int -> Int) -> 
            Matriz Int -> Matriz Int -> Matriz Int
opMatriz2 f p q =
    array (bounds p) [((i,j), f (p!(i,j)) (q!(i,j))) | i <- [1..m], j <- [1..n]]
    where (_,(m,n)) = bounds p

-- abrdelrod rubvilval ivaruicam erisancha
opMatriz3 :: (Int -> Int -> Int) -> Matriz Int -> Matriz Int -> Matriz Int
opMatriz3 f p q =
    listArray (bounds p) [f x y | (x,y) <- zip (elems p) (elems q)]

-- manpende
opMatriz4 :: (Int -> Int -> Int) -> 
            Matriz Int -> Matriz Int -> Matriz Int
opMatriz4 f p q =
    listArray a 
    $ zipWith f [p!(i,j) | i <- [1..n], j <- [1..m]]
                [q!(i,j) | i <- [1..n], j <- [1..m]] 
    where a@(_,(n,m)) = bounds p

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

-- fracruzam juanarcon
algunMenor :: Matriz Int -> [Int]
algunMenor p =
    [x | y@((i,j),x) <- assocs p, (not.null) (vecinosMenores y)]
    where (_,(n,m)) = bounds p
          vecinosMenores :: ((Int,Int),Int) -> [Int]
          vecinosMenores ((i,j),x) =
              filter (<x) $ map (p!) [(l,k) | x <- xs,
                                              y <- xs,
                                              let l = i + x,
                                              let k = j + y,
                                              l > 0,
                                              l <= n,
                                              k > 0,
                                              k <= m,
                                              not (x == 0 && y == 0)]
              where xs = [-1..1]

-- abrdelrod rubvilval erisancha
algunMenor2 :: Matriz Int -> [Int]
algunMenor2 p = map snd (filter q (assocs p))
    where f i j x | i < 1 || i > m || j < 1 || j > n = x+1
                  | otherwise = p!(i,j)
          q ((i,j),x) = any (< x) [f k l x | k <- [i-1..i+1],
                                             l <- [j-1..j+1],
                                             (k,l) /= (i,j)]
          (m,n) = snd $ bounds p

-- manvermor
algunMenor3 :: Matriz Int -> [Int]
algunMenor3 p =
    [p!(i,j) | (i,j) <- indices p,
               or [p!(a,b) < p!(i,j) | (a,b) <- vecinos (i,j)]]
    where (m,n) = snd $ bounds p
          vecinos (i,j) = [(a,b) | a <- [max 1 (i-1)..min m (i+1)],
                                   b <- [max 1 (j-1)..min n (j+1)],
                                   (a,b) /= (i,j)]

-- manpende
algunMenor4 :: Matriz Int -> [Int]
algunMenor4 p = [p!(i,j) | i <- [1..n], j <- [1..m], tieneMenor p i j]
     where (_,(n,m)) = bounds p

tieneMenor :: Matriz Int -> Int -> Int -> Bool
tieneMenor p i j =
    any (> p!(i,j)) [p!(x,y) | x <- [a1..a2], y <- [b1..b2]]
    where a1 = max 1 (i-1)
          b1 = max 1 (j-1) 
          a2 = min n (i+1)
          b2 = min m (j+1)
          (_,(n,m)) = bounds p

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

-- fracruzam
esAutovector :: (Fractional a, Eq a) => Vector a -> Matriz a -> Bool
esAutovector v p = all (z==) zs
    where (_,(m,n)) = bounds p
          ys = elems v
          pv = [prodEscalar (filaMat i) ys | i <- [1..m]]
          (z:zs) = divide pv ys
          --filaMat :: (Fractional a, Eq a) => Int -> [a]
          filaMat i = [p!(i,j) | j <- [1..n]]
          prodEscalar :: (Fractional a, Eq a) => [a] -> [a] -> a
          prodEscalar xs ys = sum (zipWith (*) xs ys)
          divide :: (Fractional a, Eq a) => [a] -> [a] -> [a]
          divide (a:as) (b:bs) | b == 0 = divide as bs
                               | otherwise = (a/b) : divide as bs
          divide  _      _     = []

-- fracruzam : ¿A alguien se le ocurre como comprobar que son proporcionales
--              evitando el 0/0?

-- abrdelrod rubvilval manvermor juanarcon
esAutovector2 :: (Fractional a, Eq a) => Vector a -> Matriz a -> Bool
esAutovector2 v p = length (nub zs) == 1
  where ys = filter (/= (0,0)) $ zip (elems v) (multPor p v)
        zs = map (\(x,y) -> y/x) ys

separa _ [] = []
separa n xs = take n xs : (separa n (drop n xs))

-- Comentario: La definición anterior se puede simplificar eliminando
-- paréntesis. 
              
multPor p v = [sum $ zipWith (*) xs (elems v) | xs <- separa m (elems p)]
    where m = fst $ snd (bounds p)

-- manpende erisancha
esAutovector3 :: (Fractional a, Eq a) => Vector a -> Matriz a -> Bool
esAutovector3 v p = esProporcional (elems (producto v p)) (elems v)
  
producto :: (Fractional a, Eq a) => Vector a -> Matriz a -> Vector a
producto v p = 
    listArray (1,n) [prodEscalar (filaMat i p) v | i <- [1..n]]
    where n = snd (bounds v)

esProporcional :: (Fractional a, Eq a) => [a] -> [a] -> Bool
esProporcional (x:xs) (y:ys) | x == 0 = esProporcional xs ys
                             | y == 0 = all (==0) ys 
                             | otherwise = map (*(x/y)) ys == xs
esProporcional _ _ = True

filaMat3 :: Num a => Int -> Matriz a -> Vector a
filaMat3 i p = array (1,n) [(j,p!(i,j)) | j <- [1..n]]
    where n = snd $ snd $ bounds p 

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

-- fracruzam
autovalorAsociado :: (Fractional a, Eq a) => 
                     Matriz a -> Vector a -> Maybe a
autovalorAsociado p v | all (z==) zs = Just z
                      | otherwise    = Nothing
    where (_,(m,n)) = bounds p
          ys = elems v
          pv = [prodEscalar (filaMat i) ys | i <- [1..m]]
          (z:zs) = divide pv ys
          --filaMat :: (Fractional a, Eq a) => Int -> [a]
          filaMat i = [p!(i,j) | j <- [1..n]]
          prodEscalar :: (Fractional a, Eq a) => [a] -> [a] -> a
          prodEscalar xs ys = sum (zipWith (*) xs ys)
          divide :: (Fractional a, Eq a) => [a] -> [a] -> [a]
          divide (a:as) (b:bs) | b == 0 = divide as bs
                               | otherwise = (a/b) : divide as bs
          divide  _      _     = []

-- abrdelrod rubvilval manvermor juanarcon
autovalorAsociado2 :: (Fractional a, Eq a) => Matriz a -> Vector a -> Maybe a
autovalorAsociado2 p v | esAutovector v p = Just $ head zs
                       | otherwise = Nothing
      where ys = filter (/= (0,0)) $ zip (elems v) (multPor p v)
            zs =  map (\(x,y) -> y/x) ys

-- manpende erisancha
autovalorAsociado3 :: (Fractional a, Eq a) => 
                     Matriz a -> Vector a -> Maybe a
autovalorAsociado3 p v 
     | esAutovector v p = buscaAutovalor (elems (producto v p)) (elems v)
     | otherwise = Nothing
 
buscaAutovalor :: (Fractional a, Eq a) => [a] -> [a] -> Maybe a
buscaAutovalor (x:xs) (y:ys) | x == 0 && y == 0 = buscaAutovalor xs ys
                             | y == 0 = Just x
                             | otherwise = Just (x/y)
buscaAutovalor _ _ = Just 0

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
        
-- fracruzam alvalvdom1 juanarcon abrdelrod rubvilval manvermor manpende
-- ivaruicam erisancha
borraCols :: Int -> Int -> Matriz Int -> Matriz Int
borraCols j1 j2 p =
    listArray (o,(n,m-2)) [x | ((i,j),x) <- assocs p, j /= j1, j /= j2]
    where (o,(n,m)) = bounds p

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

-- fracruzam
cambiaM :: (Int, Int) -> Matriz Int -> Matriz Int
cambiaM (a,b) p = p // [(ij,cambia $ p!ij) | ij <- xs]
    where (_,(n,m)) = bounds p
          xs = union [(a,j) | j <- [1..m]] [(i,b) | i <- [1..n]]
          cambia :: Int -> Int
          cambia x | x == 0    = 1
                   | x == 1    = 0
                   | otherwise = x

-- alvalvdom1 juanarcon rubvilval manvermor manpende ivaruicam erisancha

cambiaM2 :: (Int, Int) -> Matriz Int -> Matriz Int
cambiaM2 (a,b) p =
    array ((1,1),(m,n)) [((i,j),f i j) | i <- [1..m], j <- [1..n]]
    where f i j | i==a || j==b = cambia (p!(i,j))
                | otherwise = p!(i,j)
          (_,(m,n)) = bounds p
                       
cambia :: Int -> Int
cambia 1 = 0
cambia 0 = 1
cambia x = x

-- abrdelrod
cambiaM3 :: (Int, Int) -> Matriz Int -> Matriz Int
cambiaM3 (a,b) p =
    p // [((i,j),f i j) | (i,j) <- [(a,k) | k <- [1..n]] ++
                                   [(l,b) | l <- [1..m]]]
    where (m,n) = snd (bounds p)
          f i j | notElem (p!(i,j)) [0,1] = p!(i,j)
                | otherwise = 1 - p!(i,j)

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

-- fracruzam
quitaRepetidosFila :: Int -> Matriz Int -> Matriz Int
quitaRepetidosFila x p =
    p // [cambia (xj,p!xj) | j <- [2..m], let xj = (x,j)]
    where (_,(_,m)) = bounds p
          cambia :: ((Int,Int),Int) -> ((Int,Int),Int)
          cambia k@((a,b),c) | esRepetido k = ((a,b),0)
                             | otherwise    = k
          esRepetido :: ((Int,Int),Int) -> Bool
          esRepetido ((a,b),c) = or [p!(a,j) == c | j <- [1..b-1]]

-- abrdelrod manvermor manpende juanarcon
quitaRepetidosFila2 :: Int -> Matriz Int -> Matriz Int
quitaRepetidosFila2 x p = p // [((x,j),f x j) | j <- [1..n]]
    where f i j | j == 1 = p!(i,j)
                | notElem (p!(i,j)) [p!(i,k) | k <- [1..j-1]] = p!(i,j)
                | otherwise = 0
          (_,(_,n)) = bounds p
                      
-- ivaruicam erisancha
quitaRepetidosFila3 :: Int -> Matriz Int -> Matriz Int
quitaRepetidosFila3 x p =
    listArray ((1,1),(m,n)) (concat [cambia i [] (fila i p)| i<-[1..m]])
    where cambia i v [] = v
          cambia i v (y:ys) | i/=x = y:ys
                            | elem y v = cambia i (v++[0]) ys
                            | otherwise = cambia i(v++[y]) ys
          fila i p = [p!(i,j) | j <- [1..n]]
          (_,(m,n)) = bounds p

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

-- fracruzam juanarcon
sumaVecinos :: Matriz Int -> Matriz Int
sumaVecinos p = listArray b [suma ij | ij <- range b]
    where b@(_,(n,m)) = bounds p
          suma :: (Int,Int) -> Int
          suma (i,j) = sum $ map (p!) [(l,k) | x <- xs,
                                               y <- xs,
                                               let l = i + x, 
                                               let k = j + y,
                                               l > 0,
                                               l <= n,
                                               k > 0,
                                               k <= m,
                                               not (x == 0 && y == 0)]
              where xs = [-1..1]

-- abrdelrod erisancha
sumaVecinos2 :: Matriz Int -> Matriz Int
sumaVecinos2 p = listArray (bounds p) [f i j | i <- [1..m], j <- [1..n]]
    where (m,n) = snd $ bounds p
          f i j = sum (map g [(k,l) | k <- [i-1..i+1],
                                      l <- [j-1..j+1],
                                      (k,l) /= (i,j)])
          g (i,j) | i < 1 || i > m || j < 1 || j > n = 0
                  | otherwise = p!(i,j)

-- manvermor
sumaVecinos3 :: Matriz Int -> Matriz Int
sumaVecinos3 p =
    array ((1,1),(m,n)) [ ((i,j),f i j) | i <- [1..m], j <- [1..n]]
    where (_,(m,n)) = bounds p 
          f i j = sum [ p!(a,b) | (a,b) <- vecinos (i,j)]
          vecinos (i,j) = [ (a,b) | a <- [max 1 (i-1)..min m (i+1)],
                                    b <- [max 1 (j-1)..min n (j+1)],
                                    (a,b) /= (i,j)]

-- manpende
sumaVecinos4 :: Matriz Int -> Matriz Int
sumaVecinos4 p = array a [((i,j),suma p i j) | i <- [1..n], j <- [1..m]]
    where a@(_,(n,m)) = bounds p

suma :: Matriz Int -> Int -> Int -> Int
suma p i j = sum [p!(x,y) | x <- [a1..a2], y <- [b1..b2]] - p!(i,j)
    where a1 = max 1 (i-1)
          a2 = min n (i+1)
          b1 = max 1 (j-1)
          b2 = min m (j+1)
          (_,(n,m)) = bounds p

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

-- fracruzam juanarcon abrdelrod rubvilval manpende
ampliaColumnas :: Matriz a -> Matriz a -> Matriz a
ampliaColumnas p1 p2 = array ((1,1),(n,m+m')) xs
    where (_,(n,m)) = bounds p1
          (_,(_,m')) = bounds p2
          xs = assocs p1 ++ [((i,j+m),x) | ((i,j),x) <- assocs p2]

-- manvermor erisancha
ampliaColumnas2 :: Matriz a -> Matriz a -> Matriz a
ampliaColumnas2 p1 p2 =
    listArray ((1,1),(m,x)) (concat $ mezcla xss yss)
    where (_,(m,n)) = bounds p1
          (_,(y,z)) = bounds p2
          x = n+z
          xss = listas n (elems p1) 
          yss = listas z (elems p2)

mezcla :: [t] -> [t] -> [t]
mezcla [] [] = []
mezcla (xs:xss) (ys:yss) = xs:ys:mezcla xss yss

listas :: Int -> [a] -> [[a]]
listas n [] = []
listas n xs = take n xs : listas n (drop n xs)

-- ivaruicam
ampliaColumnas3 :: Matriz a -> Matriz a -> Matriz a
ampliaColumnas3 p1 p2 =
    listArray ((1,1),(m,n+b))
              (concat[(filaLista i p1) ++ (filaLista i p2) | i <- [1..m]])
    where (_,(m,n)) = bounds p1
          (_,(a,b)) = bounds p2

-- Comentario: La definición anterior se puede simplificar eliminando paréntesis
                      
filaLista i p = [p!(i,j) | j <- [1..n]]
    where (_,(m,n)) = bounds p

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

-- fracruzam juanarcon manvermor
esBisimetrica :: Eq a => Matriz a -> Bool
esBisimetrica p = all (simetrica p) xs && all (simetrica q) ys
    where b@(_,(_,m)) = bounds p
          xs = assocs p
          ys = [((i,m-j+1),x) | ((i,j),x) <- xs]
          q = array b ys
          simetrica :: Eq a => Matriz a -> ((Int,Int),a) -> Bool
          simetrica p ((i,j),x) = p!(j,i) == x 

-- abrdelrod erisancha
esBisimetrica2 :: Eq a => Matriz a -> Bool
esBisimetrica2 p = m == n && all q (indices p)
    where q (i,j) = p!(i,j) == p!(j,i) && p!(n-i+1,j) == p!(i,n-j+1)
          (m,n) = snd $ bounds p

-- manpende
esBisimetrica3 :: (Num a, Eq a) => Matriz a -> Bool
esBisimetrica3 p =
    m == n && all (simetrico p) [i | i <- [1..mitad n]]
    where (_,(n,m)) = bounds p
          mitad x = div x 2 + rem x 2

-- Comentario: La definición anterior se puede simplificar eliminando
-- listas innecesarias en [i | i <- [1..mitad n]].
                              
simetrico :: (Num a, Eq a) => Matriz a -> Int -> Bool
simetrico p i =
    all (== elmFila p i) $ elmCol p i :
                           map reverse [elmFila p (n-i+1), elmCol p (n-i+1)]
    where (_,(n,_)) = bounds p 

elmFila :: (Num a, Eq a) => Matriz a -> Int -> [a]
elmFila p i = elems $ filaMat i p

elmCol :: (Num a, Eq a) => Matriz a -> Int -> [a]
elmCol p i = elems $ columnaMat i p

filaMat2 :: Eq a => Int -> Matriz a -> Vector a
filaMat2 i p = array (1,n) [(j,p!(i,j)) | j <- [1..n]]
    where n = snd $ snd $ bounds p 

columnaMat2 :: Eq a => Int -> Matriz a -> Vector a
columnaMat2 j p = array (1,n) [(i,p!(i,j)) | i <- [1..n]]
    where n = fst $ snd $ bounds p

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

-- fracruzam
matrizPorBloques :: Matriz a -> Matriz a -> Matriz a -> Matriz a ->
                    Matriz a
matrizPorBloques p1 p2 p3 p4 = array ((1,1),(m,n)) xs
    where (_,(m1,n1)) = bounds p1
          (_,(m2,_)) = bounds p2
          (_,(_,n3)) = bounds p3
          (m,n) = (m1+m2,n1+n3)
          xs = assocs p1
               ++ [((i,j+n1),x)    | ((i,j),x) <- assocs p2]
               ++ [((i+m1,j),x)    | ((i,j),x) <- assocs p3]
               ++ [((i+m1,j+n1),x) | ((i,j),x) <- assocs p4]

-- abrdelrod
matrizPorBloques2 p1 p2 p3 p4 =
    array ((1,1),(2*n,2*n)) [f i j | i <- [1..2*n],j <- [1..2*n]]
    where (_,(_,n)) = bounds p1
          f i j | i <= n && j <= n = p1!(i,j)
                | i <= n = p2!(i,j-n)
                | i > n && j > n = p4!(i-n,j-n)
                | otherwise = p3!(i-n,j)

-- manvermor juanarcon erisancha
matrizPorBloques3 :: Matriz a -> Matriz a -> Matriz a -> Matriz a ->
                    Matriz a
matrizPorBloques3 p1 p2 p3 p4 =
    listArray ((1,1),(2*m,2*n)) (xs ++ ys)
    where (_,(m,n)) = bounds p1
          (_,(_,y)) = bounds p2
          (_,(_,t)) = bounds p3
          (_,(_,d)) = bounds p4
          xs = concat $ mezcla (listas n (elems p1)) (listas y (elems p2))
          ys = concat $ mezcla (listas t (elems p3)) (listas d (elems p4))

-- manpende
matrizPorBloques4 :: Matriz a -> Matriz a -> Matriz a -> Matriz a ->
                    Matriz a
matrizPorBloques4 p1 p2 p3 p4 =
    ampliaFilas (ampliaColumnas p1 p2)
                (ampliaColumnas p3 p4)

ampliaFilas :: Matriz a -> Matriz a -> Matriz a
ampliaFilas p1 p2 = array ((1,1),(n+n',m)) xs
    where (_,(n,m)) = bounds p1
          (_,(n',_)) = bounds p2
          xs = assocs p1 ++ [((i+n,j),x) | ((i,j),x) <- assocs p2] 


matrizPorBloques5 :: Matriz a -> Matriz a -> Matriz a -> Matriz a ->
                    Matriz a
matrizPorBloques5 p1 p2 p3 p4 =
    listArray ((1,1),(2*n,2*n)) (elems (ampliaColumnas p1 p2)
                                 ++ elems (ampliaColumnas p3 p4))
    where (_,(m,n)) = bounds p1

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

-- fracruzam manpende
sumaColumnas :: Matriz Int -> Matriz Int
sumaColumnas p = listArray b (map suma (range b))
    where b@(_,(m,n)) = bounds p
          suma :: (Int,Int) -> Int
          suma (i,1) = p!(i,1) + p!(i,n)
          suma (i,j) = p!(i,j) + p!(i,j-1)

-- juanarcon abrdelrod manvermor erisancha
sumaColumnas2 :: Matriz Int -> Matriz Int
sumaColumnas2 p = 
    array (o,(n,m)) [((i,j),f i j) | i<- [1..n], j<- [1..m]]
    where (o,(n,m)) = bounds p
          f i j | j == 1 = p!(i,j) + p!(i,m)
                | otherwise = p!(i,j) + p!(i,j-1)

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

-- fracruzam juanarcon erisancha
esPiramidal :: (Eq a, Num a) => Matriz a -> Bool
esPiramidal p = all piramidal (range b)
    where b@(_,(m,n)) = bounds p
          piramidal :: (Int,Int) -> Bool
          piramidal (i,j) | j <= k || j > l = p!(i,j) == 0
                          | otherwise       = p!(i,j) == 1
              where k = m - i
                    l = n - k

-- abrdelrod
esPiramidal2 :: (Eq a, Num a) => Matriz a -> Bool
esPiramidal2 p = all q [0..m-1]
   where q a = filas !! a == xs ++ replicate (2*a+1) 1 ++ xs
             where xs = replicate (div n 2 - a) 0
         filas = [[p!(i,k) | k <- [1..n]] | i <- [1..m]]
         (m,n) = snd $ bounds p

-- manvermor
esPiramidal3 :: (Eq a, Num a) => Matriz a -> Bool
esPiramidal3 p
    | p == listArray ((1,1),(1,1)) [1] = True
    | otherwise = [1,3..n] == [length $ filter (/= 0) xs
                              | xs <- listas n (elems p)]
                              && nub (elems p) == [0,1]
    where (_,(_,n)) = bounds p

--manpende
esPiramidal4 :: (Eq a, Num a) => Matriz a -> Bool
esPiramidal4 p = piramidalAux 1 p
    where (_,(n,_)) = bounds p


piramidalAux :: (Eq a, Num a) => Int -> Matriz a -> Bool
piramidalAux x p
    | x == n    = xs == replicate b 0 ++ replicate a 1 ++  replicate b 0
    | x >= m    = xs == replicate m 1
    | otherwise = xs == (replicate b 0 ++ replicate a 1 ++ replicate b 0)
                        && piramidalAux (x+1) p
    where a = 2*x - rem m 2
          b = div (m-a) 2
          (_,(n,m)) = bounds p
          xs = elmFila p x

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
--    matriz1 = listArray ((1,1),(5,5)) [2, 2, 2, 2, 2, 
--                                       2, 0, 0, 0, 2, 
--                                       2, 0, 0, 0, 2, 
--                                       2, 0, 0, 0, 2, 
--                                       2, 2, 2, 2, 2]
--    matriz2 = listArray ((1,1),(5,5)) [2.0, 2.0, 2.0, 2.0, 2.0, 
--                                       2.0, 0.8, 0.4, 0.8, 2.0, 
--                                       2.0, 0.4, 0.0, 0.4, 2.0, 
--                                       2.0, 0.8, 0.4, 0.8, 2.0, 
--                                       2.0, 2.0, 2.0, 2.0, 2.0]
-- 
-- Definir la función 
--    iteracion_jacobi:: Matriz Float -> Matriz Float
-- tal que (iteracion_jacobi p) es la matriz obtenida aplicándole una
-- transformación de Jacobi a la matriz p. Por ejemplo,
--    iteracion_jacobi matriz1  ==  matriz2
-- ---------------------------------------------------------------------

matriz1 :: Matriz Float
matriz1 = listArray ((1,1),(5,5)) [2, 2, 2, 2, 2, 
                                   2, 0, 0, 0, 2, 
                                   2, 0, 0, 0, 2, 
                                   2, 0, 0, 0, 2, 
                                   2, 2, 2, 2, 2]

matriz2 :: Matriz Float                                
matriz2 = listArray ((1,1),(5,5)) [2.0, 2.0, 2.0, 2.0, 2.0, 
                                   2.0, 0.8, 0.4, 0.8, 2.0, 
                                   2.0, 0.4, 0.0, 0.4, 2.0, 
                                   2.0, 0.8, 0.4, 0.8, 2.0, 
                                   2.0, 2.0, 2.0, 2.0, 2.0]

-- fracruzam
iteracion_jacobi :: Matriz Float -> Matriz Float
iteracion_jacobi p = listArray b [jacobi ij | ij <- range b]
  where b@(_,(m,n)) = bounds p
        jacobi :: (Int,Int) -> Float
        jacobi (i,j) | i == 1 || j == 1 || i == n || j == n = p!(i,j)
                     | otherwise = 0.2 * (p!(i,j) + sum ys + sum zs)
          where xs = [-1,1]
                ys = [p!(k,j) | l <- xs, let k = i + l, k > 0, k <= n]
                zs = [p!(i,k) | l <- xs, let k = j + l, k > 0, k <= n]

-- abrdelrod manvermor manpende juanarcon ivaruicam erisancha
iteracion_jacobi2 :: Matriz Float -> Matriz Float
iteracion_jacobi2 p =
    listArray (bounds p) [f i j | i <- [1..m],j <- [1..n]]
    where (m,n) = snd $ bounds p
          f i j | i == 1 || i == m || j == 1 || j == m = p!(i,j)
                | otherwise = 
                    0.2*(p!(i,j)+p!(i+1,j)+p!(i-1,j)+p!(i,j+1)+p!(i,j-1))

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

-- fracruzam
creaTridiagonal :: Int -> Matriz Int
creaTridiagonal n = listArray ((1,1),(n,n)) xs
  where k = n - 2
        xs = 1:1:replicate k 0 
             ++ concat [x:y:y:replicate k 0 | x <- [1..n-1], let y = x + 1]
             ++ replicate k 0 ++ [n,n+1]

-- abrdelrod
creaTridiagonal2 :: Int -> Matriz Int
creaTridiagonal2 n =
    listArray ((1,1),(n,n)) [f i j | i <- [1..n], j <- [1..n]]
    where f i j | elem i [j-1,j] = i
                | i == j+1 = i-1
                | otherwise = 0

-- manvermor manpende juanarcon erisancha
creaTridiagonal3 :: Int -> Matriz Int
creaTridiagonal3 n =
    array ((1,1),(n,n)) [((i,j),f i j) | i <- [1..n], j <- [1..n]]
    where f i j | i == j = i
                | (i-1,j) == (j,j) = j
                | (i,j-1) == (i,i) = i
                | otherwise = 0

-- ----------------------------------------------------------------------------
-- Ejercicio 26.2. Definir la función 
--    esTridiagonal :: Matriz Int -> Bool
-- tal que (esTridiagonal p) se verifica si la matriz p es tridiagonal. Por 
-- ejemplo,
--    esTridiagonal (creaTridiagonal 5)               ==  True
--    esTridiagonal (listArray ((1,1),(3,3)) [1..9])  ==  False
-- ----------------------------------------------------------------------------

-- fracruzam abrdelrod manvermor manpende juanarcon erisancha
esTridiagonal :: Matriz Int -> Bool
esTridiagonal p = all tridigonal (range b)
  where b@(_,(n,m)) = bounds p
        tridigonal :: (Int,Int) -> Bool
        tridigonal (i,j)
            | i == j || i == j + 1 || i + 1 == j = p!(i,j) /= 0
            | otherwise                          = p!(i,j) == 0

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

-- fracruzam
vandermonde :: [Integer] -> Matriz Integer
vandermonde xs = array ((1,1),(n,n)) ys
  where ys = concat [map (\(x,i) -> ((i,m+1),x^m)) zs | m <- [0..n-1]]
        n = length xs
        zs = zip xs [1..]

-- abrdelrod erisancha
vandermonde2 :: [Integer] -> Matriz Integer
vandermonde2 xs =
    listArray ((1,1),(n,n)) [(xs!!(i-1))^(j-1) | i <- [1..n], j <- [1..n]]
    where n = length xs

-- manvermor manpende juanarcon ivaruicam
vandermonde3:: [Integer] -> Matriz Integer
vandermonde3 xs =
    listArray ((1,1),(d,d)) [ x^n | x <- xs, n <-[0..d-1]]
    where d = length xs

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

-- fracruzam
esMonomial :: Matriz Int -> Bool
esMonomial p = all esListaMonomial xss
    where (_,(_,n)) = bounds p
          xss = takeWhile (not . null)
                          (map (take n) (iterate (drop n) (elems p)))

-- abrdelrod manpende juanarcon ivaruicam erisancha
esMonomial2 :: Matriz Int -> Bool
esMonomial2 p = all esListaMonomial filas
    where filas = [[p!(i,k) | k <- [1..m]] | i <- [1..n]]
          (m,n) = snd $ bounds p

-- manvermor
esMonomial3 :: Matriz Int -> Bool
esMonomial3 p = [1..n] == sort (concat (map posTodas (listas n $ elems p)))
  where (_,(_,n)) = bounds p
                    
-- Comentario: La definición anterior se puede simplificar usando concatMap.

posTodas :: (Eq a, Num t, Num a) => [a] -> [t]
posTodas xs = [ posicion x xs | x <- xs, x /=0]

posicion :: (Eq a, Num a1) => a -> [a] -> a1
posicion n (x:xs) | n == x = 1
                  | otherwise = 1 + posicion n xs

-- (esListaMonomial xs) se verifica si todos los elementos de xs excepto
-- uno son nulos. Por ejemplo,
--    esListaMonomial [0,3,0,0]  ==  True
--    esListaMonomial [0,3,0,2]  ==  False
--    esListaMonomial [0,0,0,0]  ==  False

-- fracruzam
esListaMonomial :: [Int] -> Bool
esListaMonomial (x:xs) | x /= 0    = all (0==) xs
                       | otherwise = esListaMonomial xs
esListaMonomial  _     = False

-- abrdelrod manvermor manpende juanarcon ivaruicam
esListaMonomial2 :: [Int] -> Bool
esListaMonomial2 xs = length (filter (/= 0) xs) == 1

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

-- fracruzam
matrizPascal :: Int -> Matriz Int 
matrizPascal n = listArray ((1,1),(n,n)) (concat xs)
  where
    xs = zipWith (\xs y -> xs ++ replicate y 0) listaPascal [n-1,n-2..0]
    listaPascal :: [[Int]]
    listaPascal =
       [1] : map (\ys@(_:xs) -> 1: zipWith (+) ys xs ++ [1]) listaPascal

-- abrdelrod manpende juanarcon ivaruicam
matrizPascal2 :: Int -> Matriz Int 
matrizPascal2 n =
    listArray ((1,1),(n,n)) [f i j | i <- [1..n], j <- [1..n]]
      where f i j | j == 1 = 1
                  | i == 1 = 0
                  | otherwise = f (i-1) (j-1) + f (i-1) j

-- manvermor erisancha
matrizPascal3 :: Int -> Matriz Int 
matrizPascal3 n = listArray ((1,1),(n,n)) (concat xss)
    where xss = [ take n xs | xs <- [ ys ++ repeat 0 | ys <- take n pascal]]

pascal :: [[Int]]
pascal = [1] : map f pascal
    where f xs = zipWith (+) (0:xs) (xs++[0])

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

-- abrdelrod
ternasPrimasOrden :: Int -> [[Int]]
ternasPrimasOrden n = concatMap h (indices p)
    where vecinos i j = filter isPrime [f k l | k <- [i-1..i+1],
                                                l <- [j-1..j+1],
                                                (k,l) /= (i,j)]
          f i j | i < 1 || i > n || j < 1 || j > n = 0
                | otherwise = p!(i,j)
          g = concat.aux 
          aux (x:xs) = [[x,z] | z <- xs] : aux xs
          aux _ = []
          h i@(a,b) | isPrime (p!i) =  map (p!i:) (g $ vecinos a b)
                    | otherwise = []
          p = listArray ((1,1),(n,n)) [1..n^2]

-- manvermor erisancha
ternasPrimasOrden2 :: Int -> [[Int]]
ternasPrimasOrden2 n =
    filter (\xs -> all isPrime xs) $ todasTernas (matrizOrden n)

-- Comentario: La definición anterior se puede simplificar eliminando el lambda.

todasTernas :: (Enum t, Enum t1, Num t, Num t1, Ix t, Ix t1) 
               => Array (t, t1) e -> [[e]]
todasTernas p =
    [[p!(i,j),p!(i',j'),p!(i'',j'')] | (i,j) <- indices p,
                                       ((i',j'):ps) <- tails (vecinos (i,j)),
                                       (i'',j'') <- ps]
    where (_,(m,n)) = bounds p
          vecinos (i,j) = [(a,b) | a <- [max 1 (i-1)..min m (i+1)],
                                   b <- [max 1 (j-1)..min n (j+1)],
                                   (a,b) /= (i,j)]

matrizOrden :: (Enum e, Num e, Ix e) => e -> Array (e, e) e
matrizOrden n = listArray ((1,1),(n,n)) [1..n^2]

-- manpende juanarcon
ternasPrimasOrden3 :: Int -> [[Int]]
ternasPrimasOrden3 n = [xs | xs <- xss, length xs == 3]
     where xss = map nub [[p!(i,j),y,z] | i <- [1..n],
                                          j <- [1..n], 
                                          y <- vecinos i j p,
                                          z <- vecinos i j p,
                                          isPrime (p!(i,j)),
                                          isPrime y && isPrime z,
                                          z>y]
           p = matrizOrden n

matrizOrden2 :: Int -> Matriz Int
matrizOrden2 n = listArray ((1,1),(n,n)) [1..n^2]
 
vecinos :: Int -> Int -> Matriz Int -> [Int]
vecinos i j p = [p!(x,y) | x <- [a1..a2], y <- [b1..b2]]
    where a1 = max 1 (i-1)
          a2 = min n (i+1)
          b1 = max 1 (j-1)
          b2 = min m (j+1)
          (_,(n,m)) = bounds p
