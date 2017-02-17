-- I1M 2016-17: Relación 16 (9 de febrero de 2017)
-- Método de Gauss para triangularizar matrices.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- El objetivo de esta relación es definir el método de Gauss para
-- triangularizar matrices.

-- Además, en algunos ejemplos de usan matrices con números racionales.
-- En Haskell, el número racional x/y se representa por x%y. El TAD de
-- los números racionales está definido en el módulo Data.Ratio.
 
-- ---------------------------------------------------------------------
-- Importación de librerías                                           --
-- ---------------------------------------------------------------------

import Data.Array
import Data.Ratio
import Data.Maybe

-- ---------------------------------------------------------------------
-- Tipos de los vectores y de las matrices                            --
-- ---------------------------------------------------------------------

-- Los vectores son tablas cuyos índices son números naturales.
type Vector a = Array Int a
 
-- Las matrices son tablas cuyos índices son pares de números
-- naturales. 
type Matriz a = Array (Int,Int) a

-- ---------------------------------------------------------------------
-- Funciones auxiliares                                               --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    listaMatriz :: Num a => [[a]] -> Matriz a
-- tal que (listaMatriz xss) es la matriz cuyas filas son los elementos
-- de xss. Por ejemplo,
--    ghci> listaMatriz [[1,3,5],[2,4,7]]
--    array ((1,1),(2,3)) [((1,1),1),((1,2),3),((1,3),5),
--                         ((2,1),2),((2,2),4),((2,3),7)]
-- --------------------------------------------------------------------

listaMatriz :: Num a => [[a]] -> Matriz a
listaMatriz xss = listArray ((1,1),(m,n)) (concat xss)
    where m = length xss
          n = length (head xss)

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    separa :: Int -> [a] -> [[a]]
-- tal que (separa n xs) es la lista obtenida separando los elementos de
-- xs en grupos de n elementos (salvo el último que puede tener menos de
-- n elementos). Por ejemplo, 
--    separa 3 [1..11]  ==  [[1,2,3],[4,5,6],[7,8,9],[10,11]]
-- ---------------------------------------------------------------------

separa :: Int -> [a] -> [[a]]
separa _ [] = []
separa n xs = take n xs : separa n (drop n xs)

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
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

matrizLista :: Num a => Matriz a -> [[a]]
matrizLista p = separa (numColumnas p) (elems p)

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función
--    numFilas :: Num a => Matriz a -> Int
-- tal que (numFilas m) es el número de filas de la matriz m. Por
-- ejemplo,
--    numFilas (listaMatriz [[1,3,5],[2,4,7]])  ==  2
-- ---------------------------------------------------------------------

numFilas :: Num a => Matriz a -> Int
numFilas = fst . snd . bounds

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la función
--    numColumnas :: Num a => Matriz a -> Int
-- tal que (numColumnas m) es el número de columnas de la matriz
-- m. Por ejemplo,
--    numColumnas (listaMatriz [[1,3,5],[2,4,7]])  ==  3
-- ---------------------------------------------------------------------

numColumnas :: Num a => Matriz a -> Int
numColumnas = snd . snd . bounds

-- ---------------------------------------------------------------------
-- Ejercicio 6. Definir la función
--    dimension :: Num a => Matriz a -> (Int,Int)
-- tal que (dimension m) es la dimensión de la matriz m. Por ejemplo, 
--    dimension (listaMatriz [[1,3,5],[2,4,7]])  ==  (2,3)
-- ---------------------------------------------------------------------

dimension :: Num a => Matriz a -> (Int,Int)
dimension p = (numFilas p, numColumnas p)

-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir la función
--    diagonalPral :: Num a => Matriz a -> Vector a
-- tal que (diagonalPral p) es la diagonal principal de la matriz p. Por
-- ejemplo, 
--    ghci> let p = listaMatriz [[5,1,0],[3,2,6]]
--    ghci> diagonalPral p
--    array (1,2) [(1,5),(2,2)]
--    ghci> elems (diagonalPral p)
--    [5,2]
-- ---------------------------------------------------------------------

diagonalPral :: Num a => Matriz a -> Vector a
diagonalPral p = array (1,n) [(i,p!(i,i)) | i <- [1..n]]
    where n = min (numFilas p) (numColumnas p)

-- ---------------------------------------------------------------------
-- Transformaciones elementales                                       --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 8. Definir la función
--    intercambiaFilas :: Num a => Int -> Int -> Matriz a -> Matriz a
-- tal que (intercambiaFilas k l p) es la matriz obtenida intercambiando
-- las filas k y l de la matriz p. Por ejemplo, 
--    ghci> let p = listaMatriz [[5,1,0],[3,2,6],[4,6,9]]
--    ghci> intercambiaFilas 1 3 p
--    array ((1,1),(3,3)) [((1,1),4),((1,2),6),((1,3),9),
--                         ((2,1),3),((2,2),2),((2,3),6),
--                         ((3,1),5),((3,2),1),((3,3),0)]
--    ghci> matrizLista (intercambiaFilas 1 3 p)
--    [[4,6,9],[3,2,6],[5,1,0]]
-- ---------------------------------------------------------------------

-- albcercid marjimcom monlagare marlobrip antlopgom2 antmorper3
-- roscargar paumacpar beagongon1 enrnarbej manruiber belbenzam
-- eledejim2 cargonler pabrabmon margirmon migibagar margarflo5
-- antbeacar 
intercambiaFilas :: Num a => Int -> Int -> Matriz a -> Matriz a
intercambiaFilas k l p =
  listArray ((1,1),(m,n)) [p!(t i,j) | i <- [1..m], j <- [1..n]]
  where t i | k == i    = l
            | l == i    = k
            | otherwise = i
        (m,n) = dimension p

-- margarvil14 josdeher
intercambiaFilas2 :: Num a => Int -> Int -> Matriz a -> Matriz a
intercambiaFilas2 k l p =
  array ((1,1), (m,n)) [((i,j), p! f i j) | i <- [1..m], j <- [1..n]]
  where (m,n) = dimension p
        f i j | i == k    = (l,j)
              | i == l    = (k,j)
              | otherwise = (i,j)

-- eliguivil

intercambiaFilas3 :: Num a => Int -> Int -> Matriz a -> Matriz a
intercambiaFilas3 k l p = p // [if   i == k
                                then ((i,j),p!(l,j))
                                else ((i,j),p!(k,j))
                               | i <- [k,l],
                                 j <- [1..numColumnas p]]

-- ---------------------------------------------------------------------
-- Ejercicio 9. Definir la función
--    intercambiaColumnas :: Num a => Int -> Int -> Matriz a -> Matriz a
-- tal que (intercambiaColumnas k l p) es la matriz obtenida
-- intercambiando las columnas k y l de la matriz p. Por ejemplo, 
--    ghci> let p = listaMatriz [[5,1,0],[3,2,6],[4,6,9]]
--    ghci> matrizLista (intercambiaColumnas 1 3 p)
--    [[0,1,5],[6,2,3],[9,6,4]]
-- ---------------------------------------------------------------------

-- albcercid marjimcom monlagare marlobrip antlopgom2 antmorper3
-- roscargar paumacpar juacasnie enrnarbej beagongon1 manruiber
-- belbenzam eledejim2 cargonler pabrabmon migibagar natruipin 
-- margarflo5 antbeacar

intercambiaColumnas :: Num a => Int -> Int -> Matriz a -> Matriz a
intercambiaColumnas k l p =
  listArray ((1,1),(m,n)) [p!(i,t j) | i <- [1..m],
                                       j <- [1..n]]
  where t i | k == i    = l
            | l == i    = k
            | otherwise = i
        (m,n) = dimension p

-- margarvil14 josdeher
intercambiaColumnas2 :: Num a => Int -> Int -> Matriz a -> Matriz a
intercambiaColumnas2 k l p =
  array ((1,1), (m,n))
        [((i,j), p ! f i j)| i <- [1..m], j <- [1..n]]
  where (m,n) = dimension p
        f i j | j == k    = (i,l)
              | j == l    = (i,k)
              | otherwise = (i,j)

-- eliguivil
intercambiaColumnas3 :: Num a => Int -> Int -> Matriz a -> Matriz a
intercambiaColumnas3 k l p = p // [if   j == k
                                   then ((i,j),p!(i,l))
                                   else ((i,j),p!(i,k))
                                  | i <- [1..numFilas p],
                                    j <- [k,l]]

-- margirmon
intercambiaColumnas4 :: Num a => Int -> Int -> Matriz a -> Matriz a
intercambiaColumnas4 k l p = intercambiaFilas k l (traspuesta p)

traspuesta :: Num a => Matriz a -> Matriz a
traspuesta p = array ((1,1),(c,f)) [((i,j),p!(j,i)) | i <- [1..c],
                                                      j <- [1..f]]
  where c = numColumnas p
        f = numFilas p

-- ---------------------------------------------------------------------
-- Ejercicio 10. Definir la función
--    multFilaPor :: Num a => Int -> a -> Matriz a -> Matriz a
-- tal que (multFilaPor k x p) es a matriz obtenida multiplicando la
-- fila k de la matriz p por el número x. Por ejemplo,
--    ghci> let p = listaMatriz [[5,1,0],[3,2,6],[4,6,9]]
--    ghci> matrizLista (multFilaPor 2 3 p)
--    [[5,1,0],[9,6,18],[4,6,9]]
-- ---------------------------------------------------------------------

-- eliguivil enrnarbej
multFilaPor :: Num a => Int -> a -> Matriz a -> Matriz a
multFilaPor k x p = p // [((k,j), x * p!(k,j)) 
                         | j <- [1..numColumnas p]]

-- margarvil14 marlobrip josdeher
multFilaPor2 :: Num a => Int -> a -> Matriz a -> Matriz a
multFilaPor2 k x p =
  array ((1,1), (m,n))
        [((i,j), f i j) | i <- [1..m], j <- [1..n]]
  where (m,n) = dimension p
        f i j | i == k     = x*p!(i,j)
              | otherwise  = p!(i,j)

-- albcercid marjimcom monlagare antmorper3 antlopgom2 carmarcar5
-- roscargar paumacpar juacasnie enrnarbej beagongon1 manruiber
-- belbenzam eledejim2 cargonler pabrabmon margirmon migibagar natruipin 
-- margarflo5 antbeacar
multFilaPor3 :: Num a => Int -> a -> Matriz a -> Matriz a
multFilaPor3 k x p =
  listArray ((1,1),(m,n)) [t i * p!(i,j) | i <- [1..m],
                                           j <- [1..n]]
  where t i | k == i    = x
            | otherwise = 1
        (m,n) = dimension p

-- ---------------------------------------------------------------------
-- Ejercicio 11. Definir la función
--    sumaFilaFila :: Num a => Int -> Int -> Matriz a -> Matriz a
-- tal que (sumaFilaFila k l p) es la matriz obtenida sumando la fila l
-- a la fila k de la matriz p. Por ejemplo,
--    ghci> let p = listaMatriz [[5,1,0],[3,2,6],[4,6,9]]
--    ghci> matrizLista (sumaFilaFila 2 3 p)
--    [[5,1,0],[7,8,15],[4,6,9]]
-- ---------------------------------------------------------------------

-- eliguivil enrnarbej
sumaFilaFila :: Num a => Int -> Int -> Matriz a -> Matriz a
sumaFilaFila k l p = p // [((k,j), p!(l,j)+p!(k,j))
                          | j <- [1..numColumnas p]]

-- margarvil14 marlobrip josdeher
sumaFilaFila2 :: Num a => Int -> Int -> Matriz a -> Matriz a
sumaFilaFila2 k l p =
  array ((1,1), (m,n))
        [((i,j), f i j) | i <- [1..m], j <- [1..n]]
  where (m,n) = dimension p
        f i j | i == k     = p!(i,j) + p!(l,j)
              | otherwise  = p!(i,j)

-- albcercid marjimcom monlagare antmorper3 carmarcar5 roscargar
-- paumacpar juacasnie manruiber belbenzam eledejim2 cargonler 
-- pabrabmon margirmon migibagar natruipin margarflo5 antbeacar
sumaFilaFila3 :: Num a => Int -> Int -> Matriz a -> Matriz a
sumaFilaFila3 k l p =
  listArray ((1,1),(m,n)) [p!(i,j) + t i * p!(l,j)
                          | i <- [1..m],
                            j <- [1..n]]
  where t i | k == i    = 1
            | otherwise = 0
        (m,n) = dimension p

-- antlopgom2
sumaFilaFila4 :: Num a => Int -> Int -> Matriz a -> Matriz a
sumaFilaFila4 k l p =
  listArray ((1,1),(m,n)) [f (i,j) + p!(i,j) | i <- [1..m], j <- [1..n]]
  where f (i,j) | i == k    = p!(l,j) 
                | otherwise = 0
        (m,n) = dimension p

-- ---------------------------------------------------------------------
-- Ejercicio 12. Definir la función
--    sumaFilaPor :: Num a => Int -> Int -> a -> Matriz a -> Matriz a
-- tal que (sumaFilaPor k l x p) es la matriz obtenida sumando a la fila
-- k de la matriz p la fila l multiplicada por x. Por ejemplo,
--    ghci> let p = listaMatriz [[5,1,0],[3,2,6],[4,6,9]]
--    ghci> matrizLista (sumaFilaPor 2 3 10 p)
--    [[5,1,0],[43,62,96],[4,6,9]]
-- ---------------------------------------------------------------------

-- eliguivil enrnarbej
sumaFilaPor :: Num a => Int -> Int -> a -> Matriz a -> Matriz a
sumaFilaPor k l x p = p // [((k,j), x * p!(l,j) + p!(k,j))
                           | j <- [1..numColumnas p]]

-- albcercid marjimcom monlagare marlobrip antmorper3 roscargar
-- paumacpar manruiber belbenzam eledejim2 cargonler pabrabmon 
-- margirmon migibagar margarflo5 antbeacar
sumaFilaPor2 :: Num a => Int -> Int -> a -> Matriz a -> Matriz a
sumaFilaPor2 k l x p =
  listArray ((1,1),(m,n)) [p!(i,j)+ t i * p!(l,j) | i <- [1..m],
                                                   j <- [1..n]]
  where t i | k == i    = x
            | otherwise = 0
        (m,n) = dimension p

-- margarvil14 josdeher
sumaFilaPor3 :: Num a => Int -> Int -> a -> Matriz a -> Matriz a
sumaFilaPor3 k l x p =
  array ((1,1), (m,n))
        [((i,j), f i j) | i <- [1..m], j <- [1..n]]
  where (m,n) = dimension p
        f i j | i == k    = p!(i,j) + x*p!(l,j)
              | otherwise = p!(i,j)

-- antlopgom2 juacasnie natruipin
sumaFilaPor4 :: Num a => Int -> Int -> a -> Matriz a -> Matriz a
sumaFilaPor4 k l x p =
  listArray ((1,1),(m,n)) [f (i,j) + p!(i,j) | i <- [1..m], j <- [1..n]]
  where f (i,j) |  i == k = x*p!(l,j)
                |  otherwise = 0
        (m,n) = dimension p

-- carmarcar5
sumaFilaPor5 :: Num a => Int -> Int -> a -> Matriz a -> Matriz a
sumaFilaPor5 k l x p = sumaFilaFila k l q
  where q = multFilaPor l x p

-- Comentario: Es incorrecta. Por ejemplo,
--    λ> matrizLista (sumaFilaPor 2 3 10 p)
--    [[5,1,0],[43,62,96],[40,60,90]]

-- ---------------------------------------------------------------------
-- Triangularización de matrices                                      --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 13. Definir la función
--    buscaIndiceDesde :: (Num a, Eq a) => 
--                        Matriz a -> Int -> Int -> Maybe Int
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

-- albcercid antmorper3 roscargar paumacpar enrnarbej belbenzam pabrabmon
-- josdeher
buscaIndiceDesde :: (Num a, Eq a) => Matriz a -> Int -> Int -> Maybe Int
buscaIndiceDesde p j i
  | null xs   = Nothing
  | otherwise = Just (fst (head xs))
  where xs = [(t,p!(t,j)) | t <- [i..n], p!(t,j) /= 0]
        n  = numFilas p

-- margarvil14
buscaIndiceDesde2 :: (Num a, Eq a) => Matriz a -> Int -> Int -> Maybe Int
buscaIndiceDesde2 p j i
  | null xs   = Nothing
  | otherwise = Just (head xs)
  where xs = [k | ((k,z),y) <- assocs p, j == z, y /= 0, k >= i]

-- marjimcom natruipin
buscaIndiceDesde3 :: (Num a, Eq a) => Matriz a -> Int -> Int -> Maybe Int
buscaIndiceDesde3 p j i
  | null xs   = Nothing
  | otherwise = Just (head xs)
  where
    xs = [k | k <- [i..m], p!(k,j) /= 0]
    m  = numFilas p

-- juacasnie
buscaIndiceDesde4 :: (Num a, Eq a) => Matriz a -> Int -> Int -> Maybe Int
buscaIndiceDesde4 p j i
  | all (==0) (concat (drop (i-1) (matrizLista p))) = Nothing
  | p ! (i,j) /= 0 = Just i
  | p ! (i,j) == 0 = buscaIndiceDesde4 p j (i+1)

-- ---------------------------------------------------------------------
-- Ejercicio 14. Definir la función
--    buscaPivoteDesde :: (Num a, Eq a) => 
--                        Matriz a -> Int -> Int -> Maybe a
-- tal que (buscaPivoteDesde p j i) es el elemento de la matriz p en la
-- posición (k,j) donde k es (buscaIndiceDesde p j i). Por ejemplo,
--    ghci> let p = listaMatriz [[5,1,0],[3,2,6],[4,6,9]]
--    ghci> buscaPivoteDesde p 3 2
--    Just 6
--    ghci> let q = listaMatriz [[5,1,1],[3,2,0],[4,6,0]]
--    ghci> buscaPivoteDesde q 3 2
--    Nothing
-- ---------------------------------------------------------------------

-- margarvil14
buscaPivoteDesde :: (Num a, Eq a) => Matriz a -> Int -> Int -> Maybe a
buscaPivoteDesde p j i
  | null xs   = Nothing
  | otherwise = Just (head xs)
  where xs = [y | ((k,z),y) <- assocs p, j == z, y /= 0, k >=i]

-- albcercid marjimcom antmorper3 roscargar paumacpar belbenzam
-- pabrabmon migibagar natruipin josdeher
buscaPivoteDesde2 :: (Num a, Eq a) => Matriz a -> Int -> Int -> Maybe a
buscaPivoteDesde2 p j i
  | null xs   = Nothing
  | otherwise = Just (snd (head xs))
  where xs = [(t,p!(t,j)) | t <- [i..n], p!(t,j) /= 0]
        n  = numFilas p

-- juacasnie
buscaPivoteDesde3 :: (Num a, Eq a) => Matriz a -> Int -> Int -> Maybe a
buscaPivoteDesde3 p j i
  | isNothing (buscaIndiceDesde p j i) = Nothing
  | otherwise = Just (p! (fromJust (buscaIndiceDesde p j i), j))

-- enrnarbej
buscaPivoteDesde4 :: (Num a, Eq a) => Matriz a -> Int -> Int -> Maybe a
buscaPivoteDesde4 p j i =
  (\x -> p!(x,j)) <$> buscaIndiceDesde p j i

-- ---------------------------------------------------------------------
-- Ejercicio 15. Definir la función
--    anuladaColumnaDesde :: (Num a, Eq a) => 
--                            Int -> Int -> Matriz a -> Bool
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

-- enrnarbej josdeher
anuladaColumnaDesde :: (Num a, Eq a) => Matriz a -> Int -> Int -> Bool
anuladaColumnaDesde p j i = isNothing (buscaIndiceDesde p j (i + 1))

-- paumacpar 
anuladaColumnaDesde2 :: (Num a, Eq a) => Matriz a -> Int -> Int -> Bool
anuladaColumnaDesde2 p j i
  | isNothing (buscaIndiceDesde p j i) = True
  | otherwise = False 

-- albcercid marjimcom antmorper3 roscargar juacasnie pabrabmon
-- migibagar natruipin 

anuladaColumnaDesde3 :: (Num a, Eq a) => Matriz a -> Int -> Int -> Bool
anuladaColumnaDesde3 p j i = and [p!(t,j) == 0 | t <-[i+1..m]]
  where m = numFilas p

-- ---------------------------------------------------------------------
-- Ejercicio 16. Definir la función
--    anulaEltoColumnaDesde :: (Fractional a, Eq a) => 
--                             Matriz a -> Int -> Int -> Matriz a
-- tal que (anulaEltoColumnaDesde p j i) es la matriz obtenida a partir
-- de p anulando el primer elemento de la columna j por debajo de la
-- fila i usando el elemento de la posición (i,j). Por ejemplo,
--    ghci> let p = listaMatriz [[2,3,1],[5,0,5],[8,6,9]] :: Matriz Double
--    ghci> matrizLista (anulaEltoColumnaDesde p 2 1)
--    [[2.0,3.0,1.0],[5.0,0.0,5.0],[4.0,0.0,7.0]]
-- ---------------------------------------------------------------------

-- albcercid marjimcom antmorper3 roscargar juacasnie pabrabmon paumacpar
anulaEltoColumnaDesde :: (Fractional a, Eq a) => 
                          Matriz a -> Int -> Int -> Matriz a
anulaEltoColumnaDesde p j i
  | isNothing q = p
  | otherwise   = sumaFilaPor t i x p
  where t = unJust q
        x = -(p!(t,j))/(p!(i,j))
        unJust (Just a) = a
        q = buscaIndiceDesde p j (i+1)

-- enrnarbej josdeher
anulaEltoColumnaDesde2 :: (Fractional a, Eq a) =>
                          Matriz a -> Int -> Int -> Matriz a
anulaEltoColumnaDesde2 p j i
  | anuladaColumnaDesde p j i = p
  | otherwise                 = sumaFilaPor x i mul p
  where
    Just a = buscaPivoteDesde p j (i+1)
    Just x = buscaIndiceDesde p j (i+1)
    mul = (-1)*a/(p!(i,j))

-- ---------------------------------------------------------------------
-- Ejercicio 17. Definir la función
--    anulaColumnaDesde :: (Fractional a, Eq a) => 
--                         Matriz a -> Int -> Int -> Matriz a
-- tal que (anulaColumnaDesde p j i) es la matriz obtenida anulando
-- todos los elementos de la columna j de la matriz p por debajo del la
-- posición (i,j) (se supone que el elemento p_(i,j) es no nulo). Por
-- ejemplo, 
--    ghci> let p = listaMatriz [[2,2,1],[5,4,5],[10,8,9]] :: Matriz Double
--    ghci> matrizLista (anulaColumnaDesde p 2 1)
--    [[2.0,2.0,1.0],[1.0,0.0,3.0],[2.0,0.0,5.0]]
--    ghci> let p = listaMatriz [[4,5],[2,7%2],[6,10]] 
--    ghci> matrizLista (anulaColumnaDesde p 1 1)
--    [[4 % 1,5 % 1],[0 % 1,1 % 1],[0 % 1,5 % 2]]
-- ---------------------------------------------------------------------

-- albcercid antmorper3 roscargar marjimcom pabrabmon paumacpar
anulaColumnaDesde :: (Fractional a, Eq a) => 
                     Matriz a -> Int -> Int -> Matriz a
anulaColumnaDesde p j i
  | isNothing q = p
  | otherwise = anulaColumnaDesde x j i
  where x = anulaEltoColumnaDesde p j i
        q = buscaIndiceDesde p j (i+1)

-- juacasnie 
anulaColumnaDesde2 :: (Fractional a, Eq a) => 
                     Matriz a -> Int -> Int -> Matriz a
anulaColumnaDesde2 p j i =
  array ((1,1),(m,n)) [((k,l), t k l) | k <- [1..m], l <- [1..n]]
  where t k l | k <= i = p ! (k,l)
              | otherwise = p!(k,l) + ( -(p!(k,j)) / (p!(i,j)))*(p!(i,l))
        (m,n) = dimension p

-- enrnarbej
anulaColumnaDesde3 :: (Fractional a, Eq a) => 
                      Matriz a -> Int -> Int -> Matriz a
anulaColumnaDesde3 p j i =
  until (\m -> anuladaColumnaDesde3 m j i)
        (\m -> anulaEltoColumnaDesde m j i)
        p

-- josdeher
anulaColumnaDesde4 :: (Fractional a, Eq a) => 
                     Matriz a -> Int -> Int -> Matriz a
anulaColumnaDesde4 p j i | anuladaColumnaDesde p j i = p
                         | otherwise = anulaColumnaDesde4 x j i
               where x = anulaEltoColumnaDesde p j i

-- ---------------------------------------------------------------------
-- Algoritmo de Gauss para triangularizar matrices                    --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 18. Definir la función
--    elementosNoNulosColDesde :: (Num a, Eq a) => 
--                                Matriz a -> Int -> Int -> [a]
-- tal que (elementosNoNulosColDesde p j i) es la lista de los elementos
-- no nulos de la columna j a partir de la fila i. Por ejemplo,
--    ghci> let p = listaMatriz [[3,2],[5,1],[0,4]]
--    ghci> elementosNoNulosColDesde p 1 2
--    [5]
-- ---------------------------------------------------------------------

-- albcercid antmorper3 juacasnie roscargar marjimcom enrnarbej pabrabmon
-- paumacpar josdeher
elementosNoNulosColDesde :: (Num a, Eq a) => Matriz a -> Int -> Int -> [a]
elementosNoNulosColDesde p j i =
  [p!(t,j) | t <- [i..m], p!(t,j) /= 0]
  where m = numFilas p

-- ---------------------------------------------------------------------
-- Ejercicio 19. Definir la función
--    existeColNoNulaDesde :: (Num a, Eq a) => 
--                            Matriz a -> Int -> Int -> Bool
-- tal que (existeColNoNulaDesde p j i) se verifica si la matriz p tiene
-- una columna a partir de la j tal que tiene algún elemento no nulo por
-- debajo de la fila i; es decir, si la submatriz de p obtenida
-- eliminando las i-1 primeras filas y las j-1 primeras columnas es no
-- nula. Por ejemplo, 
--    ghci> let p = listaMatriz [[3,2,5],[5,0,0],[6,0,0]]
--    ghci> existeColNoNulaDesde p 2 2
--    False
--    ghci> let q = listaMatriz [[3,2,5],[5,7,0],[6,0,0]]
--    ghci> existeColNoNulaDesde q 2 2
-- ---------------------------------------------------------------------
  
-- albcercid antmorper3 roscargar marjimcom pabrabmon paumacpar josdeher
existeColNoNulaDesde :: (Num a, Eq a) => Matriz a -> Int -> Int -> Bool
existeColNoNulaDesde p j i =
  any (/= 0) [p!(a,b) | a <- [i..m], b <- [j..n]]
  where (m,n) = dimension p

-- juacasnie enrnarbej
existeColNoNulaDesde2 :: (Num a, Eq a) => Matriz a -> Int -> Int -> Bool
existeColNoNulaDesde2 p j i =
  and [elementosNoNulosColDesde p l i /= [] | l <- [j..n]]
  where n = numColumnas p

-- ---------------------------------------------------------------------
-- Ejercicio 20. Definir la función
--    menorIndiceColNoNulaDesde :: (Num a, Eq a) => 
--                                 Matriz a -> Int -> Int -> Maybe Int
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

-- albcercid antmorper3 juacasnie roscargar marjimcom enrnarbej pabrabmon
-- paumacpar josdeher
menorIndiceColNoNulaDesde :: (Num a, Eq a) => 
                             Matriz a -> Int -> Int -> Maybe Int
menorIndiceColNoNulaDesde p j i
  | null q    = Nothing
  | otherwise = Just (fst $ head q)
  where q = [(b,p!(a,b)) | b <- [j..n], a <- [i..m], p!(a,b) /= 0]
        (m,n) = dimension p

-- ----------------------------------------------------------------------
-- Ejercicio 21. Definir la función
--    gaussAux :: (Fractional a, Eq a) => 
--                Matriz a -> Int -> Int -> Matriz a
-- tal que (gaussAux p) es la matriz que en el que las i-1 primeras
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
--    ghci> matrizLista (gaussAux p 2 2)
--    [[1.0,2.0,3.0],[1.0,2.0,4.0],[2.0,0.0,1.0]]
-- ---------------------------------------------------------------------

-- albcercid antmorper3 juacasnie enrnarbej pabrabmon paumacpar

gaussAux :: (Fractional a, Eq a) => Matriz a -> Int -> Int -> Matriz a
gaussAux p i j
  | (m,n) == (i,j) || not (existeColNoNulaDesde p j i) = p
  | otherwise = gaussAux p' (i+1) (j+1)
  where (m,n) = dimension p
        j' = unJust (menorIndiceColNoNulaDesde p j i)
        unJust (Just a) = a
        p1 = intercambiaColumnas j j' p
        i' = head [ a | a <- [i..m], p1!(a,j) /= 0]
        p2 = intercambiaFilas i i' p1
        p' = anulaColumnaDesde p2 j i

-- josdeher
gaussAux2 :: (Fractional a, Eq a) => Matriz a -> Int -> Int -> Matriz a
gaussAux2 p i j | dimension p == (i,j) = p
                | not (existeColNoNulaDesde p j i) = p
                | otherwise = gaussAux2 p' (i+1) (j+1)
       where j' = fromJust (menorIndiceColNoNulaDesde p j i)
             p1 = intercambiaColumnas j j' p
             i' = fromJust (menorIndiceColNoNulaDesde (traspuesta2 p) j i)
             p2 = intercambiaFilas i i' p1
             p' = anulaColumnaDesde p2 j i
                       
traspuesta2 :: Num a => Matriz a -> Matriz a
traspuesta2 p =
  listArray ((1,1),(n,m)) [ p!(i,j) | j <- [1..n], i <- [1..m]]
  where (m,n) = dimension p

-- ---------------------------------------------------------------------
-- Ejercicio 22. Definir la función
--    gauss :: (Fractional a, Eq a) => Matriz a -> Matriz a
-- tal que (gauss p) es la triangularización de la matriz p por el método
-- de Gauss. Por ejemplo, 
--    ghci> let p = listaMatriz [[1.0,2,3],[1,2,4],[1,2,5]]
--    ghci> gauss p
--    array ((1,1),(3,3)) [((1,1),1.0),((1,2),3.0),((1,3),2.0),
--                         ((2,1),0.0),((2,2),1.0),((2,3),0.0),
--                         ((3,1),0.0),((3,2),0.0),((3,3),0.0)]
--    ghci> matrizLista (gauss p)
--    [[1.0,3.0,2.0],[0.0,1.0,0.0],[0.0,0.0,0.0]]
--    ghci> let p = listaMatriz [[3.0,2,3],[1,2,4],[1,2,5]]
--    ghci> matrizLista (gauss p)
--    [[3.0,2.0,3.0],[0.0,1.3333333333333335,3.0],[0.0,0.0,1.0]]
--    ghci> let p = listaMatriz [[3%1,2,3],[1,2,4],[1,2,5]]
--    ghci> matrizLista (gauss p)
--    [[3 % 1,2 % 1,3 % 1],[0 % 1,4 % 3,3 % 1],[0 % 1,0 % 1,1 % 1]]
--    ghci> let p = listaMatriz [[1.0,0,3],[1,0,4],[3,0,5]]
--    ghci> matrizLista (gauss p)
--    [[1.0,3.0,0.0],[0.0,1.0,0.0],[0.0,0.0,0.0]]
-- ---------------------------------------------------------------------

-- albcercid antmorper3 juacasnie enrnarbej pabrabmon paumacpar josdeher
gauss :: (Fractional a, Eq a) => Matriz a -> Matriz a
gauss p = gaussAux p 1 1

-- ---------------------------------------------------------------------
-- Determinante                                                       --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 23. Definir la función
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
--    ghci> gaussCAux (listaMatriz [[1.0,2,3],[1,2,4],[1,2,5]]) 1 1 0
--    (1,array ((1,1),(3,3)) [((1,1),1.0),((1,2),3.0),((1,3),2.0),
--                            ((2,1),0.0),((2,2),1.0),((2,3),0.0),
--                            ((3,1),0.0),((3,2),0.0),((3,3),0.0)])
-- ---------------------------------------------------------------------

-- albcercid antmorper3 juacasnie enrnarbej pabrabmon
gaussCAux :: (Fractional a, Eq a) => 
             Matriz a -> Int -> Int -> Int -> (Int,Matriz a)
gaussCAux p i j c
  | (m,n) == (i,j) || not (existeColNoNulaDesde p j i) = (c,p)
  | otherwise = gaussCAux p' (i+1) (j+1) (c + x + y)
  where (m,n) = dimension p
        j' = unJust (menorIndiceColNoNulaDesde p j i)
        unJust (Just a) = a
        p1 = intercambiaColumnas j j' p
        i' = head [ a | a <- [i..m], p1!(a,j) /= 0]
        p2 = intercambiaFilas i i' p1
        p' = anulaColumnaDesde p2 j i
        x | i == i' = 0
          | otherwise = 1
        y | j == j' = 0
          | otherwise = 1

-- josdeher
gaussCAux2 :: (Fractional a, Eq a) => 
             Matriz a -> Int -> Int -> Int -> (Int,Matriz a)
gaussCAux2 p i j c
  | dimension p == (i,j) = (c,p)
  | not (existeColNoNulaDesde p j i) = (c,p)
  | otherwise = gaussCAux2 p' (i+1) (j+1) (c + x + y)
  where j' = fromJust (menorIndiceColNoNulaDesde p j i)
        p1 = intercambiaColumnas j j' p
        i' = fromJust (menorIndiceColNoNulaDesde (traspuesta p) j i)
        p2 = intercambiaFilas i i' p1
        p' = anulaColumnaDesde p2 j i
        x  | i == i' = 0
           | otherwise = 1
        y  | j == j' = 0
           | otherwise = 1

-- ---------------------------------------------------------------------
-- Ejercicio 24. Definir la función
--    gaussC :: (Fractional a, Eq a) => Matriz a -> Matriz a
-- tal que (gaussC p) es el par (n,q), donde q es la triangularización
-- de la matriz p por el método de Gauss y n es el número de
-- intercambios de columnas y filas que se han producido durante el
-- cálculo. Por ejemplo,  
--    ghci> gaussC (listaMatriz [[1.0,2,3],[1,2,4],[1,2,5]])
--    (1,array ((1,1),(3,3)) [((1,1),1.0),((1,2),3.0),((1,3),2.0),
--                            ((2,1),0.0),((2,2),1.0),((2,3),0.0),
--                            ((3,1),0.0),((3,2),0.0),((3,3),0.0)])
-- ---------------------------------------------------------------------

-- albcercid antmorper3 juacasnie enrnarbej pabrabmon josdeher
gaussC :: (Fractional a, Eq a) => Matriz a -> (Int,Matriz a)
gaussC p = gaussCAux p 1 1 0

-- ---------------------------------------------------------------------
-- Ejercicio 25. Definir la función
--    determinante :: (Fractional a, Eq a) => Matriz a -> a
-- tal que (determinante p) es el determinante de la matriz p. Por
-- ejemplo, 
--    ghci> determinante (listaMatriz [[1.0,2,3],[1,3,4],[1,2,5]])
--    2.0
-- ---------------------------------------------------------------------

-- enrnarbej pabrabmon josdeher
determinante :: (Fractional a, Eq a) => Matriz a -> a
determinante p = (-1)^n * (product . diagonalPral) m
  where
    (n,m) = gaussC p

-- albcercid antmorper3 juacasnie 
determinante2 :: (Fractional a, Eq a) => Matriz a -> a
determinante2 p
  | n == 1 = head $ elems p
  | otherwise = sum [(-1)^(i+1) * p!(i,1) * f i 1 p | i <- [1..n]]
  where n = numFilas p
        f i j p = determinante $ submatriz i j p

submatriz :: Num a => Int -> Int -> Matriz a -> Matriz a
submatriz i j p =
  listArray ((1,1),(m-1,n-1))
            [p!(a,b) | a <- [1..m],
                       b <- [1..n],
                       a /= i,
                       b /= j]
  where (m,n) = dimension p
