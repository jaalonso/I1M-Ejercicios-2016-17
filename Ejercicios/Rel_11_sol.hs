-- I1M 2015-16: Relación 11 (13 de noviembre de 2015)
-- Tipos de datos algebraicos: Árboles binarios.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- En esta relación se presenta ejercicios sobre árboles binarios
-- definidos como tipos de datos algebraicos.
-- 
-- Los ejercicios corresponden al tema 9 que se encuentra en 
--    http://www.cs.us.es/~jalonso/cursos/i1m-15/temas/tema-9.html

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Test.QuickCheck
import Control.Monad

-- ---------------------------------------------------------------------
-- Nota. En los siguientes ejercicios se trabajará con los árboles
-- binarios definidos como sigue 
--    data Arbol a = H 
--                 | N a (Arbol a) (Arbol a)
--                 deriving (Show, Eq)
-- Por ejemplo, el árbol
--         9 
--        / \
--       /   \
--      3     7
--     / \  
--    2   4 
-- se representa por
--    N 9 (N 3 (H 2) (H 4)) (H 7) 
-- ---------------------------------------------------------------------

data Arbol a = H a
             | N a (Arbol a) (Arbol a)
             deriving (Show, Eq)

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    nHojas :: Arbol a -> Int
-- tal que (nHojas x) es el número de hojas del árbol x. Por ejemplo,
--    nHojas (N 9 (N 3 (H 2) (H 4)) (H 7))  ==  3
-- ---------------------------------------------------------------------

-- guache fracruzam
nHojas :: Arbol a -> Int
nHojas (H _)     = 1
nHojas (N x y z) = nHojas y + nHojas z

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Definir la función
--    nNodos :: Arbol a -> Int
-- tal que (nNodos x) es el número de nodos del árbol x. Por ejemplo,
--    nNodos (N 9 (N 3 (H 2) (H 4)) (H 7))  ==  2
-- ---------------------------------------------------------------------

-- guache
nNodos :: Arbol a -> Int
nNodos (H _)     = 0
nNodos (N x y z) = 1 + (nNodos y + nNodos z)

-- Comentario: La definición anterior se puede simplificar.

-- fracruzam
nNodos2 :: Arbol a -> Int
nNodos2 (H _)       = 0
nNodos2 (N a ai ad) = 1 + nNodos ai + nNodos ad

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Comprobar con QuickCheck que en todo árbol binario el
-- número de sus hojas es igual al número de sus nodos más uno.
-- ---------------------------------------------------------------------

-- guache fracruzam

-- La propiedad es
prop_nHojas :: Arbol Int -> Bool
prop_nHojas x = nHojas x == 1 + nNodos x

-- La comprobación es
--    *Main> quickCheck prop_nHojas
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Definir la función
--    profundidad :: Arbol a -> Int
-- tal que (profundidad x) es la profundidad del árbol x. Por ejemplo,
--    profundidad (N 9 (N 3 (H 2) (H 4)) (H 7))              ==  2
--    profundidad (N 9 (N 3 (H 2) (N 1 (H 4) (H 5))) (H 7))  ==  3
--    profundidad (N 4 (N 5 (H 4) (H 2)) (N 3 (H 7) (H 4)))  ==  2
-- ---------------------------------------------------------------------

profundidad :: Arbol a -> Int
profundidad = undefined 

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Comprobar con QuickCheck que para todo árbol biario
-- x, se tiene que
--    nNodos x <= 2^(profundidad x) - 1
-- ---------------------------------------------------------------------

-- guache fracruzam

-- La propiedad es
prop_nNodosProfundidad :: Arbol Int -> Bool
prop_nNodosProfundidad x = nNodos x <= 2^(profundidad x) - 1

-- La comprobación es
--    *Main> quickCheck prop_nNodosProfundidad 
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. Definir la función
--    preorden :: Arbol a -> [a]
-- tal que (preorden x) es la lista correspondiente al recorrido
-- preorden del árbol x; es decir, primero visita la raíz del árbol, a
-- continuación recorre el subárbol izquierdo y, finalmente, recorre el
-- subárbol derecho. Por ejemplo,
--    preorden (N 9 (N 3 (H 2) (H 4)) (H 7))  ==  [9,3,2,4,7]
-- ---------------------------------------------------------------------

-- guache fracruzam
preorden :: Arbol a -> [a]
preorden (H a)      = [a]
preorden (N a b c ) = a : preorden b ++ preorden c

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Comprobar con QuickCheck que la longitud de la lista
-- obtenida recorriendo un árbol en sentido preorden es igual al número
-- de nodos del árbol más el número de hojas.
-- ---------------------------------------------------------------------

-- guache fracruzam

-- La propiedad es
prop_length_preorden :: Arbol Int -> Bool
prop_length_preorden x =  length (preorden x) == nNodos x + nHojas x

-- La comprobación es
--    *Main> quickCheck prop_length_preorden
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la función
--    postorden :: Arbol a -> [a]
-- tal que (postorden x) es la lista correspondiente al recorrido
-- postorden del árbol x; es decir, primero recorre el subárbol
-- izquierdo, a continuación el subárbol derecho y, finalmente, la raíz
-- del árbol. Por ejemplo,
--    postorden (N 9 (N 3 (H 2) (H 4)) (H 7))  ==  [2,4,3,7,9]
-- ---------------------------------------------------------------------

-- fracruzam
postorden :: Arbol a -> [a]
postorden (H a)       = [a]
postorden (N a ai ad) = postorden ai ++ postorden ad ++ [a]

-- ---------------------------------------------------------------------
-- Ejercicio 6.1. Definir, usando un acumulador, la función
--    preordenIt :: Arbol a -> [a]
-- tal que (preordenIt x) es la lista correspondiente al recorrido
-- preorden del árbol x; es decir, primero visita la raíz del árbol, a
-- continuación recorre el subárbol izquierdo y, finalmente, recorre el
-- subárbol derecho. Por ejemplo,
--    preordenIt (N 9 (N 3 (H 2) (H 4)) (H 7))  ==  [9,3,2,4,7]
-- 
-- Nota: No usar (++) en la definición
-- ---------------------------------------------------------------------

-- fracruzam
preordenIt :: Arbol a -> [a]
preordenIt x = preordenItAux x []
    where preordenItAux :: Arbol a -> [a] -> [a]
          preordenItAux (H a) xs       = a:xs
          preordenItAux (N a ai ad) xs = 
              a : preordenItAux ai (preordenItAux ad xs)

-- ---------------------------------------------------------------------
-- Ejercicio 6.2. Comprobar con QuickCheck que preordenIt es equivalente
-- a preorden. 
-- ---------------------------------------------------------------------

-- fracruzam
-- La propiedad es
prop_preordenIt :: Arbol Int -> Bool
prop_preordenIt x = preordenIt x == preorden x

-- La comprobación es
--    *Main> quickCheck prop_preordenIt
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 7.1. Definir la función
--    espejo :: Arbol a -> Arbol a
-- tal que (espejo x) es la imagen especular del árbol x. Por ejemplo,
--    espejo (N 9 (N 3 (H 2) (H 4)) (H 7)) == N 9 (H 7) (N 3 (H 4) (H 2))
-- ---------------------------------------------------------------------

-- fracruzam
espejo :: Arbol a -> Arbol a
espejo (H a)       = H a
espejo (N a ai ad) = (N a (espejo ad) (espejo ai))

-- Comentario: La definición anterior se puede simplificar.

-- ---------------------------------------------------------------------
-- Ejercicio 7.2. Comprobar con QuickCheck que para todo árbol x,
--    espejo (espejo x) = x
-- ---------------------------------------------------------------------

-- fracruzam
-- La propiedad es
prop_espejo :: Arbol Int -> Bool
prop_espejo x = (espejo . espejo) x == x

-- La comprobación es
--    *Main> quickCheck prop_espejo
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 7.3. Comprobar con QuickCheck que para todo árbol binario
-- x, se tiene que
--    reverse (preorden (espejo x)) = postorden x
-- ---------------------------------------------------------------------

-- fracruzam

-- La propiedad es
prop_reverse_preorden_espejo :: Arbol Int -> Bool
prop_reverse_preorden_espejo x = 
    (reverse . preorden . espejo) x == postorden x

-- La comprobación es
--    *Main> quickCheck prop_reverse_preorden_espejo
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 7.4. Comprobar con QuickCheck que para todo árbol x,
--    postorden (espejo x) = reverse (preorden x)
-- ---------------------------------------------------------------------

-- fracruzam

-- La propiedad es
prop_recorrido :: Arbol Int -> Bool
prop_recorrido x = 
    (postorden . espejo) x == (reverse . preorden) x

-- La comprobación es
--    *Main> quickCheck prop_recorrido
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 8.1. La función take está definida por
--    take :: Int -> [a] -> [a]
--    take 0            = []
--    take (n+1) []     = []
--    take (n+1) (x:xs) = x : take n xs
-- 
-- Definir la función 
--    takeArbol ::  Int -> Arbol a -> Arbol a
-- tal que (takeArbol n t) es el subárbol de t de profundidad n. Por
-- ejemplo,
--    takeArbol 0 (N 9 (N 3 (H 2) (H 4)) (H 7)) == H 9
--    takeArbol 1 (N 9 (N 3 (H 2) (H 4)) (H 7)) == N 9 (H 3) (H 7)
--    takeArbol 2 (N 9 (N 3 (H 2) (H 4)) (H 7)) == N 9 (N 3 (H 2) (H 4)) (H 7)
--    takeArbol 3 (N 9 (N 3 (H 2) (H 4)) (H 7)) == N 9 (N 3 (H 2) (H 4)) (H 7)
-- ---------------------------------------------------------------------

-- fracruzam
takeArbol :: Int -> Arbol a -> Arbol a
takeArbol _ (H a)       = H a
takeArbol 0 (N a ai ad) = H a
takeArbol n (N a ai ad) = N a (takeArbol (n-1) ai) (takeArbol (n-1) ad)

-- ---------------------------------------------------------------------
-- Ejercicio 8.2. Comprobar con QuickCheck que la profundidad de 
-- (takeArbol n x) es menor o igual que n, para todo número natural n y
-- todo árbol x. 
-- ---------------------------------------------------------------------

-- fracruzam

-- La propiedad es
prop_takeArbol :: Int -> Arbol Int -> Property
prop_takeArbol n x = n > 0 ==> profundidad (takeArbol n x) <= n

-- La comprobación es
-- *Main> quickCheck prop_takeArbol
-- *** Failed! Falsifiable (after 10 tests and 2 shrinks): 
-- 2
-- N 4 (N (-8) (H (-2)) (N (-9) (H (-8)) (H (-4)))) (N (-2) (N 1 (H 1)
-- (N (-3)(H---5)) (H (-6)))) (N 3 (N (-7) (H (-1)) (H 0)) (H (-3))))

-- ---------------------------------------------------------------------
-- Ejercicio 9. La función
--    repeat :: a -> [a]
-- está definida de forma que (repeat x) es la lista formada por
-- infinitos elementos x. Por ejemplo,
--    repeat 3  ==  [3,3,3,3,3,3,3,3,3,3,3,3,3,...
-- La definición de repeat es
--    repeat x = xs where xs = x:xs
-- 
-- Definir la función
--    repeatArbol :: a -> Arbol a
-- tal que (repeatArbol x) es es árbol con infinitos nodos x. Por
-- ejemplo, 
--    takeArbol 0 (repeatArbol 3) == H 3
--    takeArbol 1 (repeatArbol 3) == N 3 (H 3) (H 3)
--    takeArbol 2 (repeatArbol 3) == N 3 (N 3 (H 3) (H 3)) (N 3 (H 3) (H 3))
-- ---------------------------------------------------------------------

-- fracruzam
repeatArbol :: a -> Arbol a
repeatArbol x = (N x ai ad)
    where ai = (N x ai ai)
          ad = (N x ad ad)

-- Comentario: La definición anterior se puede mejorar (observando que
-- los subárboles izquierdo y derecho son iguales).

-- ---------------------------------------------------------------------
-- Ejercicio 10.1. La función 
--    replicate :: Int -> a -> [a]
-- está definida por 
--    replicate n = take n . repeat
-- es tal que (replicate n x) es la lista de longitud n cuyos elementos
-- son x. Por ejemplo,
--    replicate 3 5  ==  [5,5,5]
-- 
-- Definir la función 
--    replicateArbol :: Int -> a -> Arbol a
-- tal que (replicate n x) es el árbol de profundidad n cuyos nodos son
-- x. Por ejemplo,
--    replicateArbol 0 5  ==  H 5
--    replicateArbol 1 5  ==  N 5 (H 5) (H 5)
--    replicateArbol 2 5  ==  N 5 (N 5 (H 5) (H 5)) (N 5 (H 5) (H 5))
-- ---------------------------------------------------------------------

-- fracruzam
replicateArbol :: Int -> a -> Arbol a
replicateArbol n x = takeArbol n (repeatArbol x)

-- ---------------------------------------------------------------------
-- Ejercicio 10.2. Comprobar con QuickCheck que el número de hojas de 
-- (replicateArbol n x) es 2^n, para todo número natural n
--
-- Nota. Al hacer la comprobación limitar el tamaño de las pruebas como
-- se indica a continuación
--    quickCheckWith (stdArgs {maxSize=7}) prop_replicateArbol
-- ---------------------------------------------------------------------

-- fracruzam

-- La propiedad es
prop_replicateArbol :: Int -> Int -> Property
prop_replicateArbol n x = n > 0 ==> nHojas (replicateArbol n x) == 2^n

-- La comprobación es
--    *Main> quickCheck prop_replicateArbol
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 11.1. Definir la función
--    mapArbol :: (a -> a) -> Arbol a -> Arbol a
-- tal que (mapArbol f x) es el árbol obtenido aplicándole a cada nodo de
-- x la función f. Por ejemplo,
--    ghci> mapArbol (*2) (N 9 (N 3 (H 2) (H 4)) (H 7)) 
--    N 18 (N 6 (H 4) (H 8)) (H 14)
-- ---------------------------------------------------------------------

-- fracruzam
mapArbol :: (a -> a) -> Arbol a -> Arbol a
mapArbol f (H a)       = H (f a)
mapArbol f (N a ai ad) = N (f a) (mapArbol f ai) (mapArbol f ad)

-- ---------------------------------------------------------------------
-- Ejercicio 11.2. Comprobar con QuickCheck que 
--    (mapArbol (1+)) . espejo = espejo . (mapArbol (1+))
-- ---------------------------------------------------------------------

-- fracruzam

-- La propiedad es
prop_mapArbol_espejo :: Arbol Int -> Bool
prop_mapArbol_espejo x = 
    ((mapArbol (1+)) . espejo) x == (espejo . (mapArbol (1+))) x

-- La comprobación es
--    *Main> quickCheck prop_mapArbol_espejo
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 11.3. Comprobar con QuickCheck que
--    (map (1+)) . preorden = preorden . (mapArbol (1+)) 
-- ---------------------------------------------------------------------

-- fracruzam

-- La propiedad es
prop_map_preorden :: Arbol Int -> Bool
prop_map_preorden x = 
     ((map (1+)) . preorden) x == (preorden . (mapArbol (1+))) x

-- La comprobación es
--    *Main> quickCheck prop_map_preorden
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Nota. Para comprobar propiedades de árboles con QuickCheck se
-- utilizará el siguiente generador.
-- ---------------------------------------------------------------------

instance Arbitrary a => Arbitrary (Arbol a) where
  arbitrary = sized arbol
    where
      arbol 0       = liftM H arbitrary 
      arbol n | n>0 = oneof [liftM H arbitrary,
                             liftM3 N arbitrary subarbol subarbol]
                      where subarbol = arbol (div n 2)
