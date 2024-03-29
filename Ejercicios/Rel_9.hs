-- I1M 2016-17: Relaci�n 9 (16 de noviembre de 2016)
-- Tipos de datos algebraicos: �rboles binarios.
-- Departamento de Ciencias de la Computaci�n e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducci�n                                                       --
-- ---------------------------------------------------------------------

-- En esta relaci�n se presenta ejercicios sobre �rboles binarios
-- definidos como tipos de datos algebraicos.
-- 
-- Los ejercicios corresponden al tema 9 que se encuentra en 
--    http://www.cs.us.es/~jalonso/cursos/i1m-16/temas/tema-9.html

-- ---------------------------------------------------------------------
-- � Librer�as auxiliares                                             --
-- ---------------------------------------------------------------------

import Test.QuickCheck
import Control.Monad

-- ---------------------------------------------------------------------
-- Nota. En los siguientes ejercicios se trabajar� con los �rboles
-- binarios definidos como sigue 
--    data Arbol a = H 
--                 | N a (Arbol a) (Arbol a)
--                 deriving (Show, Eq)
-- Por ejemplo, el �rbol
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
-- Ejercicio 1.1. Definir la funci�n
--    nHojas :: Arbol a -> Int
-- tal que (nHojas x) es el n�mero de hojas del �rbol x. Por ejemplo,
--    nHojas (N 9 (N 3 (H 2) (H 4)) (H 7))  ==  3
-- ---------------------------------------------------------------------

nHojas :: Arbol a -> Int
nHojas = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Definir la funci�n
--    nNodos :: Arbol a -> Int
-- tal que (nNodos x) es el n�mero de nodos del �rbol x. Por ejemplo,
--    nNodos (N 9 (N 3 (H 2) (H 4)) (H 7))  ==  2
-- ---------------------------------------------------------------------

nNodos :: Arbol a -> Int
nNodos = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 1.3. Comprobar con QuickCheck que en todo �rbol binario el
-- n�mero de sus hojas es igual al n�mero de sus nodos m�s uno.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_nHojas :: Arbol Int -> Bool
prop_nHojas x = undefined

-- La comprobaci�n es

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Definir la funci�n
--    profundidad :: Arbol a -> Int
-- tal que (profundidad x) es la profundidad del �rbol x. Por ejemplo,
--    profundidad (N 9 (N 3 (H 2) (H 4)) (H 7))              ==  2
--    profundidad (N 9 (N 3 (H 2) (N 1 (H 4) (H 5))) (H 7))  ==  3
--    profundidad (N 4 (N 5 (H 4) (H 2)) (N 3 (H 7) (H 4)))  ==  2
-- ---------------------------------------------------------------------

profundidad :: Arbol a -> Int
profundidad = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Comprobar con QuickCheck que para todo �rbol biario
-- x, se tiene que
--    nNodos x <= 2^(profundidad x) - 1
-- ---------------------------------------------------------------------

-- La propiedad es
prop_nNodosProfundidad :: Arbol Int -> Bool
prop_nNodosProfundidad x = undefined

-- La comprobaci�n es

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Definir la funci�n
--    preorden :: Arbol a -> [a]
-- tal que (preorden x) es la lista correspondiente al recorrido
-- preorden del �rbol x; es decir, primero visita la ra�z del �rbol, a
-- continuaci�n recorre el sub�rbol izquierdo y, finalmente, recorre el
-- sub�rbol derecho. Por ejemplo,
--    preorden (N 9 (N 3 (H 2) (H 4)) (H 7))  ==  [9,3,2,4,7]
-- ---------------------------------------------------------------------

preorden :: Arbol a -> [a]
preorden = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Comprobar con QuickCheck que la longitud de la lista
-- obtenida recorriendo un �rbol en sentido preorden es igual al n�mero
-- de nodos del �rbol m�s el n�mero de hojas.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_length_preorden :: Arbol Int -> Bool
prop_length_preorden x = undefined

-- La comprobaci�n es

-- ---------------------------------------------------------------------
-- Ejercicio 3.3. Definir la funci�n
--    postorden :: Arbol a -> [a]
-- tal que (postorden x) es la lista correspondiente al recorrido
-- postorden del �rbol x; es decir, primero recorre el sub�rbol
-- izquierdo, a continuaci�n el sub�rbol derecho y, finalmente, la ra�z
-- del �rbol. Por ejemplo,
--    postorden (N 9 (N 3 (H 2) (H 4)) (H 7))  ==  [2,4,3,7,9]
-- ---------------------------------------------------------------------

postorden :: Arbol a -> [a]
postorden = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 3.4. Definir, usando un acumulador, la funci�n
--    preordenIt :: Arbol a -> [a]
-- tal que (preordenIt x) es la lista correspondiente al recorrido
-- preorden del �rbol x; es decir, primero visita la ra�z del �rbol, a
-- continuaci�n recorre el sub�rbol izquierdo y, finalmente, recorre el
-- sub�rbol derecho. Por ejemplo,
--    preordenIt (N 9 (N 3 (H 2) (H 4)) (H 7))  ==  [9,3,2,4,7]
-- 
-- Nota: No usar (++) en la definici�n
-- ---------------------------------------------------------------------

preordenIt :: Arbol a -> [a]
preordenIt x = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 3.5. Comprobar con QuickCheck que preordenIt es equivalente
-- a preorden. 
-- ---------------------------------------------------------------------

-- La propiedad es
prop_preordenIt :: Arbol Int -> Bool
prop_preordenIt x = undefined

-- La comprobaci�n es

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. Definir la funci�n
--    espejo :: Arbol a -> Arbol a
-- tal que (espejo x) es la imagen especular del �rbol x. Por ejemplo,
--    espejo (N 9 (N 3 (H 2) (H 4)) (H 7)) == N 9 (H 7) (N 3 (H 4) (H 2))
-- ---------------------------------------------------------------------

espejo :: Arbol a -> Arbol a
espejo = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Comprobar con QuickCheck que para todo �rbol x,
--    espejo (espejo x) = x
-- ---------------------------------------------------------------------

-- La propiedad es
prop_espejo :: Arbol Int -> Bool
prop_espejo x = undefined

-- La comprobaci�n es

-- ---------------------------------------------------------------------
-- Ejercicio 4.3. Comprobar con QuickCheck que para todo �rbol binario
-- x, se tiene que
--    reverse (preorden (espejo x)) = postorden x
-- ---------------------------------------------------------------------

-- La propiedad es
prop_reverse_preorden_espejo :: Arbol Int -> Bool
prop_reverse_preorden_espejo x = undefined

-- La comprobaci�n es

-- ---------------------------------------------------------------------
-- Ejercicio 4.4. Comprobar con QuickCheck que para todo �rbol x,
--    postorden (espejo x) = reverse (preorden x)
-- ---------------------------------------------------------------------

-- La propiedad es
prop_recorrido :: Arbol Int -> Bool
prop_recorrido x = undefined

-- La comprobaci�n es

-- ---------------------------------------------------------------------
-- Ejercicio 5.1. La funci�n take est� definida por
--    take :: Int -> [a] -> [a]
--    take 0            = []
--    take (n+1) []     = []
--    take (n+1) (x:xs) = x : take n xs
-- 
-- Definir la funci�n 
--    takeArbol ::  Int -> Arbol a -> Arbol a
-- tal que (takeArbol n t) es el sub�rbol de t de profundidad n. Por
-- ejemplo,
--    takeArbol 0 (N 9 (N 3 (H 2) (H 4)) (H 7)) == H 9
--    takeArbol 1 (N 9 (N 3 (H 2) (H 4)) (H 7)) == N 9 (H 3) (H 7)
--    takeArbol 2 (N 9 (N 3 (H 2) (H 4)) (H 7)) == N 9 (N 3 (H 2) (H 4)) (H 7)
--    takeArbol 3 (N 9 (N 3 (H 2) (H 4)) (H 7)) == N 9 (N 3 (H 2) (H 4)) (H 7)
-- ---------------------------------------------------------------------
 
takeArbol :: Int -> Arbol a -> Arbol a
takeArbol = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 5.2. Comprobar con QuickCheck que la profundidad de 
-- (takeArbol n x) es menor o igual que n, para todo n�mero natural n y
-- todo �rbol x. 
-- ---------------------------------------------------------------------

-- La propiedad es
prop_takeArbol:: Int -> Arbol Int -> Property
prop_takeArbol n x = undefined

-- La comprobaci�n es

-- ---------------------------------------------------------------------
-- Ejercicio 6.1. La funci�n
--    repeat :: a -> [a]
-- est� definida de forma que (repeat x) es la lista formada por
-- infinitos elementos x. Por ejemplo,
--    repeat 3  ==  [3,3,3,3,3,3,3,3,3,3,3,3,3,...
-- La definici�n de repeat es
--    repeat x = xs where xs = x:xs
-- 
-- Definir la funci�n
--    repeatArbol :: a -> Arbol a
-- tal que (repeatArbol x) es es �rbol con infinitos nodos x. Por
-- ejemplo, 
--    takeArbol 0 (repeatArbol 3) == H 3
--    takeArbol 1 (repeatArbol 3) == N 3 (H 3) (H 3)
--    takeArbol 2 (repeatArbol 3) == N 3 (N 3 (H 3) (H 3)) (N 3 (H 3) (H 3))
-- ---------------------------------------------------------------------

repeatArbol :: a -> Arbol a
repeatArbol x = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 6.2. La funci�n 
--    replicate :: Int -> a -> [a]
-- est� definida por 
--    replicate n = take n . repeat
-- es tal que (replicate n x) es la lista de longitud n cuyos elementos
-- son x. Por ejemplo,
--    replicate 3 5  ==  [5,5,5]
-- 
-- Definir la funci�n 
--    replicateArbol :: Int -> a -> Arbol a
-- tal que (replicate n x) es el �rbol de profundidad n cuyos nodos son
-- x. Por ejemplo,
--    replicateArbol 0 5  ==  H 5
--    replicateArbol 1 5  ==  N 5 (H 5) (H 5)
--    replicateArbol 2 5  ==  N 5 (N 5 (H 5) (H 5)) (N 5 (H 5) (H 5))
-- ---------------------------------------------------------------------

replicateArbol :: Int -> a -> Arbol a
replicateArbol n = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 6.3. Comprobar con QuickCheck que el n�mero de hojas de 
-- (replicateArbol n x) es 2^n, para todo n�mero natural n
--
-- Nota. Al hacer la comprobaci�n limitar el tama�o de las pruebas como
-- se indica a continuaci�n
--    quickCheckWith (stdArgs {maxSize=7}) prop_replicateArbol
-- ---------------------------------------------------------------------

-- La propiedad es
prop_replicateArbol :: Int -> Int -> Property
prop_replicateArbol n x = undefined

-- La comprobaci�n es

-- ---------------------------------------------------------------------
-- Ejercicio 7.1. Definir la funci�n
--    mapArbol :: (a -> a) -> Arbol a -> Arbol a
-- tal que (mapArbol f x) es el �rbol obtenido aplic�ndole a cada nodo de
-- x la funci�n f. Por ejemplo,
--    ghci> mapArbol (*2) (N 9 (N 3 (H 2) (H 4)) (H 7)) 
--    N 18 (N 6 (H 4) (H 8)) (H 14)
-- ---------------------------------------------------------------------

mapArbol :: (a -> a) -> Arbol a -> Arbol a
mapArbol = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 7.2. Comprobar con QuickCheck que 
--    (mapArbol (1+)) . espejo = espejo . (mapArbol (1+))
-- ---------------------------------------------------------------------

-- La propiedad es
prop_mapArbol_espejo :: Arbol Int -> Bool
prop_mapArbol_espejo x = undefined

-- La comprobaci�n es

-- ---------------------------------------------------------------------
-- Ejercicio 7.3. Comprobar con QuickCheck que
--    (map (1+)) . preorden = preorden . (mapArbol (1+)) 
-- ---------------------------------------------------------------------

-- La propiedad es
prop_map_preorden :: Arbol Int -> Bool
prop_map_preorden x = undefined

-- La comprobaci�n es

-- ---------------------------------------------------------------------
-- Nota. Para comprobar propiedades de �rboles con QuickCheck se
-- utilizar� el siguiente generador.
-- ---------------------------------------------------------------------

instance Arbitrary a => Arbitrary (Arbol a) where
  arbitrary = sized arbol
    where
      arbol 0       = liftM H arbitrary 
      arbol n | n>0 = oneof [liftM H arbitrary,
                             liftM3 N arbitrary subarbol subarbol]
                      where subarbol = arbol (div n 2)

