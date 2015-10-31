-- I1M 2015-16: Rel_9.hs (31 de Octubre de 2015)
-- Operaciones conjuntistas con listas.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Definir, por comprensión, la función
--    subconjunto :: Eq a => [a] -> [a] -> Bool
-- tal que (subconjunto xs ys) se verifica si xs es un subconjunto de
-- ys; es decir, si todos los elementos de xs pertenecen a ys. Por
-- ejemplo, 
--    subconjunto [3,2,3] [2,5,3,5]  ==  True
--    subconjunto [3,2,3] [2,5,6,5]  ==  False
-- ---------------------------------------------------------------------

-- guache
subconjunto :: Eq a => [a] -> [a] -> Bool
subconjunto xs ys = and [elem x ys | x <- xs]

-- guache
subconjunto2 :: Eq a => [a] -> [a] -> Bool
subconjunto2 xs ys = xs == [x | x <- xs, elem x ys]

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Definir, por recursión, la función
--    subconjuntoR :: Eq a => [a] -> [a] -> Bool
-- tal que (subconjuntoR xs ys) se verifica si xs es un subconjunto de
-- ys; es decir, si todos los elementos de xs pertenecen a ys. Por
-- ejemplo, 
--    subconjuntoR [3,2,3] [2,5,3,5]  ==  True
--    subconjuntoR [3,2,3] [2,5,6,5]  ==  False
-- ---------------------------------------------------------------------

-- guache
subconjuntoR :: Eq a => [a] -> [a] -> Bool
subconjuntoR xs ys = sub xs ys
    where sub []     _  = True
          sub (x:xs) ys = elem x ys && sub xs ys

-- Comentario: La definición anterior se puede simplificar.

-- ---------------------------------------------------------------------
-- Ejercicio 1.3. Comprobar con QuickCheck que las definiciones
-- subconjunto y subconjuntoR son equivalentes.
-- ---------------------------------------------------------------------

-- guache

-- La propiedad es
prop_subconjuntoR :: [Int] -> [Int] -> Bool
prop_subconjuntoR xs ys = subconjuntoR xs ys == subconjunto xs ys

-- La comprobación es
--    *Main> quickCheck prop_subconjuntoR
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 1.4. Definir, mediante all, la función 
--    subconjuntoA :: Eq a => [a] -> [a] -> Bool
-- tal que (subconjuntoA xs ys) se verifica si xs es un subconjunto de
-- ys. Por ejemplo,
--    subconjuntoA [1,3,2,3] [1,2,3]  ==  True
--    subconjuntoA [1,3,4,3] [1,2,3]  ==  False
-- ---------------------------------------------------------------------

-- guache
subconjuntoA :: Eq a => [a] -> [a] -> Bool
subconjuntoA xs ys = all (\x -> elem x ys) xs

-- Comentario: La definición anterior se puede simplificar.
 
-- ---------------------------------------------------------------------
-- Ejercicio 1.5. Comprobar con QuickCheck que las funciones subconjunto
-- y subconjuntoA son equivalentes.
-- ---------------------------------------------------------------------

-- guache

-- La propiedad es
prop_subconjuntoA :: [Int] -> [Int] -> Bool
prop_subconjuntoA xs ys = subconjuntoA xs ys == subconjunto xs ys
 
-- La comprobación es
--    *Main> quickCheck prop_subconjuntoA
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    iguales :: Eq a => [a] -> [a] -> Bool
-- tal que (iguales xs ys) se verifica si xs e ys son iguales; es decir,
-- tienen los mismos elementos. Por ejemplo, 
--    iguales [3,2,3] [2,3]    ==  True
--    iguales [3,2,3] [2,3,2]  ==  True
--    iguales [3,2,3] [2,3,4]  ==  False
--    iguales [2,3] [4,5]      ==  False
-- ---------------------------------------------------------------------

iguales :: Eq a => [a] -> [a] -> Bool
iguales xs ys = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Definir, por comprensión, la función
--    union :: Eq a => [a] -> [a] -> [a]
-- tal que (union xs ys) es la unión de los conjuntos xs e ys. Por
-- ejemplo, 
--    union [3,2,5] [5,7,3,4]  ==  [3,2,5,7,4]
-- ---------------------------------------------------------------------

union :: Eq a => [a] -> [a] -> [a]
union xs ys = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Definir, por comprensión, la función
--    unionR :: Eq a => [a] -> [a] -> [a]
-- tal que (unionR xs ys) es la unión de los conjuntos xs e ys. Por
-- ejemplo, 
--    unionR [3,2,5] [5,7,3,4]  ==  [2,5,7,3,4]
-- ---------------------------------------------------------------------

unionR :: Eq a => [a] -> [a] -> [a]
unionR = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 3.3. Comprobar con QuickCheck que union y unionR son
-- equivalentes. 
-- ---------------------------------------------------------------------

-- La propiedad es
prop_union :: [Int] -> [Int] -> Bool
prop_union xs ys = undefined

-- La comprobación es

-- ---------------------------------------------------------------------
-- Ejercicio 4. Comprobar con QuickCheck que la unión es conmutativa.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_union_conmutativa :: [Int] -> [Int] -> Bool
prop_union_conmutativa xs ys = undefined

-- La comprobación es

-- ---------------------------------------------------------------------
-- Ejercicio 5.1. Definir, por comprensión, la función
--    interseccion :: Eq a => [a] -> [a] -> [a]
-- tal que (interseccion xs ys) es la intersección de xs e ys. Por
-- ejemplo, 
--    interseccion [3,2,5] [5,7,3,4]  ==  [3,5]
--    interseccion [3,2,5] [9,7,6,4]  ==  []
-- ---------------------------------------------------------------------

-- guache
interseccion :: Eq a => [a] -> [a] -> [a]
interseccion xs ys = [x | x <- xs, elem x ys]

-- ---------------------------------------------------------------------
-- Ejercicio 5.2. Definir, por recursión, la función
--    interseccionR :: Eq a => [a] -> [a] -> [a]
-- tal que (interseccionR xs ys) es la intersección de xs e ys. Por
-- ejemplo, 
--    interseccionR [3,2,5] [5,7,3,4]  ==  [3,5]
--    interseccionR [3,2,5] [9,7,6,4]  ==  []
-- ---------------------------------------------------------------------

-- guache
interseccionR :: Eq a => [a] -> [a] -> [a]
interseccionR _ [] = []
interseccionR [] _ = []
interseccionR (x:xs) ys | elem x ys = x : interseccionR xs ys
                        | otherwise = interseccionR xs ys

-- Comentario: La definición anterior se puede simplificar.

-- ---------------------------------------------------------------------
-- Ejercicio 5.3. Comprobar con QuickCheck que interseccion e
-- interseccionR son equivalentes.
-- ---------------------------------------------------------------------

-- guache

-- La propiedad es
prop_interseccion :: [Int] -> [Int] -> Bool
prop_interseccion xs ys = interseccionR xs ys == interseccion xs ys

-- La comprobación es
--    *Main Data.List> quickCheck prop_interseccion
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 6. Comprobar con QuickCheck si se cumple la siguiente
-- propiedad 
--    A ∪ (B ∩ C) = (A ∪ B) ∩ C
-- donde se considera la igualdad como conjuntos. En el caso de que no
-- se cumpla verificar el contraejemplo calculado por QuickCheck.
-- ---------------------------------------------------------------------

prop_union_interseccion :: [Int] -> [Int] -> [Int] -> Bool
prop_union_interseccion xs ys zs = undefined

-- La comprobación es 

-- ---------------------------------------------------------------------
-- Ejercicio 7.1. Definir, por comprensión, la función
--    diferencia :: Eq a => [a] -> [a] -> [a]
-- tal que (diferencia xs ys) es la diferencia entre los conjuntos xs e
-- ys; es decir, la lista de los elementos que sólo pertenecen a xs. Por
-- ejemplo,  
--    diferencia [3,2,5,6] [5,7,3,4]  ==  [2,6]
--    diferencia [3,2,5] [5,7,3,2]    ==  []
-- ---------------------------------------------------------------------

-- guache
diferencia :: Eq a => [a] -> [a] -> [a]
diferencia xs ys = [x | x <- xs, notElem x ys]

-- ---------------------------------------------------------------------
-- Ejercicio 7.2. Definir, por recursión, la función
--    diferenciaR :: Eq a => [a] -> [a] -> [a]
-- tal que (diferenciaR xs ys) es la diferencia entre los conjuntos xs e
-- ys; es decir, la lista de los elementos que sólo pertenecen a xs. Por
-- ejemplo,  
--    diferenciaR [3,2,5,6] [5,7,3,4]  ==  [2,6]
--    diferenciaR [3,2,5] [5,7,3,2]    ==  []
-- ---------------------------------------------------------------------

-- guache
diferenciaR :: Eq a => [a] -> [a] -> [a]
diferenciaR [] _  = []
diferenciaR xs [] = xs
diferenciaR (x:xs) ys | notElem x ys = x : diferenciaR  xs ys
                      | otherwise    = diferenciaR  xs ys

-- Comentario: La definición anterior se puede simplificar.

-- ---------------------------------------------------------------------
-- Ejercicio 7.3. Comprobar con QuickCheck que diferencia y diferenciaR 
-- son equivalentes.
-- ---------------------------------------------------------------------

-- guache

-- La propiedad es
prop_diferencia :: [Int] -> [Int] -> Bool
prop_diferencia xs ys = diferenciaR xs ys == diferencia xs ys

-- La comprobación es
--    *Main> quickCheck prop_diferencia
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 8. Comprobar con QuickCheck si la diferencia es
-- conmutativa. 
-- ---------------------------------------------------------------------

-- guache
prop_diferencia_conmutativa :: [Int] -> [Int] -> Bool
prop_diferencia_conmutativa xs ys = diferencia xs ys == diferencia ys xs

-- La comprobación es
--    *Main Data.List> quickCheck prop_diferencia_conmutativa
--    *** Failed! Falsifiable (after 2 tests): 
--    []
--    [0]

-- ---------------------------------------------------------------------
-- Ejercicio 9. Comprobar con QuickCheck si se cumple la siguiente
-- propiedad: A \ B ⊂ A
-- ---------------------------------------------------------------------

-- guache

-- La propiedad es
prop_diferencia_subconjunto :: [Int] -> [Int] -> Bool
prop_diferencia_subconjunto xs ys = subconjunto (diferencia xs ys) xs

-- La comprobación es
--    > quickCheck prop_diferencia_subconjunto
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 10. Comprobar con QuickCheck si se cumple la siguiente
-- propiedad: (A \ B) ∩ B = ∅.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_diferencia_interseccion :: [Int] -> [Int] -> Bool
prop_diferencia_interseccion xs ys = 
    interseccion (diferencia xs ys) ys == []
                
-- La comprobación es
--    *Main> quickCheck prop_diferencia_interseccion
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 11.1. Definir, por comprensión, la función
--    producto :: Eq a => [a] -> [a] -> [(a,a)]
-- tal que (producto xs ys) es el producto cartesiano de xs e ys. Por
-- ejemplo, 
--   producto [1,3] [2,4] == [(1,2),(1,4),(3,2),(3,4)]
-- ---------------------------------------------------------------------

-- guache
producto :: Eq a => [a] -> [a] -> [(a,a)]
producto xs ys = [(a,b) |a <- xs, b <- ys]

-- ---------------------------------------------------------------------
-- Ejercicio 11.2. Definir, por recursión, la función
--    productoR :: Eq a => [a] -> [a] -> [(a,a)]
-- tal que (productoR xs ys) es el producto cartesiano de xs e ys. Por
-- ejemplo, 
--   productoR [1,3] [2,4] == [(1,2),(1,4),(3,2),(3,4)]
-- ---------------------------------------------------------------------

-- guache
productoR :: Eq a => [a] -> [a] -> [(a,a)]
productoR = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 11.3. Comprobar con QuickCheck que producto y productoR 
-- son equivalentes.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_producto :: [Int] -> [Int] -> Bool
prop_producto xs ys = undefined

-- La comprobación es

-- ---------------------------------------------------------------------
-- Ejercicio 12. Comprobar con QuickCheck que el número de elementos
-- de (producto xs ys) es el producto del número de elementos de xs y de
-- ys. 
-- ---------------------------------------------------------------------

-- La propiedad es
prop_elementos_producto :: [Int] -> [Int] -> Bool
prop_elementos_producto xs ys = undefined

-- La comprobación es

-- ---------------------------------------------------------------------
-- Ejercicio 13. Definir la función 
--    subconjuntos :: [a] -> [[a]]
-- tal que (subconjuntos xs) es la lista de las subconjuntos de la lista
-- xs. Por ejemplo, 
--    ghci> subconjuntos [2,3,4]
--    [[2,3,4],[2,3],[2,4],[2],[3,4],[3],[4],[]]
--    ghci> subconjuntos [1,2,3,4]
--    [[1,2,3,4],[1,2,3],[1,2,4],[1,2],[1,3,4],[1,3],[1,4],[1],
--       [2,3,4],  [2,3],  [2,4],  [2],  [3,4],  [3],  [4], []]
-- ---------------------------------------------------------------------

subconjuntos :: [a] -> [[a]]
subconjuntos = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 14. Comprobar con QuickChek que el número de elementos de
-- (subconjuntos xs) es 2 elevado al número de elementos de xs.
--
-- Nota. Al hacer la comprobación limitar el tamaño de las pruebas como
-- se indica a continuación
--    quickCheckWith (stdArgs {maxSize=7}) prop_subconjuntos
-- ---------------------------------------------------------------------

-- La propiedad es
prop_subconjuntos :: [Int] -> Bool
prop_subconjuntos xs = undefined

-- La comprobación es
