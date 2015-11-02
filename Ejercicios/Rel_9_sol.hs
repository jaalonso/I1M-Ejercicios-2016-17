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

-- guache paocabper erisancha alvalvdom1 ivaruicam enrvalmor manvermor
-- pabmorgar 
subconjunto :: Eq a => [a] -> [a] -> Bool
subconjunto xs ys = and [elem x ys | x <- xs]

-- guache marcamde3 manpende marvilmor juamorrom1 josllagam anaagusil
-- isrbelnun javoliher 
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

-- guache paocabper rubvilval isrbelnun javoliher
subconjuntoR :: Eq a => [a] -> [a] -> Bool
subconjuntoR [] _      = True
subconjuntoR (x:xs) ys = elem x ys && subconjunto xs ys

-- erisancha marcamde3 alvalvdom1 anaagusil manvermor pabmorgar
subconjuntoR2 :: Eq a => [a] -> [a] -> Bool
subconjuntoR2 [] _      = True
subconjuntoR2 xs []     = False
subconjuntoR2 (x:xs) ys = elem x ys && subconjuntoR2 xs ys

-- Comentario: La definición anterior se puede simplificar.

-- manpende ivaruicam marvilmor josllagam enrvalmor
subconjuntoR3 :: Eq a => [a] -> [a] -> Bool
subconjuntoR3 [] _ = True
subconjuntoR3 (x:xs) ys | elem x ys = subconjuntoR3 xs ys
                        | otherwise = False

-- juamorrom1
subconjuntoR4 :: Eq a => [a] -> [a] -> Bool
subconjuntoR4 [] _      = True
subconjuntoR4 (x:xs) ys = any (== x) ys && subconjuntoR xs ys

-- ---------------------------------------------------------------------
-- Ejercicio 1.3. Comprobar con QuickCheck que las definiciones
-- subconjunto y subconjuntoR son equivalentes.
-- ---------------------------------------------------------------------

-- guache paocabper erisancha marcamde3 manpende alvalvdom1 ivaruicam
-- marvilmor juamorrom1 josllagam enrvalmor anaagusil manvermor
-- pabmorgar isrbelnun javoliher rubvilval

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

-- guache paocabper erisancha marcamde3 manpende alvalvdom1 marvilmor
-- juamorrom1 enrvalmor anaagusil josllagam pabmorgar isrbelnun
-- javoliher rubvilval ivaruicam

subconjuntoA :: Eq a => [a] -> [a] -> Bool
subconjuntoA xs ys =  all (`elem` ys) xs

-- ---------------------------------------------------------------------
-- Ejercicio 1.5. Comprobar con QuickCheck que las funciones subconjunto
-- y subconjuntoA son equivalentes.
-- ---------------------------------------------------------------------

-- guache paocabper erisancha manpende alvalvdom1 marvilmor juamorrom1
-- enrvalmor marcamde3 anaagusil josllagam pabmorgar isrbelnun javoliher
-- rubvilval ivaruicam

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

-- guache paocabper erisancha manpende alvalvdom1 marvilmor juamorrom1
-- enrvalmor anaagusil manvermor pabmorgar josllagam isrbelnun javoliher
-- ivaruicam

iguales :: Eq a => [a] -> [a] -> Bool
iguales xs ys = subconjunto xs ys && subconjunto ys xs

-- marcamde3
iguales2 :: Eq a => [a] -> [a] -> Bool
iguales2 xs ys = all (`elem` xs) ys && all (`elem` ys) xs

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Definir, por comprensión, la función
--    union :: Eq a => [a] -> [a] -> [a]
-- tal que (union xs ys) es la unión de los conjuntos xs e ys. Por
-- ejemplo, 
--    union [3,2,5] [5,7,3,4]  ==  [3,2,5,7,4]
-- ---------------------------------------------------------------------

-- guache paocabper erisancha marcamde3 enrvalmor anaagusil manvermor
-- pabmorgar josllagam isrbelnun alvalvdom1 javoliher rubvilval ivaruicam

union :: Eq a => [a] -> [a] -> [a]
union xs ys = xs ++ [x | x <- ys, notElem x xs] 

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Definir, por recursión, la función
--    unionR :: Eq a => [a] -> [a] -> [a]
-- tal que (unionR xs ys) es la unión de los conjuntos xs e ys. Por
-- ejemplo, 
--    unionR [3,2,5] [5,7,3,4]  ==  [2,5,7,3,4]
-- ---------------------------------------------------------------------

-- juamorrom1
unionR :: Eq a => [a] -> [a] -> [a]
unionR [] ys = ys
unionR xs [] = xs
unionR (x:xs) ys | all (/= x) ys = x: unionR xs ys
                 | otherwise     = unionR xs ys

-- Comentario: La definición anterior se puede simplificar.

-- manvermor
unionR2 :: Eq a => [a] -> [a] -> [a]
unionR2 [] ys = ys
unionR2 xs [] = xs
unionR2 (x:xs) ys | notElem x ys = [x] ++ unionR2 xs ys
                  | otherwise    = unionR2 xs ys

-- Comentario: La definición anterior se puede mejorar.

-- josllagam isrbelnun alvalvdom1 javoliher rubvilval ivaruicam

unionR3 :: Eq a => [a] -> [a] -> [a]
unionR3 [] ys = ys
unionR3 xs [] = xs
unionR3 (x:xs) ys | elem x ys = unionR xs ys 
                  | otherwise = x : unionR xs ys

-- Comentario: La definición anterior se puede simplificar.

-- ---------------------------------------------------------------------
-- Ejercicio 3.3. Comprobar con QuickCheck que union y unionR son
-- equivalentes. 
-- ---------------------------------------------------------------------

-- guache paocabper manpende marvilmor anaagusil manvermor juamorrom1
-- enrvalmor josllagam isrbelnun alvalvdom1 javoliher rubvilval
-- ivaruicam 

-- La propiedad es
prop_union :: [Int] -> [Int] -> Bool
prop_union xs ys = iguales (unionR xs ys)(union xs ys)

-- La comprobación es
--    *Main>  quickCheck prop_union
--    +++ OK, passed 100 tests.

-- erisancha pabmorgar
prop_union3 :: [Int] -> [Int] -> Bool
prop_union3 xs ys = union xs ys `iguales` unionR xs ys

-- La comprobación es
--    *Main> quickCheck prop_union
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Nota. En los ejercicios de comprobación de propiedades, cuando se
-- trata con igualdades se usa la igualdad conjuntista (definida por la
-- función iguales) en lugar de la igualdad de lista (definida por ==)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 4. Comprobar con QuickCheck que la unión es conmutativa.
-- ---------------------------------------------------------------------
 
-- guache paocabper manpende enrvalmor anaagusil manvermor juamorrom1
-- isrbelnun alvalvdom1 javoliher rubvilval ivaruicam

-- La propiedad es
prop_union_conmutativa :: [Int] -> [Int] -> Bool
prop_union_conmutativa xs ys = iguales (union xs ys) (union ys xs)

-- La comprobación es
--  *Main> quickCheck prop_union_conmutativa
--  +++ OK, passed 100 tests.

-- erisancha pabmorgar josllagam
prop_union_conmutativa2 :: [Int] -> [Int] -> Bool
prop_union_conmutativa2 xs ys = union xs ys `iguales` union ys xs

-- ---------------------------------------------------------------------
-- Ejercicio 5.1. Definir, por comprensión, la función
--    interseccion :: Eq a => [a] -> [a] -> [a]
-- tal que (interseccion xs ys) es la intersección de xs e ys. Por
-- ejemplo, 
--    interseccion [3,2,5] [5,7,3,4]  ==  [3,5]
--    interseccion [3,2,5] [9,7,6,4]  ==  []
-- ---------------------------------------------------------------------

-- guache paocabper erisancha manpende alvalvdom1 marvilmor enrvalmor
-- pabmorgar anaagusil manvermor josllagam isrbelnun javoliher rubvilval
-- ivaruicam  

interseccion :: Eq a => [a] -> [a] -> [a]
interseccion xs ys = [x | x <- xs, elem x ys]

-- juamorrom1
interseccion2 :: Eq a => [a] -> [a] -> [a]
interseccion2 xs ys = [x | x <- xs, any (==x) ys]

-- Comentario: La definición anterior se puede simplificar.

-- ---------------------------------------------------------------------
-- Ejercicio 5.2. Definir, por recursión, la función
--    interseccionR :: Eq a => [a] -> [a] -> [a]
-- tal que (interseccionR xs ys) es la intersección de xs e ys. Por
-- ejemplo, 
--    interseccionR [3,2,5] [5,7,3,4]  ==  [3,5]
--    interseccionR [3,2,5] [9,7,6,4]  ==  []
-- ---------------------------------------------------------------------

-- guache erisancha manpende enrvalmor anaagusil manvermor juamorrom1
-- pabmorgar josllagam alvalvdom1 rubvilval
interseccionR :: Eq a => [a] -> [a] -> [a]
interseccionR (x:xs) ys | elem x ys = x : interseccionR xs ys
                        | otherwise = interseccionR xs ys
interseccionR _ _ = []

-- isrbelnun javoliher ivaruicam
interseccionR2 :: Eq a => [a] -> [a] -> [a]
interseccionR2 xs [] = []
interseccionR2 [] xs = []
interseccionR2 (x:xs) ys | elem x ys = x : interseccion xs ys
                         | otherwise = interseccion xs ys

-- Comentario: La definición anterior se puede simplificar.

-- ---------------------------------------------------------------------
-- Ejercicio 5.3. Comprobar con QuickCheck que interseccion e
-- interseccionR son equivalentes.
-- ---------------------------------------------------------------------

-- guache manpende enrvalmor anaagusil manvermor juamorrom1
-- isrbelnun alvalvdom1 javoliher rubvilval ivaruicam

-- La propiedad es
prop_interseccion :: [Int] -> [Int] -> Bool
prop_interseccion xs ys = 
    iguales (interseccionR xs ys) (interseccion xs ys)

-- La comprobación es
--    > quickCheck prop_interseccion
--    +++ OK, passed 100 tests.
 
-- erisancha pabmorgar josllagam
prop_interseccion2 :: [Int] -> [Int] -> Bool
prop_interseccion2 xs ys = 
    interseccion xs ys `iguales` interseccionR xs ys

-- ---------------------------------------------------------------------
-- Ejercicio 6. Comprobar con QuickCheck si se cumple la siguiente
-- propiedad 
--    A ∪ (B ∩ C) = (A ∪ B) ∩ C
-- donde se considera la igualdad como conjuntos. En el caso de que no
-- se cumpla verificar el contraejemplo calculado por QuickCheck.
-- ---------------------------------------------------------------------

-- guache erisancha manpende enrvalmor anaagusil manvermor juamorrom1
-- pabmorgar josllagam isrbelnun alvalvdom1 javoliher rubvilval
-- ivaruicam 

prop_union_interseccion :: [Int] -> [Int] -> [Int] -> Bool
prop_union_interseccion xs ys zs = 
    iguales (union xs (interseccion ys zs)) 
            (interseccion zs (union xs ys))

-- La comprobación es 
--    *Main> quickCheck prop_union_interseccion
--    *** Failed! Falsifiable (after 2 tests): 
--    [0]
--    []
--    []
-- 
-- Un contraejemplo es A = [0]  B = C = []
--    A ∪ (B ∩ C) = [0] ∪ ([] ∩ []) = [0]  
--    (A ∪ B) ∩ C = ([0] ∪ []) ∩ [] = [] 
--    [0] /= []    
-- entonces hemos probado que esa propiedad no se cumple en general.

-- ---------------------------------------------------------------------
-- Ejercicio 7.1. Definir, por comprensión, la función
--    diferencia :: Eq a => [a] -> [a] -> [a]
-- tal que (diferencia xs ys) es la diferencia entre los conjuntos xs e
-- ys; es decir, la lista de los elementos que sólo pertenecen a xs. Por
-- ejemplo,  
--    diferencia [3,2,5,6] [5,7,3,4]  ==  [2,6]
--    diferencia [3,2,5] [5,7,3,2]    ==  []
-- ---------------------------------------------------------------------

-- guache erisancha manpende alvalvdom1 enrvalmor anaagusil manvermor
-- juamorrom1 pabmorgar josllagam isrbelnun javoliher rubvilval
-- ivaruicam 
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

-- guache enrvalmor anaagusil pabmorgar
diferenciaR :: Eq a => [a] -> [a] -> [a]
diferenciaR (x:xs) ys | notElem x ys = x : diferenciaR  xs ys
                      | otherwise    = diferenciaR  xs ys
diferenciaR x _ = x

-- erisancha manpende alvalvdom1 manvermor juamorrom1 josllagam
-- isrbelnun javoliher
diferenciaR2 :: Eq a => [a] -> [a] -> [a]
diferenciaR2 (x:xs) ys | notElem x ys = x : diferenciaR2  xs ys
                       | otherwise    = diferenciaR2  xs ys
diferenciaR2 xs [] = xs
diferenciaR2 [] ys = []

-- Comentario: La definición anterior se puede simplificar.

-- rubvilval ivaruicam
diferenciaR3 :: Eq a => [a] -> [a] -> [a]
diferenciaR3 [] _ = []
diferenciaR3 (x:xs) ys | elem x ys = diferenciaR xs ys
                       | otherwise = x : diferenciaR xs ys

-- ---------------------------------------------------------------------
-- Ejercicio 7.3. Comprobar con QuickCheck que diferencia y diferenciaR 
-- son equivalentes.
-- ---------------------------------------------------------------------

-- guache manpende alvalvdom1 enrvalmor anaagusil manvermor juamorrom1 
-- josllagam isrbelnun rubvilval ivaruicam

-- La propiedad es
prop_diferencia :: [Int] -> [Int] -> Bool
prop_diferencia xs ys = iguales (diferenciaR xs ys) (diferencia xs ys)

-- La comprobación es
--    *Main> quickCheck prop_diferencia
--    +++ OK, passed 100 tests.

-- erisancha pabmorgar
prop_diferencia2 :: [Int] -> [Int] -> Bool
prop_diferencia2 xs ys = diferencia xs ys `iguales` diferenciaR xs ys

-- ---------------------------------------------------------------------
-- Ejercicio 8. Comprobar con QuickCheck si la diferencia es
-- conmutativa. 
-- ---------------------------------------------------------------------

-- guache manpende alvalvdom1 enrvalmor anaagusil manvermor juamorrom1
-- isrbelnun javoliher rubvilval ivaruicam

prop_diferencia_conmutativa :: [Int] -> [Int] -> Bool
prop_diferencia_conmutativa xs ys =  
    iguales (diferencia xs ys) (diferencia ys xs)

-- La comprobación es
--    *Main Data.List> quickCheck prop_diferencia_conmutativa
--    *** Failed! Falsifiable (after 2 tests): 
--    []
--    [0]

-- erisancha pabmorgar josllagam
prop_diferencia_conmutativa2 :: [Int] -> [Int] -> Bool
prop_diferencia_conmutativa2 xs ys = 
    diferencia xs ys `iguales` diferencia ys xs

-- ---------------------------------------------------------------------
-- Ejercicio 9. Comprobar con QuickCheck si se cumple la siguiente
-- propiedad: A \ B ⊂ A
-- ---------------------------------------------------------------------

-- guache erisancha manpende alvalvdom1 enrvalmor anaagusil manvermor
-- juamorrom1 pabmorgar josllagam isrbelnun javoliher rubvilval ivaruicam

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

-- guache manpende alvalvdom1 enrvalmor anaagusil manvermor juamorrom1
-- pabmorgar josllagam isrbelnun javoliher rubvilval ivaruicam

-- La propiedad es
prop_diferencia_interseccion :: [Int] -> [Int] -> Bool
prop_diferencia_interseccion xs ys = 
     iguales (interseccion (diferencia xs ys) ys) []
                
-- La comprobación es
--    *Main> quickCheck prop_diferencia_interseccion
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 11.1. Definir, por comprensión, la función
--    producto :: [a] -> [a] -> [(a,a)]
-- tal que (producto xs ys) es el producto cartesiano de xs e ys. Por
-- ejemplo, 
--   producto [1,3] [2,4] == [(1,2),(1,4),(3,2),(3,4)]
-- ---------------------------------------------------------------------

-- guache erisancha manpende alvalvdom1 enrvalmor anaagusil manvermor
-- juamorrom1 pabmorgar isrbelnun josllagam javoliher rubvilval
-- ivaruicam 
producto :: [a] -> [a] -> [(a,a)]
producto xs ys = [(a,b) |a <- xs, b <- ys]

-- ---------------------------------------------------------------------
-- Ejercicio 11.2. Definir, por recursión, la función
--    productoR :: [a] -> [a] -> [(a,a)]
-- tal que (productoR xs ys) es el producto cartesiano de xs e ys. Por
-- ejemplo, 
--   productoR [1,3] [2,4] == [(1,2),(1,4),(3,2),(3,4)]
-- ---------------------------------------------------------------------

-- guache
productoR:: [a] -> [b] -> [(a, b)]
productoR (x:xs) ys = map (\y -> (x,y)) ys ++ productoR xs ys
productoR _      _  = []

-- guache
productoR2 :: [a] -> [a] -> [(a,a)]
productoR2 [] ys    = []
productoR2 (a:b) ys = (aux a ys) ++ (productoR2 b ys)

aux :: a -> [a] -> [(a,a)]
aux x []    = []
aux x (a:b) = (x,a) : (aux x b)

--  manvermor
productoR6 :: [a] -> [a] -> [(a,a)]
productoR6 [] ys     = []
productoR6 xs []     = []
productoR6 (x:xs) ys = zip (replicate (length ys) x) ys ++ productoR6 xs ys

-- isrbelnun
productoR3 :: Eq a => [a] -> [a] -> [(a,a)]
productoR3 [] _ = []
productoR3 _ [] = []
productoR3 (x:xs) (y:ys) = 
    (x,y) : (productoR [x] ys ++ productoR xs [y] ++ producto xs ys)

-- Comentario: La definición anterior se puede simplificar.

-- josllagam rubvilval
productoR7 :: Eq a => [a] -> [a] -> [(a,a)]
productoR7 [] _      = []
productoR7 (x:xs) ys = [(x,y) | y <- ys] ++ productoR xs ys

-- javoliher
productoR8 :: Eq a => [a] -> [a] -> [(a,a)]
productoR8 [] ys     = []
productoR8 xs []     = []
productoR8 (x:xs) ys = zip (repeat x) ys ++ productoR8 xs ys

-- Comentario: La definición anterior se puede simplificar.

-- ---------------------------------------------------------------------
-- Ejercicio 11.3. Comprobar con QuickCheck que producto y productoR 
-- son equivalentes.
-- ---------------------------------------------------------------------

-- guache erisancha manpende enrvalmor anaagusil manvermor juamorrom1 pabmorgar
-- josllagam rubvilval

-- La propiedad es
prop_producto :: [Int] -> [Int] -> Bool
prop_producto xs ys = productoR xs ys == producto xs ys

-- La comprobación es
--    *Main> quickCheck prop_producto
--    +++ OK, passed 100 tests.

-- isrbelnun
prop_producto2 :: [Int] -> [Int] -> Bool
prop_producto2 xs ys = iguales (producto xs ys) (productoR xs ys)

-- ---------------------------------------------------------------------
-- Ejercicio 12. Comprobar con QuickCheck que el número de elementos
-- de (producto xs ys) es el producto del número de elementos de xs y de
-- ys. 
-- ---------------------------------------------------------------------

-- guache alvalvdom1 enrvalmor anaagusil manvermor pabmorgar isrbelnun
-- josllagam javoliher rubvilval

-- La propiedad es
prop_elementos_producto :: [Int] -> [Int] -> Bool
prop_elementos_producto xs ys = 
    length (producto xs ys) == (length xs) * (length ys)

-- La comprobación es
--    *Main> quickCheck prop_elementos_producto 
--    +++ OK, passed 100 tests.

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

-- guache erisancha enrvalmor anaagusil isrbelnun josllagam rubvilval
subconjuntos :: [a] -> [[a]]
subconjuntos []     = [[]]
subconjuntos (x:xs) = subconjuntos xs ++ [x:ys | ys <- subconjuntos xs] 

-- Comentario: La definición anterior se puede mejorar.

-- guache 
subconjuntos2 :: [a] -> [[a]]
subconjuntos2 []  = [[]]
subconjuntos2 (x:xs) = subconjuntos2 xs ++ map (x:) (subconjuntos2 xs)

-- Comentario: La definición anterior se puede mejorar.

-- ---------------------------------------------------------------------
-- Ejercicio 14. Comprobar con QuickChek que el número de elementos de
-- (subconjuntos xs) es 2 elevado al número de elementos de xs.
--
-- Nota. Al hacer la comprobación limitar el tamaño de las pruebas como
-- se indica a continuación
--    quickCheckWith (stdArgs {maxSize=7}) prop_subconjuntos
-- ---------------------------------------------------------------------

-- guache erisancha enrvalmor anaagusil juamorrom1 pabmorgar isrbelnun
-- josllagam javoliher rubvilval

-- La propiedad es
prop_subconjuntos :: [Int] -> Bool
prop_subconjuntos xs =  length (subconjuntos xs) == 2^(length xs)

-- La comprobación es
--    *Main>  quickCheckWith (stdArgs {maxSize=7}) prop_subconjuntos
--    +++ OK, passed 100 tests.
