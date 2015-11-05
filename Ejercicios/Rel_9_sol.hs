-- I1M 2015-16: Rel_9_sol.hs (31 de Octubre de 2015)
-- Operaciones conjuntistas con listas.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- En estas relación se definen operaciones conjuntistas sobre listas.

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

subconjunto :: Eq a => [a] -> [a] -> Bool
subconjunto xs ys = 
    [x | x <- xs, x `elem` ys] == xs

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Definir, por recursión, la función
--    subconjuntoR :: Eq a => [a] -> [a] -> Bool
-- tal que (subconjuntoR xs ys) se verifica si xs es un subconjunto de
-- ys; es decir, si todos los elementos de xs pertenecen a ys. Por
-- ejemplo, 
--    subconjuntoR [3,2,3] [2,5,3,5]  ==  True
--    subconjuntoR [3,2,3] [2,5,6,5]  ==  False
-- ---------------------------------------------------------------------

subconjuntoR :: Eq a => [a] -> [a] -> Bool
subconjuntoR [] _      = True
subconjuntoR (x:xs) ys = x `elem` ys && subconjuntoR xs ys

-- ---------------------------------------------------------------------
-- Ejercicio 1.3. Comprobar con QuickCheck que las definiciones
-- subconjunto y subconjuntoR son equivalentes.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_subconjuntoR :: [Int] -> [Int] -> Bool
prop_subconjuntoR xs ys =
    subconjuntoR xs ys == subconjunto xs ys

-- La comprobación es
--    λ> quickCheck prop_subconjuntoR
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 1.4. Definir, mediante all, la función 
--    subconjuntoA :: Eq a => [a] -> [a] -> Bool
-- tal que (subconjuntoA xs ys) se verifica si xs es un subconjunto de
-- ys. Por ejemplo,
--    subconjuntoA [1,3,2,3] [1,2,3]  ==  True
--    subconjuntoA [1,3,4,3] [1,2,3]  ==  False
-- ---------------------------------------------------------------------
 
subconjuntoA :: Eq a => [a] -> [a] -> Bool
subconjuntoA xs ys = all (`elem` ys) xs
 
-- ---------------------------------------------------------------------
-- Ejercicio 1.5. Comprobar con QuickCheck que las funciones subconjunto
-- y subconjuntoA son equivalentes.
-- ---------------------------------------------------------------------
 
-- La propiedad es
prop_subconjuntoA :: [Int] -> [Int] -> Bool
prop_subconjuntoA xs ys =
    subconjunto xs ys == subconjuntoA xs ys  
 
-- La comprobación es
--    λ> quickCheck prop_subconjuntoA
--    OK, passed 100 tests.
 
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
iguales xs ys =
    subconjunto xs ys && subconjunto ys xs

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Definir, por comprensión, la función
--    union :: Eq a => [a] -> [a] -> [a]
-- tal que (union xs ys) es la unión de los conjuntos xs e ys. Por
-- ejemplo, 
--    union [3,2,5] [5,7,3,4]  ==  [3,2,5,7,4]
-- ---------------------------------------------------------------------

union :: Eq a => [a] -> [a] -> [a]
union xs ys = xs ++ [y | y <- ys, y `notElem` xs]

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Definir, por recursión, la función
--    unionR :: Eq a => [a] -> [a] -> [a]
-- tal que (unionR xs ys) es la unión de los conjuntos xs e ys. Por
-- ejemplo, 
--    unionR [3,2,5] [5,7,3,4]  ==  [2,5,7,3,4]
-- ---------------------------------------------------------------------

unionR :: Eq a => [a] -> [a] -> [a]
unionR []     ys = ys
unionR (x:xs) ys | x `elem` ys = union xs ys
                  | otherwise   = x : union xs ys

-- ---------------------------------------------------------------------
-- Ejercicio 3.3. Comprobar con QuickCheck que union y unionR son
-- equivalentes. 
-- ---------------------------------------------------------------------

-- La propiedad es
prop_union :: [Int] -> [Int] -> Bool
prop_union xs ys =
    union xs ys `iguales` unionR xs ys

-- La comprobación es
--    λ> quickCheck prop_union
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Nota. En los ejercicios de comprobación de propiedades, cuando se
-- trata con igualdades se usa la igualdad conjuntista (definida por la
-- función iguales) en lugar de la igualdad de lista (definida por ==)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 4. Comprobar con QuickCheck que la unión es conmutativa.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_union_conmutativa :: [Int] -> [Int] -> Bool
prop_union_conmutativa xs ys =
    union xs ys `iguales` union ys xs

-- La comprobación es
--    λ> quickCheck prop_union_conmutativa
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 5.1. Definir, por comprensión, la función
--    interseccion :: Eq a => [a] -> [a] -> [a]
-- tal que (interseccion xs ys) es la intersección de xs e ys. Por
-- ejemplo, 
--    interseccion [3,2,5] [5,7,3,4]  ==  [3,5]
--    interseccion [3,2,5] [9,7,6,4]  ==  []
-- ---------------------------------------------------------------------

interseccion :: Eq a => [a] -> [a] -> [a]
interseccion xs ys =
    [x | x <- xs, x `elem` ys]

-- ---------------------------------------------------------------------
-- Ejercicio 5.2. Definir, por recursión, la función
--    interseccionR :: Eq a => [a] -> [a] -> [a]
-- tal que (interseccionR xs ys) es la intersección de xs e ys. Por
-- ejemplo, 
--    interseccionR [3,2,5] [5,7,3,4]  ==  [3,5]
--    interseccionR [3,2,5] [9,7,6,4]  ==  []
-- ---------------------------------------------------------------------

interseccionR :: Eq a => [a] -> [a] -> [a]
interseccionR []     ys = []
interseccionR (x:xs) ys | x `elem` ys = x : interseccionR xs ys
                        | otherwise   = interseccionR xs ys

-- ---------------------------------------------------------------------
-- Ejercicio 5.3. Comprobar con QuickCheck que interseccion e
-- interseccionR son equivalentes.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_interseccion :: [Int] -> [Int] -> Bool
prop_interseccion xs ys =
    interseccion xs ys `iguales` interseccionR xs ys

-- La comprobación es
--    λ> quickCheck prop_interseccion
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 6. Comprobar con QuickCheck si se cumple la siguiente
-- propiedad 
--    A ∪ (B ∩ C) = (A ∪ B) ∩ C
-- donde se considera la igualdad como conjuntos. En el caso de que no
-- se cumpla verificar el contraejemplo calculado por QuickCheck.
-- ---------------------------------------------------------------------

prop_union_interseccion :: [Int] -> [Int] -> [Int] -> Bool
prop_union_interseccion xs ys zs =
    iguales (union xs (interseccion ys zs))
            (interseccion (union xs ys) zs)

-- La comprobación es 
--    λ> quickCheck prop_union_interseccion
--    *** Failed! Falsifiable (after 3 tests and 2 shrinks): 
--    [0]
--    []
--    []
-- 
-- Por tanto, la propiedad no se cumple y un contraejemplo es 
--    A = [0], B = [] y C = []
-- ya que entonces,
--    A ∪ (B ∩ C) = [0] ∪ ([] ∩ []) = [0] ∪ [] = [0] 
--    (A ∪ B) ∩ C = ([0] ∪ []) ∩ [] = [0] ∩ [] = []

-- ---------------------------------------------------------------------
-- Ejercicio 7.1. Definir, por comprensión, la función
--    diferencia :: Eq a => [a] -> [a] -> [a]
-- tal que (diferencia xs ys) es la diferencia entre los conjuntos xs e
-- ys; es decir, la lista de los elementos que sólo pertenecen a xs. Por
-- ejemplo,  
--    diferencia [3,2,5,6] [5,7,3,4]  ==  [2,6]
--    diferencia [3,2,5] [5,7,3,2]    ==  []
-- ---------------------------------------------------------------------

diferencia :: Eq a => [a] -> [a] -> [a]
diferencia xs ys = [x | x <- xs, x `notElem` ys]

-- ---------------------------------------------------------------------
-- Ejercicio 7.2. Definir, por recursión, la función
--    diferenciaR :: Eq a => [a] -> [a] -> [a]
-- tal que (diferenciaR xs ys) es la diferencia entre los conjuntos xs e
-- ys; es decir, la lista de los elementos que sólo pertenecen a xs. Por
-- ejemplo,  
--    diferenciaR [3,2,5,6] [5,7,3,4]  ==  [2,6]
--    diferenciaR [3,2,5] [5,7,3,2]    ==  []
-- ---------------------------------------------------------------------

diferenciaR :: Eq a => [a] -> [a] -> [a]
diferenciaR [] ys = []
diferenciaR (x:xs) ys | x `elem` ys = diferenciaR xs ys
                      | otherwise   = x : diferenciaR xs ys

-- ---------------------------------------------------------------------
-- Ejercicio 7.3. Comprobar con QuickCheck que diferencia y diferenciaR 
-- son equivalentes.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_diferencia :: [Int] -> [Int] -> Bool
prop_diferencia xs ys =
    diferencia xs ys `iguales` diferenciaR xs ys

-- La comprobación es
--    λ> quickCheck prop_diferencia
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 8. Comprobar con QuickCheck si la diferencia es
-- conmutativa. 
-- ---------------------------------------------------------------------

prop_diferencia_conmutativa :: [Int] -> [Int] -> Bool
prop_diferencia_conmutativa xs ys =
    iguales (diferencia xs ys) (diferencia ys xs)

-- La comprobación es
--    λ> quickCheck prop_diferencia_conmutativa
--    *** Failed! Falsifiable (after 2 tests and 2 shrinks): 
--    [0]
--    []
-- que es un contraejemplo, ya que
--    [0] - [] = [0]
--    [] - [0] = []

-- ---------------------------------------------------------------------
-- Ejercicio 9. Comprobar con QuickCheck si se cumple la siguiente
-- propiedad: A \ B ⊂ A
-- ---------------------------------------------------------------------

-- La propiedad es
prop_diferencia_subconjunto :: [Int] -> [Int] -> Bool
prop_diferencia_subconjunto xs ys =
    subconjunto (diferencia xs ys) xs

-- La comprobación es
--    λ> quickCheck prop_diferencia_subconjunto
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
--    λ> quickCheck prop_diferencia_interseccion
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 11.1. Definir, por comprensión, la función
--    producto :: [a] -> [a] -> [(a,a)]
-- tal que (producto xs ys) es el producto cartesiano de xs e ys. Por
-- ejemplo, 
--   producto [1,3] [2,4] == [(1,2),(1,4),(3,2),(3,4)]
-- ---------------------------------------------------------------------

producto :: [a] -> [a] -> [(a,a)]
producto xs ys = [(x,y) | x <- xs, y <- ys]

-- ---------------------------------------------------------------------
-- Ejercicio 11.2. Definir, por recursión, la función
--    productoR :: [a] -> [a] -> [(a,a)]
-- tal que (productoR xs ys) es el producto cartesiano de xs e ys. Por
-- ejemplo, 
--   productoR [1,3] [2,4] == [(1,2),(1,4),(3,2),(3,4)]
-- ---------------------------------------------------------------------

productoR :: [a] -> [a] -> [(a,a)]
productoR []     _  = []
productoR (x:xs) ys = [(x,y) | y <- ys] ++ productoR xs ys

-- ---------------------------------------------------------------------
-- Ejercicio 11.3. Comprobar con QuickCheck que producto y productoR 
-- son equivalentes.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_producto :: [Int] -> [Int] -> Bool
prop_producto xs ys =
    producto xs ys `iguales` productoR xs ys

-- La comprobación es
--    λ> quickCheck prop_producto
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 12. Comprobar con QuickCheck que el número de elementos
-- de (producto xs ys) es el producto del número de elementos de xs y de
-- ys. 
-- ---------------------------------------------------------------------

-- La propiedad es
prop_elementos_producto :: [Int] -> [Int] -> Bool
prop_elementos_producto xs ys =
    length (producto xs ys) == length xs * length ys

-- La comprobación es
--    λ> quickCheck prop_elementos_producto
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

subconjuntos :: [a] -> [[a]]
subconjuntos []     = [[]]
subconjuntos (x:xs) = [x:ys | ys <- sub] ++ sub
    where sub = subconjuntos xs  

-- Cambiando la comprensión por map se obtiene
subconjuntos' :: [a] -> [[a]]
subconjuntos' []     = [[]]
subconjuntos' (x:xs) = sub ++ map (x:) sub
    where sub = subconjuntos' xs  

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
prop_subconjuntos xs =
    length (subconjuntos xs) == 2 ^ length xs

-- La comprobación es
--    λ> quickCheckWith (stdArgs {maxSize=7}) prop_subconjuntos
--    +++ OK, passed 100 tests.
