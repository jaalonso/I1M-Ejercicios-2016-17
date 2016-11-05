-- I1M 2016-17: Rel_7.hs (2 de noviembre de 2016)
-- Operaciones conjuntistas con listas.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- En estas relación se definen operaciones conjuntistas sobre
-- listas. Algunas de dichas operaciones están definidas en la librería
-- Data.List. El objetivo de esta relación es redefinirlas sin usar la
-- librería.

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Test.QuickCheck
import Data.List

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    subconjunto :: Eq a => [a] -> [a] -> Bool
-- tal que (subconjunto xs ys) se verifica si xs es un subconjunto de
-- ys; es decir, si todos los elementos de xs pertenecen a ys. Por
-- ejemplo, 
--    subconjunto [3,2,3] [2,5,3,5]  ==  True
--    subconjunto [3,2,3] [2,5,6,5]  ==  False
-- ---------------------------------------------------------------------

-- enrnarbej congomgom juaorture pabrabmon antmorper3 marjimcom
-- beagongon1 margarflo5 eledejim2 belbenzam
subconjunto :: Eq a => [a] -> [a] -> Bool
subconjunto xs ys = and [x `elem` ys | x<- xs]

-- enrnarbej eliguivil paumacpar manruiber natruipin
subconjunto2 :: Eq a => [a] -> [a] -> Bool
subconjunto2 xs ys = all ( `elem` ys) xs

-- enrnarbej congomgom albcercid
subconjunto3 :: Eq a => [a] -> [a] -> Bool
subconjunto3 [] _ = True
subconjunto3 (x:xs) ys = x `elem` ys && subconjunto3 xs ys

-- enrnarbej
subconjunto4 :: Eq a => [a] -> [a] -> Bool
subconjunto4 xs ys = foldl' (\x y -> x && y `elem` ys) True xs

-- pabrabmon cargonler roscargar juacasnie migibagar
subconjunto5 :: Eq a => [a] -> [a] -> Bool
subconjunto5 [] ys = True
subconjunto5 (x:xs) ys
  | elem x ys == True = subconjunto5 xs ys
  | otherwise         = False

-- Comentario: La definición anterior se puede simplificar.

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

-- enrnarbej eliguivil juaorture congomgom pabrabmon antmorper3 paumacpar 
-- marjimcom beagongon1 cargonler roscargar albcercid margarflo5 manruiber
-- juacasnie migibagar eledejim2 belbenzam natruipin
iguales :: Eq a => [a] -> [a] -> Bool
iguales xs ys = subconjunto xs ys && subconjunto ys xs

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Definir la función
--    union :: Eq a => [a] -> [a] -> [a]
-- tal que (union xs ys) es la unión de los conjuntos xs e ys. Por
-- ejemplo, 
--    union [3,2,5] [5,7,3,4]  ==  [3,2,5,7,4]
-- ---------------------------------------------------------------------

-- Nota (juaorture) la función unión ya existe en la librería Data.List

-- enrnarbej juaorture congomgom antmorper3 paumacpar beagongon1
-- cargonler roscargar margarflo5 manruiber juacasnie migibagar
-- eledejim2 natruipin
union1 :: Eq a => [a] -> [a] -> [a]
union1 xs ys = nub (xs ++ ys)

-- eliguivil
union2 :: Eq a => [a] -> [a] -> [a]
union2 xs ys = sinRepeticiones (xs ++ ys)

sinRepeticiones :: Eq a => [a] -> [a]
sinRepeticiones [] = []
sinRepeticiones (x:xs)
  | elem    x xs = sinRepeticiones xs
  | notElem x xs = x : sinRepeticiones xs

-- Comentario: La definición anterior se puede mejorar.

-- pabrabmon marjimcom belbenzam
union3 :: Eq a => [a] -> [a] -> [a]
union3 xs ys = [x | x <- xs, notElem x ys] ++ ys

-- albcercid
union4 :: Eq a => [a] -> [a] -> [a]
union4 xs ys = xs ++ unidos xs ys

unidos xs [] = []
unidos xs (y:ys) | y `elem` xs = unidos xs ys
                 | otherwise   = y : unidos xs ys

-- ---------------------------------------------------------------------
-- Nota. En los ejercicios de comprobación de propiedades, cuando se
-- trata con igualdades se usa la igualdad conjuntista (definida por la
-- función iguales) en lugar de la igualdad de lista (definida por ==)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Comprobar con QuickCheck que la unión es conmutativa.
-- ---------------------------------------------------------------------

-- enrnarbej eliguivil juaorture congomgom pabrabmon antmorper3 paumacpar 
-- marjimcom beagongon1 cargonler roscargar albcercid margarflo5 manruiber
-- juacasnie migibagar eledejim2 belbenzam natruipin
-- La propiedad es
prop_union_conmutativa :: [Int] -> [Int] -> Bool
prop_union_conmutativa xs ys = iguales (union1 xs ys) (union1 ys xs)

-- La comprobación es
-- Prelude> quickCheck prop_union_conmutativa
-- +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. Definir la función
--    interseccion :: Eq a => [a] -> [a] -> [a]
-- tal que (interseccion xs ys) es la intersección de xs e ys. Por
-- ejemplo, 
--    interseccion [3,2,5] [5,7,3,4]  ==  [3,5]
--    interseccion [3,2,5] [9,7,6,4]  ==  []
-- ---------------------------------------------------------------------

-- enrnarbej congomgom cargonler roscargar margarflo5 natruipin
interseccion :: Eq a => [a] -> [a] -> [a]
interseccion xs ys = nub [ x | x <- xs , elem x ys]

-- enrnarbej
interseccion2 :: Eq a => [a] -> [a] -> [a]
interseccion2 xs ys = filter (`elem` ys) (nub xs) 

-- eliguivil juacasnie
interseccion3 :: Eq a => [a] -> [a] -> [a]
interseccion3 xs ys =
  sinRepeticiones [x | x <- xs
                     , x <- ys
                     , elem x xs && elem x ys]

-- Comentario: La definición anterior se puede mejorar. Por ejemplo,
--    λ> sum (interseccion3 [1..500] [1..500])
--    125250
--    (3.22 secs, 90,254,096 bytes)
--    λ> sum (interseccion' [1..500] [1..500])
--    125250
--    (0.01 secs, 0 bytes)

-- pabrabmon antmorper3 beagongon1 albcercid manruiber belbenzam
-- eledejim2
interseccion5 :: Eq a => [a] -> [a] -> [a]
interseccion5 xs ys = [x | x <- xs , x `elem` ys]

-- paumacpar 
interseccion6 :: Eq a => [a] -> [a] -> [a]
interseccion6 xs ys = nub (filter (`elem` ys) xs) 

-- migibagar
interseccion7 :: Eq a => [a] -> [a] -> [a]
interseccion7 xs [] = []
interseccion7 [] xs = []
interseccion7 (x:xs) ys
  | elem x ys  =  x : interseccion7 xs ys
  | otherwise  =  interseccion7 xs ys

-- Comentario: La definición anterior se puede simplificar.

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Comprobar con QuickCheck si se cumple la siguiente
-- propiedad 
--    A ∪ (B ∩ C) = (A ∪ B) ∩ C
-- donde se considera la igualdad como conjuntos. En el caso de que no
-- se cumpla verificar el contraejemplo calculado por QuickCheck.
-- ---------------------------------------------------------------------

-- enrnarbej pabrabmon antmorper3 beagongon1 cargonler roscargar albcercid 
-- margarflo5 manruiber juacasnie migibagar belbenzam eledejim2 natruipin
prop_union_interseccion :: [Int] -> [Int] -> [Int] -> Bool
prop_union_interseccion xs ys zs =
  iguales (union1 xs (interseccion ys zs))
          (interseccion (union1 xs ys) zs)

-- eliguivil
prop_union_interseccion2 :: [Int] -> [Int] -> [Int] -> Bool
prop_union_interseccion2 xs ys zs =
  iguales (xs `union` (ys `interseccion` zs))
          ((xs `union` ys) `interseccion` zs)

-- ContraEjemplo: [0] [] []
-- union1 [0] (interseccion [] []) = [0]
-- interseccion (union [0] []) [] = []

-- juaorture paumacpar 
prop_union_interseccion3:: [Int] -> [Int] -> [Int] -> Bool
prop_union_interseccion3 xs ys zs =
  (xs `union` ( ys `interseccion` zs ) )
  `iguales` (( xs `union`ys ) `interseccion` zs)

-- La comprobación es
--    *Main> quickCheck prop_union_interseccion
--    *** Failed! Falsifiable (after 5 tests and 2 shrinks): 
--    [0]
--    []
--    []

-- ---------------------------------------------------------------------
-- Ejercicio 5.1. Definir la función
--    producto :: [a] -> [a] -> [(a,a)]
-- tal que (producto xs ys) es el producto cartesiano de xs e ys. Por
-- ejemplo, 
--   producto [1,3] [2,4] == [(1,2),(1,4),(3,2),(3,4)]
-- ---------------------------------------------------------------------

-- enrnarbej eliguivil juaorture congomgom pabrabmon antmorper3 paumacpar 
-- beagongon1 cargonler roscargar albcercid margarflo5 manruiber
-- juacasnie migibagar belbenzam eledejim2
producto :: [a] -> [a] -> [(a,a)]
producto xs ys = [(x,y) | x <- xs, y <- ys]

-- ---------------------------------------------------------------------
-- Ejercicio 5.2. Comprobar con QuickCheck que el número de elementos
-- de (producto xs ys) es el producto del número de elementos de xs y de
-- ys. 
-- ---------------------------------------------------------------------

-- enrnarbej eliguivil congomgom pabrabmon antmorper3 paumacpar juacasnie
-- beagongon1 cargonler juaorture roscargar albcercid margarflo5
-- manruiber migibagar belbenzam eledejim2
-- La propiedad es
prop_elementos_producto :: [Int] -> [Int] -> Bool
prop_elementos_producto xs ys =
  length (producto xs ys) == length xs * length ys

-- La comprobación es
--    Prelude> quickCheck prop_elementos_producto
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 6.1. Definir la función 
--    subconjuntos :: [a] -> [[a]]
-- tal que (subconjuntos xs) es la lista de las subconjuntos de la lista
-- xs. Por ejemplo, 
--    ghci> subconjuntos [2,3,4]
--    [[2,3,4],[2,3],[2,4],[2],[3,4],[3],[4],[]]
--    ghci> subconjuntos [1,2,3,4]
--    [[1,2,3,4],[1,2,3],[1,2,4],[1,2],[1,3,4],[1,3],[1,4],[1],
--       [2,3,4],  [2,3],  [2,4],  [2],  [3,4],  [3],  [4], []]
-- ---------------------------------------------------------------------

-- enrnarbej pabrabmon antmorper3 beagongon1 albcercid margarflo5 manruiber
-- juacasnie eledejim2 natruipin
subconjuntos :: [a] -> [[a]]
subconjuntos [] = [[]] 
subconjuntos (x:xs) = [x:c | c <- sc] ++ sc
  where sc = subconjuntos xs

-- eliguivil
subconjuntos2 :: [a] -> [[a]]
subconjuntos2 xs = [[]] ++ [[x] | x <- xs] ++ combinaciones' xs

combinaciones' :: [a] -> [[a]]
combinaciones' []     = []
combinaciones' (x:xs) =
  [x:[x'] | x' <- xs] ++
  combinaciones' xs ++
  [x:x' | x' <- combinaciones' xs]

-- Comentario: La definición anterior se puede mejorar. Por ejemplo,
--    λ> length (subconjuntos2 [1..20])
--    1048576
--    (3.11 secs, 2,113,305,128 bytes)
--    λ> length (subconjuntos' [1..20])
--    1048576
--    (0.30 secs, 156,905,208 bytes)

-- enrnarbej
subconjuntos3 :: [a] -> [[a]]
subconjuntos3 xs = foldr (\x y -> [x:c | c <- y] ++ y) [[]] xs

-- ---------------------------------------------------------------------
-- Ejercicio 6.2. Comprobar con QuickChek que el número de elementos de
-- (subconjuntos xs) es 2 elevado al número de elementos de xs.
--
-- Nota. Al hacer la comprobación limitar el tamaño de las pruebas como
-- se indica a continuación
--    quickCheckWith (stdArgs {maxSize=7}) prop_subconjuntos
-- ---------------------------------------------------------------------

--enrnarbej pabrabmon antmorper3 beagongon1 juaorture roscargar eliguivil 
--juacasnie albcercid margarflo5 manruiber eledejim2
 
-- La propiedad es
prop_subconjuntos :: [Int] -> Bool
prop_subconjuntos xs =
  length (subconjuntos3 xs) == 2^(length xs)

-- Prelude> quickCheckWith (stdArgs {maxSize=7}) prop_subconjuntos
-- +++ OK, passed 100 tests.
