-- I1M 2015-16: Relación 27 (11 de marzo de 2016)
-- Relaciones binarias homogéneas con la librería Data.Set.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- El objetivo de esta relación de ejercicios es definir propiedades y
-- operaciones sobre las relaciones binarias (homogéneas) usando la
-- librería Data.Set.
--
-- Como referencia se puede usar el artículo de la wikipedia
-- http://bit.ly/HVHOPS 

-- ---------------------------------------------------------------------
-- § Pragmas                                                          --
-- ---------------------------------------------------------------------

{-# LANGUAGE TypeSynonymInstances, 
             FlexibleInstances #-}

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Test.QuickCheck
import Data.Set

-- ---------------------------------------------------------------------
-- Ejercicio 1. Una relación binaria R sobre un conjunto A puede
-- representar mediante un par (xs,ps) donde xs es el conjunto de los
-- elementos de A (el universo de R) y ps es el conjunto de pares de R
-- (el grafo de R). Definir el tipo de dato (Rel a) para representar las
-- relaciones binarias sobre a.  
-- ---------------------------------------------------------------------

type Rel a = (Set a, Set (a,a))

-- ---------------------------------------------------------------------
-- Nota. En los ejemplos usaremos las siguientes relaciones binarias: 
--    r1, r2, r3 :: Rel Int
--    r1 = (fromList [1..9],fromList [(1,3), (2,6), (8,9), (2,7)])
--    r2 = (fromList [1..9],fromList [(1,3), (2,6), (8,9), (3,7)])
--    r3 = (fromList [1..9],fromList [(1,3), (2,6), (8,9), (3,6)])
-- ---------------------------------------------------------------------

r1, r2, r3 :: Rel Int
r1 = (fromList [1..9],fromList [(1,3), (2,6), (8,9), (2,7)])
r2 = (fromList [1..9],fromList [(1,3), (2,6), (8,9), (3,7)])
r3 = (fromList [1..9],fromList [(1,3), (2,6), (8,9), (3,6)])

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    universo :: Ord a => Rel a -> Set a
-- tal que (universo r) es el universo de la relación r. Por ejemplo, 
--    universo r1  ==  fromList [1,2,3,4,5,6,7,8,9]
-- ---------------------------------------------------------------------

-- blaruiher fracruzam josllagam manvermor javperlag manpende
-- alvalvdom1 silgongal paocabper abrdelrod juamorrom1 rubvilval
-- juanarcon
universo :: Ord a => Rel a -> Set a
universo = fst

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--    grafo :: Ord a => Rel a -> [(a,a)]
-- tal que (grafo r) es el grafo de la relación r. Por ejemplo, 
--    grafo r1  ==  fromList [(1,3),(2,6),(2,7),(8,9)]
-- ---------------------------------------------------------------------

-- blaruiher fracruzam josllagam manvermor javperlag manpende
-- alvalvdom1 silgongal paocabper abrdelrod juamorrom1 rubvilval
-- juanarcon
grafo :: Ord a => Rel a -> Set (a,a)
grafo = snd

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función
--    reflexiva :: Ord a => Rel a -> Bool
-- tal que (reflexiva r) se verifica si la relación r es reflexiva. Por
-- ejemplo, 
--    ghci> reflexiva (fromList [1,3], fromList [(1,1),(1,3),(3,3)])
--    True
--    ghci> reflexiva (fromList [1,2,3], fromList [(1,1),(1,3),(3,3)])
--    False
-- ---------------------------------------------------------------------

-- fracruzam
reflexiva :: Ord a => Rel a -> Bool
reflexiva (u,g) = all (\n ->  (n,n) `elem` elems g) (elems u)

-- josllagam
reflexiva1 :: Ord a => Rel a -> Bool
reflexiva1 r = 
    length (elems (universo r)) == 
    length [(x,y)| (x,y) <- elems (grafo r), x == y]

-- manvermor javperlag manpende alvalvdom1 silgongal paocabper abrdelrod
-- juanarcon
reflexiva2 :: Ord a => Rel a -> Bool
reflexiva2 (u,g) = and [ member (x,x) g | x <- elems u]

-- manvermor
reflexiva3 :: Ord a => Rel a -> Bool
reflexiva3 (u,g) = and [elem x ys | x <- [(z,y) | (z,y) <- zip xs xs]]
    where xs = toList u
          ys = toList g

-- juamorrom1
reflexiva4 :: Ord a => Rel a -> Bool
reflexiva4 (u,g) = ur == Prelude.filter (`elem` g) ur
    where ur = [(x,x) | x <- (elems u)]
          gs = elems g

-- Comentario: La definición anterior se puede simplificar.

-- rubvilval
reflexiva5 :: Ord a => Rel a -> Bool
reflexiva5 r = isSubsetOf (fromList $ zip a a) (grafo r)
    where a = elems $ universo r


-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la función
--    simetrica :: Ord a => Rel a -> Bool
-- tal que (simetrica r) se verifica si la relación r es simétrica. Por
-- ejemplo, 
--    ghci> simetrica (fromList [1,3], fromList [(1,1),(1,3),(3,1)])
--    True
--    ghci> simetrica (fromList [1,3], fromList [(1,1),(1,3),(3,2)])
--    False
--    ghci> simetrica (fromList [1,3], fromList [])
--    True
-- ---------------------------------------------------------------------

-- manvermor javperlag manpende josllagam alvalvdom1  silgongal
-- paocabper abrderod juanarcon
simetrica :: Ord a => Rel a -> Bool
simetrica (u,g) = and [member (x,y) g | (y,x) <- elems g]

-- fracruzam
simetrica2 :: Ord a => Rel a -> Bool
simetrica2 (_,g) = and [(j,i) `elem` xs | (i,j) <- xs]
  where xs = elems g

-- juamorrom1 rubvilval
simetrica3 :: Ord a => Rel a -> Bool
simetrica3 (u,g) = all (\(x,y) -> elem (y,x) gs) gs
    where gs = elems g

-- ---------------------------------------------------------------------
-- Ejercicio 6. Definir la función
--    subconjunto :: Ord a => Set a -> Set a -> Bool
-- tal que (subconjunto c1 c2) se verifica si c1 es un subconjunto de
-- c2. Por ejemplo,
--    subconjunto (fromList [1,3]) (fromList [3,1,5])  ==  True
--    subconjunto (fromList [3,1,5]) (fromList [1,3])  ==  False
-- ---------------------------------------------------------------------

-- manvermor fracruzam javperlag manpende josllagam alvalvdom1 silgongal
-- paocabper abrdelrod rubvilval juamorrom1 juanarcon
subconjunto :: Ord a => Set a -> Set a -> Bool
subconjunto = isSubsetOf

-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir la función
--    composicion :: Ord a => Rel a -> Rel a -> Rel a
-- tal que (composicion r s) es la composición de las relaciones r y
-- s. Por ejemplo, 
--    ghci> let r1 = (fromList [1,2], fromList [(1,2),(2,2)])
--    ghci> let r2 = (fromList [1,2], fromList [(2,1)])
--    ghci> let r3 = (fromList [1,2], fromList [(1,1)])
--    ghci> composicion r1 r2
--    (fromList [1,2],fromList [(1,1),(2,1)])
--    ghci> composicion r1 r3
--    (fromList [1,2,3,4,5,6,7,8,9],fromList [])
-- ---------------------------------------------------------------------

-- manvermor fracruzam javperlag josllagam alvavldom1 silgongal
-- paocabper abrdelrod rubvilval juamorrom1 juanarcon
composicion :: Ord a => Rel a -> Rel a -> Rel a
composicion (u,ga) (_,gb) = 
    (u,fromList [(x,y) | (x,za) <- elems ga, 
                         (zb,y) <- elems gb, 
                         za == zb])

-- ---------------------------------------------------------------------
-- Ejercicio 8. Definir la función
--    transitiva :: Ord a => Rel a -> Bool
-- tal que (transitiva r) se verifica si la relación r es transitiva. 
-- Por ejemplo,
--    ghci> transitiva (fromList [1,3,5],fromList [(1,1),(1,3),(3,1),(3,3),(5,5)])
--    True
--    ghci> transitiva (fromList [1,3,5],fromList [(1,1),(1,3),(3,1),(5,5)])
--    False
-- ---------------------------------------------------------------------

-- manvermor manpende alvalvdom1 paocabper abrdelrod juanarcon
transitiva :: Ord a => Rel a -> Bool
transitiva r@(_,g) = subconjunto (grafo (composicion r r)) g

-- fracruzam josllagam
transitiva2 :: Ord a => Rel a -> Bool
transitiva2 (_,g) =
    and [(x,z) `elem` xs | (x,y) <- xs, (w,z) <- xs, y == w]
    where xs = elems g

-- silgongal
transitiva3 :: Ord a => Rel a -> Bool
transitiva3 (_,g) = 
    and [member (x,y) g | (x,z) <- elems g, (z',y) <- elems g, z == z']

-- juamorrom1
transitiva4 :: Ord a => Rel a -> Bool
transitiva4 (u,g) = 
    all (`elem` gs) [(a,d) | (a,b) <- gs, (c,d) <- gs, b == c ]
    where gs = elems g

-- rubvilval
transitiva5 :: Ord a => Rel a -> Bool
transitiva5 (r1,r2) = 
    subconjunto
    (fromList [(a,d) | (a,b) <- elems r2, (c,d) <- elems r2, b == c])
    r2

-- ---------------------------------------------------------------------
-- Ejercicio 9. Definir la función
--    esEquivalencia :: Ord a => Rel a -> Bool
-- tal que (esEquivalencia r) se verifica si la relación r es de
-- equivalencia. Por ejemplo,
--    ghci> esEquivalencia (fromList [1,3,5],
--                          fromList [(1,1),(1,3),(3,1),(3,3),(5,5)])
--    True
--    ghci> esEquivalencia (fromList [1,2,3,5],
--                          fromList [(1,1),(1,3),(3,1),(3,3),(5,5)])
--    False
--    ghci> esEquivalencia (fromList [1,3,5],
--                          fromList [(1,1),(1,3),(3,3),(5,5)])
--    False
-- ---------------------------------------------------------------------

-- manvermor fracruzam javperlag manpende josllagam alvalvdom1 silgongal
-- paocabper abrdelrod rubvilval juamorrom1 juanarcon
esEquivalencia :: Ord a => Rel a -> Bool
esEquivalencia r = reflexiva r && simetrica r && transitiva r

-- ---------------------------------------------------------------------
-- Ejercicio 10. Definir la función
--    irreflexiva :: Ord a => Rel a -> Bool
-- tal que (irreflexiva r) se verifica si la relación r es irreflexiva;
-- es decir, si ningún elemento de su universo está relacionado con 
-- él mismo. Por ejemplo,
--    ghci> irreflexiva (fromList [1,2,3],fromList [(1,2),(2,1),(2,3)])
--    True
--    ghci> irreflexiva (fromList [1,2,3],fromList [(1,2),(2,1),(3,3)])
--    False
-- ---------------------------------------------------------------------

-- manvermor javperlag manpende josllagam alvalvdom1 silgongal paocabper
-- abrdelrod juanarcon
irreflexiva :: Ord a => Rel a -> Bool
irreflexiva (u,g) = and [notMember (x,x) g | x <- elems u]

-- fracruzam
irreflexiva2 :: Ord a => Rel a -> Bool
irreflexiva2 (_,g) = all (\(x,y) -> x /= y) (elems g)

-- juamorrom1
irreflexiva3 :: Ord a => Rel a -> Bool
irreflexiva3 (u,g) = Prelude.filter (`elem` gs) [(x,x) | x <- us ] == []
    where us = elems u
          gs = elems g

-- Comentario: La definición anterior se puede simplificar usando null.

-- rubvilval
irreflexiva4 :: Ord a => Rel a -> Bool
irreflexiva4 (r1,r2) = all (`notElem` (elems r2)) (zip a a)
                        where a = elems r1

-- ---------------------------------------------------------------------
-- Ejercicio 11. Definir la función
--    antisimetrica :: Ord a => Rel a -> Bool
-- tal que (antisimetrica r) se verifica si la relación r es
-- antisimétrica; es decir, si (x,y) e (y,x) están relacionado, entonces
-- x=y. Por ejemplo,
--    antisimetrica (fromList [1,2],fromList [(1,2)])        ==  True
--    antisimetrica (fromList [1,2],fromList [(1,2),(2,1)])  ==  False
--    antisimetrica (fromList [1,2],fromList [(1,1),(2,1)])  ==  True
-- ---------------------------------------------------------------------

-- manvermor paocabper juanarcon
antisimetrica :: Ord a => Rel a -> Bool
antisimetrica (_,g) = 
    [] == [ (x,y) | (x,y) <- elems g, x /= y, member (y,x) g]

-- fracruzam manpende alvalvdom1 silgongal
antisimetrica2 :: Ord a => Rel a -> Bool
antisimetrica2 (_,g) =
  and [ x == y | (x,y) <- xs, (w,z) <- xs, y == w, x == z]
  where xs = elems g

-- javperlag josllagam abrdelrod
antisimetrica3 :: Ord a => Rel a -> Bool
antisimetrica3 r = and [notMember (b,a) g | (a,b) <- elems g, b /= a]
    where g = grafo r

-- juamorrom1
antisimetrica4 :: Ord a => Rel a -> Bool
antisimetrica4 (u,g) = antisimetricos == simetricos  
    where simetricos     = [ (x,y) | (x,y) <- gs, elem (y,x) gs]
          antisimetricos = [ (x,y) | (x,y) <- gs, elem (y,x) gs, x == y]
          gs = elems g

-- rubvilval
antisimetrica5 :: Ord a => Rel a -> Bool
antisimetrica5 (r1,r2) = 
    and [a == b | (a,b) <- er2 , elem (b,a) er2]
    where er2 = elems r2

-- ---------------------------------------------------------------------
-- Ejercicio 12. Definir la función
--    total :: Ord a => Rel a -> Bool
-- tal que (total r) se verifica si la relación r es total; es decir, si
-- para cualquier par x, y de elementos del universo de r, se tiene que
-- x está relacionado con y ó y etá relacionado con x. Por ejemplo,
--    total (fromList [1,3],fromList [(1,1),(3,1),(3,3)])  ==  True
--    total (fromList [1,3],fromList [(1,1),(3,1)])        ==  False
--    total (fromList [1,3],fromList [(1,1),(3,3)])        ==  False
-- ---------------------------------------------------------------------

-- manvermor javperlag manpende josllagam alvalvdom1 silgongal juanarcon
total :: Ord a => Rel a -> Bool
total (u,g) = 
    and [ member (x,y) g || member (y,x) g | x <- elems u, y <- elems u]

-- fracruzam paocabper juamorrom1 rubvilval
total2 :: Ord a => Rel a -> Bool
total2 (u,g) =
    and [(x,y) `elem` ys || (y,x) `elem` ys | x <- xs, y <- xs]
    where xs = elems u
          ys = elems g

-- abrdelrod
total3 :: Ord a => Rel a -> Bool
total3 (u,g) = 
    and [any (`member` g) [(x,y),(y,x)] | x <- elems u, y <- elems u]

-- ---------------------------------------------------------------------
-- Ejercicio 13. Comprobar con QuickCheck que las relaciones totales son
-- reflexivas. 
-- ---------------------------------------------------------------------

-- manvermor fracruzam manpende josllagam alvalvdom1 silgongal paocabper
-- abrdelrod rubvilval juamorrom1 juanarcon
prop_total_reflexiva :: Rel Int -> Property
prop_total_reflexiva r = total r ==> reflexiva r

-- La comprobación es
--    ghci> quickCheck prop_total_reflexiva
--    *** Gave up! Passed only 85 tests.

-- ---------------------------------------------------------------------
-- § Clausuras                                                        --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 14. Definir la función
--    clausuraReflexiva :: Ord a => Rel a -> Rel a  
-- tal que (clausuraReflexiva r) es la clausura reflexiva de r; es
-- decir, la menor relación reflexiva que contiene a r. Por ejemplo,
--    ghci> clausuraReflexiva (fromList [1,3], fromList [(1,1),(3,1)])
--    (fromList [1,3],fromList [(1,1),(3,1),(3,3)])
-- ---------------------------------------------------------------------

-- manvermor javperlag manpende josllagam alvalvdom1 silgongal paocabper
-- juanarcon
clausuraReflexiva :: Ord a => Rel a -> Rel a  
clausuraReflexiva (u,g) = (u, union (fromList [ (x,x) | x <- elems u]) g)

-- Comentario: La definición anterior se puede simplificar eliminando
-- paréntesis. 

-- fracruzam abrdelrod
clausuraReflexiva2 :: Ord a => Rel a -> Rel a  
clausuraReflexiva2 (u,g) = (u, fromList $ clausura ys ++ xs)
  where xs = elems g
        ys = elems u
        clausura :: [a] -> [(a,a)]
        clausura xs = [(x,x) | x <- xs]

-- juamorrom1
clausuraReflexiva3 :: Ord a => Rel a -> Rel a  
clausuraReflexiva3 (u,g) = (u,fromList (aux [(x,x) | x <- us] gs))
    where aux [] ys = ys
          aux (x:xs) ys | elem x ys = aux xs ys
                        | otherwise = x:aux xs ys
          us = elems u
          gs = elems g

-- rubvilval
clausuraReflexiva4 :: Ord a => Rel a -> Rel a  
clausuraReflexiva4 (r1,r2) = 
    (r1, union r2 (fromList $ zip er1 er1))
    where er1 = elems r1

-- ---------------------------------------------------------------------
-- Ejercicio 15. Comprobar con QuickCheck que clausuraReflexiva es
-- reflexiva. 
-- ---------------------------------------------------------------------

-- manvermor fracruzam javperlag manpende josllagam alvalvdom1 silgongal
-- paocabper abrdelrod rubvilval juamorrom1 juanarcon
prop_ClausuraReflexiva :: Rel Int -> Bool
prop_ClausuraReflexiva r = reflexiva (clausuraReflexiva r)

-- La comprobación es
--    ghci> quickCheck prop_ClausuraReflexiva
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 16. Definir la función
--    clausuraSimetrica :: Ord a => Rel a -> Rel a  
-- tal que (clausuraSimetrica r) es la clausura simétrica de r; es
-- decir, la menor relación simétrica que contiene a r. Por ejemplo,
--    ghci> clausuraSimetrica (fromList [1,3,5],fromList [(1,1),(3,1),(1,5)])
--    (fromList [1,3,5],fromList [(1,1),(1,3),(1,5),(3,1),(5,1)])
-- ---------------------------------------------------------------------

-- manvermor javperlag manpende josllagam alvalvdom1 silgongal paocabper
-- juanarcon rubvilval
clausuraSimetrica :: Ord a => Rel a -> Rel a  
clausuraSimetrica (u,g) = 
    (u,union g (fromList [(y,x) | (x,y) <- elems g]))

-- Comentario: La definición anterior se puede simplificar eliminando
-- paréntesis.

-- fracruzam abrdelrod juamorrom1
clausuraSimetrica2 :: Ord a => Rel a -> Rel a  
clausuraSimetrica2 (u,g) = (u, fromList $ clausura xs ++ xs)
    where xs = elems g
          clausura :: Eq a => [(a,a)] -> [(a,a)]
          clausura xs = [(y,x) | (x,y) <- xs, x /= y]

-- ---------------------------------------------------------------------
-- Ejercicio 17. Comprobar con QuickCheck que clausuraSimetrica es
-- simétrica. 
-- ---------------------------------------------------------------------

-- manvermor fracruzam javperlag manpende josllagam alvalvdom1 silgongal
-- paocabper abrdelrod rubvilval juamorrom1 juanarcon
prop_ClausuraSimetrica :: Rel Int -> Bool
prop_ClausuraSimetrica r = simetrica (clausuraSimetrica r)

-- La comprobación es
--    ghci> quickCheck prop_ClausuraSimetrica
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 18. Definir la función
--    clausuraTransitiva :: Ord a => Rel a -> Rel a  
-- tal que (clausuraTransitiva r) es la clausura transitiva de r; es
-- decir, la menor relación transitiva que contiene a r. Por ejemplo,
--    ghci> clausuraTransitiva (fromList [1..6],fromList [(1,2),(2,5),(5,6)])
--    (fromList [1,2,3,4,5,6],fromList [(1,2),(1,5),(1,6),(2,5),(2,6),(5,6)])
-- ---------------------------------------------------------------------

-- Por recursion acumulando pares 
-- manvermor silgongal
clausuraTransitiva :: Ord a => Rel a -> Rel a  
clausuraTransitiva (u,g) = (u, claTransAux g)
    where claTransAux r | subconjunto (asoc r r) r = r
                        | otherwise = claTransAux (union r (asoc r r))
          asoc p q = fromList [(x,y) | (x,za) <- elems p, 
                                       (zb,y) <- elems q, 
                                       za == zb]
                                         
-- Comentario: La definición anterior se puede simplificar eliminando
-- paréntesis.

-- fracruzam manpende josllagam paocabper abrdelrod juamorrom1 juanarcon
-- rubvilval
clausuraTransitiva2 :: Ord a => Rel a -> Rel a  
clausuraTransitiva2 (u,g)
    | transitiva (u,g) = (u,g)
    | otherwise = clausuraTransitiva2 (u, fromList $ clausura xs ++ xs)
  where xs = elems g
        clausura :: Eq a => [(a,a)] -> [(a,a)]
        clausura xs =
            [(x,z) | (x,y) <- xs, x /= y, (w,z) <- xs, y == w]

-- ---------------------------------------------------------------------
-- Ejercicio 19. Comprobar con QuickCheck que clausuraTransitiva es
-- transitiva. 
-- ---------------------------------------------------------------------

-- manvermor fracruzam manpende josllagam alvalvdom1 silgongal paocabper
-- abrdelrod rubvilval juamorrom1 juanarcon
prop_ClausuraTransitiva :: Rel Int -> Bool
prop_ClausuraTransitiva r = transitiva (clausuraTransitiva r)

-- La comprobación es
--    ghci> quickCheckWith (stdArgs {maxSize=7}) prop_ClausuraTransitiva
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- § Generador de relaciones                                          --
-- ---------------------------------------------------------------------

-- genSet es un generador de relaciones binarias. Por ejemplo,
--    ghci> sample genRel
--    (fromList [0],fromList [])
--    (fromList [-1,1],fromList [(-1,1)])
--    (fromList [-3,-2],fromList [])
--    (fromList [-2,0,1,6],fromList [(0,0),(6,0)])
--    (fromList [-7,0,2],fromList [(-7,0),(2,0)])
--    (fromList [2,11],fromList [(2,2),(2,11),(11,2),(11,11)])
--    (fromList [-4,-2,1,4,5],fromList [(1,-2),(1,1),(1,5)])
--    (fromList [-4,-3,-2,6,7],fromList [(-3,-4),(7,-3),(7,-2)])
--    (fromList [-9,-7,0,10],fromList [(10,-9)])
--    (fromList [-10,3,8,10],fromList [(3,3),(10,-10)])
--    (fromList [-10,-9,-7,-6,-5,-4,-2,8,12],fromList [])
genRel :: (Arbitrary a, Integral a) => Gen (Rel a)
genRel = do xs <- listOf1 arbitrary
            ys <- listOf (elements [(x,y) | x <- xs, y <- xs])
            return (fromList xs, fromList ys)

instance (Arbitrary a, Integral a) => Arbitrary (Rel a) where
    arbitrary = genRel
