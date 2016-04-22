-- I1M 2015-16: Relación 31 (8 de abril de 2016)
-- Funciones con el TAD de los montículos.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- El objetivo de esta relación de ejercicios es definir funciones sobre 
-- el TAD de las montículos, utilizando las implementaciones estudiadas
-- en el tema 20 que se encuenta en
--    http://www.cs.us.es/~jalonso/cursos/i1m-15/temas/tema-20.html
-- 
-- Para realizar los ejercicios hay que tener instalada la librería I1M
-- que contiene la implementación de TAD de los montículos. Los pasos
-- para instalarla son los siguientes:
-- + Descargar el paquete I1M desde http://bit.ly/1pbnDqm
-- + Descomprimirlo (y se crea el directorio I1M-master.zip).
-- + Cambiar al directorio I1M-master.
-- + Ejecutar cabal install I1M.cabal
-- 
-- Otra forma es descargar la implementación del TAD de montículos:
-- + Monticulo.hs que está en http://bit.ly/1oNy2HT

-- ---------------------------------------------------------------------
-- Importación de librerías                                           --
-- ---------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances #-}

import Data.List
import Test.QuickCheck

-- Hay que elegir una implementación del TAD montículos:
-- import Monticulo
import I1M.Monticulo 

-- ---------------------------------------------------------------------
-- Ejemplos                                                           --
-- ---------------------------------------------------------------------

-- Para los ejemplos se usarán los siguientes montículos.
m1, m2, m3 :: Monticulo Int
m1 = foldr inserta vacio [6,1,4,8]
m2 = foldr inserta vacio [7,5]
m3 = foldr inserta vacio [6,1,4,8,7,5]

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    numeroDeNodos :: Ord a => Monticulo a -> Int
-- tal que (numeroDeNodos m) es el número de nodos del montículo m. Por
-- ejemplo, 
--    numeroDeNodos m1  ==  4
-- ---------------------------------------------------------------------

-- fracruzam manvermor jespergue javperlag josllagam rubvilval alvalvdom1
-- manpende ivaruicam erisancha isrbelnun silgongal juanarcon lucgamgal
-- javoliher abrdelrod juamorrom1
numeroDeNodos :: Ord a => Monticulo a -> Int
numeroDeNodos m | esVacio m = 0
                | otherwise = 1 + numeroDeNodos (resto m)

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    filtra :: Ord a => (a -> Bool) -> Monticulo a -> Monticulo a
-- tal que (filtra p m) es el montículo con los nodos del montículo m
-- que cumplen la propiedad p. Por ejemplo,
--    ghci> m1
--    M 1 2 (M 4 1 (M 8 1 Vacio Vacio) Vacio) (M 6 1 Vacio Vacio)
--    ghci> filtra even m1
--    M 4 1 (M 6 1 (M 8 1 Vacio Vacio) Vacio) Vacio
--    ghci> filtra odd m1
--    M 1 1 Vacio Vacio
-- ---------------------------------------------------------------------

-- fracruzam manvermor jespergue javperlag josllagam rubvilval alvalvdom1
-- manpende ivaruicam erisancha isrbelnun silgongal juanarcon lucgamgal
-- javoliher abrdelrod juamorrom1
filtra :: Ord a => (a -> Bool) -> Monticulo a -> Monticulo a
filtra p m | esVacio m = vacio
           | p x       = inserta x (filtra p (resto m))
           | otherwise =            filtra p (resto m)
    where x = menor m

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--    menores :: Ord a => Int -> Monticulo a -> [a]
-- tal que (menores n m) es la lista de los n menores elementos del
-- montículo m. Por ejemplo,
--    ghci> m1
--    M 1 2 (M 4 1 (M 8 1 Vacio Vacio) Vacio) (M 6 1 Vacio Vacio)
--    ghci> menores 3 m1
--    [1,4,6]
--    λ> menores 10 m1
--    [1,4,6,8]
-- ---------------------------------------------------------------------

-- manvermor
menores :: Ord a => Int -> Monticulo a -> [a]
menores n m | esVacio m = []
            | otherwise = take n (minM : menores n rm)
    where minM = menor m
          rm   = resto m

-- alvalvdom1 manpende fracruzam ivaruicam erisancha isrbelnun silgongal
-- juanarcon lucgamgal javoliher abrdelrod
menores3 :: Ord a => Int -> Monticulo a -> [a]
menores3 0 m = []
menores3 n m | esVacio m = []
             | otherwise = menor m : menores3 (n-1) (resto m)

-- juamorrom1
menores4 :: Ord a => Int -> Monticulo a -> [a]
menores4 n m = take n (aux m)
    where aux m | esVacio m  = []
                | otherwise  = (menor m) : aux (resto m)

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función
--    restantes :: Ord a => Int -> Monticulo a -> Monticulo a
-- tal que (restantes n m) es el montículo obtenido eliminando los n
-- menores elementos del montículo m. Por ejemplo,
--    ghci> m1
--    M 1 2 (M 4 1 (M 8 1 Vacio Vacio) Vacio) (M 6 1 Vacio Vacio)
--    ghci> restantes 3 m1
--    M 8 1 Vacio Vacio
--    ghci> restantes 2 m1
--    M 6 1 (M 8 1 Vacio Vacio) Vacio
--    λ> restantes 7 m1
--    Vacio
-- ---------------------------------------------------------------------

-- alvalvdom1 manpende manvermor fracruzam ivaruicam erisancha isrbelnun
-- silgongal juanarcon lucgamgal abrdelrod juamorrom1
restantes :: Ord a => Int -> Monticulo a -> Monticulo a
restantes 0 m = m
restantes n m | esVacio m = vacio
              | otherwise = restantes (n-1) (resto m)

-- javoliher
restantes2 :: Ord a => Int -> Monticulo a -> Monticulo a
restantes2 n m = last (take (n+1) (iterate resto m))

-- Comentario: La definición anterior falla en el último ejemplo.

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la función
--    lista2Monticulo :: Ord a => [a] -> Monticulo a
-- tal que (lista2Monticulo xs) es el montículo cuyos nodos son los
-- elementos de la lista xs. Por ejemplo,
--    ghci> lista2Monticulo [2,5,3,7]
--    M 2 1 (M 3 2 (M 7 1 Vacio Vacio) (M 5 1 Vacio Vacio)) Vacio
-- ---------------------------------------------------------------------

-- fracruzam manvermor jespergue javperlag josllagam rubvilval alvalvdom1
-- manpende ivaruicam erisancha isrbelnun silgongal juanarcon javoliher
-- abrdelrod juamorrom1
lista2Monticulo :: Ord a => [a] -> Monticulo a
lista2Monticulo = foldr inserta vacio

-- ---------------------------------------------------------------------
-- Ejercicio 6. Definir la función
--    monticulo2Lista :: Ord a => Monticulo a -> [a]
-- tal que (monticulo2Lista m) es la lista ordenada de los nodos del
-- montículo m. Por ejemplo,
--    ghci> m1
--    M 1 2 (M 4 1 (M 8 1 Vacio Vacio) Vacio) (M 6 1 Vacio Vacio)
--    ghci> monticulo2Lista m1
--    [1,4,6,8]
-- ---------------------------------------------------------------------

-- fracruzam manvermor jespergue javperlag josllagam alvalvdom1 manpende
-- isrbelnun javoliher abrdelrod juamorrom1
monticulo2Lista :: Ord a => Monticulo a -> [a]
monticulo2Lista m
    | esVacio m = []
    | otherwise = menor m : monticulo2Lista (resto m)

-- rubvilval ivaruicam erisancha silgongal juanarcon lucgamgal
monticulo2Lista2 :: Ord a => Monticulo a -> [a]
monticulo2Lista2 m = menores (numeroDeNodos m) m

-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir la función
--    ordenada :: Ord a => [a] -> Bool
-- tal que (ordenada xs) se verifica si xs es una lista ordenada de
-- forma creciente. Por ejemplo,
--    ordenada [3,5,9]  ==  True
--    ordenada [3,5,4]  ==  False
--    ordenada [7,5,4]  ==  False
-- ---------------------------------------------------------------------

-- manvermor: (Falta poner <= para que se verifique la propiedad posterior)

-- fracruzam manvermor jespergue javperlag josllagam rubvilval alvalvdom1
-- manpende ivaruicam erisancha silgongal juanarcon lucgamgal javoliher abrdelrod
-- juamorrom1
ordenada :: Ord a => [a] -> Bool
ordenada (x:y:xs) = x <= y && ordenada (y:xs)
ordenada _        = True

-- isrbelnun
ordenada2 :: Ord a => [a] -> Bool
ordenada2 [] = True
ordenada2 (x:xs) | all (>=x) xs  = ordenada2 xs
                 | otherwise    = False

-- ---------------------------------------------------------------------
-- Ejercicio 8. Comprobar con QuickCheck que para todo montículo m,
-- (monticulo2Lista m) es una lista ordenada creciente.
-- ---------------------------------------------------------------------

-- manvermor jespergue fracruzam josllagam rubvilval alvalvdom1 manpende
-- ivaruicam erisancha isrbelnun silgongal juanarcon lucgamgal javoliher
-- abrdelrod 
-- La propiedad es
prop_monticulo2Lista_ordenada :: Monticulo Int -> Bool
prop_monticulo2Lista_ordenada m = ordenada (monticulo2Lista m)

-- juamorrom1
prop_monticulo2Lista_ordenada2 :: Monticulo Int -> Bool
prop_monticulo2Lista_ordenada2 = ordenada . monticulo2Lista

-- La comprobación es
--    ghci> quickCheck prop_monticulo2Lista_ordenada
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 10. Usando monticulo2Lista y lista2Monticulo, definir la
-- función 
--    ordena :: Ord a => [a] -> [a]
-- tal que (ordena xs) es la lista obtenida ordenando de forma creciente
-- los elementos de xs. Por ejemplo,
--    ordena [7,5,3,6,5]  ==  [3,5,5,6,7]
-- ---------------------------------------------------------------------

-- manvermor jespergue fracruzam javperlag josllagam rubvilval alvalvdom1
-- manpende ivaruicam erisancha juanarcon
ordena :: Ord a => [a] -> [a]
ordena xs = monticulo2Lista $ lista2Monticulo xs

-- Comentario: La definición anterior se puede simplificar.

-- isrbelnun silgongal lucgamgal javoliher abrdelrod juamorrom1
ordena2 :: Ord a => [a] -> [a]
ordena2 = monticulo2Lista . lista2Monticulo
            
-- ---------------------------------------------------------------------
-- Ejercicio 11. Comprobar con QuickCheck que para toda lista xs,
-- (ordena xs) es una lista ordenada creciente.
-- ---------------------------------------------------------------------

-- manvermor jespergue fracruzam javperlag josllagam rubvilval alvalvdom1
-- ivaruicam erisancha isrbelnun silgongal juanarcon lucgamgal javoliher
-- abrdelrod 
-- La propiedad es 
prop_ordena_ordenada :: [Int] -> Bool
prop_ordena_ordenada xs = ordenada (ordena xs)

-- juamorrom1
prop_ordena_ordenada2 :: [Int] -> Bool
prop_ordena_ordenada2 = ordenada . ordena

-- La comprobación es
--    ghci> quickCheck prop_ordena_ordenada
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 12. Definir la función
--    borra :: Eq a => a -> [a] -> [a]
-- tal que (borra x xs) es la lista obtenida borrando una ocurrencia de
-- x en la lista xs. Por ejemplo, 
--    borra 1 [1,2,1]  ==  [2,1]
--    borra 3 [1,2,1]  ==  [1,2,1]
-- ---------------------------------------------------------------------

-- manvermor jespergue fracruzam javperlag josllagam rubvilval alvalvdom1
-- manpende  erisancha isrbelnun silgongal juanarcon lucgamgal javoliher
-- abrdelrod juamorrom1
borra :: Eq a => a -> [a] -> [a]
borra _ []                 = []
borra x (y:xs) | x == y    = xs
               | otherwise = y : borra x xs

-- ivaruicam
borra2 :: Eq a => a -> [a] -> [a]
borra2 = delete 

-- ---------------------------------------------------------------------
-- Ejercicio 14. Definir la función esPermutación tal que 
-- (esPermutación xs ys) se verifique si xs es una permutación de
-- ys. Por ejemplo,  
--    esPermutación [1,2,1] [2,1,1]  ==  True
--    esPermutación [1,2,1] [1,2,2]  ==  False
-- ---------------------------------------------------------------------

-- manvermor jespergue javperlag josllagam rubvilval alvalvdom1 fracruzam
-- ivaruicam erisancha isrbelnun silgongal juanarcon lucgamgal javoliher
-- abrdelrod 
esPermutacion :: Eq a => [a] -> [a] -> Bool
esPermutacion [] [] = True
esPermutacion [] ys = False
esPermutacion (x:xs) ys = elem x ys && esPermutacion xs (borra x ys)

-- juamorrom1
esPermutacion2:: Eq a => [a] -> [a] -> Bool
esPermutacion2 xs ys = elem xs (permutations ys)

-- Comentario: La definición anterior se puede mejorar.

-- ---------------------------------------------------------------------
-- Ejercicio 15. Comprobar con QuickCheck que para toda lista xs,
-- (ordena xs) es una permutación de xs.
-- ---------------------------------------------------------------------

-- manvermor jespergue fracruzam javperlag josllagam rubvilval alvalvdom1
-- manpende ivaruicam erisancha isrbelnun silgongal juanarcon lucgamgal 
-- javoliher abrdelrod juamorrom1
-- La propiedad es
prop_ordena_permutacion :: [Int] -> Bool
prop_ordena_permutacion xs = esPermutacion xs (ordena xs)

-- La comprobación es
--    ghci> quickCheck prop_ordena_permutacion
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Generador de montículos                                            --
-- ---------------------------------------------------------------------

-- genMonticulo es un generador de montículos. Por ejemplo,
--    ghci> sample genMonticulo
--    VacioM
--    M (-1) 1 (M 1 1 VacioM VacioM) VacioM
--    ...
genMonticulo :: Gen (Monticulo Int)
genMonticulo = do xs <- listOf arbitrary
                  return (foldr inserta vacio xs)

-- Montículo es una instancia de la clase arbitraria.
instance Arbitrary (Monticulo Int) where
    arbitrary = genMonticulo
