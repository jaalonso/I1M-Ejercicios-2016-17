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

numeroDeNodos :: Ord a => Monticulo a -> Int
numeroDeNodos m
    | esVacio m = 0
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

filtra :: Ord a => (a -> Bool) -> Monticulo a -> Monticulo a
filtra p m
    | esVacio m = vacio
    | p mm      = inserta mm (filtra p rm)
    | otherwise = filtra p rm
    where mm = menor m
          rm = resto m

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

menores :: Ord a => Int -> Monticulo a -> [a]
menores 0 m  = []
menores n m | esVacio m = []
            | otherwise = menor m : menores (n-1) (resto m)

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

restantes :: Ord a => Int -> Monticulo a -> Monticulo a
restantes 0 m  = m
restantes n m | esVacio m = vacio
              | otherwise = restantes (n-1) (resto m)

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la función
--    lista2Monticulo :: Ord a => [a] -> Monticulo a
-- tal que (lista2Monticulo xs) es el montículo cuyos nodos son los
-- elementos de la lista xs. Por ejemplo,
--    ghci> lista2Monticulo [2,5,3,7]
--    M 2 1 (M 3 2 (M 7 1 Vacio Vacio) (M 5 1 Vacio Vacio)) Vacio
-- ---------------------------------------------------------------------

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

monticulo2Lista :: Ord a => Monticulo a -> [a]
monticulo2Lista m
    | esVacio m = []
    | otherwise = menor m : monticulo2Lista (resto m)

-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir la función
--    ordenada :: Ord a => [a] -> Bool
-- tal que (ordenada xs) se verifica si xs es una lista ordenada de
-- forma creciente. Por ejemplo,
--    ordenada [3,5,9]  ==  True
--    ordenada [3,5,4]  ==  False
--    ordenada [7,5,4]  ==  False
-- ---------------------------------------------------------------------

ordenada :: Ord a => [a] -> Bool
ordenada (x:y:zs) = x <= y && ordenada (y:zs)
ordenada _        = True

-- ---------------------------------------------------------------------
-- Ejercicio 8. Comprobar con QuickCheck que para todo montículo m,
-- (monticulo2Lista m) es una lista ordenada creciente.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_monticulo2Lista_ordenada :: Monticulo Int -> Bool
prop_monticulo2Lista_ordenada m =
    ordenada (monticulo2Lista m)

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

ordena :: Ord a => [a] -> [a]
ordena  = monticulo2Lista . lista2Monticulo

-- ---------------------------------------------------------------------
-- Ejercicio 11. Comprobar con QuickCheck que para toda lista xs,
-- (ordena xs) es una lista ordenada creciente.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_ordena_ordenada :: [Int] -> Bool
prop_ordena_ordenada xs =
    ordenada (ordena xs)

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

borra :: Eq a => a -> [a] -> [a]
borra x []                 = []
borra x (y:ys) | x == y    = ys
               | otherwise = y : borra x ys

-- ---------------------------------------------------------------------
-- Ejercicio 14. Definir la función esPermutación tal que 
-- (esPermutación xs ys) se verifique si xs es una permutación de
-- ys. Por ejemplo,  
--    esPermutación [1,2,1] [2,1,1]  ==  True
--    esPermutación [1,2,1] [1,2,2]  ==  False
-- ---------------------------------------------------------------------

esPermutacion :: Eq a => [a] -> [a] -> Bool
esPermutacion []     []     = True
esPermutacion []     (y:ys) = False
esPermutacion (x:xs) ys     = elem x ys && esPermutacion xs (borra x ys)

-- ---------------------------------------------------------------------
-- Ejercicio 15. Comprobar con QuickCheck que para toda lista xs,
-- (ordena xs) es una permutación de xs.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_ordena_permutacion :: [Int] -> Bool
prop_ordena_permutacion xs =
    esPermutacion (ordena xs) xs

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
