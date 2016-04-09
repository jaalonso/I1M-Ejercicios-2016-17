-- I1M 2015-16: Relaci�n 31 (8 de abril de 2016)
-- Funciones con el TAD de los mont�culos.
-- Departamento de Ciencias de la Computaci�n e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducci�n                                                       --
-- ---------------------------------------------------------------------

-- El objetivo de esta relaci�n de ejercicios es definir funciones sobre 
-- el TAD de las mont�culos, utilizando las implementaciones estudiadas
-- en el tema 20 que se encuenta en
--    http://www.cs.us.es/~jalonso/cursos/i1m-15/temas/tema-20.html

-- Para realizar los ejercicios hay que tener instalada la librer�a I1M
-- que contiene la implementaci�n de TAD de los mont�culos. Los pasos
-- para instalarla son los siguientes:
-- + Descargar el paquete I1M desde http://bit.ly/1pbnDqm
-- + Descomprimirlo (y se crea el directorio I1M-master.zip).
-- + Cambiar al directorio I1M-master.
-- + Ejecutar cabal install I1M.cabal

-- ---------------------------------------------------------------------
-- Importaci�n de librer�as                                           --
-- ---------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances #-}

import I1M.Monticulo     
import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejemplos                                                           --
-- ---------------------------------------------------------------------

-- Para los ejemplos se usar�n los siguientes mont�culos.
m1, m2, m3 :: Monticulo Int
m1 = foldr inserta vacio [6,1,4,8]
m2 = foldr inserta vacio [7,5]
m3 = foldr inserta vacio [6,1,4,8,7,5]

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la funci�n
--    numeroDeNodos :: Ord a => Monticulo a -> Int
-- tal que (numeroDeNodos m) es el n�mero de nodos del mont�culo m. Por
-- ejemplo, 
--    numeroDeNodos m1  ==  4
-- ---------------------------------------------------------------------

numeroDeNodos :: Ord a => Monticulo a -> Int
numeroDeNodos m = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la funci�n
--    filtra :: Ord a => (a -> Bool) -> Monticulo a -> Monticulo a
-- tal que (filtra p m) es el mont�culo con los nodos del mont�culo m
-- que cumplen la propiedad p. Por ejemplo,
--    ghci> m1
--    M 1 2 (M 4 1 (M 8 1 Vacio Vacio) Vacio) (M 6 1 Vacio Vacio)
--    ghci> filtra even m1
--    M 4 1 (M 6 1 (M 8 1 Vacio Vacio) Vacio) Vacio
--    ghci> filtra odd m1
--    M 1 1 Vacio Vacio
-- ---------------------------------------------------------------------

filtra :: Ord a => (a -> Bool) -> Monticulo a -> Monticulo a
filtra p m = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la funci�n
--    menores :: Ord a => Int -> Monticulo a -> [a]
-- tal que (menores n m) es la lista de los n menores elementos del
-- mont�culo m. Por ejemplo,
--    ghci> m1
--    M 1 2 (M 4 1 (M 8 1 Vacio Vacio) Vacio) (M 6 1 Vacio Vacio)
--    ghci> menores 3 m1
--    [1,4,6]
-- ---------------------------------------------------------------------

menores :: Ord a => Int -> Monticulo a -> [a]
menores = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la funci�n
--    restantes :: Ord a => Int -> Monticulo a -> Monticulo a
-- tal que (restantes n m) es el mont�culo obtenido rliminando los n
-- menores elementos del mont�culo m. Por ejemplo,
--    ghci> m1
--    M 1 2 (M 4 1 (M 8 1 Vacio Vacio) Vacio) (M 6 1 Vacio Vacio)
--    ghci> restantes 3 m1
--    M 8 1 Vacio Vacio
--    ghci> restantes 2 m1
--    M 6 1 (M 8 1 Vacio Vacio) Vacio
-- ---------------------------------------------------------------------

restantes :: Ord a => Int -> Monticulo a -> Monticulo a
restantes = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la funci�n
--    lista2Monticulo :: Ord a => [a] -> Monticulo a
-- tal que (lista2Monticulo xs) es el mont�culo cuyos nodos son los
-- elementos de la lista xs. Por ejemplo,
--    ghci> lista2Monticulo [2,5,3,7]
--    M 2 1 (M 3 2 (M 7 1 Vacio Vacio) (M 5 1 Vacio Vacio)) Vacio
-- ---------------------------------------------------------------------

lista2Monticulo :: Ord a => [a] -> Monticulo a
lista2Monticulo = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 6. Definir la funci�n
--    monticulo2Lista :: Ord a => Monticulo a -> [a]
-- tal que (monticulo2Lista m) es la lista ordenada de los nodos del
-- mont�culo m. Por ejemplo,
--    ghci> m1
--    M 1 2 (M 4 1 (M 8 1 Vacio Vacio) Vacio) (M 6 1 Vacio Vacio)
--    ghci> monticulo2Lista m1
--    [1,4,6,8]
-- ---------------------------------------------------------------------

monticulo2Lista :: Ord a => Monticulo a -> [a]
monticulo2Lista m = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir la funci�n
--    ordenada :: Ord a => [a] -> Bool
-- tal que (ordenada xs) se verifica si xs es una lista ordenada de
-- forma creciente. Por ejemplo,
--    ordenada [3,5,9]  ==  True
--    ordenada [3,5,4]  ==  False
--    ordenada [7,5,4]  ==  False
-- ---------------------------------------------------------------------

ordenada :: Ord a => [a] -> Bool
ordenada = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 8. Comprobar con QuickCheck que para todo mont�culo m,
-- (monticulo2Lista m) es una lista ordenada creciente.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_monticulo2Lista_ordenada :: Monticulo Int -> Bool
prop_monticulo2Lista_ordenada m = undefined

-- La comprobaci�n es
--    ghci> quickCheck prop_monticulo2Lista_ordenada
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 10. Usando monticulo2Lista y lista2Monticulo, definir la
-- funci�n 
--    ordena :: Ord a => [a] -> [a]
-- tal que (ordena xs) es la lista obtenida ordenando de forma creciente
-- los elementos de xs. Por ejemplo,
--    ordena [7,5,3,6,5]  ==  [3,5,5,6,7]
-- ---------------------------------------------------------------------

ordena :: Ord a => [a] -> [a]
ordena = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 11. Comprobar con QuickCheck que para toda lista xs,
-- (ordena xs) es una lista ordenada creciente.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_ordena_ordenada :: [Int] -> Bool
prop_ordena_ordenada xs = undefined

-- La comprobaci�n es
--    ghci> quickCheck prop_ordena_ordenada
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 12. Definir la funci�n
--    borra :: Eq a => a -> [a] -> [a]
-- tal que (borra x xs) es la lista obtenida borrando una ocurrencia de
-- x en la lista xs. Por ejemplo, 
--    borra 1 [1,2,1]  ==  [2,1]
--    borra 3 [1,2,1]  ==  [1,2,1]
-- ---------------------------------------------------------------------

borra :: Eq a => a -> [a] -> [a]
borra = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 14. Definir la funci�n esPermutaci�n tal que 
-- (esPermutaci�n xs ys) se verifique si xs es una permutaci�n de
-- ys. Por ejemplo,  
--    esPermutaci�n [1,2,1] [2,1,1]  ==  True
--    esPermutaci�n [1,2,1] [1,2,2]  ==  False
-- ---------------------------------------------------------------------

esPermutacion :: Eq a => [a] -> [a] -> Bool
esPermutacion = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 15. Comprobar con QuickCheck que para toda lista xs,
-- (ordena xs) es una permutaci�n de xs.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_ordena_permutacion :: [Int] -> Bool
prop_ordena_permutacion xs = undefined

-- La comprobaci�n es
--    ghci> quickCheck prop_ordena_permutacion
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Generador de mont�culos                                            --
-- ---------------------------------------------------------------------

-- genMonticulo es un generador de mont�culos. Por ejemplo,
--    ghci> sample genMonticulo
--    VacioM
--    M (-1) 1 (M 1 1 VacioM VacioM) VacioM
--    ...
genMonticulo :: Gen (Monticulo Int)
genMonticulo = do xs <- listOf arbitrary
                  return (foldr inserta vacio xs)

-- Mont�culo es una instancia de la clase arbitraria.
instance Arbitrary (Monticulo Int) where
    arbitrary = genMonticulo