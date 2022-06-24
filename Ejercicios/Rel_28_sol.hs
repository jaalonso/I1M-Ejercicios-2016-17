-- I1M 2016-17: Relación 28 (21 de abril de 2017)
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
--    http://www.cs.us.es/~jalonso/cursos/i1m-16/temas/tema-20.html

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
import Data.List

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

-- albcercid margarvil14 josdeher pabrabmon josrodgal7 natmarmar2
-- antmorper3 alvfercen fatfervaz paumacpar marlobrip marjimcom marmerzaf
-- eledejim2 enrnarbej migibagar
numeroDeNodos :: Ord a => Monticulo a -> Int
numeroDeNodos m | esVacio m = 0
                | otherwise = 1 + numeroDeNodos (resto m)

-- joscasgom1 carmarcar5
--    numeroDeNodos2 :: Ord a => Monticulo a -> Int
--    numeroDeNodos2 m = length (elementos m)

-- Comentario: La función elementos no se exporta en el módulo de
-- Monticulos. 

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

-- albcercid josdeher carmarcar5 pabrabmon josrodgal7 antmorper3
-- fatfervaz paumacpar marlobrip marjimcom eledejim2 enrnarbej migibagar
filtra :: Ord a => (a -> Bool) -> Monticulo a -> Monticulo a
filtra p m | esVacio m = vacio
           | p a       = inserta a (filtra p b)
           | otherwise = filtra p b
  where a = menor m
        b = resto m

-- joscasgom1 natmarmar2 alvfercen marmerzaf
filtra2 :: Ord a => (a -> Bool) -> Monticulo a -> Monticulo a
filtra2 p m = lista2Monticulo (filter p (monticulo2Lista m))

-- margarvil14
filtra3 :: Ord a => (a -> Bool) -> Monticulo a -> Monticulo a
filtra3 p m = lista2Monticulo xs
  where xs = filter p (monticulo2Lista m)

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

-- albcercid joscasgom1 josdeher carmarcar5 pabrabmon josrodgal7 natmarmar2
-- antmorper3 alvfercen fatfervaz paumacpar marlobrip marmerzaf
-- eledejim2 enrnarbej 
menores :: Ord a => Int -> Monticulo a -> [a]
menores 0 m = []
menores n m | esVacio m = []
            | otherwise = menor m : menores (n-1) (resto m)

-- margarvil14
menores2 :: Ord a => Int -> Monticulo a -> [a]
menores2 0 _ = []
menores2 1 m = [menor m]
menores2 n m | n <= l    = [menor m] ++ menores2 (n-1) (resto m)
             | otherwise = menores2 l m
  where l = numeroDeNodos m

-- Comentario: La definición anterior se puede mejorar. Por ejemplo,
--    λ> maximum (menores2 100 (foldr inserta vacio [1..10000]))
--    100
--    (1.68 secs, 276,918,344 bytes)
--    λ> maximum (menores 100 (foldr inserta vacio [1..10000]))
--    100
--    (0.02 secs, 0 bytes)

-- migibagar
menores3 :: Ord a => Int -> Monticulo a -> [a]
menores3 n m = sort (aux [] n m)
  where aux xs 0 _ = xs
        aux xs n m | n > h      =  aux xs h m
                   | otherwise  =  aux ((menor m):xs) (n-1) (resto m)
          where h = numeroDeNodos m

-- Comentario: La definición anterior se puede mejorar. Por ejemplo,
--    λ> maximum (menores3 100 (foldr inserta vacio [1..10000]))
--    100
--    (1.69 secs, 277,633,688 bytes)
--    λ> maximum (menores 100 (foldr inserta vacio [1..10000]))
--    100
--    (0.02 secs, 0 bytes)

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

-- albcercid joscasgom1 josdeher carmarcar5 pabrabmon josrodgal7 natmarmar2
-- antmorper3 alvfercen fatfervaz paumacpar marlobrip marmerzaf
-- eledejim2 enrnarbej migibagar
restantes :: Ord a => Int -> Monticulo a -> Monticulo a
restantes n m | esVacio m = vacio
              | n == 0    = m
              | otherwise = restantes (n-1) (resto m)

-- margarvil14
restantes2 :: Ord a => Int -> Monticulo a -> Monticulo a
restantes2 n m | n >= l    = vacio
               | otherwise = lista2Monticulo ys
       where l = numeroDeNodos m
             xs = menores2 n m
             ys = (monticulo2Lista m) \\ xs

-- Comentario: La definición anterior se puede mejorar. Por ejemplo,
--    λ> numeroDeNodos (restantes2 150 (foldr inserta vacio [1..10000]))
--    9850
--    (2.50 secs, 424,095,440 bytes)
--    λ> numeroDeNodos (restantes 150 (foldr inserta vacio [1..10000]))
--    9850
--    (0.04 secs, 6,426,504 bytes)
             
-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la función
--    lista2Monticulo :: Ord a => [a] -> Monticulo a
-- tal que (lista2Monticulo xs) es el montículo cuyos nodos son los
-- elementos de la lista xs. Por ejemplo,
--    ghci> lista2Monticulo [2,5,3,7]
--    M 2 1 (M 3 2 (M 7 1 Vacio Vacio) (M 5 1 Vacio Vacio)) Vacio
-- ---------------------------------------------------------------------

-- albcercid josdeher pabrabmon josrodgal7 natmarmar2 antmorper3 paumacpar 
-- enrnarbej migibagar
lista2Monticulo :: Ord a => [a] -> Monticulo a
lista2Monticulo = foldr inserta vacio

-- joscasgom1 alvfercen
--    lista2Monticulo2 :: Ord a => [a] -> Monticulo a
--    lista2Monticulo2  []    = Vacio
--    lista2Monticulo2  [x]   = inserta x Vacio
--    lista2Monticulo2 (x:xs) = inserta x (lista2Monticulo xs)

-- Comentario: El constructor Vacio no se exporta.

-- margarvil14 carmarcar5 fatfervaz marlobrip marmerzaf eledejim2
lista2Monticulo3 :: Ord a => [a] -> Monticulo a
lista2Monticulo3 []     = vacio
lista2Monticulo3 (x:xs) = inserta x (lista2Monticulo3 xs)

-- Comentario: La definición anterior se puede simplificar.

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

-- albcercid joscasgom1 margarvil14 josdeher carmarcar5 pabrabmon
-- josrodgal7 natmarmar2 antmorper3 alvfercen paumacpar fatfervaz
-- marlobrip marmerzaf eledejim2 enrnarbej migibagar
monticulo2Lista :: Ord a => Monticulo a -> [a]
monticulo2Lista m | esVacio m = [] 
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

-- albcercid joscasgom1 margarvil14 josdeher carmarcar5 pabrabmon
-- josrodgal7 natmarmar2 antmorper3 alvfercen paumacpar fatfervaz
-- marlobrip marmerzaf eledejim2 enrnarbej 
ordenada :: Ord a => [a] -> Bool
ordenada []       = True
ordenada [x]      = True
ordenada (x:y:xs) = x <= y && ordenada (y:xs)

-- enrnarbej: se puede simplificar.

-- migibagar
ordenada2 :: Ord a => [a] -> Bool
ordenada2 xs = sort xs == xs

-- Comparación de eficiencia:
--    λ> ordenada2 [1..10^7]
--    True
--    (6.46 secs, 975,891,328 bytes)
--    λ> ordenada [1..10^7]
--    True
--    (18.23 secs, 3,523,681,640 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 8. Comprobar con QuickCheck que para todo montículo m,
-- (monticulo2Lista m) es una lista ordenada creciente.
-- ---------------------------------------------------------------------

-- albcercid joscasgom1 josdeher carmarcar5 pabrabmon josrodgal7
-- natmarmar2 migibagar antmorper3 alvfercen paumacpar fatfervaz
-- marlobrip marmerzaf eledejim2 enrnarbej 

-- La propiedad es
prop_monticulo2Lista_ordenada :: Monticulo Int -> Bool
prop_monticulo2Lista_ordenada = ordenada . monticulo2Lista

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

-- albcercid joscasgom1 margarvil14 josdeher carmarcar5 pabrabmon
-- josrodgal7 natmarmar2 antmorper3 alvfercen paumacpar fatfervaz
-- marlobrip marmerzaf eledejim2 enrnarbej migibagar
ordena :: Ord a => [a] -> [a]
ordena = monticulo2Lista . lista2Monticulo

-- ---------------------------------------------------------------------
-- Ejercicio 11. Comprobar con QuickCheck que para toda lista xs,
-- (ordena xs) es una lista ordenada creciente.
-- ---------------------------------------------------------------------

-- albcercid joscasgom1 josdeher carmarcar5 pabrabmon josrodgal7
-- natmarmar2 migibagar antmorper3 alvfercen paumacpar fatfervaz
-- marlobrip marmerzaf eledejim2 enrnarbej 

-- La propiedad es
prop_ordena_ordenada :: [Int] -> Bool
prop_ordena_ordenada = ordenada . ordena

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

-- albcercid joscasgom1 carmarcar5 pabrabmon josrodgal7 natmarmar2
-- antmorper3 alvfercen paumacpar fatfervaz marlobrip marmerzaf
-- eledejim2 enrnarbej 
borra :: Eq a => a -> [a] -> [a]
borra x   []   = []
borra x (y:xs) | x == y    = xs
               | otherwise = y: borra x xs

-- josdeher
borra2 :: Eq a => a -> [a] -> [a]
borra2 a (x:xs) | notElem a (x:xs) = (x:xs)
                | a == x           = xs
                | otherwise        = x : borra2 a xs

-- Comparación de eficiencia:
--    λ> maximum (borra2 10000 [1..10000])
--    9999
--    (2.10 secs, 9,216,608 bytes)
--    λ> maximum (borra 10000 [1..10000])
--    9999
--    (0.04 secs, 6,972,736 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 14. Definir la función esPermutación tal que 
-- (esPermutación xs ys) se verifique si xs es una permutación de
-- ys. Por ejemplo,  
--    esPermutación [1,2,1] [2,1,1]  ==  True
--    esPermutación [1,2,1] [1,2,2]  ==  False
-- ---------------------------------------------------------------------

-- albcercid joscasgom1 josdeher carmarcar5 pabrabmon josrodgal7
-- antmorper3 paumacpar eledejim2
esPermutacion :: Eq a => [a] -> [a] -> Bool
esPermutacion xs   []   = xs == []
esPermutacion xs (y:ys) = elem y xs && esPermutacion (borra y xs) ys

-- margarvil14 natmarmar2 alvfercen fatfervaz marlobrip marmerzaf
esPermutacion2 :: Eq a => [a] -> [a] -> Bool
esPermutacion2 xs ys = xs `elem` permutations ys 

-- Comparación de eficiencia:
--    λ> esPermutacion2 [12,11..1] [1..12]
--    True
--    (11.51 secs, 17,976,788,200 bytes)
--    λ> esPermutacion [12,11..1] [1..12]
--    True
--    (0.01 secs, 0 bytes)

-- enrnarbej
esPermutacion3 :: Eq a => [a] -> [a] -> Bool
esPermutacion3 []       ys = ys == []   
esPermutacion3 p@(x:xs) ys = length p == length ys &&
                             esPermutacion xs (borra x ys) 

-- ---------------------------------------------------------------------
-- Ejercicio 15. Comprobar con QuickCheck que para toda lista xs,
-- (ordena xs) es una permutación de xs.
-- ---------------------------------------------------------------------

-- albcercid joscasgom1 josdeher carmarcar5 pabrabmon josrodgal7 natmarmar2
-- antmorper3 alvfercen paumacpar fatfervaz marlobrip marmerzaf
-- eledejim2 enrnarbej 

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
