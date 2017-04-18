-- I1M 2016-17: Relación 23 (8 de marzo de 2017)
-- El TAD de las pilas.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- El objetivo de esta relación de ejercicios es definir funciones sobre 
-- el TAD de las pilas, utilizando las implementaciones estudiadas en el 
-- tema 14 cuyas transparencias se encuentran en 
--    http://www.cs.us.es/~jalonso/cursos/i1m-16/temas/tema-14.html
-- 
-- Para realizar los ejercicios hay que instalar la librería I1M que
-- contiene la implementación de TAD de las pilas. Los pasos para
-- instalarla son los siguientes:
-- + Descargar el paquete I1M desde http://bit.ly/1pbnDqm
-- + Descomprimirlo (y se crea el directorio I1M-master.zip).
-- + Cambiar al directorio I1M-master.
-- + Ejecutar cabal install I1M.cabal
--
-- Otra forma es descargar las implementaciones de las implementaciones
-- de las pilas:
-- + PilaConTipoDeDatoAlgebraico.hs que está en http://bit.ly/21z3g49
-- + PilaConListas.hs               que está en http://bit.ly/21z3oAD

-- ---------------------------------------------------------------------
-- Importación de librerías                                           --
-- ---------------------------------------------------------------------

import Data.List
import Test.QuickCheck

-- Hay que elegir una implementación del TAD pilas.
-- import PilaConTipoDeDatoAlgebraico
-- import PilaConListas
import I1M.Pila

-- ---------------------------------------------------------------------
-- A lo largo de la relación de ejercicios usaremos los siguientes
-- ejemplos de pilas:
-- ---------------------------------------------------------------------

p1 = foldr apila vacia [1..20]
p2 = foldr apila vacia [2,5..18]
p3 = foldr apila vacia [3..10]
p4 = foldr apila vacia [4,(-1),7,3,8,10,0,3,3,4]
p5 = foldr apila vacia [1..5]

-- ---------------------------------------------------------------------
-- Ejercicio 1: Definir la función
--    filtraPila :: (a -> Bool) -> Pila a -> Pila a
-- tal que (filtraPila p pila) es la pila con los elementos de pila
-- que verifican el predicado p, en el mismo orden. Por ejemplo,
--    ghci> p1
--    1|2|3|4|5|6|7|8|9|10|11|12|13|14|15|16|17|18|19|20|-
--    ghci> filtraPila even p1
--    2|4|6|8|10|12|14|16|18|20|-

-- ---------------------------------------------------------------------

filtraPila :: (a -> Bool) -> Pila a -> Pila a
filtraPila p q
    | esVacia q = vacia
    | p cq      = apila cq (filtraPila p dq)
    | otherwise = filtraPila p dq
    where cq = cima q
          dq = desapila q

-- ---------------------------------------------------------------------
-- Ejercicio 2: Definir la función
--    mapPila :: (a -> a) -> Pila a -> Pila a
-- tal que (mapPila f pila) es la pila formada con las imágenes por f de
-- los elementos de pila, en el mismo orden. Por ejemplo,
--    ghci> mapPila (+7) p1
--    8|9|10|11|12|13|14|15|16|17|18|19|20|21|22|23|24|25|26|27|-
-- ---------------------------------------------------------------------

mapPila :: (a -> a) -> Pila a -> Pila a
mapPila = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 3: Definir la función
--    pertenecePila :: (Eq a) => a -> Pila a -> Bool
-- tal que (pertenecePila y p) se verifica si y sólo si y es un elemento
-- de la pila p. Por ejemplo,
--    pertenecePila 7 p1  == True
--    pertenecePila 70 p1 == False
-- ---------------------------------------------------------------------

pertenecePila :: (Eq a) => a -> Pila a -> Bool
pertenecePila = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 4: definir la función
--    contenidaPila :: (Eq a) => Pila a -> Pila a -> Bool
-- tal que (contenidaPila p1 p2) se verifica si y sólo si todos los
-- elementos de p1 son elementos de p2. Por ejemplo,
--    contenidaPila p2 p1 == True
--    contenidaPila p1 p2 == False
-- ---------------------------------------------------------------------

contenidaPila :: (Eq a) => Pila a -> Pila a -> Bool
contenidaPila = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 4: Defiir la función
--    prefijoPila :: (Eq a) => Pila a -> Pila a -> Bool
-- tal que (prefijoPila p1 p2) se verifica si la pila p1 es justamente
-- un prefijo de la pila p2. Por ejemplo,
--    prefijoPila p3 p2 == False
--    prefijoPila p5 p1 == True
-- ---------------------------------------------------------------------

prefijoPila :: (Eq a) => Pila a -> Pila a -> Bool
prefijoPila = undefined 

-- ---------------------------------------------------------------------
-- Ejercicio 5: Definir la función
--    subPila :: (Eq a) => Pila a -> Pila a -> Bool
-- tal que (subPila p1 p2) se verifica si p1 es una subpila de p2.
-- Por ejemplo, 
--    subPila p2 p1 == False
--    subPila p3 p1 == True
-- ---------------------------------------------------------------------

subPila :: (Eq a) => Pila a -> Pila a -> Bool
subPila = undefined 

-- ---------------------------------------------------------------------
-- Ejercicio 6: Definir la función
--    ordenadaPila :: (Ord a) => Pila a -> Bool
-- tal que (ordenadaPila p) se verifica si los elementos de la pila p
-- están ordenados en orden creciente. Por ejemplo,
--    ordenadaPila p1 == True
--    ordenadaPila p4 == False
-- ---------------------------------------------------------------------

ordenadaPila :: (Ord a) => Pila a -> Bool
ordenadaPila = undefined 

-- ---------------------------------------------------------------------
-- Ejercicio 7.1: Definir una función
--    lista2Pila :: [a] -> Pila a
-- tal que (lista2Pila xs) es una pila formada por los elementos de xs.
-- Por ejemplo,
--    lista2Pila [1..6] == 1|2|3|4|5|6|-

-- ---------------------------------------------------------------------

lista2Pila :: [a] -> Pila a
lista2Pila xs = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 7.2: Definir una función
--  pila2Lista :: Pila a -> [a]
-- tal que (pila2Lista p) es la lista formada por los elementos de p.
-- Por ejemplo,
--    pila2Lista p2 == [2,5,8,11,14,17]
-- ---------------------------------------------------------------------

pila2Lista :: Pila a -> [a]
pila2Lista = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 7.3: Comprobar con QuickCheck que la función pila2Lista es
-- la inversa de  lista2Pila, y recíprocamente.
-- ---------------------------------------------------------------------

prop_pila2Lista p = undefined

-- ghci> quickCheck prop_pila2Lista
-- +++ OK, passed 100 tests.

prop_lista2Pila xs = undefined

-- ghci> quickCheck prop_lista2Pila
-- +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 9.1: Definir la función 
--    ordenaInserPila :: (Ord a) => Pila a -> Pila a
-- tal que (ordenaInserPila p) es una pila con los elementos de la pila
-- p, ordenados por inserción. Por ejemplo,
--    ghci> ordenaInserPila p4
--    -1|0|3|3|3|4|4|7|8|10|-
-- ---------------------------------------------------------------------

ordenaInserPila :: (Ord a) => Pila a -> Pila a
ordenaInserPila = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 9.2: Comprobar con QuickCheck que la pila 
---    (ordenaInserPila p) 
-- está ordenada correctamente.

prop_ordenaInserPila p = undefined

-- ghci> quickCheck prop_ordenaInserPila
-- +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 10.1: Definir la función
--    nubPila :: (Eq a) => Pila a -> Pila a
-- tal que (nubPila p) es una pila con los elementos de p sin
-- repeticiones. Por ejemplo,
--    ghci> p4
--    4|-1|7|3|8|10|0|3|3|4|-
--    ghci> nubPila p4
--    -1|7|8|10|0|3|4|-
-- ---------------------------------------------------------------------

nubPila :: (Eq a) => Pila a -> Pila a
nubPila = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 10.2: Definir la propiedad siguiente: "las composición de
-- las funciones nub y pila2Lista coincide con la composición de las
-- funciones pila2Lista y nubPila", y comprobarla con quickCheck.
-- En caso de ser falsa, redefinir la función nubPila para que se
-- verifique la propiedad.
-- ---------------------------------------------------------------------

-- La propiedad es

prop_nubPila p = undefined

-- La comprobación es

-- ---------------------------------------------------------------------
-- Ejercicio 11: Definir la función 
--    maxPila :: (Ord a) => Pila a -> a
-- tal que (maxPila p) sea el mayor de los elementos de la pila p. Por
-- ejemplo, 
--    ghci> p4
--    4|-1|7|3|8|10|0|3|3|4|-
--    ghci> maxPila p4
--    10
-- ---------------------------------------------------------------------

maxPila :: (Ord a) => Pila a -> a
maxPila = undefined

-- ---------------------------------------------------------------------
-- Generador de pilas                                                 --
-- ---------------------------------------------------------------------

-- genPila es un generador de pilas. Por ejemplo,
--    ghci> sample genPila
--    -
--    0|0|-
--    -
--    -6|4|-3|3|0|-
--    -
--    9|5|-1|-3|0|-8|-5|-7|2|-
--    -3|-10|-3|-12|11|6|1|-2|0|-12|-6|-
--    2|-14|-5|2|-
--    5|9|-
--    -1|-14|5|-
--    6|13|0|17|-12|-7|-8|-19|-14|-5|10|14|3|-18|2|-14|-11|-6|-
genPila :: (Arbitrary a, Num a) => Gen (Pila a)
genPila = do xs <- listOf arbitrary
             return (foldr apila vacia xs)
  
-- El tipo pila es una instancia del arbitrario. 
instance (Arbitrary a, Num a) => Arbitrary (Pila a) where
    arbitrary = genPila


