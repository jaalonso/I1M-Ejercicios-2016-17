-- I1M 2015-16: Relación 23 (26 de febrero de 2016)
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
--    http://www.cs.us.es/~jalonso/cursos/i1m-15/temas/tema-14.html
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
import PilaConTipoDeDatoAlgebraico
-- import PilaConListas
-- import I1M.Pila

-- ---------------------------------------------------------------------
-- Ejemplos
-- ---------------------------------------------------------------------

-- A lo largo de esta relación de ejercicios usaremos los siguientes
-- ejemplos de pilas
p1, p2, p3, p4, p5 :: Pila Int
p1 = foldr apila vacia [1..20]
p2 = foldr apila vacia [2,5..18]
p3 = foldr apila vacia [3..10]
p4 = foldr apila vacia [4,-1,7,3,8,10,0,3,3,4]
p5 = foldr apila vacia [1..5]

-- ---------------------------------------------------------------------
-- Ejercicio 1: Definir la función
--    filtraPila :: (a -> Bool) -> Pila a -> Pila a
-- tal que (filtraPila p q) es la pila obtenida con los elementos de la
-- pila q que verifican el predicado p, en el mismo orden. Por ejemplo,
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
-- tal que (mapPila f p) es la pila formada con las imágenes por f de
-- los elementos de la pila p, en el mismo orden. Por ejemplo,
--    ghci> mapPila (+7) p1
--    8|9|10|11|12|13|14|15|16|17|18|19|20|21|22|23|24|25|26|27|-
-- ---------------------------------------------------------------------

-- manvermor jespergue josllagam fracruzam alvalvdom1 juamorrom1 abrdelrod
-- manpende
mapPila :: (a -> a) -> Pila a -> Pila a
mapPila f p | esVacia p = vacia
            | otherwise = apila (f cp) (mapPila f dp)
            where cp = cima p
                  dp = desapila p

-- ---------------------------------------------------------------------
-- Ejercicio 3: Definir la función
--    pertenecePila :: (Eq a) => a -> Pila a -> Bool
-- tal que (pertenecePila y p) se verifica si y sólo si y es un elemento
-- de la pila p. Por ejemplo,
--    pertenecePila 7 p1  == True
--    pertenecePila 70 p1 == False
-- ---------------------------------------------------------------------

-- manvermor jespergue josllagam fracruzam alvalvdom1 juamorrom1 abrdelrod
-- manpende
pertenecePila :: (Eq a) => a -> Pila a -> Bool
pertenecePila y p 
    | esVacia p = False
    | otherwise = cima p == y || pertenecePila y (desapila p)

-- ---------------------------------------------------------------------
-- Ejercicio 4: definir la función
--    contenidaPila :: (Eq a) => Pila a -> Pila a -> Bool
-- tal que (contenidaPila p1 p2) se verifica si y sólo si todos los
-- elementos de p1 son elementos de p2. Por ejemplo,
--    contenidaPila p2 p1 == True
--    contenidaPila p1 p2 == False
-- ---------------------------------------------------------------------

-- manvermor jespergue josllagam
contenidaPila :: (Eq a) => Pila a -> Pila a -> Bool
contenidaPila p1 p2 = all (`elem` ys) xs
    where xs = pila2Lista p1
          ys = pila2Lista p2

-- manvermor fracruzam alvalvdom1 manpende
contenidaPila2 :: (Eq a) => Pila a -> Pila a -> Bool
contenidaPila2 p1 p2 
    | esVacia p1 = True
    | esVacia p2 = esVacia p1
    | otherwise = pertenecePila cp1 p2 && contenidaPila2 dp1 p2
    where cp1 = cima p1
          dp1 = desapila p1

-- Comentario: La definición anterior se puede simplificar.

-- ¿Qué se supone que debe dar contenidaPila vacia vacia?
--    λ> contenidaPila vacia vacia
--    True

-- juamorrom1
contenidaPila3 :: (Eq a) => Pila a -> Pila a -> Bool
contenidaPila3 p1 p2 = isSubsequenceOf (pila2Lista p1) (pila2Lista p2)

-- abrdelrod
contenidaPila4 :: (Eq a) => Pila a -> Pila a -> Bool
contenidaPila4 p1 p2 
    | esVacia p1 = True
    | otherwise  = pertenecePila (cima p1) p2 && contenidaPila4 (desapila p1) p2

-- ---------------------------------------------------------------------
-- Ejercicio 4: Defiir la función
--    prefijoPila :: Eq a => Pila a -> Pila a -> Bool
-- tal que (prefijoPila p1 p2) se verifica si la pila p1 es justamente
-- un prefijo de la pila p2. Por ejemplo,
--    prefijoPila p3 p2 == False
--    prefijoPila p5 p1 == True
-- ---------------------------------------------------------------------

-- manvermor jespergue josllagam juamorrom1
prefijoPila :: Eq a => Pila a -> Pila a -> Bool
prefijoPila p1 p2 = isPrefixOf (pila2Lista p1) (pila2Lista p2)

-- manvermor fracruzam abrdelrod manpende
prefijoPila2 :: Eq a => Pila a -> Pila a -> Bool
prefijoPila2 p1 p2 
    | esVacia p1 = True
    | esVacia p2 = False
    | otherwise = cp1 == cp2 && prefijoPila2 dp1 dp2
    where dp1 = desapila p1
          dp2 = desapila p2
          cp1 = cima p1
          cp2 = cima p2

-- alvalvdom1
prefijoPila3 :: Eq a => Pila a -> Pila a -> Bool
prefijoPila3 p1 p2 
    | esVacia p1 = True
    | esVacia p2 = esVacia p1
    | otherwise = cima p1 == cima p2 && prefijoPila3 (desapila p1) (desapila p2)

-- ¿Qué se supone que debe dar prefijoPila vacia vacia?
--    λ> prefijoPila vacia vacia
--    True

-- ---------------------------------------------------------------------
-- Ejercicio 5: Definir la función
--    subPila :: Eq a => Pila a -> Pila a -> Bool
-- tal que (subPila p1 p2) se verifica si p1 es una subpila de p2.
-- Por ejemplo, 
--    subPila p2 p1 == False
--    subPila p3 p1 == True
-- ---------------------------------------------------------------------

-- josllagam
subPila :: Eq a => Pila a -> Pila a -> Bool
subPila p q = elem (pila2Lista p) (subsequences (pila2Lista q))

-- Comentario: La definición anterior se puede mejorar.

-- manvermor manpende
subPila2 :: (Eq a) => Pila a -> Pila a -> Bool
subPila2 p1 p2 | esVacia p1 = True
               | esVacia p2 = False
               | cp1 == cp2 = prefijoPila dp1 dp2 || subPila2 p1 dp2
               | otherwise = subPila2 p1 dp2
    where cp1 = cima p1
          cp2 = cima p2
          dp1 = desapila p1
          dp2 = desapila p2

-- fracruzam
subPila3 :: (Eq a) => Pila a -> Pila a -> Bool
subPila3 p q | localiza (cima p) q == Nothing = False
             | otherwise = prefijoPila p loc ||
                           subPila3 p (desapila loc)
  where Just loc = localiza (cima p) q
        localiza :: Eq a => a -> Pila a -> Maybe (Pila a)
        localiza x p | esVacia p = Nothing
                     | x == cp   = Just p
                     | otherwise = localiza x dp
          where cp = cima p
                dp = desapila p

-- alvalvdom1 abrdelrod
subPila4 :: (Eq a) => Pila a -> Pila a -> Bool
subPila4 p1 p2 | esVacia p1 = True
               | esVacia p2 = esVacia p1
               | otherwise = prefijoPila p1 p2 || subPila4 p1 (desapila p2)
-- juamorrom1
subPila5 :: (Eq a) => Pila a -> Pila a -> Bool
subPila5 p1 p2 = isInfixOf (pila2Lista p1) (pila2Lista p2)

-- ---------------------------------------------------------------------
-- Ejercicio 6: Definir la función
--    ordenadaPila :: (Ord a) => Pila a -> Bool
-- tal que (ordenadaPila p) se verifica si los elementos de la pila p
-- están ordenados en orden creciente. Por ejemplo,
--    ordenadaPila p1 == True
--    ordenadaPila p4 == False
-- ---------------------------------------------------------------------

-- manvermor jespergue josllagam
ordenadaPila :: (Ord a) => Pila a -> Bool
ordenadaPila p | esVacia p = True
               | otherwise = p == lista2Pila (sort $ pila2Lista p)

-- manvermor fracruzam alvalvdom1 juamorrom1 manpende
ordenadaPila2 :: (Ord a) => Pila a -> Bool
ordenadaPila2 p | esVacia p  = True
                | esVacia dp = True
                | otherwise  = cp <= cdp && ordenadaPila2 dp
    where cp  = cima p
          cdp = cima $ desapila p
          dp  = desapila p

-- abrdelrod
ordenadaPila3 :: (Ord a) => Pila a -> Bool
ordenadaPila3 p 
    | any esVacia [p,dp] = True
    | otherwise          = cima p <= cima dp && ordenadaPila3 dp
    where dp = desapila p

-- ---------------------------------------------------------------------
-- Ejercicio 7.1: Definir una función
--    lista2Pila :: [a] -> Pila a
-- tal que (lista2Pila xs) es una pila formada por los elementos de xs.
-- Por ejemplo,
--    lista2Pila [1..6] == 1|2|3|4|5|6|-
-- ---------------------------------------------------------------------

-- manvermor jespergue alvalvdom1 manpende
lista2Pila :: [a] -> Pila a
lista2Pila xs = foldr apila vacia xs

-- fracruzam josllagam juamorrom1 abrdelrod
lista2Pila2 :: [a] -> Pila a
lista2Pila2 (x:xs) = apila x $ lista2Pila2 xs
lista2Pila2 []     = vacia

-- ---------------------------------------------------------------------
-- Ejercicio 7.2: Definir una función
--    pila2Lista :: Pila a -> [a]
-- tal que (pila2Lista p) es la lista formada por los elementos de p.
-- Por ejemplo,
--    pila2Lista p2 == [2,5,8,11,14,17]
-- ---------------------------------------------------------------------

-- manvermor jespergue fracruzam alvalvdom1 josllagam juamorrom1
-- abrdelrod manpende
pila2Lista :: Pila a -> [a]
pila2Lista p | esVacia p = [] 
             | otherwise = (cima p): pila2Lista (desapila p)

-- fracruzam : No son necesarios los paréntesis en cima p

-- ---------------------------------------------------------------------
-- Ejercicio 7.3: Comprobar con QuickCheck que la función pila2Lista es
-- la inversa de  lista2Pila, y recíprocamente.
-- ---------------------------------------------------------------------

-- manvermor jespergue fracruzam alvalvdom1 josllagam juamorrom1
-- abrdelrod manpende
prop_pila2Lista p = lista2Pila (pila2Lista p) == p

-- ghci> quickCheck prop_pila2Lista
-- +++ OK, passed 100 tests.

-- manvermor fracruzam alvalvdom1 josllagam juamorrom1 abrdelrod manpende
prop_lista2Pila xs = pila2Lista (lista2Pila xs) == xs

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

-- manvermor jespergue juamorrom1
ordenaInserPila :: (Ord a) => Pila a -> Pila a
ordenaInserPila p = lista2Pila $ sort (pila2Lista p)

-- Comentario: La definición anterior no usa el método de inserción.

-- fracruzam josllagam manpende
ordenaInserPila2 :: (Ord a) => Pila a -> Pila a
ordenaInserPila2 = lista2Pila . sort . pila2Lista

-- Comentario: La definición anterior no usa el método de inserción.

-- abrdelrod
ordenaInserPila3 :: Ord a => Pila a -> Pila a
ordenaInserPila3 p 
    | esVacia p = p
    | otherwise = apila minp (ordenaInserPila3 (quita minp p))
    where cp = cima p
          dp = desapila p
          minp = (minimum.pila2Lista) p
          quita x p' | esVacia p' = p'
                     | x == cima p' = desapila p'
                     | otherwise = apila (cima p') (quita x (desapila p'))

-- fracruzam
ordenaInserPila4 :: Ord a => Pila a -> Pila a
ordenaInserPila4 p = ordenaAcu (apila (cima p) vacia) (desapila p)
  where ordenaAcu :: Ord a => Pila a -> Pila a -> Pila a
        ordenaAcu a p | esVacia p = a
                      | otherwise = ordenaAcu (posiciona (cima p) a)
                                              (desapila p)
        posiciona :: Ord a => a -> Pila a -> Pila a
        posiciona x p | esVacia p || x < cima p = apila x p
                      | otherwise = apila (cima p)
                                          (posiciona x (desapila p))

-- ---------------------------------------------------------------------
-- Ejercicio 9.2: Comprobar con QuickCheck que la pila 
---    (ordenaInserPila p) 
-- está ordenada correctamente.
-- ---------------------------------------------------------------------

-- manvermor jespergue fracruzam josllagam juamorrom1 abrdelrod manpende
prop_ordenaInserPila p = ordenadaPila (ordenaInserPila p)

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

-- manvermor jespergue 
nubPila :: (Eq a) => Pila a -> Pila a
nubPila p = lista2Pila $ nub $ pila2Lista p

-- fracruzam josllagam juamorrom1 abrdelrod manpende
nubPila2 :: (Eq a) => Pila a -> Pila a
nubPila2 = lista2Pila . nub . pila2Lista

-- alvalvdom1
nubPila3 :: (Eq a) => Pila a -> Pila a
nubPila3 p | esVacia p = vacia
           | pertenecePila cp dp = ndp
           | otherwise = apila cp ndp
    where cp  = cima p
          dp  = desapila p
          ndp = nubPila dp

-- ---------------------------------------------------------------------
-- Ejercicio 10.2: Definir la propiedad siguiente: "las composición de
-- las funciones nub y pila2Lista coincide con la composición de las
-- funciones pila2Lista y nubPila", y comprobarla con QuickCheck.
-- En caso de ser falsa, redefinir la función nubPila para que se
-- verifique la propiedad.
-- ---------------------------------------------------------------------

-- manvermor josllagam juamorrom1 abrdelrod manpende
-- La propiedad es
prop_nubPila p = (nub . pila2Lista) p == (pila2Lista . nubPila) p

-- La comprobación es
-- +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 11: Definir la función 
--    maxPila :: Ord a => Pila a -> a
-- tal que (maxPila p) sea el mayor de los elementos de la pila p. Por
-- ejemplo, 
--    ghci> p4
--    4|-1|7|3|8|10|0|3|3|4|-
--    ghci> maxPila p4
--    10
-- ---------------------------------------------------------------------

-- manvermor jespergue josllagam juamorrom1
maxPila :: Ord a => Pila a -> a
maxPila p = maximum (pila2Lista p)

-- fracruzam
maxPila2 :: Ord a => Pila a -> a
maxPila2 p = auxMax (cima p) (desapila p)
  where auxMax :: Ord a => a -> Pila a -> a
        auxMax x p | esVacia p = x
                   | x > cp    = auxMax x dp
                   | otherwise = auxMax cp dp
          where cp = cima p
                dp = desapila p

-- abrdelrod manpende
maxPila3 :: (Ord a) => Pila a -> a
maxPila3 = maximum . pila2Lista

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
