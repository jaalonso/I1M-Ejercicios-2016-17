-- I1M 2015-16: Relación 24 (26 de febrero de 2016)
-- El TAD de las colas.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- El objetivo de esta relación de ejercicios es definir funciones sobre 
-- el TAD de las colas, utilizando las implementaciones estudiadas en el 
-- tema 15 transparencias se encuentran en 
--    http://www.cs.us.es/~jalonso/cursos/i1m-15/temas/tema-15.html
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
-- de las colas:
-- + ColaConListas.hs    que está en http://bit.ly/21z3wQL
-- + ColaConDosListas.hs que está en http://bit.ly/21z3AQp

-- ---------------------------------------------------------------------
-- Importación de librerías                                           --
-- ---------------------------------------------------------------------

import Data.List
import Data.Maybe
import Test.QuickCheck

-- Hay que elegir una implementación del TAD colas:
import ColaConListas
-- import ColaConDosListas
-- import I1M.Cola
    
-- ---------------------------------------------------------------------
-- Nota. A lo largo de la relación de ejercicios usaremos los siguientes
-- ejemplos de colas:
c1, c2, c3, c4, c5, c6 :: Cola Int
c1 = foldr inserta vacia [1..20]
c2 = foldr inserta vacia [2,5..18]
c3 = foldr inserta vacia [3..10]
c4 = foldr inserta vacia [4,-1,7,3,8,10,0,3,3,4]
c5 = foldr inserta vacia [15..20]
c6 = foldr inserta vacia (reverse [1..20])
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1: Definir la función
--    ultimoCola :: Cola a -> a
-- tal que (ultimoCola c) es el último elemento de la cola c. Por
-- ejemplo:
--    ultimoCola c4 == 4
--    ultimoCola c5 == 15
-- ---------------------------------------------------------------------

-- manvermor alvalvdom1 josllagam jespergue abrdelrod fracruzam
-- juamorrom1 pabmorgar carruirui3 marvilmor erisancha juanarcon
-- rubvilval manpende blaruiher silgongal isrbelnun
ultimoCola :: Cola a -> a
ultimoCola c 
    | esVacia c  = error "Cola Vacia"
    | esVacia rc = primero c
    | otherwise  = ultimoCola rc
    where rc = resto c

-- ---------------------------------------------------------------------
-- Ejercicio 2: Definir la función
--    longitudCola :: Cola a -> Int
-- tal que (longitudCola c) es el número de elementos de la cola c. Por
-- ejemplo, 
--     longitudCola c2 == 6
-- ---------------------------------------------------------------------

-- manvermor blaruiher
longitudCola :: Cola a -> Int
longitudCola c | esVacia c  = 0
               | esVacia rc = 1
               | otherwise  = 1 + longitudCola rc
    where rc = resto c

-- Comentario: La definición anterior se puede simplificar.

-- alvalvdom1 josllagam jespergue abrdelrod fracruzam juamorrom1 
-- pabmorgar carruirui3 marvilmor erisancha juanarcon rubvilval
-- manpende silgongal isrbelnun
longitudCola2 :: Cola a -> Int
longitudCola2 c | esVacia c = 0
                | otherwise = 1 + longitudCola (resto c)

-- ---------------------------------------------------------------------
-- Ejercicio 3: Definir la función 
--    todosVerifican :: (a -> Bool) -> Cola a -> Bool
-- tal que (todosVerifican p c) se verifica si todos los elementos de la
-- cola c cumplen la propiedad p. Por ejemplo,
--    todosVerifican (>0) c1 == True
--    todosVerifican (>0) c4 == False
-- ---------------------------------------------------------------------

-- manvermor alvalvdom1 josllagam jespergue abrdelrod fracruzam
-- juamorrom1 pabmorgar carruirui3 marvilmor erisancha juanarcon
-- rubvilval manpende blaruiher silgongal isrbelnun
todosVerifican :: (a -> Bool) -> Cola a -> Bool
todosVerifican p c 
    | esVacia c = True
    | otherwise = p (primero c) && todosVerifican p (resto c)

-- ---------------------------------------------------------------------
-- Ejercicio 4: Definir la función
--    algunoVerifica :: (a -> Bool) -> Cola a -> Bool
-- tal que (algunoVerifica p c) se verifica si algún elemento de la cola
-- c cumple la propiedad p. Por ejemplo,
--   algunoVerifica (<0) c1 == False
--   algunoVerifica (<0) c4 == True
-- ---------------------------------------------------------------------

-- manvermor alvalvdom1 josllagam jespergue abrdelrod fracruzam
-- juamorrom1 pabmorgar carruirui3 marvilmor erisancha juanarcon
-- rubvilval manpende blaruiher silgongal isrbelnun
algunoVerifica :: (a -> Bool) -> Cola a -> Bool
algunoVerifica p c 
    | esVacia c = False
    | otherwise = p (primero c) || algunoVerifica p (resto c)

-- ---------------------------------------------------------------------
-- Ejercicio 5: Definir la función
--    ponAlaCola :: Cola a -> Cola a -> Cola a
-- tal que (ponAlaCola c1 c2) es la cola que resulta de poner los
-- elementos de c2 a la cola de c1. Por ejemplo,
--    ponAlaCola c2 c3 == C [17,14,11,8,5,2,10,9,8,7,6,5,4,3]
-- ---------------------------------------------------------------------

-- manvermor alvalvdom1 josllagam jespergue abrdelrod fracruzam
-- juamorrom1 pabmorgar carruirui3 marvilmor erisancha juanarcon
-- rubvilval manpende blaruiher silgongal isrbelnun
ponAlaCola :: Cola a -> Cola a -> Cola a 
ponAlaCola c1 c2 
    | esVacia c2 = c1
    | otherwise = ponAlaCola (inserta pc2 c1) rc2
    where pc2 = primero c2
          rc2 = resto c2

-- ---------------------------------------------------------------------
-- Ejercicio 6: Definir la función
--    mezclaColas :: Cola a -> Cola a -> Cola a
-- tal que (mezclaColas c1 c2) es la cola formada por los elementos de
-- c1 y c2 colocados en una cola, de forma alternativa, empezando por
-- los elementos de c1. Por ejemplo,
--    mezclaColas c2 c4 == C [17,4,14,3,11,3,8,0,5,10,2,8,3,7,-1,4]
-- ---------------------------------------------------------------------

-- manvermor josllagam jespergue juamorrom1 alvalvdom1
mezclaColas :: Cola a -> Cola a -> Cola a
mezclaColas c1 c2 = mezclaAux c1 c2 vacia
   where mezclaAux c1 c2 c 
             | esVacia c1 = ponAlaCola c c2
             | esVacia c2 = ponAlaCola c c1
             | otherwise  = mezclaAux rc1 rc2 (inserta pc2(inserta pc1 c))
             where pc1 = primero c1
                   pc2 = primero c2
                   rc1 = resto c1
                   rc2 = resto c2

-- abrdelrod pabmorgar rubvilval manpende blaruiher isrbelnun
mezclaColas2 :: Cola a -> Cola a -> Cola a
mezclaColas2 c1 c2 
    | esVacia c1 = c2
    | esVacia c2 = c1
    | otherwise = ponAlaCola c' c''
    where c'  = foldr inserta vacia [primero c2,primero c1] 
          c'' = mezclaColas2 (resto c1) (resto c2)

-- fracruzam carruirui3 marvilmor erisancha juanarcon
-- fracruzam : Mejor invertir el orden de los datos.
mezclaColas3 :: Cola a -> Cola a -> Cola a
mezclaColas3 = mezclaAcu vacia
    where mezclaAcu c d e
              | esVacia e = ponAlaCola c d
              | esVacia d = ponAlaCola c e
              | otherwise = mezclaAcu (inserta pe $ inserta pd c)
                                      (resto d) (resto e)
              where pe = primero e
                    pd = primero d

-- ---------------------------------------------------------------------
-- Ejercicio 7: Definir la función
--    agrupaColas :: [Cola a] -> Cola a
-- tal que (agrupaColas [c1,c2,c3,...,cn]) es la cola formada mezclando
-- las colas de la lista como sigue: mezcla c1 con c2, el resultado con
-- c3, el resultado con c4, y así sucesivamente. Por ejemplo,
--    ghci> agrupaColas [c3,c3,c4]
--    C [10,4,10,3,9,3,9,0,8,10,8,8,7,3,7,7,6,-1,6,4,5,5,4,4,3,3]
-- ---------------------------------------------------------------------

-- manvermor josllagam jespergue juamorrom1 alvalvdom1 pabmorgar juanarcon
-- rubvilval 
agrupaColas :: [Cola a] -> Cola a
agrupaColas []         = vacia
agrupaColas [c]        = c
agrupaColas (c1:c2:cn) = agrupaColas $ mezclaColas c1 c2 : cn

-- abrdelrod manpende
agrupaColas2 :: [Cola a] -> Cola a
agrupaColas2 = foldl mezclaColas vacia

-- fracruzam carruirui3 marvilmor erisancha
agrupaColas3 :: [Cola a] -> Cola a
agrupaColas3 = foldl1 mezclaColas

-- isrbelnun
agrupaColas4 [vacia]  = vacia
agrupaColas4 (x:y:xs) = agrupaColas ((mezclaColas x y):xs)

-- ---------------------------------------------------------------------
-- Ejercicio 8: Definir la función
--    perteneceCola :: Eq a => a -> Cola a -> Bool
-- tal que (perteneceCola x c) se verifica si x es un elemento de la
-- cola c. Por ejemplo, 
--    perteneceCola 7 c1  == True
--    perteneceCola 70 c1 == False
-- ---------------------------------------------------------------------

-- manvermor josllagam jespergue abrdelrod fracruzam juamorrom1
-- alvalvdom1 pabmorgar manpende blaruiher silgongal isrbelnun
perteneceCola :: Eq a => a -> Cola a -> Bool
perteneceCola y c 
    | esVacia c = False
    | otherwise = primero c == y || perteneceCola y (resto c)
                                

-- carruirui3 marvilmor erisancha juanarcon rubvilval
perteneceCola2 :: Eq a => a -> Cola a -> Bool
perteneceCola2 x = algunoVerifica (==x)

-- ---------------------------------------------------------------------
-- Ejercicio 9: Definir la función
--    contenidaCola :: Eq a => Cola a -> Cola a -> Bool
-- tal que (contenidaCola c1 c2) se verifica si todos los elementos de
-- c1 son elementos de c2. Por ejemplo, 
--    contenidaCola c2 c1 == True
--    contenidaCola c1 c2 == False
-- ---------------------------------------------------------------------

-- manvermor josllagam jespergue pabmorgar juanarcon rubvilval blaruiher
-- isrbelnun
contenidaCola :: Eq a => Cola a -> Cola a -> Bool
contenidaCola c1 c2 
    | esVacia c1 = True
    | esVacia c2 = esVacia c1
    | otherwise  = perteneceCola pc1 c2 && contenidaCola rc1 c2
    where pc1 = primero c1
          rc1 = resto c1

-- abrdelrod fracruzam juamorrom1 alvalvdom1
contenidaCola2 :: Eq a => Cola a -> Cola a -> Bool
contenidaCola2 c1 c2 
    | esVacia c1 = True
    | otherwise = perteneceCola (primero c1) c2 &&
                  contenidaCola2 (resto c1) c2

-- carruirui3 marvilmor erisancha manpende
contenidaCola3 :: Eq a => Cola a -> Cola a -> Bool
contenidaCola3 c1 c2 = todosVerifican (flip perteneceCola c2) c1 

-- ---------------------------------------------------------------------
-- Ejercicio 10: Definir la función
--    prefijoCola :: Eq a => Cola a -> Cola a -> Bool
-- tal que (prefijoCola c1 c2) se verifica si la cola c1 es un prefijo
-- de la cola c2. Por ejemplo, 
--    prefijoCola c3 c2 == False
--    prefijoCola c5 c1 == True
-- ---------------------------------------------------------------------

-- manvermor josllagam abrdelrod fracruzam juamorrom1 jespergue
-- alvalvdom1 carruirui3 marvilmor erisancha juanarcon rubvilval
-- manpende blaruiher isrbelnun
prefijoCola :: Eq a => Cola a -> Cola a -> Bool
prefijoCola c1 c2 
    | esVacia c1 = True
    | esVacia c2 = False
    | otherwise = pc1 == pc2 && prefijoCola rc1 rc2
    where pc1 = primero c1
          pc2 = primero c2
          rc1 = resto c1
          rc2 = resto c2

-- ---------------------------------------------------------------------
-- Ejercicio 11: Definir la función
--    subCola :: Eq a => Cola a -> Cola a -> Bool
-- tal que (subCola c1 c2) se verifica si c1 es una subcola de c2. Por
-- ejemplo,  
--    subCola c2 c1 == False
--    subCola c3 c1 == True
-- ---------------------------------------------------------------------

-- manvermor josllagam abrdelrod juamorrom1 jespergue alvalvdom1 
-- manpende isrbelnun
subCola :: Eq a => Cola a -> Cola a -> Bool
subCola c1 c2 | esVacia c1 = True
              | esVacia c2 = False
              | pc1 == pc2 = prefijoCola rc1 rc2 || subCola c1 rc2
              | otherwise = subCola c1 rc2
   where pc1 = primero c1
         pc2 = primero c2
         rc1 = resto c1
         rc2 = resto c2

-- ¿Qué debe dar subCola vacia vacia?
--    λ> subCola vacia vacia
--    True

-- fracruzam
-- fracruzam : fromJust e isNothing están definidos en Data.Maybe
subCola2 :: Eq a => Cola a -> Cola a -> Bool
subCola2 c d | esVacia c     = True
             | isNothing loc = False
             | otherwise     = prefijoCola c floc ||
                               subCola c (resto floc)
             where loc  = localiza (primero c) d
                   floc = fromJust loc
                   localiza :: Eq a => a -> Cola a -> Maybe (Cola a)
                   localiza x c | esVacia c        = Nothing
                                | x == primero c   = Just c
                                | otherwise        = localiza x (resto c)

-- carruirui3 marvilmor erisancha juanarcon rubvilval
subCola3 :: Eq a => Cola a -> Cola a -> Bool
subCola3 c1 c2 | esVacia c1 = True
               | esVacia c2 = False
               | otherwise = prefijoCola c1 c2 || subCola c1 (resto c2)

-- ---------------------------------------------------------------------
-- Ejercicio 12: Definir la función
--    ordenadaCola :: Ord a => Cola a -> Bool
-- tal que (ordenadaCola c) se verifica si los elementos de la cola c
-- están ordenados en orden creciente. Por ejemplo,
--    ordenadaCola c6 == True
--    ordenadaCola c4 == False
-- ---------------------------------------------------------------------

-- manvermor josllagam fracruzam juamorrom1 jespergue alvalvdom1
-- carruirui3 juanarcon rubvilval manpende isrbelnun
ordenadaCola :: Ord a => Cola a -> Bool
ordenadaCola c | esVacia c  = True
               | esVacia rc = True
               | otherwise = pc <= prc && ordenadaCola rc
     where pc = primero c
           prc = primero rc
           rc = resto c

-- abrdelrod marvilmor erisancha
ordenadaCola2 :: Ord a => Cola a -> Bool
ordenadaCola2 c 
    | any esVacia [c,rc] = True
    | otherwise = primero c <= primero rc && ordenadaCola2 rc
    where rc = resto c

-- ---------------------------------------------------------------------
-- Ejercicio 13.1: Definir una función
--    lista2Cola :: [a] -> Cola a
-- tal que (lista2Cola xs) es una cola formada por los elementos de xs.
-- Por ejemplo,
--    lista2Cola [1..6] == C [1,2,3,4,5,6]
-- ---------------------------------------------------------------------

-- manvermor jespergue alvalvdom1 rubvilval manpende isrbelnun
lista2Cola :: [a] -> Cola a
lista2Cola xs = foldr inserta vacia (reverse xs)

-- abrdelrod josllagam juamorrom1 pabmorgar marvilmor juanarcon
lista2Cola2 :: [a] -> Cola a 
lista2Cola2 [] = vacia
lista2Cola2 xs = inserta (last xs) (lista2Cola2 (init xs))

-- fracruzam
lista2Cola3 :: [a] -> Cola a
lista2Cola3 = lista2ColaAcu vacia
  where lista2ColaAcu :: Cola a -> [a] -> Cola a
        lista2ColaAcu c [] = c
        lista2ColaAcu c (x:xs) = lista2ColaAcu (inserta x c) xs

-- carruirui3 erisancha
lista2Cola4 :: [a] -> Cola a
lista2Cola4 xs = foldl (flip inserta) vacia xs

-- ---------------------------------------------------------------------
-- Ejercicio 13.2: Definir una función
--    cola2Lista :: Cola a -> [a]
-- tal que (cola2Lista c) es la lista formada por los elementos de p.
-- Por ejemplo,
--    cola2Lista c2 == [17,14,11,8,5,2]
-- ---------------------------------------------------------------------

-- manvermor abrdelrod fracruzam josllagam juamorrom1 jespergue
-- alvalvdom1 pabmorgar carruirui3 marvilmor erisancha juanarcon
-- rubvilval manpende isrbelnun
cola2Lista :: Cola a -> [a]
cola2Lista c | esVacia c = []
             | otherwise = primero c : cola2Lista (resto c)

-- ---------------------------------------------------------------------
-- Ejercicio 13.3. Comprobar con QuickCheck que la función cola2Lista es
-- la inversa de  lista2Cola, y recíprocamente.
-- ---------------------------------------------------------------------

-- manvermor jespergue alvalvdom1 pabmorgar carruirui3 erisancha
-- manpende isrbelnun
prop_cola2Lista :: Cola Int -> Bool
prop_cola2Lista c = lista2Cola (cola2Lista c) == c

-- abrdelrod fracruzam josllagam juamorrom1 pabmorgar juanarcon
-- rubvilval 
prop_cola2Lista2 :: Cola Int -> Bool
prop_cola2Lista2 c = (lista2Cola . cola2Lista) c == c

-- ghci> quickCheck prop_cola2Lista
-- +++ OK, passed 100 tests.

-- manvermor alvalvdom1 carruirui3 marvilmor erisancha manpende isrbelnun
prop_lista2Cola :: [Int] -> Bool
prop_lista2Cola xs = cola2Lista (lista2Cola xs) == xs

-- abrdelrod fracruzam josllagam juamorrom1 juanarcon rubvilval
prop_lista2Cola2 :: [Int] -> Bool
prop_lista2Cola2 xs = (cola2Lista . lista2Cola) xs == xs

-- ghci> quickCheck prop_lista2Cola
-- +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 14: Definir la función 
--    maxCola :: Ord a => Cola a -> a
-- tal que (maxCola c) es el mayor de los elementos de la cola c. Por
-- ejemplo, 
--    maxCola c4 == 10
-- ---------------------------------------------------------------------

-- manvermor alvalvdom1 pabmorgar manpende
maxCola :: Ord a => Cola a -> a
maxCola c = maximum (cola2Lista c)

-- abrdelrod josllagam juamorrom1 jespergue marvilmor juanarcon
maxCola2 :: Ord a => Cola a -> a
maxCola2 c | esVacia c = error "cola vacia"
           | otherwise = (maximum.cola2Lista) c

-- fracruzam carruirui3 erisancha
maxCola3 :: Ord a => Cola a -> a
maxCola3 p | esVacia p = error "Cola Vacía"
          | otherwise =  maximo (primero p) (resto p)
  where maximo :: Ord a => a -> Cola a -> a
        maximo x c | esVacia c = x
                   | pc > x    = maximo pc rc
                   | otherwise = maximo x rc
          where pc = primero c
                rc = resto c

-- rubvilval
maxCola4 :: Ord a => Cola a -> a
maxCola4 p | todosVerifican (<= primero p) p = primero p
           | otherwise = maxCola (resto p)

-- isrbelnun
maxCola5 :: Ord a => Cola a -> a
maxCola5 c | esVacia c           = error "lista vacia"
           | longitudCola c == 1 = p
           | p >= pr             = maxCola (inserta p (resto r))
           | otherwise           = maxCola r
    where p  = primero c
          r  = resto c
          pr = primero r

-- ---------------------------------------------------------------------
-- Generador de colas                                          --
-- ---------------------------------------------------------------------

-- genCola es un generador de colas de enteros. Por ejemplo,
--    ghci> sample genCola
--    C ([],[])
--    C ([],[])
--    C ([],[])
--    C ([],[])
--    C ([7,8,4,3,7],[5,3,3])
--    C ([],[])
--    C ([1],[13])
--    C ([18,28],[12,21,28,28,3,18,14])
--    C ([47],[64,45,7])
--    C ([8],[])
--    C ([42,112,178,175,107],[])
genCola :: (Num a, Arbitrary a) => Gen (Cola a)
genCola = frequency [(1, return vacia),
                     (30, do n <- choose (10,100)
                             xs <- vectorOf n arbitrary
                             return (creaCola xs))]
          where creaCola = foldr inserta vacia

-- El tipo cola es una instancia del arbitrario.
instance (Arbitrary a, Num a) => Arbitrary (Cola a) where
    arbitrary = genCola
