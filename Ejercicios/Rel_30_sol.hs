-- I1M 2015-16: Relación 30 (1 de abril de 2016)
-- El TAD de los multiconjuntos mediante diccionarios.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- Un multiconjunto es una coleeción de elementos en los que no importa
-- el orden de los elementos, pero sí el número de veces en que
-- aparecen. Por ejemplo, la factorización prima de un número se puede
-- representar como un multiconjunto de números primos. 
-- 
-- El objetivo de esta relación de ejercicios es implementar el TAD de
-- los multiconjuntos utilizando los diccionarios estudiados en el tema
-- 29 https://www.cs.us.es/~jalonso/cursos/i1m/temas/tema-29.html
-- 
-- El manual, con ejemplos, de la librería Data.Map se encuentra en
-- http://bit.ly/25B1na0

-- ---------------------------------------------------------------------
-- Librerías auxiliares                                               --
-- ---------------------------------------------------------------------

import qualified Data.Map as M

-- ---------------------------------------------------------------------
-- El tipo de dato de multiconjuntos                                  --
-- ---------------------------------------------------------------------

-- Un multiconjunto se puede representar mediante un diccionario donde
-- las claves son los elementos del multiconjunto y sus valores sus
-- números de ocurrencias. Por ejemplo, el multiconjunto 
--    {a, b, a, c, b, a, e}
-- se representa por el diccionario
--    [(a,3), (b,2), (c,1), (e,1)]

type MultiConj a = M.Map a Int
 
-- ---------------------------------------------------------------------
-- Construcciones de multiconjuntos                                   --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la constante
--    vacio :: MultiConj a
-- para el multiconjunto vacío. Por ejemplo,
--    vacio  ==  fromList []
-- ---------------------------------------------------------------------

-- manvermor silgongal alvalvdom1 erisancha
vacio :: MultiConj a
vacio = M.empty

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    unitario :: a -> MultiConj a
-- tal que (unitario x) es el multiconjunto cuyo único elemento es
-- x. Por ejemplo,
--    unitario 'a'  ==  fromList [('a',1)]
-- ---------------------------------------------------------------------

-- manvermor silgongal alvalvdom1 erisancha
unitario :: a -> MultiConj a
unitario x = M.singleton x 1

-- ---------------------------------------------------------------------
-- Añadir y quitar elementos                                          --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--    inserta :: Ord a => a -> MultiConj a -> MultiConj a
-- tal que (inserta x m) es el multiconjunto obtenido añadiéndole a m el 
-- elemento x. Por ejemplo,
--    ghci> inserta 'a' (unitario 'a')
--    fromList [('a',2)]
--    ghci> inserta 'b' it
--    fromList [('a',2),('b',1)]
--    ghci> inserta 'a' it
--    fromList [('a',3),('b',1)]
--    ghci> inserta 'b' it
--    fromList [('a',3),('b',2)]
-- ---------------------------------------------------------------------

-- manvermor
inserta :: Ord a => a -> MultiConj a -> MultiConj a
inserta x m | M.member x m = M.adjust (+1) x m
            | otherwise    = M.insert x 1 m

-- Comentario: La definición anterior se puede simplificar.

-- silgongal alvalvdom1 erisancha
inserta2 :: Ord a => a -> MultiConj a -> MultiConj a
inserta2 x = M.insertWith (+) x 1

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función 
--    listaAmc :: Ord a => [a] -> MultiConj a
-- tal que (listaAmc xs) es el multiconjunto cuyos elementos son los de
-- la lista xs. Por ejemplo,
--    listaAmc "ababc"  ==  fromList [('a',2),('b',2),('c',1)]
-- ---------------------------------------------------------------------

-- manvermor
listaAmc :: Ord a => [a] -> MultiConj a
listaAmc []     = vacio
listaAmc (x:xs) = inserta x (listaAmc xs)

-- Comentario: La definición anterior se puede simplificar.

-- silgongal alvalvdom1 erisancha
listaAmc2 :: Ord a => [a] -> MultiConj a
listaAmc2 xs = M.fromListWith (+) [(x,1) | x <- xs ]

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la función
--    insertaVarios :: Ord a => a -> Int -> MultiConj a -> MultiConj a
-- tal que (insertaVarios x n m) es el multiconjunto obtenido
-- añadiéndole a m n copias del elemento x. Por ejemplo, 
--    ghci> insertaVarios 'a' 3 vacio
--    fromList [('a',3)]
--    ghci> insertaVarios 'b' 2 it 
--    fromList [('a',3),('b',2)]
--    ghci> insertaVarios 'a' 2 it 
--    fromList [('a',5),('b',2)]
-- ---------------------------------------------------------------------

-- manvermor
insertaVarios :: Ord a => a -> Int -> MultiConj a -> MultiConj a
insertaVarios x n m | M.member x m = M.adjust (+n) x m
                    | otherwise    = M.insert x n m

-- Comentario: La definición anterior se puede simplificar.

--silgongal alvalvdom1 erisancha
insertaVarios2 :: Ord a => a -> Int -> MultiConj a -> MultiConj a
insertaVarios2  = M.insertWith (+) 

-- ---------------------------------------------------------------------
-- Ejercicio 6. Definir la función
--    borra :: Ord a => a -> MultiConj a -> MultiConj a
-- tal que (borra x m) es el multiconjunto obtenido borrando una
-- ocurrencia de x en m. Por ejemplo,
--    ghci> borra 'a' (listaAmc "ababc")
--    fromList [('a',1),('b',2),('c',1)]
--    ghci> borra 'a' it
--    fromList [('b',2),('c',1)]
--    ghci> borra 'a' it
--    fromList [('b',2),('c',1)]
-- ---------------------------------------------------------------------

-- manvermor silgongal
borra :: Ord a => a -> MultiConj a -> MultiConj a
borra x m | M.notMember x m = m
          | m M.! x <= 1    = M.delete x m
          | otherwise       = M.adjust (+(-1)) x m

-- Comentario: La definición anterior se puede simplificar.

-- erisancha
borra2 :: Ord a => a -> MultiConj a -> MultiConj a
borra2 = M.update f
    where f x = if x > 1 then Just (x-1) else Nothing

-- Comentario: La definición anterior se puede simplificar usando
-- guardas en lugar de condicionales.

-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir la función
--    borraVarias :: Ord a => a -> Int -> MultiConj a -> MultiConj a
-- tal que (borraVarias x n m) es el multiconjunto obtenido a partir del
-- m borrando n ocurrencias del elemento x. Por ejemplo,
--    ghci> listaAmc "ababcad"
--    fromList [('a',3),('b',2),('c',1),('d',1)]
--    ghci> borraVarias 'a' 2 (listaAmc "ababcad")
--    fromList [('a',1),('b',2),('c',1),('d',1)]
--    ghci> borraVarias 'a' 5 (listaAmc "ababcad")
--    fromList [('b',2),('c',1),('d',1)]
-- ---------------------------------------------------------------------

-- manvermor silgongal
borraVarias :: Ord a => a -> Int -> MultiConj a -> MultiConj a
borraVarias x n m | M.notMember x m = m
                  | m M.! x <= n    = M.delete x m
                  | otherwise       = M.adjust (+(-n)) x m

-- Comentario: La definición anterior se puede simplificar.

-- erisancha
borraVarias2 :: Ord a => a -> Int -> MultiConj a -> MultiConj a
borraVarias2 x n m | m M.! x <= n    = M.delete x m
                   | otherwise       = M.adjust (+(-n)) x m

-- Comentario: La definición anterior se puede simplificar.

-- ---------------------------------------------------------------------
-- Ejercicio 8. Definir la función
--    borraTodas :: Ord a => a -> MultiConj a -> MultiConj a
-- tal que (borraTodas x m) es el multiconjunto obtenido a partir del
-- m borrando todas las ocurrencias del elemento x. Por ejemplo,
--    ghci> borraTodas 'a' (listaAmc "ababcad")
--    fromList [('b',2),('c',1),('d',1)]
-- ---------------------------------------------------------------------

-- manvermor silgongal erisancha
borraTodas :: Ord a => a -> MultiConj a -> MultiConj a
borraTodas = M.delete

-- ---------------------------------------------------------------------
-- Consultas                                                          --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 9. Definir la función 
--    esVacio :: MultiConj a -> Bool
-- tal que (esVacio m) se verifica si el multiconjunto m es vacío. Por
-- ejemplo, 
--    esVacio vacio  ==  True
--    esVacio (inserta 'a' vacio)  ==  False
-- ---------------------------------------------------------------------

-- manvermor silgongal alvalvdom1 erisancha
esVacio :: MultiConj a -> Bool
esVacio = M.null 

-- ---------------------------------------------------------------------
-- Ejercicio 10. Definir la función
--    cardinal :: MultiConj a -> Int
-- tal que (cardinal m) es el número de elementos (contando las
-- repeticiones) del multiconjunto m. Por ejemplo,
--    cardinal (listaAmc "ababcad")  ==  7
-- ---------------------------------------------------------------------

-- manvermor silgongal
cardinal :: MultiConj a -> Int
cardinal m = sum (M.elems m)

-- alvalvdom1 erisancha
cardinal2 :: MultiConj a -> Int
cardinal2 = sum . M.elems

-- ---------------------------------------------------------------------
-- Ejercicio 11. Definir la función
--    cardDistintos :: MultiConj a -> Int
-- tal que (cardDistintos m) es el número de elementos (sin contar las
-- repeticiones) del multiconjunto m. Por ejemplo,
--    cardDistintos (listaAmc "ababcad")  ==  4
-- ---------------------------------------------------------------------

-- manvermor silgongal alvalvdom1 erisancha
cardDistintos :: MultiConj a -> Int
cardDistintos = M.size

-- ---------------------------------------------------------------------
-- Ejercicio 12. Definir la función
--    pertenece :: Ord a => a -> MultiConj a -> Bool
-- tal que (pertenece x m) se verifica si el elemento x pertenece al
-- multiconjunto m. Por ejemplo,
--    pertenece 'b' (listaAmc "ababcad")  ==  True
--    pertenece 'r' (listaAmc "ababcad")  ==  False
-- ---------------------------------------------------------------------

-- manvermor silgongal alvalvdom1 erisancha
pertenece :: Ord a => a -> MultiConj a -> Bool
pertenece = M.member 

-- ---------------------------------------------------------------------
-- Ejercicio 13. Definir la función
--    noPertenece :: Ord a => a -> MultiConj a -> Bool
-- tal que (noPertenece x m) se verifica si el elemento x no pertenece al
-- multiconjunto m. Por ejemplo,
--    noPertenece 'b' (listaAmc "ababcad")  ==  False
--    noPertenece 'r' (listaAmc "ababcad")  ==  True
-- ---------------------------------------------------------------------

-- manvermor silgongal alvalvdom1 erisancha
noPertenece :: Ord a => a -> MultiConj a -> Bool
noPertenece = M.notMember

-- ---------------------------------------------------------------------
-- Ejercicio 14. Definir la función
--    ocurrencias :: Ord a => a -> MultiConj a -> Int
-- tal que (ocurrencias x m) es el número de ocurrencias de x en el
-- multiconjunto m. Por ejemplo,
--    ocurrencias 'a' (listaAmc "ababcad")  ==  3
--    ocurrencias 'r' (listaAmc "ababcad")  ==  0
-- ---------------------------------------------------------------------

-- manvermor
ocurrencias :: Ord a => a -> MultiConj a -> Int
ocurrencias x m | M.notMember x m = 0
                | otherwise       = m M.! x

-- Comentario: La definición anterior se puede simplificar.

-- alvalvdom1 erisancha silgongal
ocurrencias2 :: Ord a => a -> MultiConj a -> Int
ocurrencias2 = M.findWithDefault 0

-- ---------------------------------------------------------------------
-- Ejercicio 15: Definir la función 
--    elementos :: Ord a => MultiConj a -> [a]
-- tal que (elementos m) es la lista de los elementos (sin repeticiones)
-- del multiconjunto m. Por ejemplo,
--    elementos (listaAmc "ababcad")  ==  "abcd"
-- ---------------------------------------------------------------------

-- manvermor alvalvdom1 erisancha silgongal
elementos :: Ord a => MultiConj a -> [a]
elementos = M.keys

-- ---------------------------------------------------------------------
-- Ejercicio 16.Definir la función
--    esSubmultiConj :: Ord a => MultiConj a -> MultiConj a -> Bool
-- tal que (esSubmultiConj m1 m2) se verifica si m1 es un
-- submulticonjuto de m2 (es decir; los elementos de m1 pertenecen a m2
-- con un númro de ocurrencias igual o mayor). Por ejemplo,
--    ghci> let m1 = listaAmc "ababcad"
--    ghci> let m2 = listaAmc "bcbaadaa"
--    ghci> m1
--    fromList [('a',3),('b',2),('c',1),('d',1)]
--    ghci> m2
--    fromList [('a',4),('b',2),('c',1),('d',1)]
--    ghci> esSubmultiConj m1 m2
--    True
--    ghci> esSubmultiConj m2 m1
--    False
-- ---------------------------------------------------------------------

-- erisancha silgongal
esSubmultiConj :: Ord a => MultiConj a -> MultiConj a -> Bool
esSubmultiConj = M.isSubmapOfBy (<=)

-- ---------------------------------------------------------------------
-- Elemento minimo y máximo de un multiconjunto                       --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 17. Definir la función
--    minimo :: MultiConj a -> a
-- tal que (minimo m) es el mínimo elemento del multiconjunto m. Por
-- ejemplo, 
--    minimo (listaAmc "cdacbab")  ==  'a'
-- ---------------------------------------------------------------------

-- alvalvdom1 erisancha silgongal
minimo :: MultiConj a -> a
minimo = fst . M.findMin

-- ---------------------------------------------------------------------
-- Ejercicio 18. Definir la función
--    maximo :: MultiConj a -> a
-- tal que (maximo m) es el máximo elemento del multiconjunto m. Por
-- ejemplo, 
--    maximo (listaAmc "cdacbab")  ==  'd'
-- ---------------------------------------------------------------------

-- alvalvdom1 erisancha silgongal
maximo :: MultiConj a -> a
maximo = fst . M.findMax

-- ---------------------------------------------------------------------
-- Ejercicio 19. Definir la función   
--    borraMin :: Ord a => MultiConj a -> MultiConj a
-- tal que (borraMin m) es el multiconjunto obtenido eliminando una
-- ocurrencia del menor elemento de m. Por ejemplo,
--    ghci> borraMin (listaAmc "cdacbab")
--    fromList [('a',1),('b',2),('c',2),('d',1)]
--    ghci> borraMin it
--    fromList [('b',2),('c',2),('d',1)]
-- ---------------------------------------------------------------------

-- erisancha
borraMin :: Ord a => MultiConj a -> MultiConj a
borraMin = M.updateMin f 
  where f x = if x > 1 then Just (x-1) else Nothing

-- Comentario: La definición anterior se puede simplificar.

-- silgongal
borraMin2 :: Ord a => MultiConj a -> MultiConj a
borraMin2 m | m M.! i == 1 = M.delete i m 
            | otherwise    = M.insert i ((m M.! i)-1) m
    where i = minimo m

-- Comentario: La definición anterior se puede simplificar.

-- ---------------------------------------------------------------------
-- Ejercicio 20. Definir la función   
--    borraMax :: Ord a => MultiConj a -> MultiConj a
-- tal que (borraMax m) es el multiconjunto obtenido eliminando una
-- ocurrencia del mayor elemento de m. Por ejemplo,
--    ghci> borraMax (listaAmc "cdacbab")
--    fromList [('a',2),('b',2),('c',2)]
--    ghci> borraMax it
--    fromList [('a',2),('b',2),('c',1)]
-- ---------------------------------------------------------------------

-- erisancha
borraMax :: Ord a => MultiConj a -> MultiConj a
borraMax = M.updateMax f 
  where f x = if x > 1 then Just (x-1) else Nothing

-- Comentario: La definición anterior se puede simplificar.

-- silgongal
borraMax2 :: Ord a => MultiConj a -> MultiConj a
borraMax2 m | m M.! i == 1 = M.delete i m 
            | otherwise    = M.insert i ((m M.! i)-1) m
    where i = maximo m

-- Comentario: La definición anterior se puede simplificar.

-- ---------------------------------------------------------------------
-- Ejercicio 21. Definir la función   
--    borraMinTodo :: Ord a => MultiConj a -> MultiConj a
-- tal que (borraMinTodo m) es el multiconjunto obtenido eliminando
-- todas las ocurrencias del menor elemento de m. Por ejemplo,
--    ghci> borraMinTodo (listaAmc "cdacbab")
--    fromList [('b',2),('c',2),('d',1)]
--    ghci> borraMinTodo it
--    fromList [('c',2),('d',1)]
-- ---------------------------------------------------------------------

-- erisancha silgongal
borraMinTodo :: Ord a => MultiConj a -> MultiConj a
borraMinTodo = M.deleteMin

-- ---------------------------------------------------------------------
-- Ejercicio 22. Definir la función   
--    borraMaxTodo :: Ord a => MultiConj a -> MultiConj a
-- tal que (borraMaxTodo m) es el multiconjunto obtenido eliminando
-- todas las ocurrencias del mayor elemento de m. Por ejemplo,
--    ghci> borraMaxTodo (listaAmc "cdacbab")
--    fromList [('a',2),('b',2),('c',2)]
--    ghci> borraMaxTodo it
--    fromList [('a',2),('b',2)]
-- ---------------------------------------------------------------------

-- erisancha silgongal
borraMaxTodo :: Ord a => MultiConj a -> MultiConj a
borraMaxTodo = M.deleteMax

-- ---------------------------------------------------------------------
-- Operaciones: unión, intersección y diferencia de multiconjuntos    --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 23. Definir la función
--    union :: Ord a => MultiConj a -> MultiConj a -> MultiConj a
-- tal que (union m1 m2) es la unión de los multiconjuntos m1 y m2. Por
-- ejemplo, 
--    ghci> let m1 = listaAmc "cdacba"
--    ghci> let m2 = listaAmc "acec"
--    ghci> m1
--    fromList [('a',2),('b',1),('c',2),('d',1)]
--    ghci> m2
--    fromList [('a',1),('c',2),('e',1)]
--    ghci> union m1 m2
--    fromList [('a',3),('b',1),('c',4),('d',1),('e',1)]
-- ---------------------------------------------------------------------

-- erisancha silgongal
union :: Ord a => MultiConj a -> MultiConj a -> MultiConj a
union = M.unionWith (+)

-- ---------------------------------------------------------------------
-- Ejercicio 24. Definir la función
--    unionG :: Ord a => [MultiConj a] -> MultiConj a
-- tal que (unionG ms) es la unión de la lista de multiconjuntos ms. Por
-- ejemplo, 
--    ghci> unionG (map listaAmc ["aba", "cda", "bdb"])
--    fromList [('a',3),('b',3),('c',1),('d',2)]
-- ---------------------------------------------------------------------

-- erisancha silgongal
unionG :: Ord a => [MultiConj a] -> MultiConj a
unionG  = M.unionsWith (+)

-- ---------------------------------------------------------------------
-- Ejercicio 25. Definir la función
--    diferencia :: Ord a => MultiConj a -> MultiConj a -> MultiConj a
-- tal que (diferencia m1 m2) es la diferencia de los multiconjuntos m1
-- y m2. Por ejemplo,
--    ghci> diferencia (listaAmc "abacc") (listaAmc "dcb")
--    fromList [('a',2),('c',1)]
-- ---------------------------------------------------------------------

-- erisancha
-- Esta es la función predefinida para diferencia de multiconjuntos,
-- pero no se corresponde con el ejemplo, no sé si se debe a un error
-- en este, o en la definición.
diferencia :: Ord a => MultiConj a -> MultiConj a -> MultiConj a
diferencia = M.difference

-- Comentario: El error está en la definición.

-- ---------------------------------------------------------------------
-- Ejercicio 26. Definir la función
--    interseccion :: Ord a => MultiConj a -> MultiConj a -> MultiConj a
-- tal que (interseccion m1 m2) es la intersección de los multiconjuntos
-- m1 y m2. Por ejemplo,
--    ghci> interseccion (listaAmc "abcacc") (listaAmc "bdcbc")
--    fromList [('b',1),('c',2)]
-- ---------------------------------------------------------------------

-- erisancha
-- Aquí pasa lo mismo
interseccion :: Ord a => MultiConj a -> MultiConj a -> MultiConj a
interseccion = M.intersection

-- Comentario: El error está en la definición.

-- ---------------------------------------------------------------------
-- Filtrado y partición                                               --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 27. Definir la función
--    filtra :: Ord a => (a -> Bool) -> MultiConj a -> MultiConj a
-- tal que (filtra p m) es el multiconjunto de los elementos de m que
-- verifican la propiedad p. Por ejemplo,
--    ghci> filtra (>'b') (listaAmc "abaccaded") 
--    fromList [('c',2),('d',2),('e',1)]
-- ---------------------------------------------------------------------

filtra :: Ord a => (a -> Bool) -> MultiConj a -> MultiConj a
filtra p = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 28. Definir la función
--    particion :: Ord a => 
--                 (a -> Bool) -> MultiConj a -> (MultiConj a,MultiConj a)
-- tal que (particion p m) es el par cuya primera componente consta de
-- los elementos de m que cumplen p y la segunda por los que no lo
-- cumplen. Por ejemplo, 
--    ghci> particion (>'b') (listaAmc "abaccaded") 
--    (fromList [('c',2),('d',2),('e',1)],fromList [('a',3),('b',1)])
-- ---------------------------------------------------------------------

particion :: Ord a => 
             (a -> Bool) -> MultiConj a -> (MultiConj a,MultiConj a)
particion p = undefined

-- ---------------------------------------------------------------------
-- Función aplicativa                                                 --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 29. Definir la función
--    mapMC :: Ord b => (a -> b) -> MultiConj a -> MultiConj b
-- tal que (mapMC f m) es el multiconjunto obtenido aplicando la función
-- f a todos los  elementos de m. Por ejemplo,
--    ghci> mapMC (:"N") (listaAmc "abaccaded") 
--    fromList [("aN",3),("bN",1),("cN",2),("dN",2),("eN",1)]
-- ---------------------------------------------------------------------

mapMC :: Ord b => (a -> b) -> MultiConj a -> MultiConj b
mapMC f = undefined
