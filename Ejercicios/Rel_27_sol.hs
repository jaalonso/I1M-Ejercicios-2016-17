-- I1M 2016-17: Relación 27 (19 de abril de 2016)
-- El TAD de los multiconjuntos mediante diccionarios.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- Un multiconjunto es una colección de elementos en los que no importa
-- el orden de los elementos, pero sí el número de veces en que
-- aparecen. Por ejemplo, la factorización prima de un número se puede
-- representar como un multiconjunto de números primos. 
-- 
-- El objetivo de esta relación de ejercicios es implementar el TAD de
-- los multiconjuntos utilizando los diccionarios estudiados en el tema
-- 29 https://www.cs.us.es/~jalonso/cursos/i1m-16/temas/tema-29.html
-- 
-- El manual, con ejemplos, de la librería Data.Map se encuentra en
-- http://www.cs.us.es/~jalonso/cursos/i1m-16/doc/manual-Data.Map.html

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

-- albcercid pabrabmon joscasgom1 margarvil14 monlagare josrodgal7
-- natmarmar2 paumacpar antmorper3 enrnarbej 
vacio :: MultiConj a
vacio = M.empty

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    unitario :: a -> MultiConj a
-- tal que (unitario x) es el multiconjunto cuyo único elemento es
-- x. Por ejemplo,
--    unitario 'a'  ==  fromList [('a',1)]
-- ---------------------------------------------------------------------

-- albcercid pabrabmon joscasgom1 margarvil14 monlagare josrodgal7
-- natmarmar2 paumacpar antmorper3 enrnarbej 
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

-- albcercid pabrabmon joscasgom1 margarvil14 monlagare josrodgal7
-- natmarmar2 paumacpar antmorper3 enrnarbej 
inserta :: Ord a => a -> MultiConj a -> MultiConj a
inserta x = M.insertWith (+) x 1 

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función 
--    listaAmc :: Ord a => [a] -> MultiConj a
-- tal que (listaAmc xs) es el multiconjunto cuyos elementos son los de
-- la lista xs. Por ejemplo,
--    listaAmc "ababc"  ==  fromList [('a',2),('b',2),('c',1)]
-- ---------------------------------------------------------------------

-- albcercid joscasgom1 margarvil14 monlagare josrodgal7 natmarmar2
listaAmc :: Ord a => [a] -> MultiConj a
listaAmc []     = vacio
listaAmc (x:xs) = inserta x (listaAmc xs)

-- pabrabmon paumacpar antmorper3
listaAmc2 :: Ord a => [a] -> MultiConj a
listaAmc2 xs = foldr inserta vacio xs

-- enrnarbej
listaAmc3 :: Ord a => [a] -> MultiConj a
listaAmc3 = foldr inserta vacio

-- Comentario: Las definiciones anteriores se puede mejorar.

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

-- albcercid joscasgom1 margarvil14 monlagare josrodgal7 natmarmar2
-- paumacpar antmorper3 
insertaVarios :: Ord a => a -> Int -> MultiConj a -> MultiConj a
insertaVarios x n = M.insertWith (+) x n

-- enrnarbej
insertaVarios2 :: Ord a => a -> Int -> MultiConj a -> MultiConj a
insertaVarios2 = M.insertWith (+) 

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

-- albcercid pabrabmon joscasgom1 monlagare josrodgal7 natmarmar2
-- antmorper3
borra :: Ord a => a -> MultiConj a -> MultiConj a
borra x m | not (M.member x m) = m
          | m M.! x == 1       = M.delete x m
          | otherwise          = M.insertWith (+) x (-1) m

-- paumacpar 
borra2 :: Ord a => a -> MultiConj a -> MultiConj a
borra2 x d = M.update f x d
  where f x = if (x-1) > 0 then Just (x-1) else Nothing

-- enrnarbej
borra3 :: Ord a => a -> MultiConj a -> MultiConj a
borra3 x = M.filter (> 0) . insertaVarios x (-1)

-- Comentario: Comparación de eficiencia:
--    λ> length (borra 0 (listaAmc [1..2*10^5]))
--    200000
--    (2.40 secs, 451,744,448 bytes)
--    λ> length (borra2 0 (listaAmc [1..2*10^5]))
--    200000
--    (2.32 secs, 440,893,904 bytes)
--    λ> length (borra3 0 (listaAmc [1..2*10^5]))
--    200000
--    (2.44 secs, 439,508,016 bytes)

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

-- albcercid monlagare josrodgal7 paumacpar antmorper3
borraVarias :: Ord a => a -> Int -> MultiConj a -> MultiConj a
borraVarias x 1 m = borra x m
borraVarias x n m = borra x (borraVarias x (n-1) m)

-- pabrabmon joscasgom1 natmarmar2
borraVarias2 :: Ord a => a -> Int -> MultiConj a -> MultiConj a
borraVarias2 x n m
  | not (M.member x m) = m
  | m M.! x > n        = M.insertWith (+) x (-n) m
  | otherwise          = M.delete x m

-- enrnrabej
borraVarias3 :: Ord a => a -> Int -> MultiConj a -> MultiConj a
borraVarias3 x n = M.filter (> 0) . insertaVarios x (-n)

-- ---------------------------------------------------------------------
-- Ejercicio 8. Definir la función
--    borraTodas :: Ord a => a -> MultiConj a -> MultiConj a
-- tal que (borraTodas x m) es el multiconjunto obtenido a partir del
-- m borrando todas las ocurrencias del elemento x. Por ejemplo,
--    ghci> borraTodas 'a' (listaAmc "ababcad")
--    fromList [('b',2),('c',1),('d',1)]
-- ---------------------------------------------------------------------

-- albcercid pabrabmon joscasgom1 margarvil14 monlagare josrodgal7 natmarmar2
-- paumacpar antmorper3 enrnarbej
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

-- albcercid pabrabmon joscasgom1 margarvil14 monlagare josrodgal7 natmarmar2
-- antmorper3 enrnarbej paumacpar
esVacio :: MultiConj a -> Bool
esVacio = M.null

-- ---------------------------------------------------------------------
-- Ejercicio 10. Definir la función
--    cardinal :: MultiConj a -> Int
-- tal que (cardinal m) es el número de elementos (contando las
-- repeticiones) del multiconjunto m. Por ejemplo,
--    cardinal (listaAmc "ababcad")  ==  7
-- ---------------------------------------------------------------------

-- albcercid margarvil14 josrodgal7 natmarmar2 antmorper3 enrnarbej
-- paumacpar
cardinal :: MultiConj a -> Int
cardinal = foldl (+) 0

-- pabrabmon joscasgom1 monlagare
cardinal2 :: MultiConj a -> Int
cardinal2 = sum . M.elems

-- ---------------------------------------------------------------------
-- Ejercicio 11. Definir la función
--    cardDistintos :: MultiConj a -> Int
-- tal que (cardDistintos m) es el número de elementos (sin contar las
-- repeticiones) del multiconjunto m. Por ejemplo,
--    cardDistintos (listaAmc "ababcad")  ==  4
-- ---------------------------------------------------------------------

-- albcercid pabrabmon joscasgom1 margarvil14 josrodgal7 natmarmar2
-- antmorper3 enrnarbej paumacpar
cardDistintos :: MultiConj a -> Int
cardDistintos = M.size

-- monlagare 
cardDistintos2 :: MultiConj a -> Int
cardDistintos2 = length . M.keys 

-- ---------------------------------------------------------------------
-- Ejercicio 12. Definir la función
--    pertenece :: Ord a => a -> MultiConj a -> Bool
-- tal que (pertenece x m) se verifica si el elemento x pertenece al
-- multiconjunto m. Por ejemplo,
--    pertenece 'b' (listaAmc "ababcad")  ==  True
--    pertenece 'r' (listaAmc "ababcad")  ==  False
-- ---------------------------------------------------------------------

-- albcercid pabrabmon joscasgom1 margarvil14 monlagare josrodgal7 natmarmar2
-- antmorper3 enrnarbej paumacpar
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

-- albcercid joscasgom1 josrodgal7
noPertenece :: Ord a => a -> MultiConj a -> Bool
noPertenece x = not . pertenece x

-- pabrabmon margarvil14 monlagare natmarmar2 antmorper3 enrnarbej
-- paumacpar
noPertenece2 :: Ord a => a -> MultiConj a -> Bool
noPertenece2 = M.notMember

-- ---------------------------------------------------------------------
-- Ejercicio 14. Definir la función
--    ocurrencias :: Ord a => a -> MultiConj a -> Int
-- tal que (ocurrencias x m) es el número de ocurrencias de x en el
-- multiconjunto m. Por ejemplo,
--    ocurrencias 'a' (listaAmc "ababcad")  ==  3
--    ocurrencias 'r' (listaAmc "ababcad")  ==  0
-- ---------------------------------------------------------------------

-- albcercid pabrabmon joscasgom1 margarvil14 monlagare josrodgal7 natmarmar2
-- antmorper3 paumacpar
ocurrencias :: Ord a => a -> MultiConj a -> Int
ocurrencias x m | M.member x m =  m M.! x
                | otherwise    = 0

-- enrnarbej
ocurrencias2 :: Ord a => a -> MultiConj a -> Int
ocurrencias2 = M.findWithDefault 0

-- ---------------------------------------------------------------------
-- Ejercicio 15: Definir la función 
--    elementos :: Ord a => MultiConj a -> [a]
-- tal que (elementos m) es la lista de los elementos (sin repeticiones)
-- del multiconjunto m. Por ejemplo,
--    elementos (listaAmc "ababcad")  ==  "abcd"
-- ---------------------------------------------------------------------

-- albcercid pabrabmon joscasgom1 margarvil14 monlagare josrodgal7 natmarmar2
-- antmorper3 enrnarbej paumacpar
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

-- albcercid pabrabmon joscasgom1 monlagare josrodgal7 natmarmar2
-- antmorper3 enrnarbej paumacpar
esSubmultiConj :: Ord a => MultiConj a -> MultiConj a -> Bool
esSubmultiConj = M.isSubmapOfBy (<=)

-- margarvil14
esSubmultiConj2 :: Ord a => MultiConj a -> MultiConj a -> Bool
esSubmultiConj2 a b =
  and [M.member x b && ocurrencias x a <= ocurrencias x b | x <- xs]
  where xs = M.keys a

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

-- albcercid pabrabmon joscasgom1 monlagare josrodgal7 natmarmar2
-- antmorper3 enrnarbej paumacpar
minimo :: MultiConj a -> a
minimo = fst . M.findMin

-- ---------------------------------------------------------------------
-- Ejercicio 18. Definir la función
--    maximo :: MultiConj a -> a
-- tal que (maximo m) es el máximo elemento del multiconjunto m. Por
-- ejemplo, 
--    maximo (listaAmc "cdacbab")  ==  'd'
-- ---------------------------------------------------------------------

-- albcercid pabrabmon joscasgom1 monlagare josrodgal7 natmarmar2
-- antmorper3 enrnarbej paumacpar
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

-- albcercid pabrabmon joscasgom1 monlagare josrodgal7 natmarmar2
-- antmorper3 enrnarbej paumacpar 
borraMin :: Ord a => MultiConj a -> MultiConj a
borraMin m = borra (minimo m) m

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

-- albcercid pabrabmon joscasgom1 monlagare josrodgal7 natmarmar2
-- antmorper3 enrnarbej paumacpar 
borraMax :: Ord a => MultiConj a -> MultiConj a
borraMax m = borra (maximo m) m

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

-- albcercid joscasgom1
borraMinTodo :: Ord a => MultiConj a -> MultiConj a
borraMinTodo m = borraTodas (minimo m) m

-- pabrabmon josrodgal7 natmarmar2 antmorper3 enrnarbej
-- paumacpar 
borraMinTodo2 :: Ord a => MultiConj a -> MultiConj a
borraMinTodo2 = M.deleteMin

-- monlagare
borraMinTodo3 :: Ord a => MultiConj a -> MultiConj a
borraMinTodo3 m = M.delete (minimo m) m

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

-- albcercid joscasgom1
borraMaxTodo :: Ord a => MultiConj a -> MultiConj a
borraMaxTodo m = borraTodas (maximo m) m

-- pabrabmon josrodgal7 natmarmar2 antmorper3 enrnarbej paumacpar
borraMaxTodo2 :: Ord a => MultiConj a -> MultiConj a
borraMaxTodo2 = M.deleteMax

-- monlagare
borraMaxTodo3 :: Ord a => MultiConj a -> MultiConj a
borraMaxTodo3 m = M.delete (maximo m) m

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

-- albcercid pabrabmon joscasgom1 monlagare josrodgal7 antmorper3 enrnarbej
-- paumacpar
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

-- albcercid
unionG :: Ord a => [MultiConj a] -> MultiConj a
unionG ms = foldr union vacio ms

-- pabrabmon joscasgom1 monlagare josrodgal7 antmorper3 enrnarbej
-- paumacpar 
unionG2 :: Ord a => [MultiConj a] -> MultiConj a
unionG2 = M.unionsWith (+)

-- ---------------------------------------------------------------------
-- Ejercicio 25. Definir la función
--    diferencia :: Ord a => MultiConj a -> MultiConj a -> MultiConj a
-- tal que (diferencia m1 m2) es la diferencia de los multiconjuntos m1
-- y m2. Por ejemplo,
--    ghci> diferencia (listaAmc "abacc") (listaAmc "dcb")
--    fromList [('a',2),('c',1)]
-- ---------------------------------------------------------------------

-- albcercid pabrabmon joscasgom1 josrodgal7 antmorper3
diferencia :: Ord a => MultiConj a -> MultiConj a -> MultiConj a
diferencia m1 m2 = M.filter (>0) $ M.unionWith (+) m1 (M.map ((-1)*) m2)

-- enrnabej paumacpar
diferencia2 :: Ord a => MultiConj a -> MultiConj a -> MultiConj a
diferencia2 =
  M.differenceWith (\x y -> if x - y /= 0 then Just (abs (x-y)) else Nothing)

-- ---------------------------------------------------------------------
-- Ejercicio 26. Definir la función
--    interseccion :: Ord a => MultiConj a -> MultiConj a -> MultiConj a
-- tal que (interseccion m1 m2) es la intersección de los multiconjuntos
-- m1 y m2. Por ejemplo,
--    ghci> interseccion (listaAmc "abcacc") (listaAmc "bdcbc")
--    fromList [('b',1),('c',2)]
-- ---------------------------------------------------------------------

--albcercid pabrabmon joscasgom1 monlagare josrodgal7 antmorper3 enrnarbej
--paumacpar
interseccion :: Ord a => MultiConj a -> MultiConj a -> MultiConj a
interseccion = M.intersectionWith min

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

-- albcercid pabrabmon joscasgom1 monlagare josrodgal7 antmorper3 enrnarbej
-- paumacpar
filtra :: Ord a => (a -> Bool) -> MultiConj a -> MultiConj a
filtra p = M.filterWithKey f
     where f x y = p x

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

-- albcercid pabrabmon joscasgom1 monlagare josrodgal7 antmorper3 enrnarbej
-- paumacpar
particion :: Ord a => 
             (a -> Bool) -> MultiConj a -> (MultiConj a,MultiConj a)
particion p =  M.partitionWithKey f
     where f x y = p x

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

-- albcercid pabrabmon joscasgom1 monlagare josrodgal7 antmorper3 enrnarbej
-- paumacpar
mapMC :: Ord b => (a -> b) -> MultiConj a -> MultiConj b
mapMC = M.mapKeys
