-- I1M 2015-16: Relaci�n 30 (1 de abril de 2016)
-- El TAD de los multiconjuntos mediante diccionarios.
-- Departamento de Ciencias de la Computaci�n e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducci�n                                                       --
-- ---------------------------------------------------------------------

-- Un multiconjunto es una coleeci�n de elementos en los que no importa
-- el orden de los elementos, pero s� el n�mero de veces en que
-- aparecen. Por ejemplo, la factorizaci�n prima de un n�mero se puede
-- representar como un multiconjunto de n�meros primos. 
-- 
-- El objetivo de esta relaci�n de ejercicios es implementar el TAD de
-- los multiconjuntos utilizando los diccionarios estudiados en el tema
-- 29 https://www.cs.us.es/~jalonso/cursos/i1m/temas/tema-29.html
-- 
-- El manual, con ejemplos, de la librer�a Data.Map se encuentra en
-- http://bit.ly/25B1na0

-- ---------------------------------------------------------------------
-- Librer�as auxiliares                                               --
-- ---------------------------------------------------------------------

import qualified Data.Map as M

-- ---------------------------------------------------------------------
-- El tipo de dato de multiconjuntos                                  --
-- ---------------------------------------------------------------------

-- Un multiconjunto se puede representar mediante un diccionario donde
-- las claves son los elementos del multiconjunto y sus valores sus
-- n�meros de ocurrencias. Por ejemplo, el multiconjunto 
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
-- para el multiconjunto vac�o. Por ejemplo,
--    vacio  ==  fromList []
-- ---------------------------------------------------------------------

vacio :: MultiConj a
vacio = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la funci�n
--    unitario :: a -> MultiConj a
-- tal que (unitario x) es el multiconjunto cuyo �nico elemento es
-- x. Por ejemplo,
--    unitario 'a'  ==  fromList [('a',1)]
-- ---------------------------------------------------------------------

unitario :: a -> MultiConj a
unitario x = undefined

-- ---------------------------------------------------------------------
-- A�adir y quitar elementos                                          --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la funci�n
--    inserta :: Ord a => a -> MultiConj a -> MultiConj a
-- tal que (inserta x m) es el multiconjunto obtenido a�adi�ndole a m el 
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

inserta :: Ord a => a -> MultiConj a -> MultiConj a
inserta x = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la funci�n 
--    listaAmc :: Ord a => [a] -> MultiConj a
-- tal que (listaAmc xs) es el multiconjunto cuyos elementos son los de
-- la lista xs. Por ejemplo,
--    listaAmc "ababc"  ==  fromList [('a',2),('b',2),('c',1)]
-- ---------------------------------------------------------------------

listaAmc :: Ord a => [a] -> MultiConj a
listaAmc xs = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la funci�n
--    insertaVarios :: Ord a => a -> Int -> MultiConj a -> MultiConj a
-- tal que (insertaVarios x n m) es el multiconjunto obtenido
-- a�adi�ndole a m n copias del elemento x. Por ejemplo, 
--    ghci> insertaVarios 'a' 3 vacio
--    fromList [('a',3)]
--    ghci> insertaVarios 'b' 2 it 
--    fromList [('a',3),('b',2)]
--    ghci> insertaVarios 'a' 2 it 
--    fromList [('a',5),('b',2)]
-- ---------------------------------------------------------------------

insertaVarios :: Ord a => a -> Int -> MultiConj a -> MultiConj a
insertaVarios x n = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 6. Definir la funci�n
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

borra :: Ord a => a -> MultiConj a -> MultiConj a
borra x = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir la funci�n
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

borraVarias :: Ord a => a -> Int -> MultiConj a -> MultiConj a
borraVarias x n = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 8. Definir la funci�n
--    borraTodas :: Ord a => a -> MultiConj a -> MultiConj a
-- tal que (borraTodas x m) es el multiconjunto obtenido a partir del
-- m borrando todas las ocurrencias del elemento x. Por ejemplo,
--    ghci> borraTodas 'a' (listaAmc "ababcad")
--    fromList [('b',2),('c',1),('d',1)]
-- ---------------------------------------------------------------------

borraTodas :: Ord a => a -> MultiConj a -> MultiConj a
borraTodas x = undefined

-- ---------------------------------------------------------------------
-- Consultas                                                          --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 9. Definir la funci�n 
--    esVacio :: MultiConj a -> Bool
-- tal que (esVacio m) se verifica si el multiconjunto m es vac�o. Por
-- ejemplo, 
--    esVacio vacio  ==  True
--    esVacio (inserta 'a' vacio)  ==  False
-- ---------------------------------------------------------------------

esVacio :: MultiConj a -> Bool
esVacio = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 10. Definir la funci�n
--    cardinal :: MultiConj a -> Int
-- tal que (cardinal m) es el n�mero de elementos (contando las
-- repeticiones) del multiconjunto m. Por ejemplo,
--    cardinal (listaAmc "ababcad")  ==  7
-- ---------------------------------------------------------------------

cardinal :: MultiConj a -> Int
cardinal = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 11. Definir la funci�n
--    cardDistintos :: MultiConj a -> Int
-- tal que (cardDistintos m) es el n�mero de elementos (sin contar las
-- repeticiones) del multiconjunto m. Por ejemplo,
--    cardDistintos (listaAmc "ababcad")  ==  4
-- ---------------------------------------------------------------------

cardDistintos :: MultiConj a -> Int
cardDistintos = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 12. Definir la funci�n
--    pertenece :: Ord a => a -> MultiConj a -> Bool
-- tal que (pertenece x m) se verifica si el elemento x pertenece al
-- multiconjunto m. Por ejemplo,
--    pertenece 'b' (listaAmc "ababcad")  ==  True
--    pertenece 'r' (listaAmc "ababcad")  ==  False
-- ---------------------------------------------------------------------

pertenece :: Ord a => a -> MultiConj a -> Bool
pertenece x = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 13. Definir la funci�n
--    noPertenece :: Ord a => a -> MultiConj a -> Bool
-- tal que (noPertenece x m) se verifica si el elemento x no pertenece al
-- multiconjunto m. Por ejemplo,
--    noPertenece 'b' (listaAmc "ababcad")  ==  False
--    noPertenece 'r' (listaAmc "ababcad")  ==  True
-- ---------------------------------------------------------------------

noPertenece :: Ord a => a -> MultiConj a -> Bool
noPertenece x = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 14. Definir la funci�n
--    ocurrencias :: Ord a => a -> MultiConj a -> Int
-- tal que (ocurrencias x m) es el n�mero de ocurrencias de x en el
-- multiconjunto m. Por ejemplo,
--    ocurrencias 'a' (listaAmc "ababcad")  ==  3
--    ocurrencias 'r' (listaAmc "ababcad")  ==  0
-- ---------------------------------------------------------------------

ocurrencias :: Ord a => a -> MultiConj a -> Int
ocurrencias x = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 15: Definir la funci�n 
--    elementos :: Ord a => MultiConj a -> [a]
-- tal que (elementos m) es la lista de los elementos (sin repeticiones)
-- del multiconjunto m. Por ejemplo,
--    elementos (listaAmc "ababcad")  ==  "abcd"
-- ---------------------------------------------------------------------

elementos :: Ord a => MultiConj a -> [a]
elementos = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 16.Definir la funci�n
--    esSubmultiConj :: Ord a => MultiConj a -> MultiConj a -> Bool
-- tal que (esSubmultiConj m1 m2) se verifica si m1 es un
-- submulticonjuto de m2 (es decir; los elementos de m1 pertenecen a m2
-- con un n�mro de ocurrencias igual o mayor). Por ejemplo,
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

esSubmultiConj :: Ord a => MultiConj a -> MultiConj a -> Bool
esSubmultiConj m1 m2 = undefined

-- ---------------------------------------------------------------------
-- Elemento minimo y m�ximo de un multiconjunto                       --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 17. Definir la funci�n
--    minimo :: MultiConj a -> a
-- tal que (minimo m) es el m�nimo elemento del multiconjunto m. Por
-- ejemplo, 
--    minimo (listaAmc "cdacbab")  ==  'a'
-- ---------------------------------------------------------------------

minimo :: MultiConj a -> a
minimo = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 18. Definir la funci�n
--    maximo :: MultiConj a -> a
-- tal que (maximo m) es el m�ximo elemento del multiconjunto m. Por
-- ejemplo, 
--    maximo (listaAmc "cdacbab")  ==  'd'
-- ---------------------------------------------------------------------

maximo :: MultiConj a -> a
maximo = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 19. Definir la funci�n   
--    borraMin :: Ord a => MultiConj a -> MultiConj a
-- tal que (borraMin m) es el multiconjunto obtenido eliminando una
-- ocurrencia del menor elemento de m. Por ejemplo,
--    ghci> borraMin (listaAmc "cdacbab")
--    fromList [('a',1),('b',2),('c',2),('d',1)]
--    ghci> borraMin it
--    fromList [('b',2),('c',2),('d',1)]
-- ---------------------------------------------------------------------

borraMin :: Ord a => MultiConj a -> MultiConj a
borraMin m = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 20. Definir la funci�n   
--    borraMax :: Ord a => MultiConj a -> MultiConj a
-- tal que (borraMax m) es el multiconjunto obtenido eliminando una
-- ocurrencia del mayor elemento de m. Por ejemplo,
--    ghci> borraMax (listaAmc "cdacbab")
--    fromList [('a',2),('b',2),('c',2)]
--    ghci> borraMax it
--    fromList [('a',2),('b',2),('c',1)]
-- ---------------------------------------------------------------------

borraMax :: Ord a => MultiConj a -> MultiConj a
borraMax m = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 21. Definir la funci�n   
--    borraMinTodo :: Ord a => MultiConj a -> MultiConj a
-- tal que (borraMinTodo m) es el multiconjunto obtenido eliminando
-- todas las ocurrencias del menor elemento de m. Por ejemplo,
--    ghci> borraMinTodo (listaAmc "cdacbab")
--    fromList [('b',2),('c',2),('d',1)]
--    ghci> borraMinTodo it
--    fromList [('c',2),('d',1)]
-- ---------------------------------------------------------------------

borraMinTodo :: Ord a => MultiConj a -> MultiConj a
borraMinTodo m = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 22. Definir la funci�n   
--    borraMaxTodo :: Ord a => MultiConj a -> MultiConj a
-- tal que (borraMaxTodo m) es el multiconjunto obtenido eliminando
-- todas las ocurrencias del mayor elemento de m. Por ejemplo,
--    ghci> borraMaxTodo (listaAmc "cdacbab")
--    fromList [('a',2),('b',2),('c',2)]
--    ghci> borraMaxTodo it
--    fromList [('a',2),('b',2)]
-- ---------------------------------------------------------------------

borraMaxTodo :: Ord a => MultiConj a -> MultiConj a
borraMaxTodo m = undefined

-- ---------------------------------------------------------------------
-- Operaciones: uni�n, intersecci�n y diferencia de multiconjuntos    --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 23. Definir la funci�n
--    union :: Ord a => MultiConj a -> MultiConj a -> MultiConj a
-- tal que (union m1 m2) es la uni�n de los multiconjuntos m1 y m2. Por
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

union :: Ord a => MultiConj a -> MultiConj a -> MultiConj a
union m1 m2 = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 24. Definir la funci�n
--    unionG :: Ord a => [MultiConj a] -> MultiConj a
-- tal que (unionG ms) es la uni�n de la lista de multiconjuntos ms. Por
-- ejemplo, 
--    ghci> unionG (map listaAmc ["aba", "cda", "bdb"])
--    fromList [('a',3),('b',3),('c',1),('d',2)]
-- ---------------------------------------------------------------------

unionG :: Ord a => [MultiConj a] -> MultiConj a
unionG ms = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 25. Definir la funci�n
--    diferencia :: Ord a => MultiConj a -> MultiConj a -> MultiConj a
-- tal que (diferencia m1 m2) es la diferencia de los multiconjuntos m1
-- y m2. Por ejemplo,
--    ghci> diferencia (listaAmc "abacc") (listaAmc "dcb")
--    fromList [('a',2),('c',1)]
-- ---------------------------------------------------------------------

diferencia :: Ord a => MultiConj a -> MultiConj a -> MultiConj a
diferencia m1 m2 = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 26. Definir la funci�n
--    interseccion :: Ord a => MultiConj a -> MultiConj a -> MultiConj a
-- tal que (interseccion m1 m2) es la intersecci�n de los multiconjuntos
-- m1 y m2. Por ejemplo,
--    ghci> interseccion (listaAmc "abcacc") (listaAmc "bdcbc")
--    fromList [('b',1),('c',2)]
-- ---------------------------------------------------------------------

interseccion :: Ord a => MultiConj a -> MultiConj a -> MultiConj a
interseccion m1 m2 = undefined

-- ---------------------------------------------------------------------
-- Filtrado y partici�n                                               --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 27. Definir la funci�n
--    filtra :: Ord a => (a -> Bool) -> MultiConj a -> MultiConj a
-- tal que (filtra p m) es el multiconjunto de los elementos de m que
-- verifican la propiedad p. Por ejemplo,
--    ghci> filtra (>'b') (listaAmc "abaccaded") 
--    fromList [('c',2),('d',2),('e',1)]
-- ---------------------------------------------------------------------

filtra :: Ord a => (a -> Bool) -> MultiConj a -> MultiConj a
filtra p = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 28. Definir la funci�n
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
-- Funci�n aplicativa                                                 --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 29. Definir la funci�n
--    mapMC :: Ord b => (a -> b) -> MultiConj a -> MultiConj b
-- tal que (mapMC f m) es el multiconjunto obtenido aplicando la funci�n
-- f a todos los  elementos de m. Por ejemplo,
--    ghci> mapMC (:"N") (listaAmc "abaccaded") 
--    fromList [("aN",3),("bN",1),("cN",2),("dN",2),("eN",1)]
-- ---------------------------------------------------------------------

mapMC :: Ord b => (a -> b) -> MultiConj a -> MultiConj b
mapMC f = undefined
