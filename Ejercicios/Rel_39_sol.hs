-- I1M 2015-16: Relación 39 (14 de mayo de 2016)
-- El problema del granjero mediante búsqueda en espacio de estado.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- Un granjero está parado en un lado del río y con él tiene un lobo,
-- una cabra y una repollo. En el río hay un barco pequeño. El granjero
-- desea cruzar el río con sus tres posesiones. No hay puentes y en el
-- barco hay solamente sitio para el granjero y un artículo. Si deja 
-- la cabra con la repollo sola en un lado del río la cabra comerá la
-- repollo. Si deja el lobo y la cabra en un lado, el lobo se comerá a
-- la cabra. ¿Cómo puede cruzar el granjero el río con los tres
-- artículos, sin que ninguno se coma al otro?
-- 
-- El objetivo de esta relación de ejercicios es resolver el problema
-- del granjero mediante búsqueda en espacio de estados, utilizando las
-- implementaciones estudiadas en el tema 23 
--    http://www.cs.us.es/~jalonso/cursos/i1m-15/temas/tema-23.html
-- 
-- Para realizar los ejercicios hay que tener instalada la librería I1M
-- que contiene la implementación de TAD de los polinomios. Los pasos
-- para instalarla son los siguientes:
-- + Descargar el paquete I1M desde http://bit.ly/1pbnDqm
-- + Descomprimirlo (y se crea el directorio I1M-master.zip).
-- + Cambiar al directorio I1M-master.
-- + Ejecutar cabal install I1M.cabal
-- 
-- Otra forma es descargar, en el directorio de ejercicios, la
-- implementación de la búsqueda en espacio de estado
-- + BusquedaEnEspaciosDeEstados.hs http://bit.ly/1Tcb9KB
-- 
-- Los módulos anteriores se encuentras en la página de códigos 
-- http://bit.ly/1SQnAKO
 
-- ---------------------------------------------------------------------
-- Importaciones                                                      --
-- ---------------------------------------------------------------------

-- Hay que elegir una librería
import I1M.BusquedaEnEspaciosDeEstados 
-- import BusquedaEnEspaciosDeEstados 

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir el tipo Orilla con dos constructores I y D que
-- representan las orillas izquierda y derecha, respectivamente.
-- ---------------------------------------------------------------------

data Orilla = I | D
              deriving (Eq, Show)

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir el tipo Estado como abreviatura de una tupla que
-- representan en qué orilla se encuentra cada uno de los elementos
-- (granjero, lobo, cabra, repollo). Por ejemplo, (I,D,D,I) representa
-- que el granjero está en la izquierda, que el lobo está en la derecha,
-- que la cabra está en la derecha y el repollo está en la izquierda.
-- ---------------------------------------------------------------------

type Estado = (Orilla,Orilla,Orilla,Orilla)

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir 
--    inicial:: Estado
-- tal que inicial representa el estado en el que todos están en la
-- orilla izquierda.
-- ---------------------------------------------------------------------

-- fracruzam alvalvdom1 carruirui3 marvilmor erisancha manvermor
-- ivaruicam abrdelrod juanarcon javperlag manpende rubvilval fatvilpiz
-- isrbelnun 
inicial:: Estado
inicial = (I,I,I,I)

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir 
--    final:: Estado
-- tal que final representa el estado en el que todos están en la
-- orilla derecha.
-- ---------------------------------------------------------------------

-- fracruzam alvalvdom1 carruirui3 marvilmor erisancha manvermor ivaruicam
-- juanarcon javperlag manpende rubvilval abrdelrod fatvilpiz isrbelnun
final:: Estado
final = (D,D,D,D)

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la función
--   seguro :: Estado -> Bool
-- tal que (seguro e) se verifica si el estado e es seguro; es decir,
-- que no puede estar en una orilla el lobo con la cabra sin el granjero
-- ni la cabra con el repollo sin el granjero. Por ejemplo,
--    seguro (I,D,D,I)  ==  False
--    seguro (D,D,D,I)  ==  True
--    seguro (D,D,I,I)  ==  False
--    seguro (I,D,I,I)  ==  True
-- ---------------------------------------------------------------------

-- fracruzam carruirui3 erisancha manvermor juanarcon abrdelrod
-- fatvilpiz isrbelnun 
seguro :: Estado -> Bool
seguro (g,l,c,r) | l == c && l /= g = False
                 | c == r && c /= g = False
                 | otherwise        = True

-- alvalvdom1 marvilmor javperlag rubvilval
seguro2 :: Estado -> Bool
seguro2 (g,l,c,r) = not $ g /= c && (c == l || c == r)

-- ivaruicam manpende
seguro3 :: Estado -> Bool
seguro3 (g,l,c,r) | g == l    = c == g || r == g
                  | otherwise = c == g

-- ---------------------------------------------------------------------
-- Ejercicio 6. Definir la función 
--    opuesta :: Orilla -> Orilla
-- tal que (opuesta x) es la opuesta de la orilla x. Por ejemplo
--    opuesta I = D 
-- ---------------------------------------------------------------------

-- fracruzam alvalvdom1 carruirui3 marvilmor juanarcon manpende
-- rubvilval fatvilpiz isrbelnun 
opuesta :: Orilla -> Orilla
opuesta I = D
opuesta D = I

-- erisancha manvermor ivaruicam javperlag abrdelrod
opuesta2 :: Orilla -> Orilla
opuesta2 o = if o == D then I else D

-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir la función
--    sucesoresE :: Estado -> [Estado]
-- tal que (sucesoresE e) es la lista de los sucesores seguros del
-- estado e. Por ejemplo,
--    sucesoresE (I,I,I,I)  ==  [(D,I,D,I)]
--    sucesoresE (D,I,D,I)  ==  [(I,I,D,I),(I,I,I,I)]
-- ---------------------------------------------------------------------

-- alvalvdom1 marvilmor manvermor ivaruicam juanarcon fracruzam
-- javperlag abrdelrod manpende rubvilval fatvilpiz
sucesoresE :: Estado -> [Estado]
sucesoresE (g,l,c,r) =
    filter seguro [(g',l,c,r),
                   (g',opuesta l,c,r),
                   (g',l,opuesta c,r),
                   (g',l,c,opuesta r)]
    where g' = opuesta g

-- erisancha
sucesoresE3 :: Estado -> [Estado]
sucesoresE3 e = [x | x <- opon e, seguro x]

opon (g,l,c,r) = 
    [(opuesta g,l,        c,        r),
     (opuesta g,opuesta l,c,        r),
     (opuesta g,l,        opuesta c,r),
     (opuesta g,l,        c,        opuesta r)]

-- isrbelnun
sucesoresE2 :: Estado -> [Estado]
sucesoresE2 (g,l,c,r) =
    filter seguro [(g',l,c,r),(g',l',c,r),(g',l,c',r),(g',l,c,r')]
    where g' = opuesta g
          l' = opuesta l
          c' = opuesta c
          r' = opuesta r

-- ---------------------------------------------------------------------
-- Ejercicio 8. Los nodos del espacio de búsqueda son lista de estados
-- distintos 
--    [e_n, ..., e_2, e_1]
-- donde e_1 es el estado inicial y para cada i (2 <= i <= n), e_i es un
-- sucesor de e_(i-1). 
-- 
-- Definir el tipo de datos NodoRio para representar los nodos del
-- espacio de búsqueda. Por ejemplo, 
--    ghci> :type (Nodo [(I,I,D,I),(I,I,I,I)])
--    (Nodo [(I,I,D,I),(I,I,I,I)]) :: NodoRio
-- ---------------------------------------------------------------------

data NodoRio = Nodo [Estado] 
               deriving (Eq, Show)

-- ---------------------------------------------------------------------
-- Ejercicio 9. Definir la función
--    sucesoresN :: NodoRio -> [NodoRio]
-- tal que (sucesoresN n) es la lista de los sucesores del nodo n; es
-- decir, de los nodos obtenidos ampliando n con los sucesores de su
-- primer estado que no pertenecen n. Por ejemplo, 
--    ghci> sucesoresN (Nodo [(I,I,D,I),(D,I,D,I),(I,I,I,I)])
--    [Nodo [(D,D,D,I),(I,I,D,I),(D,I,D,I),(I,I,I,I)],
--     Nodo [(D,I,D,D),(I,I,D,I),(D,I,D,I),(I,I,I,I)]]
-- ---------------------------------------------------------------------

-- alvalvdom1 erisancha manvermor juanarcon manpende rubvilval abrdelrod
sucesoresN :: NodoRio -> [NodoRio]
sucesoresN (Nodo n) = 
    [Nodo (x:n) | x <- sucesoresE (head n), notElem x n]

-- fracruzam javperlag
sucesoresN3 :: NodoRio -> [NodoRio]
sucesoresN3 (Nodo (x:xs)) = 
    [Nodo (y:x:xs) | y <- sucesoresE x, notElem y xs]

-- isrbelnun
sucesoresN4 :: NodoRio -> [NodoRio]
sucesoresN4 (Nodo (x:xs)) =
    inserta (filter (`notElem` xs) (sucesoresE x)) (Nodo (x:xs))

inserta :: [Estado] -> NodoRio -> [NodoRio]
inserta [] (Nodo ys)     = []
inserta (x:xs) (Nodo ys) = (Nodo (x:ys)) : (inserta xs (Nodo ys))

-- ---------------------------------------------------------------------
-- Ejercicio 10. Definir la función
--    esFinal:: NodoRio -> Bool
-- tal que (esFinal n) se verifica si n es un nodo final; es decir, su
-- primer elemento es el estado final. Por ejemplo,
--    esFinal (Nodo [(D,D,D,D),(I,I,I,I)])  ==  True
--    esFinal (Nodo [(I,I,D,I),(I,I,I,I)])  ==  False
-- ---------------------------------------------------------------------

-- fracruzam carruirui3 erisancha manpende
esFinal:: NodoRio -> Bool
esFinal (Nodo ((D,D,D,D):_)) = True
esFinal  _                   = False

-- alvalvdom1 marvilmor manvermor ivaruicam juanarcon javperlag
-- rubvilval abrdelrod 
esFinal2:: NodoRio -> Bool
esFinal2 (Nodo n) = head n == final

-- isrbelnun
esFinal3:: NodoRio -> Bool
esFinal3 (Nodo (x:xs)) = x == final
                               
-- ---------------------------------------------------------------------
-- Ejercicio 11. Definir la función
--    granjeroEE :: [NodoRio]
-- tal que granjeroEE son las soluciones del problema del granjero
-- mediante el patrón de búsqueda en espacio de estados. Por ejemplo,
--    ghci> head granjeroEE
--    Nodo [(D,D,D,D),(I,D,I,D),(D,D,I,D),(I,D,I,I),
--          (D,D,D,I),(I,I,D,I),(D,I,D,I),(I,I,I,I)]
--    ghci> length granjeroEE
--    2
-- ---------------------------------------------------------------------

-- fracruzam carruirui3 erisancha juanarcon isrbelnun
granjeroEE :: [NodoRio]
granjeroEE = busqueda [Nodo [inicial]]
  where busqueda :: [NodoRio] -> [NodoRio]
        busqueda (x:xs) | esFinal x = x: busqueda xs
                        | otherwise = busqueda (xs ++ sucesoresN x)
        busqueda  _     = []

-- alvalvdom1 marvilmor manvermor ivaruicam javperlag manpende rubvilval
-- abrdelrod 
granjeroEE2 :: [NodoRio]
granjeroEE2 = buscaEE sucesoresN esFinal (Nodo [inicial])
