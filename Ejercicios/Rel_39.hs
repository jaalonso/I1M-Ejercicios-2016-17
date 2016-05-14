-- I1M 2015-16: Relaci�n 39 (14 de mayo de 2016)
-- El problema del granjero mediante b�squeda en espacio de estado.
-- Departamento de Ciencias de la Computaci�n e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducci�n                                                       --
-- ---------------------------------------------------------------------

-- Un granjero est� parado en un lado del r�o y con �l tiene un lobo,
-- una cabra y una repollo. En el r�o hay un barco peque�o. El granjero
-- desea cruzar el r�o con sus tres posesiones. No hay puentes y en el
-- barco hay solamente sitio para el granjero y un art�culo. Si deja 
-- la cabra con la repollo sola en un lado del r�o la cabra comer� la
-- repollo. Si deja el lobo y la cabra en un lado, el lobo se comer� a
-- la cabra. �C�mo puede cruzar el granjero el r�o con los tres
-- art�culos, sin que ninguno se coma al otro?
-- 
-- El objetivo de esta relaci�n de ejercicios es resolver el problema
-- del granjero mediante b�squeda en espacio de estados, utilizando las
-- implementaciones estudiadas en el tema 23 
--    http://www.cs.us.es/~jalonso/cursos/i1m-15/temas/tema-23.html
-- 
-- Para realizar los ejercicios hay que tener instalada la librer�a I1M
-- que contiene la implementaci�n de TAD de los polinomios. Los pasos
-- para instalarla son los siguientes:
-- + Descargar el paquete I1M desde http://bit.ly/1pbnDqm
-- + Descomprimirlo (y se crea el directorio I1M-master.zip).
-- + Cambiar al directorio I1M-master.
-- + Ejecutar cabal install I1M.cabal
-- 
-- Otra forma es descargar, en el directorio de ejercicios, la
-- implementaci�n de la b�squeda en espacio de estado
-- + BusquedaEnEspaciosDeEstados.hs http://bit.ly/1Tcb9KB
-- 
-- Los m�dulos anteriores se encuentras en la p�gina de c�digos 
-- http://bit.ly/1SQnAKO
 
-- ---------------------------------------------------------------------
-- Importaciones                                                      --
-- ---------------------------------------------------------------------

-- Hay que elegir una librer�a
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
-- representan en qu� orilla se encuentra cada uno de los elementos
-- (granjero, lobo, cabra, repollo). Por ejemplo, (I,D,D,I) representa
-- que el granjero est� en la izquierda, que el lobo est� en la derecha,
-- que la cabra est� en la derecha y el repollo est� en la izquierda.
-- ---------------------------------------------------------------------

type Estado = (Orilla,Orilla,Orilla,Orilla)

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir 
--    inicial:: Estado
-- tal que inicial representa el estado en el que todos est�n en la
-- orilla izquierda.
-- ---------------------------------------------------------------------

inicial:: Estado
inicial = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir 
--    final:: Estado
-- tal que final representa el estado en el que todos est�n en la
-- orilla derecha.
-- ---------------------------------------------------------------------

final:: Estado
final = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la funci�n
--   seguro :: Estado -> Bool
-- tal que (seguro e) se verifica si el estado e es seguro; es decir,
-- que no puede estar en una orilla el lobo con la cabra sin el granjero
-- ni la cabra con el repollo sin el granjero. Por ejemplo,
--    seguro (I,D,D,I)  ==  False
--    seguro (D,D,D,I)  ==  True
--    seguro (D,D,I,I)  ==  False
--    seguro (I,D,I,I)  ==  True
-- ---------------------------------------------------------------------

seguro :: Estado -> Bool
seguro (g,l,c,r) = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 6. Definir la funci�n 
--    opuesta :: Orilla -> Orilla
-- tal que (opuesta x) es la opuesta de la orilla x. Por ejemplo
--    opuesta I = D 
-- ---------------------------------------------------------------------

opuesta :: Orilla -> Orilla
opuesta = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir la funci�n
--    sucesoresE :: Estado -> [Estado]
-- tal que (sucesoresE e) es la lista de los sucesores seguros del
-- estado e. Por ejemplo,
--    sucesoresE (D,I,D,I)  ==  [(I,I,D,I),(I,I,I,I)]
-- ---------------------------------------------------------------------

sucesoresE :: Estado -> [Estado]
sucesoresE e = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 8. Los nodos del espacio de b�squeda son lista de estados 
--    [e_n, ..., e_2, e_1]
-- donde e_1 es el estado inicial y para cada i (2 <= i <= n), e_i es un
-- sucesor de e_(i-1). 
-- 
-- Definir el tipo de datos NodoRio para representar los nodos del
-- espacio de b�squeda. Por ejemplo, 
--    ghci> :type (Nodo [(I,I,D,I),(I,I,I,I)])
--    (Nodo [(I,I,D,I),(I,I,I,I)]) :: NodoRio
-- ---------------------------------------------------------------------

data NodoRio = Nodo [Estado] 
               deriving (Eq, Show)

-- ---------------------------------------------------------------------
-- Ejercicio 9. Definir la funci�n
--    sucesoresN :: NodoRio -> [NodoRio]
-- tal que (sucesoresN n) es la lista de los sucesores del nodo n. Por
-- ejemplo, 
--    ghci> sucesoresN (Nodo [(I,I,D,I),(D,I,D,I),(I,I,I,I)])
--    [Nodo [(D,D,D,I),(I,I,D,I),(D,I,D,I),(I,I,I,I)],
--     Nodo [(D,I,D,D),(I,I,D,I),(D,I,D,I),(I,I,I,I)]]
-- ---------------------------------------------------------------------

sucesoresN :: NodoRio -> [NodoRio]
sucesoresN = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 10. Definir la funci�n
--    esFinal:: NodoRio -> Bool
-- tal que (esFinal n) se verifica si n es un nodo final; es decir, su
-- primer elemento es el estado final. Por ejemplo,
--    esFinal (Nodo [(D,D,D,D),(I,I,I,I)])  ==  True
--    esFinal (Nodo [(I,I,D,I),(I,I,I,I)])  ==  False
-- ---------------------------------------------------------------------

esFinal:: NodoRio -> Bool
esFinal = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 11. Definir la funci�n
--    granjeroEE :: [NodoRio]
-- tal que granjeroEE son las soluciones del problema del granjero
-- mediante el patr�n de b�squeda en espacio de estados. Por ejemplo,
--    ghci> head granjeroEE
--    Nodo [(D,D,D,D),(I,D,I,D),(D,D,I,D),(I,D,I,I),
--          (D,D,D,I),(I,I,D,I),(D,I,D,I),(I,I,I,I)]
-- ---------------------------------------------------------------------

granjeroEE :: [NodoRio]
granjeroEE = undefined

