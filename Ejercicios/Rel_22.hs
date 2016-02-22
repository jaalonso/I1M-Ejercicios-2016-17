-- I1M 2015-16: Relaci�n 22 (19 de febrero de 2016)
-- Enumeraciones de los n�meros racionales. 
-- Departamento de Ciencias de la Computaci�n e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducci�n                                                       --
-- ---------------------------------------------------------------------

-- El objetivo de esta relaci�n es construir dos enumeraciones de los
-- n�meros racionales. Concretamente, 
-- + una enumeraci�n basada en las representaciones hiperbinarias y
-- + una enumeraci�n basada en los los �rboles de Calkin-Wilf.
-- Tambi�n se incluye la comprobaci�n de la igualdad de las dos
-- sucesiones y una forma alternativa de calcular el n�mero de
-- representaciones hiperbinarias mediante la funci�n fucs.
-- 
-- Esta relaci�n se basa en los siguientes art�culos:
-- + Gaussianos "Sorpresa sumando potencias de 2" http://goo.gl/AHdAG
-- + N. Calkin y H.S. Wilf "Recounting the rationals" http://goo.gl/gVZtW
-- + Wikipedia "Calkin-Wilf tree" http://goo.gl/cB3vn

-- ---------------------------------------------------------------------
-- Importaci�n de librer�as                                           --
-- ---------------------------------------------------------------------

import Data.List
import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Numeraci�n de los racionales mediante representaciones hiperbinarias
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la constante
--    potenciasDeDos :: [Integer]
-- tal que potenciasDeDos es la lista de las potencias de 2. Por
-- ejemplo, 
--    take 10 potenciasDeDos  ==  [1,2,4,8,16,32,64,128,256,512]
-- ---------------------------------------------------------------------

potenciasDeDos :: [Integer]
potenciasDeDos = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la funci�n
--    empiezaConDos :: Eq a => a -> [a] -> Bool
-- tal que (empiezaConDos x ys) se verifica si los dos primeros
-- elementos de ys son iguales a x. Por ejemplo,
--    empiezaConDos 5 [5,5,3,7]  ==  True
--    empiezaConDos 5 [5,3,5,7]  ==  False
--    empiezaConDos 5 [5,5,5,7]  ==  True
-- ---------------------------------------------------------------------

empiezaConDos :: Eq a => a -> [a] -> Bool
empiezaConDos = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la funci�n
--    representacionesHB :: Integer -> [[Integer]]
-- tal que (representacionesHB n) es la lista de las representaciones
-- hiperbinarias del n�mero n como suma de potencias de 2 donde cada
-- sumando aparece como m�ximo 2 veces. Por ejemplo
--    representacionesHB 5  ==  [[1,2,2],[1,4]]
--    representacionesHB 6  ==  [[1,1,2,2],[1,1,4],[2,4]]
-- ---------------------------------------------------------------------

representacionesHB :: Integer -> [[Integer]]
representacionesHB n = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la funci�n
--    nRepresentacionesHB :: Integer -> Integer
-- tal que (nRepresentacionesHB n) es el n�mero de las representaciones 
-- hiperbinarias del n�mero n como suma de potencias de 2 donde cada
-- sumando aparece como m�ximo 2 veces. Por ejemplo,
--    ghci> [nRepresentacionesHB n | n <- [0..20]]
--    [1,1,2,1,3,2,3,1,4,3,5,2,5,3,4,1,5,4,7,3,8]
-- ---------------------------------------------------------------------

nRepresentacionesHB :: Integer -> Integer
nRepresentacionesHB = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la funci�n
--    termino :: Integer -> (Integer,Integer)
-- tal que (termino n) es el par formado por el n�mero de
-- representaciones hiperbinarias de n y de n+1 (que se interpreta como 
-- su cociente). Por ejemplo, 
--    termino 4  ==  (3,2)
-- ---------------------------------------------------------------------

termino :: Integer -> (Integer,Integer)
termino n = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 6. Definir la funci�n
--    sucesionHB :: [(Integer,Integer)]
-- sucesionHB es la la sucesi�n cuyo t�mino n-�simo es (termino n); es
-- decir, el par formado por el n�mero de representaciones hiperbinarias
-- de n y de n+1. Por ejemplo, 
--    ghci> take 10 sucesionHB
--    [(1,1),(1,2),(2,1),(1,3),(3,2),(2,3),(3,1),(1,4),(4,3),(3,5)]
-- ---------------------------------------------------------------------

sucesionHB :: [(Integer,Integer)]
sucesionHB = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 7. Comprobar con QuickCheck que, para todo n,
-- (nRepresentacionesHB n) y  (nRepresentacionesHB (n+1)) son primos
-- entre s�. 
-- ---------------------------------------------------------------------

prop_irreducibles :: Integer -> Property
prop_irreducibles n = undefined
    n >= 0 ==> 
    gcd (nRepresentacionesHB n) (nRepresentacionesHB (n+1)) == 1

-- La comprobaci�n es

-- ---------------------------------------------------------------------
-- Ejercicio 8. Comprobar con QuickCheck que todos los elementos de la
-- sucesionHB son distintos.
-- ---------------------------------------------------------------------

prop_distintos :: Integer -> Integer -> Bool
prop_distintos n m = undefined

-- La comprobaci�n es

-- ---------------------------------------------------------------------
-- Ejercicio 9. Definir la funci�n
--    contenido :: Integer -> Integer -> Bool
-- tal que (contenido n) se verifica si la expresiones reducidas de
-- todas las fracciones x/y, con x e y entre 1 y n, pertenecen a la
-- sucesionHB. Por ejemplo,  
--    contenidos 5  ==  True
-- ---------------------------------------------------------------------

contenido :: Integer -> Bool
contenido n = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 10. Definir la funci�n
--    indice :: (Integer,Integer) -> Integer
-- tal que (indice (a,b)) es el �ndice del par (a,b) en la sucesi�n de
-- los racionales. Por ejemplo, 
--    indice (3,2)  ==  4
-- ---------------------------------------------------------------------

indice :: (Integer,Integer) -> Integer
indice (a,b) = undefined

-- ---------------------------------------------------------------------
-- Numeraciones mediante �rboles de Calkin-Wilf                       --
-- ---------------------------------------------------------------------

-- El �rbol de Calkin-Wilf es el �rbol definido por las siguientes
-- reglas:
--    * El nodo ra�z es el (1,1)
--    * Los hijos del nodo (x,y) son (x,x+y) y (x+y,y)
-- Por ejemplo, los 4 primeros niveles del �rbol de Calkin-Wilf son 
--                         (1,1)
--                           |
--               +-----------+-----------+
--               |                       |
--             (1,2)                   (2,1)
--               |                       |
--         +-----+-----+           +-----+-----+
--         |           |           |           |
--       (1,3)       (3,2)       (2,3)       (3,1)
--         |           |           |           |         
--      +--+--+     +--+--+     +--+--+     +--+--+
--      |     |     |     |     |     |     |     | 
--    (1,4) (4,3) (3,5) (5,2) (2,5) (5,3) (3,4) (4,1)

-- ---------------------------------------------------------------------
-- Ejercicio 11. Definir la funci�n 
--    sucesores :: (Integer,Integer) -> [(Integer,Integer)]
-- tal que (sucesores (x,y)) es la lista de los hijos del par (x,y) en
-- el �rbol de Calkin-Wilf. Por ejemplo, 
--    sucesores (3,2)  ==  [(3,5),(5,2)]
-- ---------------------------------------------------------------------

sucesores :: (Integer,Integer) -> [(Integer,Integer)]
sucesores (x,y) = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 12. Definir la funci�n
--    siguiente :: [(Integer,Integer)] -> [(Integer,Integer)]
-- tal que (siguiente xs) es la lista formada por los hijos de los
-- elementos de xs en el �rbol de Calkin-Wilf. Por ejemplo, 
--    ghci> siguiente [(1,3),(3,2),(2,3),(3,1)]
--    [(1,4),(4,3),(3,5),(5,2),(2,5),(5,3),(3,4),(4,1)]
-- ---------------------------------------------------------------------

siguiente :: [(Integer,Integer)] -> [(Integer,Integer)]
siguiente xs = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 13. Definir la constante
--    nivelesCalkinWilf:: [[(Integer,Integer)]]
-- tal que nivelesCalkinWilf es la lista de los niveles del �rbol de
-- Calkin-Wilf. Por ejemplo, 
--    ghci> take 4 nivelesCalkinWilf
--    [[(1,1)],
--     [(1,2),(2,1)],
--     [(1,3),(3,2),(2,3),(3,1)],
--     [(1,4),(4,3),(3,5),(5,2),(2,5),(5,3),(3,4),(4,1)]]
-- ---------------------------------------------------------------------

nivelesCalkinWilf:: [[(Integer,Integer)]]
nivelesCalkinWilf = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 14. Definir la constante 
--    sucesionCalkinWilf :: [(Integer,Integer)]
-- tal que sucesionCalkinWilf es la lista correspondiente al recorrido
-- en anchura del �rbol de Calkin-Wilf. Por ejemplo,
--    ghci> take 10 sucesionCalkinWilf
--    [(1,1),(1,2),(2,1),(1,3),(3,2),(2,3),(3,1),(1,4),(4,3),(3,5)]
-- ---------------------------------------------------------------------

sucesionCalkinWilf :: [(Integer,Integer)]
sucesionCalkinWilf = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 15. Definir la funci�n
--    igual_sucesion_HB_CalkinWilf :: Int -> Bool
-- tal que (igual_sucesion_HB_CalkinWilf n) se verifica si los n
-- primeros t�rminos de la sucesi�n HB son iguales que los de la
-- sucesi�n de Calkin-Wilf. Por ejemplo,
--    igual_sucesion_HB_CalkinWilf 20  ==  True
-- ---------------------------------------------------------------------

igual_sucesion_HB_CalkinWilf :: Int -> Bool
igual_sucesion_HB_CalkinWilf n = undefined

-- ---------------------------------------------------------------------
-- N�mero de representaciones hiperbinarias mediante la funci�n fusc
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 16. Definir la funci�n
--    fusc :: Integer -> Integer
-- tal que 
--    fusc(0)    = 1
--    fusc(2n+1) = fusc(n)
--    fusc(2n+2) = fusc(n+1)+fusc(n)
-- Por ejemplo,
--    fusc 4  ==  3
-- ---------------------------------------------------------------------

fusc :: Integer -> Integer
fusc = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 17. Comprobar con QuickCheck que, para todo n, (fusc n) es
-- el n�mero de las representaciones hiperbinarias del n�mero n como
-- suma de potencias de 2 donde cada sumando aparece como m�ximo 2
-- veces; es decir, que las funciones fusc y nRepresentacionesHB son
-- equivalentes. 
-- ---------------------------------------------------------------------

prop_fusc :: Integer -> Bool
prop_fusc n = undefined

-- La comprobaci�n es
