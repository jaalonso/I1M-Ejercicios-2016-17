-- I1M 2015-16: Relaci�n 32 (8 de abril de 2016)
-- Algoritmos de ordenaci�n y complejidad.
-- Departamento de Ciencias de la Computaci�n e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducci�n                                                       --
-- ---------------------------------------------------------------------

-- El objetivo de esta relaci�n es presentar una recopilaci�n de los
-- algoritmos de ordenaci�n y el estudio de su complejidad.

-- ---------------------------------------------------------------------
-- � Librer�as auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.List
import qualified I1M.ColaDePrioridad as CP

-- ---------------------------------------------------------------------
-- � Ordenaci�n por selecci�n                                         --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Para ordenar una lista xs mediante el algoritmo de
-- ordenaci�n por selecci�n se selecciona el menor elemento de xs y se
-- le a�ade a la ordenaci�n por selecci�n de los restantes. Por ejemplo,
-- para ordenar la lista [3,1,4,1,5,9,2] el proceso es el siguiente:
--       ordenaPorSeleccion [3,1,4,1,5,9,2] 
--     = 1 : ordenaPorSeleccion [3,4,1,5,9,2] 
--     = 1 : 1 : ordenaPorSeleccion [3,4,5,9,2] 
--     = 1 : 1 : 2 : ordenaPorSeleccion [3,4,5,9] 
--     = 1 : 1 : 2 : 3 : ordenaPorSeleccion [4,5,9] 
--     = 1 : 1 : 2 : 3 : 4 : ordenaPorSeleccion [5,9] 
--     = 1 : 1 : 2 : 3 : 4 : 5 : ordenaPorSeleccion [9] 
--     = 1 : 1 : 2 : 3 : 4 : 5 : 9 : ordenaPorSeleccion [] 
--     = 1 : 1 : 2 : 3 : 4 : 5 : 9 : []
--     = [1,1,2,3,4,5,9]
--
-- Definir la funci�n 
--    ordenaPorSeleccion :: Ord a => [a] -> [a]
-- tal que (ordenaPorSeleccion xs) es la lista obtenida ordenando por
-- selecci�n la lista xs. Por ejemplo,
--    ordenaPorSeleccion [3,1,4,1,5,9,2]  ==  [1,1,2,3,4,5,9]
-- ---------------------------------------------------------------------

ordenaPorSeleccion :: Ord a => [a] -> [a]
ordenaPorSeleccion = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Calcular los tiempos necesarios para calcular 
--     let n = k in length (ordenaPorSeleccion [n,n-1..1])
-- para k en [1000, 2000, 3000, 4000].
-- 
-- �Cu�l es el orden de complejidad de ordenaPorSeleccion?
-- ---------------------------------------------------------------------

-- El resumen de los tiempos es
--    k    | segs.
--    -----+-----
--    1000 | 
--    2000 | 
--    3000 | 
--    4000 | 

-- ---------------------------------------------------------------------
-- Ejercicio 1.3. Definir la funci�n
--    ordenaPorSeleccion2 :: Ord a => [a] -> [a] 
-- tal que (ordenaPorSeleccion2 xs) es la lista xs ordenada por el
-- algoritmo de selecci�n, pero usando un acumulador. Por ejemplo,
--    ordenaPorSeleccion2 [3,1,4,1,5,9,2]  ==  [1,1,2,3,4,5,9]
-- ---------------------------------------------------------------------

ordenaPorSeleccion2 :: Ord a => [a] -> [a] 
ordenaPorSeleccion2 = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 1.4. Calcular los tiempos necesarios para calcular 
--    let n = k in length (ordenaPorSeleccion2 [n,n-1..1])
-- para k en [1000, 2000, 3000, 4000]
-- ---------------------------------------------------------------------

-- El resumen de los tiempos es
--    k    | segs.
--    -----+-----
--    1000 | 
--    2000 | 
--    3000 | 
--    4000 | 

-- ---------------------------------------------------------------------
-- � Ordenaci�n r�pida (Quicksort)                                    --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Para ordenar una lista xs mediante el algoritmo de
-- ordenaci�n r�pida se selecciona el primer elemento x de xs, se divide
-- los restantes en los menores o iguales que x y en los mayores que x,
-- se ordena cada una de las dos partes y se unen los resultados. Por
-- ejemplo, para ordenar la lista [3,1,4,1,5,9,2] el proceso es el
-- siguiente: 
--       or [3,1,4,1,5,9,2]
--     = or [1,1,2] ++ [3] ++ or [4,5,9]  
--     = (or [1] ++ [1] ++ or [2]) ++ [3] ++ (or [] ++ [4] ++ or [5,9])
--     = ((or [] ++ [1] ++ or []) ++ [1] ++ (or [] ++ [2] ++ or [])) 
--       ++ [3] ++ ([] ++ [4] ++ (or [] ++ [5] ++ or [9]))
--     = (([] ++ [1] ++ []) ++ [1] ++ ([] ++ [2] ++ [])) 
--       ++ [3] ++ ([4] ++ ([] ++ [5] ++ (or [] ++ [9] ++ or [])))
--     = ([1] ++ [1] ++ [2] ++ 
--       ++ [3] ++ ([4] ++ ([5] ++ (or [] ++ [9] ++ or [])))
--     = ([1] ++ [1] ++ [2] ++ 
--       ++ [3] ++ ([4] ++ ([5] ++ ([] ++ [9] ++ [])))
--     = ([1] ++ [1] ++ [2] ++ 
--       ++ [3] ++ ([4] ++ ([5] ++ [9]))
--     = [1,1,2,3,4,5,9]
--
-- Definir la funci�n 
--    ordenaRapida :: Ord a => [a] -> [a]
-- tal que (ordenaRapida xs) es la lista obtenida ordenando por
-- selecci�n la lista xs. Por ejemplo,
--    ordenaRapida [3,1,4,1,5,9,2]  ==  [1,1,2,3,4,5,9]
-- ---------------------------------------------------------------------

ordenaRapida :: Ord a => [a] -> [a]
ordenaRapida = undefined
     
-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Calcular los tiempos necesarios para calcular 
--     let n = k in length (ordenaRapida [n,n-1..1])
-- para k en [1000, 2000, 3000, 4000]
-- 
-- �Cu�l es el orden de complejidad de ordenaRapida?
-- ---------------------------------------------------------------------

-- El resumen de los tiempos es
--    k    | segs.
--    -----+------
--    1000 |
--    2000 |
--    3000 |
--    4000 | 

-- ---------------------------------------------------------------------
-- Ejercicio 2.3. Definir, usando un acumulador, la funci�n
--    ordenaRapida2 :: Ord a => [a] -> [a]
-- tal que (ordenaRapida2 xs) es la lista obtenida ordenando xs
-- por el procedimiento de ordenaci�n r�pida. Por ejemplo, 
--    ordenaRapida2 [3,1,4,1,5,9,2]  ==  [1,1,2,3,4,5,9]
-- ---------------------------------------------------------------------

ordenaRapida2 :: Ord a => [a] -> [a]
ordenaRapida2 xs = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 2.4. Calcular los tiempos necesarios para calcular 
--     let n = k in length (ordenaRapida2 [n,n-1..1])
-- para k en [1000, 2000, 3000, 4000]
-- ---------------------------------------------------------------------

-- El resumen de los tiempos es
--    k    | segs.
--    -----+------
--    1000 |
--    2000 |
--    3000 |
--    4000 |

-- ---------------------------------------------------------------------
-- � Ordenaci�n por inserci�n                                         --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Para ordenar una lista xs mediante el algoritmo de
-- ordenaci�n por inserci�n se selecciona el primer elemento x de xs, se
-- ordena el resto de xs y se inserta x en su lugar. Por ejemplo, para
-- ordenar la lista [3,1,4,1,5,9,2] el proceso es el siguiente: 
--      ordenaPorInsercion [3,1,4,1,5,9,2]
--    = 3 : ordenaPorInsercion [1,4,1,5,9,2]
--    = 3 : 1 : ordenaPorInsercion [4,1,5,9,2]
--    = 3 : 1 : 4 : ordenaPorInsercion [1,5,9,2]
--    = 3 : 1 : 4 : 1 : ordenaPorInsercion [5,9,2]
--    = 3 : 1 : 4 : 1 : 5 : ordenaPorInsercion [9,2]
--    = 3 : 1 : 4 : 1 : 5 : 9 : ordenaPorInsercion [2]
--    = 3 : 1 : 4 : 1 : 5 : 9 : 2 : ordenaPorInsercion []
--    = 3 : 1 : 4 : 1 : 5 : 9 : 2 : []
--    = 3 : 1 : 4 : 1 : 5 : 9 : [2]
--    = 3 : 1 : 4 : 1 : 5 : [2,9]
--    = 3 : 1 : 4 : 1 : [2,5,9]
--    = 3 : 1 : 4 : [1,2,5,9]
--    = 3 : 1 : [1,2,4,5,9]
--    = 3 : [1,1,2,4,5,9]
--    = [1,1,2,3,4,5,9]
--
-- Definir la funci�n 
--    ordenaPorInsercion :: Ord a => [a] -> [a]
-- tal que (ordenaPorInsercion xs) es la lista obtenida ordenando por
-- selecci�n la lista xs. Por ejemplo,
--    ordenaPorInsercion [3,1,4,1,5,9,2]  ==  [1,1,2,3,4,5,9]
-- ---------------------------------------------------------------------

ordenaPorInsercion :: Ord a => [a] -> [a]
ordenaPorInsercion = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Calcular los tiempos necesarios para calcular 
--     let n = k in length (ordenaPorInsercion [n,n-1..1])
-- para k en [1000, 2000, 3000, 4000]
--
-- �Cu�l es la complejidad de ordenaPorInsercion?
-- ---------------------------------------------------------------------

-- El resumen de los tiempos es
--    k    | segs.
--    -----+-----
--    1000 | 
--    2000 | 
--    3000 | 
--    4000 | 


-- ---------------------------------------------------------------------
-- Ejercicio 3.3. Definir, por plegados, la funci�n
--    ordenaPorInsercion2 :: Ord a => [a] -> [a]
-- tal que (ordenaPorInsercion2 xs) es la lista obtenida ordenando xs
-- por el procedimiento de ordenaci�n por inserci�n. Por ejemplo, 
--    ordenaPorInsercion2 [3,1,4,1,5,9,2]  ==  [1,1,2,3,4,5,9]
-- ---------------------------------------------------------------------

ordenaPorInsercion2 :: Ord a => [a] -> [a]
ordenaPorInsercion2 xs = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Calcular los tiempos necesarios para calcular 
--     let n = k in length (ordenaPorInsercion2 [n,n-1..1])
-- para k en [1000, 2000, 3000, 4000]
-- ---------------------------------------------------------------------

-- El resumen de los tiempos es
--    k    | segs.
--    -----+------
--    1000 | 
--    2000 | 
--    3000 | 
--    4000 | 

-- ---------------------------------------------------------------------
-- � Ordenaci�n por mezcla ("Mergesort")                              --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. Para ordenar una lista xs mediante el algoritmo de
-- ordenaci�n por mezcla se divide xs por la mitad, se ordena cada una
-- de las partes y se mezclan los resultados. Por ejemplo, para
-- ordenar la lista [3,1,4,1,5,9,2] el proceso es el siguiente: 
--      om [3,1,4,1,5,9,2]
--    = m (om [3,1,4]) (om 1,5,9,2])  
--    = m (m (om [3]) (om [1,4])) (m (om [1,5]) (om [9,2]))
--    = m (m [3] (m (om [1]) (om [4]))) 
--        (m (m (om [1]) (om [5])) (m (om [9]) (om [2])))
--    = m (m [3] (m [1] [4])) 
--        (m (m [1] [5]) (m [9] [2]))
--    = m (m [3] [1,4]) (m [1,5] [2,9])
--    = m [1,3,4] [1,2,5,9]
--    = [1,1,2,3,4,5,9]
-- donde om es ordenaPorMezcla y m es mezcla.
--
-- Definir la funci�n 
--    ordenaPorMezcla :: Ord a => [a] -> [a]
-- tal que (ordenaPorMezcla xs) es la lista obtenida ordenando por
-- selecci�n la lista xs. Por ejemplo,
--    ordenaPorMezcla [3,1,4,1,5,9,2]  ==  [1,1,2,3,4,5,9]
-- ---------------------------------------------------------------------

ordenaPorMezcla :: Ord a => [a] -> [a]
ordenaPorMezcla = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Calcular los tiempos necesarios para calcular 
--     let n = k in length (ordenaPorMezcla [n,n-1..1])
-- para k en [1000, 2000, 3000, 4000]
--
-- �Cu�l es la complejidad de ordenaPorMezcla?
-- ---------------------------------------------------------------------

-- El resumen de los tiempos es
--    k    | segs.
--    -----+-----
--    1000 | 0.02
--    2000 | 0.03
--    3000 | 0.05
--    4000 | 0.06

-- ---------------------------------------------------------------------
-- Ejercicio 4.3. Otra forma de ordenar una lista xs mediante el
-- algoritmo de ordenaci�n por mezcla consiste en dividir xs en listas
-- unitarias y mezclar los resultados. Por ejemplo, para
-- ordenar la lista [3,1,4,1,5,9,2] el proceso es el siguiente: 
--      om [3,1,4,1,5,9,2]
--    = mp [[3],[1],[4],[1],[5],[9],[2]]
--    = mp [[1,3],[1,4],[5,9],[2]]
--    = mp [[1,1,3,4],[2,5,9]]
--    = [1,1,2,3,4,5,9]
-- donde om es ordenaPorMezcla y mp es mezclaPares.
--
-- Definir la funci�n 
--    ordenaPorMezcla2 :: Ord a => [a] -> [a]
-- tal que (ordenaPorMezcla2 xs) es la lista obtenida ordenando por
-- selecci�n la lista xs. Por ejemplo,
--    ordenaPorMezcla2 [3,1,4,1,5,9,2]  ==  [1,1,2,3,4,5,9]
-- ---------------------------------------------------------------------

ordenaPorMezcla2 :: Ord a => [a] -> [a]
ordenaPorMezcla2 xs = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 4.4. Calcular los tiempos necesarios para calcular 
--     let n = k in length (ordenaPorMezcla2 [n,n-1..1])
-- para k en [1000, 2000, 3000, 4000]
-- ---------------------------------------------------------------------

-- El resumen de los tiempos es
--    k    | segs.
--    -----+-----
--    1000 | 
--    2000 | 
--    3000 | 
--    4000 | 

-- ---------------------------------------------------------------------
-- � Ordenaci�n por mont�culos ("heapsort")                           --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 5.1. El procedimiento de ordenaci�n de una lista por
-- mont�culos consiste en almacenar todos los elementos del vector a
-- ordenar en un mont�culo (heap), y luego extraer el nodo que queda
-- como nodo ra�z del mont�culo (cima) en sucesivas iteraciones
-- obteniendo el conjunto ordenado.  
-- 
-- Usando la implementaci�n de las colas de prioridad mediante
-- mont�culos (que se encuentra en la librer�a I1M.ColaDePrioridad),
-- definir la funci�n 
--    ordenaPorMonticulos :: Ord a => [a] -> [a]
-- tal que (ordenaPorMonticulos xs) es la lista obtenida ordenando xs
-- por el procedimiento de ordenaci�n por mont�culos. Por ejemplo, 
--    ordenaPorMonticulos [3,1,4,1,5,9,2]  ==  [1,1,2,3,4,5,9]
-- ---------------------------------------------------------------------

ordenaPorMonticulos :: Ord a => [a] -> [a]
ordenaPorMonticulos xs = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 5.2. Calcular los tiempos necesarios para calcular 
--     let n = k in length (ordenaPorMonticulos [n,n-1..1])
-- para k en [1000, 2000, 3000, 4000]
--
-- �Cu�l es la complejidad de ordenaPorMonticulos?
-- ---------------------------------------------------------------------

-- El resumen de los tiempos es
--    k    | segs.
--    -----+-----
--    1000 | 
--    2000 | 
--    3000 | 
--    4000 | 
