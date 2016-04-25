-- I1M 2015-16: Relación 32 (8 de abril de 2016)
-- Algoritmos de ordenación y complejidad.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- El objetivo de esta relación es presentar una recopilación de los
-- algoritmos de ordenación y el estudio de su complejidad.
-- 
-- Para realizar los ejercicios hay que tener instalada la librería I1M
-- que contiene la implementación de TAD de las colas de prioridad. Los
-- pasos para instalarla son los siguientes:
-- + Descargar el paquete I1M desde http://bit.ly/1pbnDqm
-- + Descomprimirlo (y se crea el directorio I1M-master.zip).
-- + Cambiar al directorio I1M-master.
-- + Ejecutar cabal install I1M.cabal
-- 
-- Otra forma es descargar la implementación del TAD de las colas de
-- prioridad: 
-- + ColaDePrioridadConListas.hs     que está en http://bit.ly/1TJRgv8
-- + ColaDePrioridadConMonticulos.hs que está en http://bit.ly/1TJReDn

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.List

-- Hay que elegir una implementación del TAD de las colas de prioridad:
-- import qualified ColaDePrioridadConListas as CP
-- import qualified ColaDePrioridadConMonticulos as CP
import qualified I1M.ColaDePrioridad as CP

-- ---------------------------------------------------------------------
-- § Ordenación por selección                                         --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Para ordenar una lista xs mediante el algoritmo de
-- ordenación por selección se selecciona el menor elemento de xs y se
-- le añade a la ordenación por selección de los restantes. Por ejemplo,
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
-- Definir la función 
--    ordenaPorSeleccion :: Ord a => [a] -> [a]
-- tal que (ordenaPorSeleccion xs) es la lista obtenida ordenando por
-- selección la lista xs. Por ejemplo,
--    ordenaPorSeleccion [3,1,4,1,5,9,2]  ==  [1,1,2,3,4,5,9]
-- ---------------------------------------------------------------------

-- manvermor fracruzam jespergue josllagam rubvilval manpende
-- alvalvdom1 erisancha isrbelnun silgongal lucgamgal abrdelrod
-- juamorrom1 juanarcon ivaruicam marvilmor
ordenaPorSeleccion :: Ord a => [a] -> [a]
ordenaPorSeleccion [] = []
ordenaPorSeleccion xs = min : ordenaPorSeleccion (delete min xs)
    where min = minimum xs

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Calcular los tiempos necesarios para calcular 
--     let n = k in length (ordenaPorSeleccion [n,n-1..1])
-- para k en [1000, 2000, 3000, 4000].
-- 
-- ¿Cuál es el orden de complejidad de ordenaPorSeleccion?
-- ---------------------------------------------------------------------

-- manvermor jespergue josllagam alvalvdom1 erisancha isrbelnun
-- silgongal abrdelrod juamorrom1 juanarcon ivaruicam marvilmor
orden :: Int -> Int
orden n = length (ordenaPorSeleccion [n,n-1..1])

-- El tiempo es arbitrario y depende de la función.

-- Comentario: Lo interesante no son los tiempos absolutos, sino su
-- crecimiento. 

-- El resumen de los tiempos es
--    k    | segs.
--    -----+-----
--    1000 | 0,05
--    2000 | 0,15
--    3000 | 0,36
--    4000 | 0,67

-- El orden es O(n^2)

-- ---------------------------------------------------------------------
-- Ejercicio 1.3. Definir la función
--    ordenaPorSeleccion2 :: Ord a => [a] -> [a] 
-- tal que (ordenaPorSeleccion2 xs) es la lista xs ordenada por el
-- algoritmo de selección, pero usando un acumulador. Por ejemplo,
--    ordenaPorSeleccion2 [3,1,4,1,5,9,2]  ==  [1,1,2,3,4,5,9]
-- ---------------------------------------------------------------------

-- manvermor fracruzam jespergue josllagam rubvilval manpende
-- alvalvdom1 erisancha isrbelnun silgongal abrdelrod juamorrom1
-- juanarcon ivaruicam marvilmor
ordenaPorSeleccion2 :: Ord a => [a] -> [a] 
ordenaPorSeleccion2 [] = []
ordenaPorSeleccion2 (x:xs) = aux x xs []
    where aux y [] rs = y : ordenaPorSeleccion2 rs
          aux y (x:xs) rs | x < y = aux x xs (y:rs)
                          | otherwise = aux y xs (x:rs)

-- ---------------------------------------------------------------------
-- Ejercicio 1.4. Calcular los tiempos necesarios para calcular 
--    let n = k in length (ordenaPorSeleccion2 [n,n-1..1])
-- para k en [1000, 2000, 3000, 4000]
-- ---------------------------------------------------------------------

-- manvermor jespergue josllagam alvalvdom1 erisancha isrbelnun silgongal
-- lucgamgal abrdelrod juamorrom1 juanarcon ivaruicam marvilmor
orden2 :: Int -> Int
orden2 n = length (ordenaPorSeleccion2 [n,n-1..1])

-- El resumen de los tiempos es
--    k    | segs.
--    -----+-----
--    1000 | 0,03
--    2000 | 0,10
--    3000 | 0,22
--    4000 | 0,43

-- ---------------------------------------------------------------------
-- § Ordenación rápida (Quicksort)                                    --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Para ordenar una lista xs mediante el algoritmo de
-- ordenación rápida se selecciona el primer elemento x de xs, se divide
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
-- Definir la función 
--    ordenaRapida :: Ord a => [a] -> [a]
-- tal que (ordenaRapida xs) es la lista obtenida ordenando por
-- selección la lista xs. Por ejemplo,
--    ordenaRapida [3,1,4,1,5,9,2]  ==  [1,1,2,3,4,5,9]
-- ---------------------------------------------------------------------

-- manvermor jespergue josllagam rubvilval isrbelnun lucgamgal juanarcon
-- marvilmor
ordenaRapida :: Ord a => [a] -> [a]
ordenaRapida [] = [] 
ordenaRapida (x:xs) =
    ordenaRapida minimos ++ [x] ++ ordenaRapida maximos
    where minimos = [ y | y <- xs, y <= x]
          maximos = [ y | y <- xs, y > x ]

-- fracruzam
-- Mejor añadir x con el operador (:) a la segunda lista
ordenaRapidaB :: Ord a => [a] -> [a]
ordenaRapidaB (x:xs) =
    ordenaRapidaB [n | n <- xs, n <= x] ++ x: ordenaRapidaB [n | n <- xs, n > x]
ordenaRapidaB _      = []

-- manpende alvalvdom1 erisancha silgongal juamorrom1 ivaruicam
ordenaRapidaC :: Ord a => [a] -> [a]
ordenaRapidaC [] = []
ordenaRapidaC (x:xs) = ordenaRapidaC menores ++ x:ordenaRapidaC mayores
    where menores = filter (<x) xs
          mayores = filter (>=x) xs

-- abrdelrod
ordenaRapidaD :: Ord a => [a] -> [a]
ordenaRapidaD (x:xs) = ordenaRapidaD a ++ x:ordenaRapidaD b
    where (a,b) = partition (<= x) xs

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Calcular los tiempos necesarios para calcular 
--     let n = k in length (ordenaRapida [n,n-1..1])
-- para k en [1000, 2000, 3000, 4000]
-- 
-- ¿Cuál es el orden de complejidad de ordenaRapida?
-- ---------------------------------------------------------------------

-- manvermor jespergue josllagam alvalvdom1 erisancha isrbelnun
-- silgongal lucgamgal abrdelrod juamorrom1 juanarcon ivaruicam
-- marvilmor
ordenR :: Int -> Int
ordenR n = length (ordenaRapida [n,n-1..1])

-- El resumen de los tiempos es
--    k    | segs.
--    -----+------
--    1000 | 0,29
--    2000 | 0,93
--    3000 | 2,15
--    4000 | 4,07

-- El orden es O(log n)

-- ---------------------------------------------------------------------
-- Ejercicio 2.3. Definir, usando un acumulador, la función
--    ordenaRapida2 :: Ord a => [a] -> [a]
-- tal que (ordenaRapida2 xs) es la lista obtenida ordenando xs
-- por el procedimiento de ordenación rápida. Por ejemplo, 
--    ordenaRapida2 [3,1,4,1,5,9,2]  ==  [1,1,2,3,4,5,9]
-- ---------------------------------------------------------------------

-- manvermor jespergue josllagam rubvilval isrbelnun silgongal abrdelrod
-- juamorrom1 juanarcon marvilmor
ordenaRapida2 :: Ord a => [a] -> [a]
ordenaRapida2 xs = aux xs []
    where aux [] rs = rs
          aux (x:xs) rs = aux minimos (x: aux maximos rs)
                 where minimos = [ y | y <- xs, y <= x]
                       maximos = [ y | y <- xs, y > x ]

-- fracruzam manpende alvalvdom1 erisancha ivaruicam
ordenaRapida2b :: Ord a => [a] -> [a]
ordenaRapida2b [] = []
ordenaRapida2b (x:xs) = divide x xs [] []
  where divide :: Ord a => a -> [a] -> [a] -> [a] -> [a]
        divide n (x:xs) ys zs | x <= n = divide n xs (x:ys) zs
                              | otherwise = divide n xs ys (x:zs)
        divide n  _     ys zs = ordenaRapida2b ys ++
                                n : ordenaRapida2b zs

-- ---------------------------------------------------------------------
-- Ejercicio 2.4. Calcular los tiempos necesarios para calcular 
--     let n = k in length (ordenaRapida2 [n,n-1..1])
-- para k en [1000, 2000, 3000, 4000]
-- ---------------------------------------------------------------------

-- manvermor jesprgue josllagam alvalvdom1 erisancha isrbelnun silgongal
-- abrdelrod juamorrom1 juanarcon ivaruicam marvilmor
ordenR2 :: Int -> Int
ordenR2 n =  length (ordenaRapida2 [n,n-1..1])

-- El resumen de los tiempos es
--    k    | segs.
--    -----+------
--    1000 | 0,18
--    2000 | 0,83
--    3000 | 2,02
--    4000 | 3,91

-- ---------------------------------------------------------------------
-- § Ordenación por inserción                                         --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Para ordenar una lista xs mediante el algoritmo de
-- ordenación por inserción se selecciona el primer elemento x de xs, se
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
-- Definir la función 
--    ordenaPorInsercion :: Ord a => [a] -> [a]
-- tal que (ordenaPorInsercion xs) es la lista obtenida ordenando por
-- selección la lista xs. Por ejemplo,
--    ordenaPorInsercion [3,1,4,1,5,9,2]  ==  [1,1,2,3,4,5,9]
-- ---------------------------------------------------------------------

-- manvermor josllagam jespergue rubvilval erisancha isrbelnun juamorrom1
-- silgongal lucgamgal abrdelrod juanarcon marvilmor
ordenaPorInsercion :: Ord a => [a] -> [a]
ordenaPorInsercion [] = []
ordenaPorInsercion (x:xs) = insertar x (ordenaPorInsercion xs)

insertar :: Ord a => a -> [a] -> [a]
insertar n xs = takeWhile (<=n) xs ++ [n] ++ dropWhile (<=n) xs

-- fracruzam
ordenaPorInsercionb :: Ord a => [a] -> [a]
ordenaPorInsercionb [] = []
ordenaPorInsercionb (x:xs) = inserta x (ordenaPorInsercionb xs)
  where inserta :: Ord a => a -> [a] -> [a]
        inserta x xs = ys ++ x:zs
          where (ys,zs) = divide x xs []
        divide :: Ord a => a -> [a] -> [a] -> ([a],[a])
        divide n [] ys     = (reverse ys,[])
        divide n (x:xs) ys | x < n     = divide n xs (x:ys)
                           | otherwise = (reverse ys,x:xs)

-- manpende alvalvdom1 ivaruicam
ordenaPorInsercionC :: Ord a => [a] -> [a]
ordenaPorInsercionC [] = []
ordenaPorInsercionC (x:xs) = coloca x $ ordenaPorInsercion xs

coloca :: Ord a => a -> [a] -> [a]
coloca z [] = [z]
coloca z (y:ys) | z <= y = z : y : ys
                | otherwise = y : coloca z ys
-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Calcular los tiempos necesarios para calcular 
--     let n = k in length (ordenaPorInsercion [n,n-1..1])
-- para k en [1000, 2000, 3000, 4000]
--
-- ¿Cuál es la complejidad de ordenaPorInsercion?
-- ---------------------------------------------------------------------

-- manvermor josllagam jespergue alvalvdom1 erisancha isrbelnun
-- silgongal lucgamgal abrdelrod juamorrom1 juanarcon ivaruicam
-- marvilmor
ordenI :: Int -> Int
ordenI n = length (ordenaPorInsercion [n,n-1..1])

-- El resumen de los tiempos es
--    k    | segs.
--    -----+-----
--    1000 | 0,17
--    2000 | 0,71
--    3000 | 1,88
--    4000 | 3,79

-- El orden es O(n^2)

-- ---------------------------------------------------------------------
-- Ejercicio 3.3. Definir, por plegados, la función
--    ordenaPorInsercion2 :: Ord a => [a] -> [a]
-- tal que (ordenaPorInsercion2 xs) es la lista obtenida ordenando xs
-- por el procedimiento de ordenación por inserción. Por ejemplo, 
--    ordenaPorInsercion2 [3,1,4,1,5,9,2]  ==  [1,1,2,3,4,5,9]
-- ---------------------------------------------------------------------

-- manvermor josllagam jespergue rubvilval alvalvdom1 erisancha isrbelnun
-- juamorrom1 juanarcon silgongal abrdelrod marvilmor
ordenaPorInsercion2 :: Ord a => [a] -> [a]
ordenaPorInsercion2 = foldr insertar []

-- fracruzam
ordenaPorInsercion2b :: Ord a => [a] -> [a]
ordenaPorInsercion2b = foldr inserta []
  where inserta :: Ord a => a -> [a] -> [a]
        inserta x xs = ys ++ x:zs
          where (ys,zs) = divide x xs []
        divide :: Ord a => a -> [a] -> [a] -> ([a],[a])
        divide n [] ys     = (reverse ys,[])
        divide n (x:xs) ys | x < n     = divide n xs (x:ys)
                           | otherwise = (reverse ys,x:xs)

-- manpende ivaruicam
ordenaPorInsercion2C :: Ord a => [a] -> [a]
ordenaPorInsercion2C = foldr coloca [] 
                          
-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Calcular los tiempos necesarios para calcular 
--     let n = k in length (ordenaPorInsercion2 [n,n-1..1])
-- para k en [1000, 2000, 3000, 4000]
-- ---------------------------------------------------------------------

-- manvermor josllagam jespergue alvalvdom1 isrbelnun silgongal abrdelrod
-- juamorrom1 juanarcon ivaruicam marvilmor
ordenI2 :: Int -> Int
ordenI2 n = length (ordenaPorInsercion2 [n,n-1..1])

-- El resumen de los tiempos es
--    k    | segs.
--    -----+------
--    1000 | 0,21
--    2000 | 0,72
--    3000 | 1,99
--    4000 | 3,85

-- ---------------------------------------------------------------------
-- § Ordenación por mezcla ("Mergesort")                              --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. Para ordenar una lista xs mediante el algoritmo de
-- ordenación por mezcla se divide xs por la mitad, se ordena cada una
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
-- Definir la función 
--    ordenaPorMezcla :: Ord a => [a] -> [a]
-- tal que (ordenaPorMezcla xs) es la lista obtenida ordenando por
-- selección la lista xs. Por ejemplo,
--    ordenaPorMezcla [3,1,4,1,5,9,2]  ==  [1,1,2,3,4,5,9]
-- ---------------------------------------------------------------------

-- manvermor josllagam jespergue rubvilval manpende erisancha isrbelnun
-- silgongal abrdelrod juamorrom1 juanarcon marvilmor
ordenaPorMezcla :: Ord a => [a] -> [a]
ordenaPorMezcla [] = []
ordenaPorMezcla [x] = [x]
ordenaPorMezcla xs = 
    mezcla (ordenaPorMezcla (take n xs)) (ordenaPorMezcla (drop n xs))
    where n = div (length xs) 2

mezcla :: Ord a => [a] -> [a] -> [a]
mezcla [] ys = ys
mezcla xs [] = xs
mezcla (x:xs) (y:ys) | x <= y = x: mezcla xs (y:ys)
                     | otherwise = y: mezcla (x:xs) ys

-- fracruzam alvalvdom1 ivaruicam
ordenaPorMezclab :: Ord a => [a] -> [a]
ordenaPorMezclab [] = []
ordenaPorMezclab [x] = [x]
ordenaPorMezclab xs = mezcla (ordenaPorMezclab zs) (ordenaPorMezclab ys)
    where (zs,ys) = splitAt (length xs `div` 2) xs
          mezcla :: Ord a => [a] -> [a] -> [a]
          mezcla    []     []  = []
          mezcla    xs     []  = xs
          mezcla    []     ys  = ys
          mezcla (x:xs) (y:ys) | x < y     = x : mezcla xs (y:ys)
                               | otherwise = y : mezcla (x:xs) ys

-- Comentario: La definición anterior se puede simplificar.

-- alvalvdom1: En la definición de mezcla, sobra el caso 
-- "mezcla [] [] = []".

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Calcular los tiempos necesarios para calcular 
--     let n = k in length (ordenaPorMezcla [n,n-1..1])
-- para k en [1000, 2000, 3000, 4000]
--
-- ¿Cuál es la complejidad de ordenaPorMezcla?
-- ---------------------------------------------------------------------

-- manvermor josllagam jespergue alvalvdom1 erisancha isrbelnun
-- silgongal abrdelrod juamorrom1 juanarcon ivaruicam marvilmor
ordenM :: Int -> Int
ordenM n = length (ordenaPorMezcla [n,n-1..1]) 

-- El resumen de los tiempos es
--    k    | segs.
--    -----+-----
--    1000 | 0.01
--    2000 | 0.02
--    3000 | 0.02
--    4000 | 0.03

-- El orden es O(log n)

-- ---------------------------------------------------------------------
-- Ejercicio 4.3. Otra forma de ordenar una lista xs mediante el
-- algoritmo de ordenación por mezcla consiste en dividir xs en listas
-- unitarias y mezclar los resultados. Por ejemplo, para
-- ordenar la lista [3,1,4,1,5,9,2] el proceso es el siguiente: 
--      om [3,1,4,1,5,9,2]
--    = mp [[3],[1],[4],[1],[5],[9],[2]]
--    = mp [[1,3],[1,4],[5,9],[2]]
--    = mp [[1,1,3,4],[2,5,9]]
--    = [1,1,2,3,4,5,9]
-- donde om es ordenaPorMezcla y mp es mezclaPares.
--
-- Definir la función 
--    ordenaPorMezcla2 :: Ord a => [a] -> [a]
-- tal que (ordenaPorMezcla2 xs) es la lista obtenida ordenando por
-- mezcla la lista xs. Por ejemplo,
--    ordenaPorMezcla2 [3,1,4,1,5,9,2]  ==  [1,1,2,3,4,5,9]
-- ---------------------------------------------------------------------

-- manvermor josllagam jespergue rubvilval alvalvdom1 isrbelnun
-- silgongal juamorrom1 juanarcon marvilmor
ordenaPorMezcla2 :: Ord a => [a] -> [a]
ordenaPorMezcla2 xs = aux (unitarios xs)
          where aux [x] = x
                aux zs = aux $ mezclaPares zs

unitarios :: [t] -> [[t]]
unitarios xs = [[x] | x <- xs]

mezclaPares :: Ord a => [[a]] -> [[a]]
mezclaPares [] = []
mezclaPares [x] = [x]
mezclaPares (xs:ys:xss) = mezcla xs ys : mezclaPares xss

-- fracruzam
ordenaPorMezcla2b :: Ord a => [a] -> [a]
ordenaPorMezcla2b xs = foldr1 mezcla [[x] | x <- xs]
  where mezcla :: Ord a => [a] -> [a] -> [a]
        mezcla    []     []  = []
        mezcla    xs     []  = xs
        mezcla    []     ys  = ys
        mezcla (x:xs) (y:ys) | x < y     = x : mezcla xs (y:ys)
                             | otherwise = y : mezcla (x:xs) ys

-- Comentario: La definición anterior se puede simplificar.
                                           
-- manpende erisancha abrdelrod
ordenaPorMezcla2C :: Ord a => [a] -> [a]
ordenaPorMezcla2C = concat . mezclaPares2 . group 

mezclaPares2 :: Ord a => [[a]] -> [[a]]
mezclaPares2 []       = []
mezclaPares2 [x]      = [x]
mezclaPares2 (x:y:xs) = mezclaPares2 $ mezcla x y : mezclaPares2 xs

-- ---------------------------------------------------------------------
-- Ejercicio 4.4. Calcular los tiempos necesarios para calcular 
--     let n = k in length (ordenaPorMezcla2 [n,n-1..1])
-- para k en [1000, 2000, 3000, 4000]
-- ---------------------------------------------------------------------

-- manvermor josllagam jespergue alvalvdom1 erisancha isrbelnun
-- silgongal abrdelrod juamorrom1 juanarcon marvilmor
ordenM2 :: Int -> Int
ordenM2 n = length (ordenaPorMezcla2 [n,n-1..1])

-- El resumen de los tiempos es
--    k    | segs.
--    -----+-----
--    1000 | 0,01
--    2000 | 0,01
--    3000 | 0,02
--    4000 | 0,02

-- ---------------------------------------------------------------------
-- § Ordenación por montículos ("heapsort")                           --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 5.1. El procedimiento de ordenación de una lista por
-- montículos consiste en almacenar todos los elementos del vector a
-- ordenar en un montículo (heap), y luego extraer el nodo que queda
-- como nodo raíz del montículo (cima) en sucesivas iteraciones
-- obteniendo el conjunto ordenado.  
-- 
-- Usando la implementación de las colas de prioridad mediante
-- montículos (que se encuentra en la librería I1M.ColaDePrioridad),
-- definir la función 
--    ordenaPorMonticulos :: Ord a => [a] -> [a]
-- tal que (ordenaPorMonticulos xs) es la lista obtenida ordenando xs
-- por el procedimiento de ordenación por montículos. Por ejemplo, 
--    ordenaPorMonticulos [3,1,4,1,5,9,2]  ==  [1,1,2,3,4,5,9]
-- ---------------------------------------------------------------------

-- manvermor fracruzam josllagam jespergue rubvilval manpende isrbelnun
-- alvalvdom1 silgongal lucgamgal abrdelrod juamorrom1 juanarcon 
-- marvilmor
ordenaPorMonticulos :: Ord a => [a] -> [a]
ordenaPorMonticulos = cP2Lista . lista2CP

lista2CP :: (Ord a, Foldable t) => t a -> CP.CPrioridad a
lista2CP xs = foldr CP.inserta CP.vacia xs

-- alvalvdom1: Se podrían eliminar las listas "xs" de la definición de
-- lista2CP 
              
cP2Lista :: Ord t => CP.CPrioridad t -> [t]
cP2Lista p | CP.esVacia p = []
           | otherwise    = pm : cP2Lista rp
      where pm = CP.primero p
            rp = CP.resto p

-- ---------------------------------------------------------------------
-- Ejercicio 5.2. Calcular los tiempos necesarios para calcular 
--     let n = k in length (ordenaPorMonticulos [n,n-1..1])
-- para k en [1000, 2000, 3000, 4000]
--
-- ¿Cuál es la complejidad de ordenaPorMonticulos?
-- ---------------------------------------------------------------------

-- manvermor josllagam jespergue alvalvdom1 erisancha isrbelnun
-- silgongal lucgamgal abrdelrod juamorrom1 juanarcon marvilmor
ordenMont :: Int -> Int
ordenMont n = length (ordenaPorMonticulos [n,n-1..1])

-- El resumen de los tiempos es
--    k    | segs.
--    -----+-----
--    1000 | 0,04
--    2000 | 0,08
--    3000 | 0,13
--    4000 | 0,17

-- El orden es O(log n)
