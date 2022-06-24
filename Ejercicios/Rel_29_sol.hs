-- I1M 2016-17: Relación 29 (25 de abril de 2017)
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

-- albcercid enrnarbej joscasgom1 josdeher glovizcas carmarcar5 pabrabmon
-- paumacpar fatfervaz cescarde antmorper3 eledejim2 alvfercen roscargar
-- margirmon natmarmar2 marmerzaf josrodgal7 beagongon1
ordenaPorSeleccion :: Ord a => [a] -> [a]
ordenaPorSeleccion [] = []
ordenaPorSeleccion xs = t : ordenaPorSeleccion (delete t xs)
  where t = minimum xs

-- fraferpoy
ordenaPorSeleccionA :: Ord a => [a] -> [a]
ordenaPorSeleccionA [] = []
ordenaPorSeleccionA xs =
  head (sort xs): ordenaPorSeleccionA (delete (head (sort xs)) xs)

-- Comentario: Usa sort.

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Calcular los tiempos necesarios para calcular 
--     let n = k in length (ordenaPorSeleccion [n,n-1..1])
-- para k en [1000, 2000, 3000, 4000].
-- 
-- ¿Cuál es el orden de complejidad de ordenaPorSeleccion?
-- ---------------------------------------------------------------------

-- albcercid enrnarbej joscasgom1 josdeher glovizcas pabrabmon paumacpar
-- fatfervaz antmorper3 eledejim2 roscargar margirmon josrodgal7 beagongon1

-- El resumen de los tiempos es
--    k    | segs.
--    -----+-----
--    1000 | 0.03
--    2000 | 0.08
--    3000 | 0.22
--    4000 | 0.41

-- fraferpoy

-- El resumen de los tiempos es
--    k    | segs.
--    -----+-----
--    1000 | 0.06
--    2000 | 0.09
--    3000 | 0.23 
--    4000 | 0.44

-- ---------------------------------------------------------------------
-- Ejercicio 1.3. Definir la función
--    ordenaPorSeleccion2 :: Ord a => [a] -> [a] 
-- tal que (ordenaPorSeleccion2 xs) es la lista xs ordenada por el
-- algoritmo de selección, pero usando un acumulador. Por ejemplo,
--    ordenaPorSeleccion2 [3,1,4,1,5,9,2]  ==  [1,1,2,3,4,5,9]
-- ---------------------------------------------------------------------

-- albcercid enrnarbej joscasgom1 josdeher pabrabmon paumacpar fatfervaz
-- antmorper3 eledejim2 roscargar margirmon natmarmar2 beagongon1
ordenaPorSeleccion2 :: Ord a => [a] -> [a] 
ordenaPorSeleccion2 = aux []
  where aux v [] = reverse v
        aux v xs = aux (x:v) (delete x xs)
          where x = minimum xs

-- glovizcas carmarcar5 cescarde fraferpoy alvfercen marmerzaf josrodgal7
ordenaPorSeleccion2a :: Ord a => [a] -> [a] 
ordenaPorSeleccion2a xs = aux [] xs
  where aux xs [] = xs
        aux ys xs = aux (x:ys) (delete x xs)
          where x = maximum xs                        
                
-- ---------------------------------------------------------------------
-- Ejercicio 1.4. Calcular los tiempos necesarios para calcular 
--    let n = k in length (ordenaPorSeleccion2 [n,n-1..1])
-- para k en [1000, 2000, 3000, 4000]
-- ---------------------------------------------------------------------

-- albcercid enrnarbej joscasgom1 josdeher pabrabmon paumacpar fatfervaz
-- antmorper3 eledejim2 roscargar margirmon beagongon1

-- El resumen de los tiempos es
--    k    | segs.
--    -----+-----
--    1000 | 0.01
--    2000 | 0.09
--    3000 | 0.22
--    4000 | 0.41

-- glovizcas

-- El resumen de los tiempos es
--    k    | segs.
--    -----+-----
--    1000 | 0.02
--    2000 | 0.06
--    3000 | 0.14
--    4000 | 0.30

-- Comentario (cescarde): mi ordenador es tela de lento. 
-- Realiza el mismo cálculo en el doble de tiempo

-- fraferpoy

-- El resumen de los tiempos es
--    k    | segs.
--    -----+-----
--    1000 | 0.03
--    2000 | 0.11
--    3000 | 0.27
--    4000 | 0.52

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

-- albcercid enrnarbej joscasgom1 josdeher pabrabmon paumacpar
-- cescarde antmorper3 roscargar margirmon marmerzaf josrodgal7
ordenaRapida :: Ord a => [a] -> [a]
ordenaRapida [] = []
ordenaRapida (x:xs) = ordenaRapida a ++ x:ordenaRapida b
  where (a,b) = partition (<= x) xs

-- glovizcas carmarcar5 fatfervaz fraferpoy eledejim2 alvfercen natmarmar2
ordenaRapida1 :: Ord a => [a] -> [a]
ordenaRapida1 [] = []
ordenaRapida1 (x:xs) =
  ordenaRapida (filter (<= x) xs) ++ [x] ++
  ordenaRapida (filter (>  x) xs)

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Calcular los tiempos necesarios para calcular 
--     let n = k in length (ordenaRapida [n,n-1..1])
-- para k en [1000, 2000, 3000, 4000]
-- 
-- ¿Cuál es el orden de complejidad de ordenaRapida?
-- ---------------------------------------------------------------------

-- albcercid enrnarbej joscasgom1 josdeher glovizcas pabrabmon paumacpar
-- fatfervaz antmorper3 eledejim2 roscargar margirmon

-- El resumen de los tiempos es
--    k    | segs.
--    -----+------
--    1000 | 0.16
--    2000 | 0.66
--    3000 | 1.47
--    4000 | 2.98

-- cescarde: 
--    k    | segs.
--    -----+------
--    1000 | 0.24
--    2000 | 1.27
--    3000 | 3.31
--    4000 | 6.64

-- fraferpoy

-- El resumen de los tiempos es
--    k    | segs.
--    -----+------
--    1000 | 0.22
--    2000 | 0.94
--    3000 | 2.94
--    4000 | 3.98

-- ---------------------------------------------------------------------
-- Ejercicio 2.3. Definir, usando un acumulador, la función
--    ordenaRapida2 :: Ord a => [a] -> [a]
-- tal que (ordenaRapida2 xs) es la lista obtenida ordenando xs
-- por el procedimiento de ordenación rápida. Por ejemplo, 
--    ordenaRapida2 [3,1,4,1,5,9,2]  ==  [1,1,2,3,4,5,9]
-- ---------------------------------------------------------------------

-- albcercid enrnarbej joscasgom1 josdeher pabrabmon paumacpar
-- cescarde antmorper3 roscargar margirmon marmerzaf josrodgal7
ordenaRapida2 :: Ord a => [a] -> [a]
ordenaRapida2 = aux []
  where aux v [] = v
        aux v (x:xs) = aux (v ++ ordenaRapida2 a ++ [x]) b
          where (a,b) = partition (<= x) xs

-- glovizcas carmarcar5 fatfervaz fraferpoy eledejim2 alvfercen natmarmar2
ordenaRapida2a :: Ord a => [a] -> [a]
ordenaRapida2a xs = aux [] xs
  where aux xs [] = xs
        aux ys (x:xs) = aux (ys ++ ordenaRapida2 a ++ [x]) b
          where a = filter (<= x) xs
                b = filter (> x ) xs

-- ---------------------------------------------------------------------
-- Ejercicio 2.4. Calcular los tiempos necesarios para calcular 
--     let n = k in length (ordenaRapida2 [n,n-1..1])
-- para k en [1000, 2000, 3000, 4000]
-- ---------------------------------------------------------------------

-- albcercid enrnarbej joscasgom1 josdeher glovizcas pabrabmon paumacpar
-- fatfervaz antmorper3 eledejim2 roscargar margirmon 

-- El resumen de los tiempos es
--    k    | segs.
--    -----+------
--    1000 | 0.09
--    2000 | 0.39
--    3000 | 0.84
--    4000 | 1.55

-- fraferpoy 

-- El resumen de los tiempos es
--    k    | segs.
--    -----+------
--    1000 | 0.17
--    2000 | 0.66
--    3000 | 1.48
--    4000 | 2.61

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

-- albcercid enrnarbej joscasgom1 josdeher glovizcas pabrabmon carmarcar5
-- paumacpar fatfervaz cescarde antmorper3 eledejim2 alvfercen roscargar
-- margirmon natmarmar2 marmerzaf josrodgal7
ordenaPorInsercion :: Ord a => [a] -> [a]
ordenaPorInsercion [] = []
ordenaPorInsercion (x:xs) = insert x (ordenaPorInsercion xs)

-- fraferpoy
ordenaPorInsercionA :: Ord a => [a] -> [a]
ordenaPorInsercionA [] = []
ordenaPorInsercionA (x:xs) = sort (x:ordenaPorInsercion xs)

-- Comentario: Usa sort.

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Calcular los tiempos necesarios para calcular 
--     let n = k in length (ordenaPorInsercion [n,n-1..1])
-- para k en [1000, 2000, 3000, 4000]
--
-- ¿Cuál es la complejidad de ordenaPorInsercion?
-- ---------------------------------------------------------------------

-- albcercid enrnarbej joscasgom1 josdeher glovizcas pabrabmon paumacpar
-- fatfervaz antmorper3 eledejim2 roscargar margirmon
 
-- El resumen de los tiempos es
--    k    | segs.
--    -----+-----
--    1000 | 0.02
--    2000 | 0.06
--    3000 | 0.14
--    4000 | 0.25

-- cescarde
--    k    | segs.
--    -----+-----
--    1000 | 0.03
--    2000 | 0.10
--    3000 | 0.28
--    4000 | 0.50

-- fraferpoy

-- El resumen de los tiempos es
--    k    | segs.
--    -----+-----
--    1000 | 0.03
--    2000 | 0.28
--    3000 | 0.47
--    4000 | 1.14

-- ---------------------------------------------------------------------
-- Ejercicio 3.3. Definir, por plegados, la función
--    ordenaPorInsercion2 :: Ord a => [a] -> [a]
-- tal que (ordenaPorInsercion2 xs) es la lista obtenida ordenando xs
-- por el procedimiento de ordenación por inserción. Por ejemplo, 
--    ordenaPorInsercion2 [3,1,4,1,5,9,2]  ==  [1,1,2,3,4,5,9]
-- ---------------------------------------------------------------------

-- albcercid enrnarbej joscasgom1 josdeher glovizcas pabrabmon carmarcar5
-- paumacpar fatfervaz cescarde fraferpoy antmorper3 eledejim2 alvfercen
-- roscargar margirmon natmarmar2 marmerzaf josrodgal7
ordenaPorInsercion2 :: Ord a => [a] -> [a]
ordenaPorInsercion2 = foldr insert []

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Calcular los tiempos necesarios para calcular 
--     let n = k in length (ordenaPorInsercion2 [n,n-1..1])
-- para k en [1000, 2000, 3000, 4000]
-- ---------------------------------------------------------------------

-- albcercid enrnarbej joscasgom1 josdeher glovizcas pabrabmon paumacpar
-- fatfervaz antmorper3 eledejim2 roscargar margirmon

-- El resumen de los tiempos es
--    k    | segs.
--    -----+------
--    1000 | 0.02
--    2000 | 0.06
--    3000 | 0.12
--    4000 | 0.23

-- cescarde
--    k    | segs.
--    -----+------
--    1000 | 0.03
--    2000 | 0.11
--    3000 | 0.25
--    4000 | 0.48

-- fraferpoy

-- El resumen de los tiempos es
--    k    | segs.
--    -----+------
--    1000 | 0.02
--    2000 | 0.06
--    3000 | 0.16
--    4000 | 0.28

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

-- albcercid josdeher paumacpar alvfercen roscargar josrodgal7
ordenaPorMezcla :: Ord a => [a] -> [a]
ordenaPorMezcla [] = []
ordenaPorMezcla [x] = [x]
ordenaPorMezcla xs = mezcla (ordenaPorMezcla a) (ordenaPorMezcla b)
      where (a,b) = splitAt n xs
            n = div (length xs) 2
            mezcla p@(x:xs) q@(y:ys) | x <= y = p ++ q
                                     | otherwise = q ++ p

-- Comentario: La definición anterior es incorrecta. Por ejemplo,
--   λ> ordenaPorMezcla [2,9,1,8]
--   [1,8,2,9]

-- enrnarbej joscasgom1 cescarde antmorper3
ordenaPorMezcla3 :: Ord a => [a] -> [a]
ordenaPorMezcla3 []  = []
ordenaPorMezcla3 [x] = [x]
ordenaPorMezcla3 xs  = m (ordenaPorMezcla3 x1) (ordenaPorMezcla3 x2)
  where
    (x1,x2) = splitAt ((length xs) `div` 2) xs
    m xs    = foldr insert xs

-- glovizcas pabrabmon carmarcar5 fatfervaz fraferpoy eledejim2 margirmon
-- marmerzaf
ordenaPorMezcla4 :: Ord a => [a] -> [a]
ordenaPorMezcla4 [] = []
ordenaPorMezcla4 [x]= [x]
ordenaPorMezcla4 xs = mezcla (ordenaPorMezcla (take n xs))
                             (ordenaPorMezcla (drop n xs))
  where l = length xs
        n = l `div` 2

mezcla [] [] = []
mezcla [] xs = xs
mezcla xs [] = xs 
mezcla us@(x:xs) vs@(y:ys)  | x < y     = x : mezcla xs vs
                            | otherwise = y : mezcla us ys

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Calcular los tiempos necesarios para calcular 
--     let n = k in length (ordenaPorMezcla [n,n-1..1])
-- para k en [1000, 2000, 3000, 4000]
--
-- ¿Cuál es la complejidad de ordenaPorMezcla?
-- ---------------------------------------------------------------------

-- albcercid enrnarbej joscasgom1 josdeher glovizcas pabrabmon paumacpar
-- fatfervaz fraferpoy antmorper3 eledejim2 roscargar margirmon

-- El resumen de los tiempos es
--    k    | segs.
--    -----+-----
--    1000 | 0.02
--    2000 | 0.03
--    3000 | 0.05
--    4000 | 0.06

-- cescarde
--    k    | segs.
--    -----+-----
--    1000 | 0.01
--    2000 | 0.02
--    3000 | 0.02
--    4000 | 0.02

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

-- albcercid josdeher glovizcas pabrabmon carmarcar5 paumacpar roscargar
-- josrodgal7
ordenaPorMezcla2 :: Ord a => [a] -> [a]
ordenaPorMezcla2 xs =
  concat $ (until ((1 ==).length) mezclaPares) $ map (:[]) xs
  where mezclaPares (x:y:xs) = (ordena x y):mezclaPares xs
        mezclaPares xs       = xs
        ordena [] y = y
        ordena x [] = x
        ordena (x:xs) (y:ys) | x <= y    = x:ordena xs (y:ys)
                             | otherwise = y:ordena (x:xs) ys
 
 
-- enrnarbej joscasgom1 cescarde antmorper3 margirmon marmerzaf
ordenaPorMezcla2b :: Ord a => [a] -> [a]
ordenaPorMezcla2b = concat . until ((1==).length) aux . map (\x-> [x])
  where
    m xs = foldr insert xs
    aux []       = []
    aux [x]      = [x]
    aux (x:y:ys) = m x y : aux ys

-- ---------------------------------------------------------------------
-- Ejercicio 4.4. Calcular los tiempos necesarios para calcular 
--     let n = k in length (ordenaPorMezcla2 [n,n-1..1])
-- para k en [1000, 2000, 3000, 4000]
-- ---------------------------------------------------------------------

-- albcercid enrnarbej joscasgom1 josdeher glovizcas pabrabmon paumacpar
-- cescarde antmorper3 roscargar margirmon

-- El resumen de los tiempos es
--    k    | segs.
--    -----+-----
--    1000 | 0.02
--    2000 | 0.00
--    3000 | 0.03
--    4000 | 0.03

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

-- albcercid enrnarbej joscasgom1 josdeher glovizcas pabrabmon paumacpar
-- cescarde antmorper3 roscargar margirmon natmarmar2 marmerzaf
ordenaPorMonticulos :: Ord a => [a] -> [a]
ordenaPorMonticulos = aux . foldr CP.inserta CP.vacia 
  where aux m | CP.esVacia m = []
              | otherwise    = CP.primero m : aux (CP.resto m)

-- ---------------------------------------------------------------------
-- Ejercicio 5.2. Calcular los tiempos necesarios para calcular 
--     let n = k in length (ordenaPorMonticulos [n,n-1..1])
-- para k en [1000, 2000, 3000, 4000]
--
-- ¿Cuál es la complejidad de ordenaPorMonticulos?
-- ---------------------------------------------------------------------

-- albcercid enrnarbej joscasgom1 josdeher glovizcas pabrabmon paumacpar
-- cescarde antmorper3 roscargar margirmon

-- El resumen de los tiempos es
--    k    | segs.
--    -----+-----
--    1000 | 0.00
--    2000 | 0.02
--    3000 | 0.02
--    4000 | 0.02
