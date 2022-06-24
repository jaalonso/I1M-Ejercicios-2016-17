-- I1M 2016-17: Relación 31 (25 de abril de 2017) 
-- Problemas básicos con el TAD de los grafos.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- El objetivo de esta relación de ejercicios es definir funciones sobre 
-- el TAD de los grafos estudiado en el tema 22 
--    http://www.cs.us.es/~jalonso/cursos/i1m-16/temas/tema-22.html
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
-- implementación del TAD de grafos 
-- + GrafoConVectorDeAdyacencia que está en http://bit.ly/1SQnG4S
-- + GrafoConMatrizDeAdyacencia que está en http://bit.ly/1SQnGlB
-- 
-- Los módulos anteriores se encuentras en la página de códigos 
-- http://bit.ly/1SQnAKO
 
-- ---------------------------------------------------------------------
-- Importación de librerías                                           --
-- ---------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

import Data.Array
import Data.List
import Test.QuickCheck

-- Hay que elegir una librería 
import I1M.Grafo
-- import GrafoConVectorDeAdyacencia 
-- import GrafoConMatrizDeAdyacencia 

-- ---------------------------------------------------------------------
-- Ejemplos                                                           --
-- ---------------------------------------------------------------------

-- Para los ejemplos se usarán los siguientes grafos.
g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11 :: Grafo Int Int
g1 = creaGrafo ND (1,5) [(1,2,12),(1,3,34),(1,5,78),
                         (2,4,55),(2,5,32),
                         (3,4,61),(3,5,44),
                         (4,5,93)]
g2 = creaGrafo D (1,5) [(1,2,12),(1,3,34),(1,5,78),
                        (2,4,55),(2,5,32),
                        (4,3,61),(4,5,93)]
g3 = creaGrafo D (1,3) [(1,2,0),(2,2,0),(3,1,0),(3,2,0)]
g4 = creaGrafo D (1,4) [(1,2,3),(2,1,5)]
g5 = creaGrafo D (1,1) [(1,1,0)]
g6 = creaGrafo D (1,4) [(1,3,0),(3,1,0),(3,3,0),(4,2,0)]
g7 = creaGrafo ND (1,4) [(1,3,0)]
g8 = creaGrafo D (1,5) [(1,1,0),(1,2,0),(1,3,0),(2,4,0),(3,1,0),
                        (4,1,0),(4,2,0),(4,4,0),(4,5,0)]
g9 = creaGrafo D (1,5) [(4,1,1),(4,3,2),(5,1,0)]
g10 = creaGrafo ND (1,3) [(1,2,1),(1,3,1),(2,3,1),(3,3,1)]
g11 = creaGrafo D (1,3) [(1,2,1),(1,3,1),(2,3,1),(3,3,1)]

-- ---------------------------------------------------------------------
-- Ejercicio 1. El grafo completo de orden n, K(n), es un grafo no
-- dirigido cuyos conjunto de vértices es {1,..n} y tiene una arista
-- entre cada par de vértices distintos. Definir la función,
--    completo :: Int -> Grafo Int Int
-- tal que (completo n) es el grafo completo de orden n. Por ejemplo,
--    ghci> completo 4
--    G ND (array (1,4) [(1,[(2,0),(3,0),(4,0)]),
--                       (2,[(1,0),(3,0),(4,0)]),
--                       (3,[(1,0),(2,0),(4,0)]),
--                       (4,[(1,0),(2,0),(3,0)])])
-- ---------------------------------------------------------------------

-- enrnarbej josdeher pabrabmon glovizcas cescarde antmorper3 eledejim2
-- albcercid roscargar natmarmar2 marmerzaf paumacpar
completo :: Int -> Grafo Int Int
completo n =
  creaGrafo ND (1,n) [(x,y,0) | x <- [1..n], y <- [x+1..n]]

-- ---------------------------------------------------------------------
-- Ejercicio 2. El ciclo de orden n, C(n), es un grafo no dirigido
-- cuyo conjunto de vértices es {1,...,n} y las aristas son
--    (1,2), (2,3), ..., (n-1,n), (n,1)
-- Definir la función
--    grafoCiclo :: Int -> Grafo Int Int
-- tal que (grafoCiclo n) es el grafo ciclo de orden n. Por ejemplo,
--    ghci> grafoCiclo 3
--    G ND (array (1,3) [(1,[(3,0),(2,0)]),(2,[(1,0),(3,0)]),(3,[(2,0),(1,0)])])
-- ---------------------------------------------------------------------

-- enrnarbej josdeher pabrabmon glovizcas antmorper3 eledejim2 albcercid
-- roscargar natmarmar2 marmerzaf
grafoCiclo :: Int -> Grafo Int Int
grafoCiclo n =
  creaGrafo ND (1,n) ((n,1,0) : zip3 [1..n] [2..n] [0,0..])

-- cescarde paumacpar
grafoCiclo2 :: Int -> Grafo Int Int
grafoCiclo2 n =
  creaGrafo ND (1,n) ([(x,x+1,0) | x <- [1..n-1]] ++ [(n,1,0)])

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--    nVertices :: (Ix v,Num p) => Grafo v p ->  Int
-- tal que (nVertices g) es el número de vértices del grafo g. Por
-- ejemplo, 
--    nVertices (completo 4)  ==  4
--    nVertices (completo 5)  ==  5
-- ---------------------------------------------------------------------

-- enrnarbej josdeher pabrabmon glovizcas cescarde antmorper3 eledejim2
-- albcercid roscargar natmarmar2 marmerzaf paumacpar
nVertices :: (Ix v,Num p) => Grafo v p ->  Int
nVertices = length . nodos

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función
--    noDirigido :: (Ix v,Num p) => Grafo v p ->  Bool
-- tal que (noDirigido g) se verifica si el grafo g es no dirigido. Por
-- ejemplo, 
--    noDirigido g1            ==  True
--    noDirigido g2            ==  False
--    noDirigido (completo 4)  ==  True
-- ---------------------------------------------------------------------

-- enrnarbej josdeher pabrabmon glovizcas cescarde antmorper3 eledejim2
-- albcercid roscargar natmarmar2 marmerzaf paumacpar 
noDirigido :: (Ix v,Num p) => Grafo v p ->  Bool
noDirigido = not . dirigido

-- ---------------------------------------------------------------------
-- Ejercicio 5. En un un grafo g, los incidentes de un vértice v es el
-- conjuntos de vértices x de g para los que hay un arco (o una arista)
-- de x a v; es decir, que v es adyacente a x. Definir la función
--    incidentes :: (Ix v,Num p) => (Grafo v p) -> v -> [v]
-- tal que (incidentes g v) es la lista de los vértices incidentes en el
-- vértice v. Por ejemplo,
--    incidentes g2 5  ==  [1,2,4]
--    adyacentes g2 5  ==  []
--    incidentes g1 5  ==  [1,2,3,4]
--    adyacentes g1 5  ==  [1,2,3,4]
-- --------------------------------------------------------------------- 

-- enrnarbej glovizcas antmorper3 eledejim2 roscargar natmarmar2
-- marmerzaf paumacpar 
incidentes :: (Ix v,Num p) => Grafo v p -> v -> [v]
incidentes g v = [ x | x <- nodos g, aristaEn g (x,v)]

-- josdeher
incidentes2 :: (Ix v,Num p) => Grafo v p -> v -> [v]
incidentes2 g v =
  concatMap (\(x,y,z) -> if y == v then [x] else []) (aristas g)

-- pabrabmon
incidentes3 :: (Ix v,Num p) => Grafo v p -> v -> [v]
incidentes3 g v = aux p g v
  where p = nodos g
        aux [] _ _ = []
        aux (x:xs) a b | b `elem` (adyacentes a x) = x : aux xs a b
                       | otherwise = aux xs a b

-- cescarde
incidentes4 :: (Ix v,Num p) => Grafo v p -> v -> [v]
incidentes4 g v = [x | x <- nodos g, v `elem` adyacentes g x]

-- albcercid
incidentes5 :: (Ix v,Num p) => Grafo v p -> v -> [v]
incidentes5 g v = filter p $ nodos g
    where p x = aristaEn g (x,v)

-- ---------------------------------------------------------------------
-- Ejercicio 6. En un un grafo g, los contiguos de un vértice v es el
-- conjuntos de vértices x de g tales que x es adyacente o incidente con
-- v. Definir la función
--    contiguos :: (Ix v,Num p) => Grafo v p -> v -> [v]
-- tal que (contiguos g v) es el conjunto de los vértices de g contiguos
-- con el vértice v. Por ejemplo,
--    contiguos g2 5  ==  [1,2,4]
--    contiguos g1 5  ==  [1,2,3,4]
-- ---------------------------------------------------------------------

-- enrnarbej glovizcas cescarde antmorper3 eledejim2  albcercid
-- roscargar natmarmar2 marmerzaf paumacpar
contiguos :: (Ix v,Num p) => Grafo v p -> v -> [v]
contiguos g v = nub (incidentes g v ++ adyacentes g v)

-- josdeher
contiguos2 :: (Ix v,Num p) => Grafo v p -> v -> [v]
contiguos2 g v = union (incidentes g v) (adyacentes g v)

-- pabrabmon
contiguos3 :: (Ix v,Num p) => Grafo v p -> v -> [v]
contiguos3 g v | noDirigido g = adyacentes g v
               | otherwise    = nub ((adyacentes g v) ++ (incidentes g v))

-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir la función
--    lazos :: (Ix v,Num p) => Grafo v p -> [(v,v)]
-- tal que (lazos g) es el conjunto de los lazos (es decir, aristas
-- cuyos extremos son iguales) del grafo g. Por ejemplo, 
--    ghci> lazos g3
--    [(2,2)]
--    ghci> lazos g2
--    []
-- ---------------------------------------------------------------------

-- enrnarbej josdeher glovizcas antmorper3 eledejim2 roscargar
-- natmarmar2 paumacpar 
lazos :: (Ix v,Num p) => Grafo v p -> [(v,v)]
lazos g = [(x,y) | (x,y,z) <- aristas g, x == y]

-- pabrabmon
lazos2 :: (Ix v,Num p) => Grafo v p -> [(v,v)]
lazos2 g = [ (n,n) | n <- nodos g , n `elem` (adyacentes g n)]

-- cescarde marmerzaf
lazos3 :: (Ix v,Num p) => Grafo v p -> [(v,v)]
lazos3 g = [(x,x) | x <- nodos g, aristaEn g (x,x)]

-- albcercid
lazos4 :: (Ix v,Num p) => Grafo v p -> [(v,v)]
lazos4 = map (\(x,y,z) -> (x,y)).filter (\ (x,y,z) -> x == y).aristas

-- ---------------------------------------------------------------------
-- Ejercicio 8. Definir la función
--    nLazos :: (Ix v,Num p) => Grafo v p ->  Int
-- tal que (nLazos g) es el número de lazos del grafo g. Por
-- ejemplo, 
--    nLazos g3  ==  1
--    nLazos g2  ==  0
-- ---------------------------------------------------------------------

-- enrnarbej pabrabmon glovizcas cescarde antmorper3 eledejim2 albcercid
-- roscargar natmarmar2 marmerzaf paumacpar
nLazos :: (Ix v,Num p) => Grafo v p ->  Int
nLazos = length . lazos 

-- ---------------------------------------------------------------------
-- Ejercicio 9. Definir la función
--    nAristas :: (Ix v,Num p) => Grafo v p ->  Int
-- tal que (nAristas g) es el número de aristas del grafo g. Si g es no
-- dirigido, las aristas de v1 a v2 y de v2 a v1 sólo se cuentan una
-- vez. Por ejemplo, 
--    nAristas g1            ==  8
--    nAristas g2            ==  7
--    nAristas g10           ==  4
--    nAristas (completo 4)  ==  6
--    nAristas (completo 5)  ==  10
-- ---------------------------------------------------------------------

-- enrnarbej pabrabmon glovizcas antmorper3 eledejim2 albcercid
-- roscargar natmarmar2 marmerzaf paumacpar
nAristas :: (Ix v,Num p) => Grafo v p ->  Int
nAristas g | noDirigido g = ((length . aristas) g + nLazos g) `div` 2
           | otherwise    = (length . aristas) g

-- josdeher
nAristas2 :: (Ix v,Num p) => Grafo v p ->  Int
nAristas2 g = sum $ map (\(x,y,z) -> if x <= y then 1 else 0) (aristas g)

-- cescarde
nAristas3 :: (Ix v,Num p) => Grafo v p ->  Int
nAristas3 g | dirigido g = length $ aristas g
            | otherwise  = length $! [(x,y) | (x,y,_) <- aristas g, x<=y]

-- ---------------------------------------------------------------------
-- Ejercicio 10. Definir la función
--    prop_nAristasCompleto :: Int -> Bool
-- tal que (prop_nAristasCompleto n) se verifica si el número de aristas
-- del grafo completo de orden n es n*(n-1)/2 y, usando la función,
-- comprobar que la propiedad se cumple para n de 1 a 20.
-- ---------------------------------------------------------------------

-- enrnarbej josdeher pabrabmon glovizcas cescarde antmorper3 eledejim2
-- albcercid roscargar natmarmar2 marmerzaf paumacpar
prop_nAristasCompleto :: Int -> Bool
prop_nAristasCompleto n = (nAristas . completo) n == n*(n-1) `div` 2

-- La comprobación es
--    ghci> and [prop_nAristasCompleto n | n <- [1..20]]
--    True

-- ---------------------------------------------------------------------
-- Ejercicio 11. El grado positivo de un vértice v de un grafo dirigido
-- g, es el número de vértices de g adyacentes con v. Definir la función  
--    gradoPos :: (Ix v,Num p) => Grafo v p -> v -> Int
-- tal que (gradoPos g v) es el grado positivo del vértice v en el grafo
-- g. Por ejemplo,
--    gradoPos g1 5  ==  4
--    gradoPos g2 5  ==  0
--    gradoPos g2 1  ==  3
-- ---------------------------------------------------------------------

-- enrnarbej josdeher pabrabmon glovizcas cescarde antmorper3 eledejim2
-- albcercid roscargar natmarmar2 marmerzaf paumacpar
gradoPos :: (Ix v,Num p) => Grafo v p -> v -> Int
gradoPos g  = length . adyacentes g

-- ---------------------------------------------------------------------
-- Ejercicio 12. El grado negativo de un vértice v de un grafo dirigido
-- g, es el número de vértices de g incidentes con v. Definir la función  
--    gradoNeg :: (Ix v,Num p) => Grafo v p -> v -> Int
-- tal que (gradoNeg g v) es el grado negativo del vértice v en el grafo
-- g. Por ejemplo,
--    gradoNeg g1 5  ==  4
--    gradoNeg g2 5  ==  3
--    gradoNeg g2 1  ==  0
-- ---------------------------------------------------------------------

-- enrnarbej josdeher pabrabmon glovizcas cescarde antmorper3 eledejim2
-- albcercid roscargar natmarmar2 marmerzaf paumacpar 
gradoNeg :: (Ix v,Num p) => Grafo v p -> v -> Int
gradoNeg g  = length . incidentes g

-- ---------------------------------------------------------------------
-- Ejercicio 13. El grado de un vértice v de un grafo dirigido g, es el
-- número de aristas de g que contiene a v. Si g es no dirigido, el
-- grado de un vértice v es el número de aristas incidentes en v, teniendo
-- en cuenta que los lazos se cuentan dos veces. Definir la función    
--    grado :: (Ix v,Num p) => Grafo v p -> v -> Int
-- tal que (grado g v) es el grado del vértice v en el grafo g. Por
-- ejemplo, 
--    grado g1  5  ==  4
--    grado g2  5  ==  3
--    grado g2  1  ==  3 
--    grado g3  2  ==  4
--    grado g3  1  ==  2
--    grado g3  3  ==  2
--    grado g5  1  ==  2
--    grado g10 3  ==  4
--    grado g11 3  ==  4
-- ---------------------------------------------------------------------

-- enrnarbej josdeher pabrabmon antmorper3 albcercid roscargar paumacpar
grado :: (Ix v,Num p) => Grafo v p -> v -> Int
grado g v
  | dirigido g = gradoNeg g v + gradoPos g v 
  | otherwise  = gradoNeg g v + (length . filter (\(x,y) -> x == v) . lazos) g

-- glovizcas eledejim2 natmarmar2 marmerzaf
grado2 :: (Ix v,Num p) => Grafo v p -> v -> Int
grado2 g v | dirigido g = gradoNeg g v + gradoPos g v 
           | otherwise  = gradoNeg g v + nLazos g

-- cescarde
grado3 :: (Ix v,Num p) => Grafo v p -> v -> Int
grado3 g v | dirigido g = gradoNeg g v + gradoPos g v
           | (v,v) `elem` lazos g = length (incidentes g v) + 1
           | otherwise = length $ incidentes g v

-- ---------------------------------------------------------------------
-- Ejercicio 14. Comprobar con QuickCheck que para cualquier grafo g, la
-- suma de los grados positivos de los vértices de g es igual que la
-- suma de los grados negativos de los vértices de g.
-- ---------------------------------------------------------------------

-- enrnarbej josdeher pabrabmon cescarde glovizcas antmorper3 eledejim2
-- albcercid roscargar paumacpar

-- La propiedad es
prop_sumaGrados :: Grafo Int Int -> Bool
prop_sumaGrados g =
  (sum . map (gradoPos g)) ng == (sum . map (gradoPos g)) ng
  where ng = nodos g

-- La comprobación es
--    ghci> quickCheck prop_sumaGrados
--    +++ OK, passed 100 tests.

-- natmarmar2 marmerzaf
prop_sumaGrados2 :: Grafo Int Int -> Bool
prop_sumaGrados2 g =
  (sum . map (gradoPos g)) ng == (sum . map (gradoNeg g)) ng
  where ng = nodos g

-- La comprobación es
--    ghci> quickCheck prop_sumaGrados
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 15. En la teoría de grafos, se conoce como "Lema del
-- apretón de manos" la siguiente propiedad: la suma de los grados de
-- los vértices de g es el doble del número de aristas de g. 
-- Comprobar con QuickCheck que para cualquier grafo g, se verifica
-- dicha propiedad.
-- ---------------------------------------------------------------------

-- enrnarbej josdeher pabrabmon cescarde glovizcas antmorper3 eledejim2
-- albcercid roscargar marmerzaf paumacpar
prop_apretonManos :: Grafo Int Int -> Bool
prop_apretonManos g = (sum . map (grado g) . nodos) g == 2 * nAristas g

-- La comprobación es
--    ghci> quickCheck prop_apretonManos
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 16. Comprobar con QuickCheck que en todo grafo, el número
-- de nodos de grado impar es par. 
-- ---------------------------------------------------------------------

-- enrnarbej josdeher pabrabmon cescarde glovizcas antmorper3 eledejim2
-- albcercid roscargar marmerzaf paumacpar
prop_numNodosGradoImpar :: Grafo Int Int -> Bool
prop_numNodosGradoImpar g =
  (even . length . filter (odd . grado g) . nodos) g

-- La comprobación es
--    ghci> quickCheck prop_numNodosGradoImpar
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 17. Definir la propiedad
--   prop_GradoCompleto :: Int -> Bool
-- tal que (prop_GradoCompleto n) se verifica si todos los vértices del
-- grafo completo K(n) tienen grado n-1. Usarla para comprobar que dicha
-- propiedad se verifica para los grafos completos de grados 1 hasta 30.
-- ---------------------------------------------------------------------

-- enrnarbej josdeher pabrabmon cescarde glovizcas antmorper3 eledejim2
-- albcercid roscargar marmerzaf paumacpar
prop_GradoCompleto :: Int -> Bool
prop_GradoCompleto n =
  (all ((n-1) ==) . map (grado cn) . nodos) cn
  where cn = completo n

-- La comprobación es                       
--    ghci> and [prop_GradoCompleto n | n <- [1..30]]
--    True

-- ---------------------------------------------------------------------
-- Ejercicio 18. Un grafo es regular si todos sus vértices tienen el
-- mismo grado. Definir la función
--    regular :: (Ix v,Num p) => Grafo v p -> Bool
-- tal que (regular g) se verifica si todos los nodos de g tienen el
-- mismo grado. 
--    regular g1            ==  False
--    regular g2            ==  False
--    regular (completo 4)  ==  True
-- ---------------------------------------------------------------------

-- enrnarbej josdeher pabrabmon cescarde antmorper3 roscargar paumacpar
regular :: (Ix v,Num p) => Grafo v p -> Bool
regular g = all (x ==) c
  where (x:c) = (map (grado g) . nodos) g

-- glovizcas eledejim2 marmerzaf
regular2 :: (Ix v,Num p) => Grafo v p -> Bool
regular2 g = length (nub (map (grado g) xs)) == 1
  where xs = nodos g

-- albcercid
regular3 :: (Ix v,Num p) => Grafo v p -> Bool
regular3 g = minimum xs == maximum xs
  where xs = map (grado g) (nodos g)

-- ---------------------------------------------------------------------
-- Ejercicio 19. Definir la propiedad
--    prop_CompletoRegular :: Int -> Int -> Bool
-- tal que (prop_CompletoRegular m n) se verifica si todos los grafos
-- completos desde el de orden m hasta el de orden m son regulares y
-- usarla para comprobar que todos los grafos completo desde el de orden
-- 1 hasta el de orden 30 son regulares.
-- --------------------------------------------------------------------- 

-- enrnarbej josdeher pabrabmon cescarde glovizcas antmorper3 eledejim2
-- albcercid roscargar marmerzaf paumacpar
prop_CompletoRegular :: Int -> Int -> Bool
prop_CompletoRegular m n = all regular (map completo [m..n])

-- La comprobación es
--    ghci> prop_CompletoRegular 1 30
--    True

-- ---------------------------------------------------------------------
-- Ejercicio 20. Un grafo es k-regular si todos sus vértices son de
-- grado k. Definir la función
--    regularidad :: (Ix v,Num p) => Grafo v p -> Maybe Int
-- tal que (regularidad g) es la regularidad de g. Por ejemplo,
--    regularidad g1              ==  Nothing
--    regularidad (completo 4)    ==  Just 3
--    regularidad (completo 5)    ==  Just 4
--    regularidad (grafoCiclo 4)  ==  Just 2
--    regularidad (grafoCiclo 5)  ==  Just 2
-- ---------------------------------------------------------------------

-- enrnarbej josdeher pabrabmon cescarde glovizcas antmorper3 eledejim2
-- albcercid roscargar marmerzaf paumacpar
regularidad :: (Ix v,Num p) => Grafo v p -> Maybe Int
regularidad g | regular g = Just ((grado g . head . nodos) g)
              | otherwise = Nothing

-- ---------------------------------------------------------------------
-- Ejercicio 21. Definir la propiedad
--    prop_completoRegular :: Int -> Bool
-- tal que (prop_completoRegular n) se verifica si el grafo completo de
-- orden n es (n-1)-regular. Por ejemplo,
--    prop_completoRegular 5  ==  True
-- y usarla para comprobar que la cumplen todos los grafos completos
-- desde orden 1 hasta 20.
-- ---------------------------------------------------------------------

-- enrnarbej josdeher pabrabmon cescarde glovizcas antmorper3 eledejim2
-- albcercid roscargar marmerzaf paumacpar
prop_completoRegular :: Int -> Bool
prop_completoRegular n = n-1 == r
  where Just r = regularidad (completo n)

-- La comprobación es                       
--    ghci> and [prop_completoRegular n | n <- [1..20]]
--    True

-- ---------------------------------------------------------------------
-- Ejercicio 22. Definir la propiedad
--    prop_cicloRegular :: Int -> Bool
-- tal que (prop_cicloRegular n) se verifica si el grafo ciclo de orden
-- n es 2-regular. Por ejemplo, 
--    prop_cicloRegular 2  ==  True
-- y usarla para comprobar que la cumplen todos los grafos ciclos
-- desde orden 3 hasta 20.
-- ---------------------------------------------------------------------

-- enrnarbej josdeher pabrabmon cescarde glovizcas antmorper3 eledejim2
-- albcercid roscargar marmerzaf paumacpar
prop_cicloRegular :: Int -> Bool
prop_cicloRegular n = 2 == r
  where Just r = regularidad (grafoCiclo n)

-- La comprobación es                       
--    ghci> and [prop_cicloRegular n | n <- [3..20]]
--    True

-- ---------------------------------------------------------------------
-- § Generador de grafos                                              --
-- ---------------------------------------------------------------------

-- (generaGND n ps) es el grafo completo de orden n tal que los pesos
-- están determinados por ps. Por ejemplo,
--    ghci> generaGND 3 [4,2,5]
--    (ND,array (1,3) [(1,[(2,4),(3,2)]),
--                     (2,[(1,4),(3,5)]),
--                      3,[(1,2),(2,5)])])
--    ghci> generaGND 3 [4,-2,5]
--    (ND,array (1,3) [(1,[(2,4)]),(2,[(1,4),(3,5)]),(3,[(2,5)])])
generaGND :: Int -> [Int] -> Grafo Int Int
generaGND n ps  = creaGrafo ND (1,n) l3
    where l1 = [(x,y) | x <- [1..n], y <- [1..n], x < y]
          l2 = zip l1 ps
          l3 = [(x,y,z) | ((x,y),z) <- l2, z > 0]

-- (generaGD n ps) es el grafo completo de orden n tal que los pesos
-- están determinados por ps. Por ejemplo,
--    ghci> generaGD 3 [4,2,5]
--    (D,array (1,3) [(1,[(1,4),(2,2),(3,5)]),
--                    (2,[]),
--                    (3,[])])
--    ghci> generaGD 3 [4,2,5,3,7,9,8,6]
--    (D,array (1,3) [(1,[(1,4),(2,2),(3,5)]),
--                    (2,[(1,3),(2,7),(3,9)]),
--                    (3,[(1,8),(2,6)])])
generaGD :: Int -> [Int] -> Grafo Int Int
generaGD n ps = creaGrafo D (1,n) l3
    where l1 = [(x,y) | x <- [1..n], y <- [1..n]]
          l2 = zip l1 ps
          l3 = [(x,y,z) | ((x,y),z) <- l2, z > 0]

-- genGD es un generador de grafos dirigidos. Por ejemplo,
--    ghci> sample genGD
--    (D,array (1,4) [(1,[(1,1)]),(2,[(3,1)]),(3,[(2,1),(4,1)]),(4,[(4,1)])])
--    (D,array (1,2) [(1,[(1,6)]),(2,[])])
--    ...
genGD :: Gen (Grafo Int Int)
genGD = do n <- choose (1,10)
           xs <- vectorOf (n*n) arbitrary
           return (generaGD n xs)

-- genGND es un generador de grafos dirigidos. Por ejemplo,
--    ghci> sample genGND
--    (ND,array (1,1) [(1,[])])
--    (ND,array (1,3) [(1,[(2,3),(3,13)]),(2,[(1,3)]),(3,[(1,13)])])
--    ...
genGND :: Gen (Grafo Int Int)
genGND = do n <- choose (1,10)
            xs <- vectorOf (n*n) arbitrary
            return (generaGND n xs)

-- genG es un generador de grafos. Por ejemplo,
--    ghci> sample genG
--    (D,array (1,3) [(1,[(2,1)]),(2,[(1,1),(2,1)]),(3,[(3,1)])])
--    (ND,array (1,3) [(1,[(2,2)]),(2,[(1,2)]),(3,[])])
--    ...
genG :: Gen (Grafo Int Int)
genG = do d <- choose (True,False)
          n <- choose (1,10)
          xs <- vectorOf (n*n) arbitrary
          if d then return (generaGD n xs)
               else return (generaGND n xs)

-- Los grafos está contenido en la clase de los objetos generables
-- aleatoriamente. 
instance Arbitrary (Grafo Int Int) where
    arbitrary = genG

