-- I1M 2015-16: Relación 38 (8 de mayo de 2016)
-- Ejercicios de exámenes sobre grafos.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- En esta relación se presenta una recopilación de ejercicios sobre
-- grafos propuestos en exámenes de la asignatura.
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
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.List
import Data.Array
import Test.QuickCheck
    
-- Hay que elegir una librería 
import I1M.Grafo
-- import GrafoConVectorDeAdyacencia 
-- import GrafoConMatrizDeAdyacencia 

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    recorridos :: [a] -> [[a]]
-- tal que (recorridos xs) es la lista de todos los posibles recorridos
-- por el grafo cuyo conjunto de vértices es xs y cada vértice se
-- encuentra conectado con todos los otros y los recorridos pasan por
-- todos los vértices una vez y terminan en el vértice inicial. Por
-- ejemplo, 
--    ghci> recorridos [2,5,3]
--    [[2,5,3,2],[5,2,3,5],[3,5,2,3],[5,3,2,5],[3,2,5,3],[2,3,5,2]]
-- Indicación: No importa el orden de los recorridos en la lista.
-- ---------------------------------------------------------------------

-- juanarcon alvalvdom1 jespergue manvermor josllagam lucgamgal javoliher
-- rubvilval silgongal
recorridos :: [a] -> [[a]]
recorridos xs = [ys ++ [head ys] | ys <- permutations xs]

-- fracruzam marvilmor
recorridos2 :: [a] -> [[a]]
recorridos2 xs = [y:ys ++ [y] | y:ys <- permutations xs]

-- abrdelrod erisancha carruirui3
recorridos3 :: [a] -> [[a]]
recorridos3 = map (\xs -> xs ++ [head xs]) . permutations

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Consideremos un grafo G = (V,E), donde V es un
-- conjunto finito de nodos ordenados y E es un conjunto de arcos. En un
-- grafo, la anchura de un nodo es el máximo de los valores absolutos de
-- la diferencia entre el valor del nodo y los de sus adyacentes; y la
-- anchura del grafo es la máxima anchura de sus nodos. Por ejemplo, en
-- el grafo  
--    grafo2 :: Grafo Int Int
--    grafo2 = creaGrafo D (1,5) [(1,2,1),(1,3,1),(1,5,1),
--                                (2,4,1),(2,5,1),
--                                (3,4,1),(3,5,1),
--                                (4,5,1)]
-- su anchura es 4 y el nodo de máxima anchura es el 5.
-- 
-- Definir la función 
--    anchura :: Grafo Int Int -> Int
-- tal que (anchuraG g) es la anchura del grafo g. Por ejemplo,
--    anchura g  ==  4
-- ---------------------------------------------------------------------

grafo2 :: Grafo Int Int
grafo2 = creaGrafo D (1,5) [(1,2,1),(1,3,1),(1,5,1),
                            (2,4,1),(2,5,1),
                            (3,4,1),(3,5,1),
                            (4,5,1)]

-- fracruzam manvermor abrdelrod josllagam lucgamgal javoliher silgongal
anchura :: Grafo Int Int -> Int
anchura g = maximum [abs (x-y) | (x,y,_) <- aristas g]

-- alvalvdom1 marvilmor rubvilval
anchura3 :: Grafo Int Int -> Int
anchura3 g = maximum (map (anchuraN g) (nodos g))
    where anchuraN :: Grafo Int Int -> Int -> Int
          anchuraN g v = maximum [abs (v-x) | x <- adyacentes g v]

-- carruirui3
anchura4 :: Grafo Int Int -> Int
anchura4 g = maximum . map (anchuraV g) $ nodos g
    where anchuraV g v = maximum . map (abs . (v-)) $ adyacentes g v

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Comprobar experimentalmente que la anchura del grafo
-- grafo cíclico de orden n es n-1.
-- ---------------------------------------------------------------------

-- juanarcon fracruzam alvalvdom1 jespergue manvermor abrdelrod erisancha
-- carruirui3 josllagam marvilmor lucgamgal javoliher rubvilval silgongal
conjetura :: Int -> Bool
conjetura n = anchura (grafoCiclo n) == n-1

grafoCiclo :: Int -> Grafo Int Int
grafoCiclo n = 
    creaGrafo ND (1,n) ((n,1,0):[(x,x+1,0) | x <- [1..n-1]])

-- La comprobación es

-- fracruzam jespergue juanarcon abrdelrod erisancha carruirui3 josllagam
-- marvilmor silgongal
prop_conjetura :: Positive Int -> Bool
prop_conjetura (Positive n) = conjetura n

-- λ> quickCheck prop_conjetura
-- +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 3. Un grafo no dirigido G se dice conexo, si para cualquier
-- par de vértices u y v en G, existe al menos una trayectoria (una
-- sucesión de vértices adyacentes) de u a v.
--
-- Definirla función 
--    conexo :: (Ix a, Num p) => Grafo a p -> Bool
-- tal que (conexo g) se verifica si el grafo g es conexo. Por ejemplo, 
--    conexo (creaGrafo ND (1,3) [(1,2,0),(3,2,0)])          ==  True
--    conexo (creaGrafo ND (1,4) [(1,2,0),(3,2,0),(4,1,0)])  ==  True
--    conexo (creaGrafo ND (1,4) [(1,2,0),(3,4,0)])          ==  False
-- ---------------------------------------------------------------------

-- juanarcon jespergue abrdelrod josllagam rubvilval
conexo :: (Ix a, Num p) => Grafo a p -> Bool
conexo g = or [esConexo xs | xs <- caminos g]
    where esConexo [x]      = True
          esConexo (x:y:xs) = aristaEn g (x,y) && esConexo (y:xs)

caminos :: (Ix a, Num p) => Grafo a p -> [[a]]
caminos g = permutations (nodos g)

-- manvermor erisancha carruirui3 marvilmor
conexo2 :: (Ix a, Num p) => Grafo a p -> Bool
conexo2 g = length (recorridoEnAnchura x g) == n
    where xs = nodos g
          x = head xs
          n = length xs

recorridoEnAnchura :: (Num p, Ix a) => a -> Grafo a p -> [a]
recorridoEnAnchura i g = reverse (ra [i] [])
    where 
      ra [] vis    = vis
      ra (c:cs) vis 
          | c `elem` vis = ra cs vis
          | otherwise    = ra (cs ++ adyacentes g c) 
                              (c:vis)

-- ---------------------------------------------------------------------
--  Ejercicio 4. Un mapa se puede representar mediante un grafo donde
--  los vértices son las regiones del mapa y hay una arista entre dos
--  vértices si las correspondientes regiones son vecinas. Por ejemplo,
--  el mapa siguiente 
--        +----------+----------+       
--        |    1     |     2    |       
--        +----+-----+-----+----+       
--        |    |           |    |       
--        | 3  |     4     | 5  |       
--        |    |           |    |       
--        +----+-----+-----+----+       
--        |    6     |     7    |       
--        +----------+----------+
-- se pueden representar por
--    mapa :: Grafo Int Int
--    mapa = creaGrafo ND (1,7)
--                     [(1,2,0),(1,3,0),(1,4,0),(2,4,0),(2,5,0),(3,4,0),
--                      (3,6,0),(4,5,0),(4,6,0),(4,7,0),(5,7,0),(6,7,0)]
-- Para colorear el mapa se dispone de 4 colores definidos por   
--    data Color = A | B | C | D deriving (Eq, Show)
-- 
-- Definir la función
--    correcta :: [(Int,Color)] -> Grafo Int Int -> Bool
-- tal que (correcta ncs m) se verifica si ncs es una coloración del
-- mapa m tal que todos las regiones vecinas tienen colores distintos. 
-- Por ejemplo, 
--    correcta [(1,A),(2,B),(3,B),(4,C),(5,A),(6,A),(7,B)] mapa == True
--    correcta [(1,A),(2,B),(3,A),(4,C),(5,A),(6,A),(7,B)] mapa == False
-- ---------------------------------------------------------------------

mapa :: Grafo Int Int
mapa = creaGrafo ND (1,7)
                 [(1,2,0),(1,3,0),(1,4,0),(2,4,0),(2,5,0),(3,4,0),
                  (3,6,0),(4,5,0),(4,6,0),(4,7,0),(5,7,0),(6,7,0)]

data Color = A | B | C | E deriving (Eq, Show)

-- fracruzam juanarcon jespergue josllagam
correcta :: [(Int,Color)] -> Grafo Int Int -> Bool
correcta ncs g = and [color x /= color y | (x,y,_) <- aristas g]
    where color :: Int -> Color
          color x = head [c | (r,c) <- ncs, r == x]

-- alvalvdom1 erisancha rubvilval
correcta2 :: [(Int,Color)] -> Grafo Int Int -> Bool
correcta2 ncs g = and [coloresVecinos a ncs g | a <- ncs]

coloresVecinos :: (Int,Color) -> [(Int,Color)] -> Grafo Int Int -> Bool
coloresVecinos (v,c) ncs g =
    all (/=c) [c' | (v',c') <- ncs, elem v' (adyacentes g v)]

-- Comentario: La definición anterior se puede simplificar usando
-- notElem y reduciendo el número de paréntesis.
        
-- manvermor
correcta3 :: [(Int,Color)] -> Grafo Int Int -> Bool
correcta3 ncs g = and [x /= y | [x,y] <- separa2 (reemplazados ncs g)]

separa2 :: [a] -> [[a]]
separa2 [] = []
separa2 zs = take 2 zs : separa2 (drop 2 zs)

reemplazados :: (Num p, Ix a) => [(a, t)] -> Grafo a p -> [t]
reemplazados xs g = [reemplaza n xs | ys <- puntos g, n <- ys]

puntos :: (Num p, Ix a) => Grafo a p -> [[a]]
puntos g = [x:[y] | (x,y,_) <- aristas g]

reemplaza :: Eq a1 => a1 -> [(a1, a)] -> a
reemplaza n xs = head [y | (x,y) <- xs, x == n]

-- abrdelrod
correcta4 :: [(Int,Color)] -> Grafo Int Int -> Bool
correcta4 ncs g = all p xs
    where xs = concat (aux ncs)
          aux (x:xs) = [(x,z) | z <- xs] : aux xs
          aux _ = []
          p ((n1,c1),(n2,c2)) | c1 /= c2 = True
                              | otherwise = notElem (n1,n2,0) (aristas g)
                                            
-- Comentario: La definición anterior se puede simplificar reduciendo
-- paréntesis. 
                                            
-- carruirui3 marvilmor
correcta5 :: [(Int,Color)] -> Grafo Int Int -> Bool
correcta5 ncs = all (\(x,y,_) -> color x /= color y) . aristas
    where color x = snd . head $ filter ((x==) . fst) ncs

-- javoliher
correcta6 :: [(Int,Color)] -> Grafo Int Int -> Bool
correcta6 ncs g = and [correctos1aTodos  g ncs x | x <- nodos g]

correctos1aTodos :: Grafo Int Int -> [(Int,Color)] -> Int -> Bool
correctos1aTodos  g ncs x =
    and [(y,z) `notElem` ncs | y <- adyacentes g x]
    where z = color x ncs

color x ncs = head [snd y | y <- ncs, fst y == x]

-- ---------------------------------------------------------------------
-- Ejercicio 5. Dado un grafo dirigido G, diremos que un nodo está
-- aislado si o bien de dicho nodo no sale ninguna arista o bien no
-- llega al nodo ninguna arista. Por ejemplo, en el siguiente grafo
-- (Tema 22, pag. 31) 
--    grafo5 = creaGrafo D (1,6) [(1,2,0),(1,3,0),(1,4,0),(3,6,0),
--                                (5,4,0),(6,2,0),(6,5,0)]
-- podemos ver que del nodo 1 salen 3 aristas pero no llega ninguna, por
-- lo que lo consideramos aislado. Así mismo, a los nodos 2 y 4 llegan
-- aristas pero no sale ninguna, por tanto también estarán aislados.
--
-- Definir la función 
--    aislados :: (Ix v, Num p) => Grafo v p -> [v]
-- tal que (aislados g) es la lista de nodos aislados del grafo g. Por
-- ejemplo, 
--    aislados grafo5 == [1,2,4]
-- ---------------------------------------------------------------------
                    
grafo5 = creaGrafo D (1,6) [(1,2,0),(1,3,0),(1,4,0),(3,6,0),
                            (5,4,0),(6,2,0),(6,5,0)]


aislados :: (Ix v, Num p) => Grafo v p -> [v]
aislados g = filter (aislado (aristas g) True True) (nodos g)
  where aislado :: (Ix v, Num p) => [(v,v,p)] -> Bool -> Bool -> v -> Bool
        aislado ((x,y,_):as) b1 b2 n
            | x == n && y == n = False
            | x == n           = aislado as False b2    n
            |           y == n = aislado as b1    False n
            | otherwise        = aislado as b1    b2    n
        aislado []           b1 b2 _ = b1 || b2

-- juanarcon jespergue josllagam
aislados1 :: (Ix v, Num p) => Grafo v p -> [v]
aislados1 g = [x | x <- nodos g, esAislado g x]

esAislado :: (Num p, Ix a) => Grafo a p -> a -> Bool
esAislado g v =
  null (adyacentes g v) || null [1 | (_,x,_) <- aristas g, x == v]

-- alvalvdom1 erisancha
aislados2 :: (Ix v, Num p) => Grafo v p -> [v]
aislados2 g = (ns \\ [v | (_,v,_) <- as]) ++ (ns \\ [v | (v,_,_) <- as])
  where ns = nodos g
        as = aristas g

-- manvermor
aislados3 :: (Ix v, Num p) => Grafo v p -> [v]
aislados3 g = nub $ aislado xs ys
  where xs = [x | (x,_,_) <- aristas g]
        ys = [y | (_,y,_) <- aristas g]
        aislado xs ys = [z | z <- xs, notElem z ys] ++
                        [y | y <- ys, notElem y xs] 

-- abrdelrod marvilmor
aislados4 :: (Ix v, Num p) => Grafo v p -> [v]
aislados4 g = [x | x <- nodos g, any (notElem x) [map f1 xs, map f2 xs]]
    where xs = aristas g
          f1 (a,_,_) = a
          f2 (_,b,_) = b

-- carruirui3
aislados5 :: (Ix v, Num p) => Grafo v p -> [v]
aislados5 g = filter (aislado g) (nodos g)

aislado :: (Ix v, Num p) => Grafo v p -> v -> Bool
aislado g v = null (adyacentes g v) || null (adyacentes' g v)

adyacentes' :: (Ix v, Num p) => Grafo v p -> v -> [v]
adyacentes' g v = nub [u | (u,v',_) <- aristas g, v == v']

-- erisancha
aislados6 :: (Ix v, Num p) => Grafo v p -> [v]
aislados6 g = 
   [x | x <- primero g, x `notElem` segundo g]
   ++ [x | x <- segundo g, x `notElem` primero g] 

primero :: (Num p, Ix a) => Grafo a p -> [a]
primero g = nub [x | (x,_,_) <- aristas g]

segundo :: (Num p, Ix a) => Grafo a p -> [a]
segundo g = nub [x | (_,x,_) <- aristas g]

-- javoliher 
aislados7 :: (Ix v, Num p) => Grafo v p -> [v]
aislados7 g =
    [x | x <- nodos g,
         (incidentes g x) == [] || (adyacentes g x) == []]

incidentes :: (Num p, Ix v) => Grafo v p -> v -> [v]
incidentes g v = [fst x | x <- (map f (aristas g)), snd x == v]
    where f (a,b,c) = (a,b)

-- rubvilval
aislados8 :: (Ix v, Num p) => Grafo v p -> [v]
aislados8 g = filter (\x -> aislado8 x g) (nodos g)

sinpeso (a,b,c) = (a,b)

llegadas x g = [y | (y,x') <- (map sinpeso (aristas g)), x' == x]

salidas  x g = [y | (x',y) <- (map sinpeso (aristas g)), x' == x]

aislado8  x g = (llegadas x g == []) || (salidas x g == [])

-- ---------------------------------------------------------------------
-- Ejercicio 6. Consideremos una implementación del TAD de los grafos,
-- por ejemplo en la que los grafos se representan mediante listas. Un
-- ejemplo de grafo es el siguiente:
--    grafo6 :: Grafo Int Int
--    grafo6 = creaGrafo D (1,6) [(1,3,2),(1,5,4),(3,5,6),(5,1,8),(5,5,10),
--                                (2,4,1),(2,6,3),(4,6,5),(4,4,7),(6,4,9)]
-- 
-- Definir la función 
--    conectados :: Grafo Int Int -> Int -> Int -> Bool
-- tal que (conectados g v1 v2) se verifica si los vértices v1 y v2
-- están conectados en el grafo g. Por ejemplo,
--    conectados grafo6 1 3  ==  True
--    conectados grafo6 1 4  ==  False
--    conectados grafo6 6 2  ==  False
--    conectados grafo6 3 1  ==  True
-- ----------------------------------------------------------------------------

grafo6 :: Grafo Int Int
grafo6 = creaGrafo D (1,6) [(1,3,2),(1,5,4),(3,5,6),(5,1,8),(5,5,10),
                            (2,4,1),(2,6,3),(4,6,5),(4,4,7),(6,4,9)]

-- juanarcon manvermor jespergue erisancha josllagam marvilmor
conectados :: Grafo Int Int -> Int -> Int -> Bool
conectados g v1 v2 = aristaEn g (v1,v2)

-- Comentario: La definición anterior no es correcta. Falla en el último
-- ejemplo. 

-- alvalvdom1
conectados2 :: Grafo Int Int -> Int -> Int -> Bool
conectados2 g v1 v2 =
    not $ null [1 | (v1',v2',_) <- aristas g, v1'==v1, v2'==v2]

-- Comentario: La definición anterior no es correcta. Falla en el último
-- ejemplo. 

-- abrdelrod
conectados3 :: Grafo Int Int -> Int -> Int -> Bool
conectados3 g v1 v2 = aux v1 []
    where  incidentes v = [x | x <- nodos g, elem v (adyacentes g x)]
           aux a xs
             | elem a (incidentes v2) = True
             | and [elem b xs | b <- adyacentes g a] = False
             | otherwise = or [aux b (a:xs) | b <- adyacentes g a, b /= a]
                                  
-- carruirui3 fracruzam
conectados4 :: Grafo Int Int -> Int -> Int -> Bool
conectados4 g v1 v2 = v2 `elem` recorridoEnAnchura v1 g
