-- I1M 2015-16: Relación 17 (21 de diciembre de 2015)
-- Mayorías parlamentarias.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- En esta relación se presenta un caso de estudio de los tipos
-- de datos algebraicos para estudiar las mayorías parlamentarias. 
-- Además, con QuickCheck, se comprueban propiedades de las funciones
-- definidas. 

-- ---------------------------------------------------------------------
-- Importación de librerías auxiliares                                --
-- ---------------------------------------------------------------------

import Data.List
import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir el tipo de datos Partido para representar los
-- partidos de un Parlamento. Los partidos son P1, P2,..., P8. La clase 
-- Partido está contenida en Eq, Ord y Show. 
-- ---------------------------------------------------------------------

data Partido
    = P1 | P2 | P3 | P4 | P5 | P6 | P7 | P8
    deriving (Eq, Ord, Show)

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir el tipo Parlamentarios para representar el
-- número de parlamentarios que posee un partido en el parlamento. 
-- ---------------------------------------------------------------------

type Parlamentarios = Integer

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir el tipo (Tabla a b) para representar una lista
-- de pares de elementos el primero de tipo a y el segundo de tipo
-- b. Definir Asamblea para representar una tabla de partidos y
-- parlamentarios.  
-- ---------------------------------------------------------------------

type Tabla a b = [(a,b)]
type Asamblea  = Tabla Partido Parlamentarios

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función
--    partidos :: Asamblea -> [Partido]
-- tal que (partidos a) es la lista de partidos en la asamblea a. Por
-- ejemplo, 
--    partidos [(P1,3),(P3,5),(P4,3)]  ==>  [P1,P3,P4]
-- ---------------------------------------------------------------------

-- fracruzam rubvilval manpende isrbelnun 
partidos :: Asamblea -> [Partido]
partidos = map fst

-- manvermor alvalvdom1 ivaruicam javperlag juanarcon
partidos2 :: Asamblea -> [Partido]
partidos2 a = [x | (x,_) <- a]

-- blaruiher
partidos3 :: Asamblea -> [Partido]
partidos3 a = [fst b | b <- a]

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la función
--    parlamentarios :: Asamblea -> Integer
-- tal que (parlamentarios a) es el número de parlamentarios en la
-- asamblea a. Por ejemplo,
--    parlamentarios [(P1,3),(P3,5),(P4,3)]  ==>  11
-- ---------------------------------------------------------------------

-- fracruzam rubvilval manpende isrbelnun
parlamentarios :: Asamblea -> Integer
parlamentarios = sum . map snd

-- manvermor alvalvdom1 ivaruicam javperlag juanarcon 
parlamentarios2 :: Asamblea -> Integer
parlamentarios2 a = sum [x | (_,x) <- a]

-- blaruiher
parlamentarios3 :: Asamblea -> Integer
parlamentarios3 a = sum [snd x | x <- a]

-- ---------------------------------------------------------------------
-- Ejercicio 6. Definir la función
--    busca :: Eq a => a -> Tabla a b -> b
-- tal que (busca x t) es el valor correspondiente a x en la tabla
-- t. Por ejemplo, 
--    ghci> busca P3 [(P1,2),(P3,19)]
--    19
--    ghci> busca P8 [(P1,2),(P3,19)]
--    *** Exception: no tiene valor en la tabla
-- ---------------------------------------------------------------------

-- fracruzam
busca :: Eq a => a -> Tabla a b -> b
busca p = snd . head . filter (\(a,_) -> p == a)

-- Comentario: La definición anterior se puede mejorar para ajustarse al
-- mensaje del 2º ejemplo,

-- manvermor alvalvdom1 ivaruicam rubvilval manpende juanarcon isrbelnun
-- blaruiher
busca2 :: Eq a => a -> Tabla a b -> b
busca2 x t = head [y | (z,y) <- t , x == z]

-- javperlag 
busca3 x ((p,n):xs)|x==p     = n
                   |otherwise= busca x xs

-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir la función
--    busca' :: Eq a => a -> Table a b -> Maybe b 
-- tal que (busca' x t) es justo el valor correspondiente a x en la
-- tabla t, o Nothing si x no tiene valor. Por ejemplo, 
--    busca' P3 parlamento   ==   Just 19
--    busca' P8 parlamento   ==   Nothing
-- ---------------------------------------------------------------------

-- fracruzam
busca' :: Eq a => a -> Tabla a b -> Maybe b
busca' p t = if null f 
             then Nothing 
             else Just $ snd $ head f
  where f = filter (\(a,_) -> p == a) t

-- manvermor alvalvdom1 ivaruicam rubvilval manpende javperlag juanarcon
-- isrbelnun 
busca'2 :: Eq a => a -> Tabla a b -> Maybe b
busca'2 x t | null xs   = Nothing
            | otherwise = Just (head xs)
    where xs =  [y | (z,y) <- t, x == z] 

-- ---------------------------------------------------------------------
-- Ejercicio 8. Comprobar con QuickCheck que si (busca' x t) es
-- Nothing, entonces x es distinto de todos los elementos de t. 
-- ---------------------------------------------------------------------

-- fracruzam rubvilval

-- La propiedad es
prop_BuscaNothing :: Integer -> [(Integer,Integer)] -> Property
prop_BuscaNothing x t = 
    busca' x t == Nothing ==> all (x /=) (map fst t)


-- manvermor alvalvdom1 ivaruicam manpende javperlag juanarcon
prop_BuscaNothing2 :: Integer -> [(Integer,Integer)] -> Property
prop_BuscaNothing2 x t = 
    busca' x t == Nothing ==> notElem x [z | (z,y) <- t] 

-- isrbelnun
prop_BuscaNothing3 :: Integer -> [(Integer,Integer)] -> Property
prop_BuscaNothing3 x t = 
    busca' x t == Nothing ==> notElem x (map fst t)

-- La comprobación es
--    *Main> quickCheck prop_BuscaNothing
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 9. Comprobar que la función busca' es equivalente a la
-- función lookup del Prelude. 
-- ---------------------------------------------------------------------

-- fracruzam manvermor alvalvdom1 ivaruicam rubvilval manpende javperlag
-- juanarcon isrbelnun

-- La propiedad es
prop_BuscaEquivLookup :: Integer -> [(Integer,Integer)] -> Bool
prop_BuscaEquivLookup x t = busca' x t == lookup x t

-- La comprobación es
--    *Main> quickCheck prop_BuscaEquivLookup
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 10. Definir el tipo Coalicion como una lista de partidos.
-- ---------------------------------------------------------------------

type Coalicion = [Partido]

-- ---------------------------------------------------------------------
-- Ejercicio 11. Definir la función
--    mayoria :: Asamblea -> Integer
-- tal que (mayoria xs) es el número de parlamentarios que se necesitan
-- para tener la mayoría en la asamblea xs. Por ejemplo,
--    mayoria [(P1,3),(P3,5),(P4,3)]   ==   6 
-- ---------------------------------------------------------------------

-- fracruzam
mayoria :: Asamblea -> Integer
mayoria xs = floor (pxs / 2 + 1)
    where pxs = fromIntegral $ parlamentarios xs

-- Comentario: La definición anterior se puede simplificar.

-- manvermor alvalvdom1 ivaruicam rubvilval juanarcon isrbelnun
mayoria2 :: Asamblea -> Integer
mayoria2 xs = ceiling $ fromIntegral (parlamentarios xs) / 2

-- Comentario: La definición anterior se puede simplificar.

-- manpende javperlag
mayoria3 :: Asamblea -> Integer
mayoria3 xs = div (parlamentarios xs) 2 + 1

-- ---------------------------------------------------------------------
-- Ejercicio 12. Definir la función
--    coaliciones :: Asamblea -> Integer -> [Coalicion]
-- tal que (coaliciones xs n) es la lista de coaliciones necesarias para
-- alcanzar n parlamentarios. Por ejemplo,
--    coaliciones [(P1,3),(P2,2),(P3,1)] 3   ==  [[P2,P3],[P1]]
--    coaliciones [(P1,3),(P3,5),(P4,3)] 6   ==  [[P3,P4],[P1,P4],[P1,P3]]
--    coaliciones [(P1,3),(P3,5),(P4,3)] 9   ==  [[P1,P3,P4]]
--    coaliciones [(P1,3),(P3,5),(P4,3)] 14  ==  []
--    coaliciones [(P1,3),(P3,5),(P4,3)] 2   ==  [[P4],[P3],[P1]]
--    coaliciones [(P1,2),(P3,5),(P4,3)] 6   ==  [[P3,P4],[P1,P3]]
-- ---------------------------------------------------------------------

-- javperlag juanarcon isrbelnun
coaliciones :: Asamblea -> Integer -> [Coalicion]
coaliciones xs n = 
    [partidos ys 
    | ys <- subsequences xs
    , parlamentarios ys >= n
    , all (<n) (map parlamentarios (init (subsequences ys)))]

-- Esta definición calcula todos los ejemplos menos el segundo del próximo 
-- ejercicio, en que obtengo [[P3],[P1,P4]] porque entiendo que si P3 hace
-- mayoría por sí solo no necesita el apoyo de P1.

-- ---------------------------------------------------------------------
-- Ejercicio 13. Definir la función
--    mayorias :: Asamblea -> [Coalicion]
-- tal que (mayorias a) es la lista de coaliciones mayoritarias en la
-- asamblea a. Por ejemplo,
--    mayorias [(P1,3),(P3,5),(P4,3)]   ==   [[P3,P4],[P1,P4],[P1,P3]]
--    mayorias [(P1,2),(P3,5),(P4,3)]   ==   [[P3,P4],[P1,P3]]
-- ---------------------------------------------------------------------

-- rubvilval manpende javperlag juanarcon isrbelnun alvalvdom1
mayorias :: Asamblea -> [Coalicion]
mayorias asamblea = coaliciones asamblea (mayoria asamblea)

-- ---------------------------------------------------------------------
-- Ejercicio 14. Definir el tipo de datos Asamblea.
-- ---------------------------------------------------------------------

data Asamblea2 = A Asamblea 
                 deriving Show

-- ---------------------------------------------------------------------
-- Ejercicio 15. Definir la propiedad
--    esMayoritaria :: Coalicion -> Asamblea -> Bool
-- tal que (esMayoritaria c a) se verifica si la coalición c es
-- mayoritaria en la asamblea a. Por ejemplo, 
--    esMayoritaria [P3,P4] [(P1,3),(P3,5),(P4,3)]   ==   True
--    esMayoritaria [P4] [(P1,3),(P3,5),(P4,3)]      ==   False
-- ---------------------------------------------------------------------

-- fracruzam manpende
esMayoritaria :: Coalicion -> Asamblea -> Bool
esMayoritaria c a = 
    sum (map (\p -> busca p a) c) >= mayoria a

-- rubvilval
esMayoritaria3 :: Coalicion -> Asamblea -> Bool
esMayoritaria3 c a = sum (valores c a) >= mayoria a
    where valores [] _ = []
          valores (c:cs) a = busca c a : valores cs a

-- juanarcon isrbelnun alvalvdom1
esMayoritaria4 :: Coalicion -> Asamblea -> Bool
esMayoritaria4 c a = elem c (mayorias a)

-- ---------------------------------------------------------------------
-- Ejercicio 16. Comprobar con QuickCheck que las coaliciones
-- obtenidas por (mayorias asamblea) son coaliciones mayoritarias en la
-- asamblea. 
-- ---------------------------------------------------------------------

-- fracruzam ivaruicam
-- La propiedad es
prop_MayoriasSonMayoritarias :: Asamblea2 -> Bool
prop_MayoriasSonMayoritarias (A a) = 
    all (\c -> esMayoritaria c a) (mayorias a)

-- manvermor manpende isrbelnun alvalvdom1
prop_MayoriasSonMayoritarias2 :: Asamblea2 -> Bool
prop_MayoriasSonMayoritarias2 (A a) = 
    and [esMayoritaria c a | c <- mayorias a] 

-- La comprobación es
--    *Main> quickCheck prop_MayoriasSonMayoritarias
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 17. Definir la función
--    esMayoritariaMinimal :: Coalicion -> Asamblea -> Bool
-- tal que (esMayoritariaMinimal c a) se verifica si la coalición c es
-- mayoritaria en la asamblea a, pero si se quita a c cualquiera de sus
-- partidos la coalición resultante no es mayoritaria. Por ejemplo, 
--    esMayoritariaMinimal [P3,P4] [(P1,3),(P3,5),(P4,3)]           ==  True
--    esMayoritariaMinimal [P1,P3,P4] [(P1,3),(P3,5),(P4,3)]        ==  False
--    esMayoritariaMinimal [P2,P3,P4] [(P1,3),(P2,2),(P3,1),(P4,1)] == True
-- ---------------------------------------------------------------------

-- ivaruicam
esMayoritariaMinimal :: Coalicion -> Asamblea -> Bool
esMayoritariaMinimal c a = 
    esMayoritaria c a && all (\x -> not (esMayoritaria x a)) (menosuna c) 

menosuna xs = take (length xs) (permutaciones xs)
    where permutaciones (x:xs) = xs : permutaciones (xs ++ [x])

-- rubvilval
esMayoritariaMinimal2 :: Coalicion -> Asamblea -> Bool
esMayoritariaMinimal2 c a = 
    esMayoritaria c a && 
    null (filter (\c -> esMayoritaria c a) (menos c))
    where menos c = [a | a <- subsequences c, length c - 1 == length a] 

-- Comentario: La definición anterior se puede mejorar.

-- manpende isrbelnun
esMayoritariaMinimal3 :: Coalicion -> Asamblea -> Bool
esMayoritariaMinimal3 c a = 
    esMayoritaria c a && 
    null [ x | x <- subsequences c, 
               genericLength x < genericLength c, 
               esMayoritaria x a] 

-- Comentario: La definición anterior se puede mejorar.

-- ---------------------------------------------------------------------
-- Ejercicio 18. Comprobar con QuickCheck si las coaliciones obtenidas
-- por (mayorias asamblea) son coaliciones mayoritarias minimales en la
-- asamblea.  
-- ---------------------------------------------------------------------

-- fracruzam

-- La propiedad es
prop_MayoriasSonMayoritariasMinimales :: Asamblea2 -> Bool
prop_MayoriasSonMayoritariasMinimales (A a) = 
    all (\c -> esMayoritariaMinimal c a) (mayorias a)
      
-- manvermor rubvilval manpende isrbelnun alvalvdom1
prop_MayoriasSonMayoritariasMinimales2 :: Asamblea2 -> Bool
prop_MayoriasSonMayoritariasMinimales2 (A a) = 
    and [esMayoritariaMinimal c a | c <- mayorias a]

-- La comprobación es
--    *Main> quickCheck prop_MayoriasSonMayoritariasMinimales
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 19. Definir la función
--    coalicionesMinimales :: Asamblea -> Integer -> [Coalicion,Parlamentarios]
-- tal que (coalicionesMinimales xs n) es la lista de coaliciones
-- minimales necesarias para alcanzar n parlamentarios. Por ejemplo, 
--    Main> coalicionesMinimales [(P1,3),(P3,5),(P4,3)] 6
--    [([P3,P4],8),([P1,P4],6),([P1,P3],8)]
--    Main> coalicionesMinimales [(P1,3),(P3,5),(P4,3)] 5
--    [([P3],5),([P1,P4],6)]
-- ---------------------------------------------------------------------

-- fracruzam
coalicionesMinimales :: Asamblea -> Integer -> [(Coalicion,Parlamentarios)]
coalicionesMinimales a n = 
  map (\l -> (map fst l, parlamentarios l)) $ 
      filter (minimal n) (subsequences a)
  where minimal :: Integer -> Asamblea -> Bool
        minimal n a = all (\(_,m) -> m > d) a && x >= n
          where d = x - n
                x = parlamentarios a

-- Comentario: La definición anterior se puede mejorar.

-- rubvilval
coalicionesMinimales2 :: Asamblea -> Integer -> [(Coalicion,Parlamentarios)]
coalicionesMinimales2 xs n = 
    [(a,b) | (a,b) <- pares xs, length a < length xs, esminimal a xs n] 

-- Comentario: La definición anterior se puede mejorar.

pares xs = zip (subsequences $ partidos xs) 
               (map sum (map (\a->corr a xs)
                             (subsequences $ partidos xs)))

esminimal a xs n = 
    sum (corr a xs) >= n && null (filter (\a->sum (corr a xs)>=n ) (menos a))
    where menos a = [x | x <- subsequences a, (length a)-1==length x]

corr [] _ = []
corr (c:cs) a = busca c a : corr cs a

-- isrbelnun
coalicionesMinimales3 :: Asamblea -> Integer -> [(Coalicion,Parlamentarios)]
coalicionesMinimales3 xs n = 
    [(map fst ps,parlamentarios ps) 
    | ps <- subsequences xs
    , parlamentarios ps >= n
    , all (<n) (map parlamentarios (init (subsequences ps)))]

-- ---------------------------------------------------------------------
-- Ejercicio 20. Definir la función
--    mayoriasMinimales :: Asamblea -> [Coalicion]
-- tal que (mayoriasMinimales a) es la lista de coaliciones mayoritarias
-- minimales en la asamblea a. Por ejemplo,
--    mayoriasMinimales [(P1,3),(P3,5),(P4,3)] == [[P3,P4],[P1,P4],[P1,P3]]
-- ---------------------------------------------------------------------

-- rubvilval
mayoriasMinimales :: Asamblea -> [Coalicion]
mayoriasMinimales a = filter (\x->esMayoritariaMinimal x a)
                             (subsequences $ partidos a)

-- Comentario: La definición anterior se puede mejorar.

-- isrbelnun
mayoriasMinimales2 :: Asamblea -> [Coalicion]
mayoriasMinimales2 asamblea = 
    map fst (coalicionesMinimales asamblea (mayoria asamblea))

-- ---------------------------------------------------------------------
-- Ejercicio 21. Comprobar con QuickCheck que las coaliciones
-- obtenidas por (mayoriasMinimales asamblea) son coaliciones
-- mayoritarias minimales en la asamblea. 
-- ---------------------------------------------------------------------

-- La propiedad es
prop_MayoriasMinimalesSonMayoritariasMinimales :: Asamblea2 -> Bool
prop_MayoriasMinimalesSonMayoritariasMinimales (A asamblea) =  
    all (\x -> esMayoritariaMinimal x asamblea) 
        (mayoriasMinimales asamblea)

-- isrbelnun
prop_MayoriasMinimalesSonMayoritariasMinimales2 :: Asamblea2 -> Bool
prop_MayoriasMinimalesSonMayoritariasMinimales2 (A asamblea) =
    and [esMayoritariaMinimal m asamblea | m <- mayoriasMinimales asamblea]

-- La comprobación es
--    *Main> quickCheck prop_MayoriasMinimalesSonMayoritariasMinimales
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Funciones auxiliares                                               --
-- ---------------------------------------------------------------------

-- (listaDe n g) es una lista de n elementos, donde cada elemento es
-- generado por g. Por ejemplo, 
--    Main> muestra (listaDe 3 (arbitrary :: Gen Int))
--    [-1,1,-1]
--    [-2,-4,-1]
--    [1,-1,0]
--    [1,-1,1]
--    [1,-1,1]
--    Main> muestra (listaDe 3 (arbitrary :: Gen Bool))
--    [False,True,False]
--    [True,True,False]
--    [False,False,True]
--    [False,False,True]
--    [True,False,True]
listaDe :: Int -> Gen a -> Gen [a]
listaDe n g = sequence [g | i <- [1..n]]

-- paresDeIgualLongitud genera pares de listas de igual longitud. Por
-- ejemplo, 
--    Main> muestra (paresDeIgualLongitud (arbitrary :: Gen Int))
--    ([-4,5],[-4,2])
--    ([],[])
--    ([0,0],[-2,-3])
--    ([2,-2],[-2,1])
--    ([0],[-1])
--    Main> muestra (paresDeIgualLongitud (arbitrary :: Gen Bool))
--    ([False,True,False],[True,True,True])
--    ([True],[True])
--    ([],[])
--    ([False],[False])
--    ([],[])
paresDeIgualLongitud :: Gen a -> Gen ([a],[a])
paresDeIgualLongitud gen =
    do n <- arbitrary
       xs <- listaDe (abs n) gen
       ys <- listaDe (abs n) gen
       return (xs,ys)

-- generaAsamblea esun generador de datos de tipo Asamblea. Por ejemplo, 
--    Main> muestra generaAsamblea
--    A [(P1,1),(P2,1),(P3,0),(P4,1),(P5,0),(P6,1),(P7,0),(P8,1)]
--    A [(P1,0),(P2,1),(P3,1),(P4,1),(P5,0),(P6,1),(P7,0),(P8,1)]
--    A [(P1,1),(P2,2),(P3,0),(P4,1),(P5,0),(P6,1),(P7,2),(P8,0)]
--    A [(P1,1),(P2,0),(P3,1),(P4,0),(P5,0),(P6,1),(P7,1),(P8,1)]
--    A [(P1,1),(P2,0),(P3,0),(P4,0),(P5,1),(P6,1),(P7,1),(P8,0)]
generaAsamblea :: Gen Asamblea2
generaAsamblea = 
    do xs <- listaDe 8 (arbitrary :: Gen Integer)
       return (A (zip [P1,P2,P3,P4,P5,P6,P7,P8] (map abs xs)))

instance Arbitrary Asamblea2 where
    arbitrary   = generaAsamblea
    -- coarbitrary = undefined
