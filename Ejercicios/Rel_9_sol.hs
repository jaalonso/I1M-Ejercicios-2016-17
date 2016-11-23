-- I1M 2016-17: Relación 9 (16 de noviembre de 2016)
-- Tipos de datos algebraicos: Árboles binarios.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- En esta relación se presenta ejercicios sobre árboles binarios
-- definidos como tipos de datos algebraicos.
-- 
-- Los ejercicios corresponden al tema 9 que se encuentra en 
--    http://www.cs.us.es/~jalonso/cursos/i1m-16/temas/tema-9.html

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Test.QuickCheck
import Control.Monad

-- ---------------------------------------------------------------------
-- Nota. En los siguientes ejercicios se trabajará con los árboles
-- binarios definidos como sigue 
--    data Arbol a = H 
--                 | N a (Arbol a) (Arbol a)
--                 deriving (Show, Eq)
-- Por ejemplo, el árbol
--         9 
--        / \
--       /   \
--      3     7
--     / \  
--    2   4 
-- se representa por
--    N 9 (N 3 (H 2) (H 4)) (H 7) 
-- ---------------------------------------------------------------------

data Arbol a = H a
             | N a (Arbol a) (Arbol a)
             deriving (Show, Eq)

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Definir la función
--    nHojas :: Arbol a -> Int
-- tal que (nHojas x) es el número de hojas del árbol x. Por ejemplo,
--    nHojas (N 9 (N 3 (H 2) (H 4)) (H 7))  ==  3
-- ---------------------------------------------------------------------


-- eliguivil roscargar paumacpar eledejim2 enrnarbej antmorper3 pabrabmon
-- josdeher albcercid ignareeva cescarde manruiber natmarmar2 cargonler
-- carmarcar5 belbenzam antbeacar josrodgal7 glovizcas margirmon alvfercen 
-- marjimcom luimotmar monlagare albagucen migibagar felsuacor
-- beagongon1 fatfervaz artmorfer margarvil14 fraferpoy congomgom josjimgon2
-- juaorture criortcar joscasgom1 marlobrip marmerzaf margarflo5 natruipin
nHojas :: Arbol a -> Int
nHojas (H _)     = 1
nHojas (N _ a b) = nHojas a + nHojas b

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Definir la función
--    nNodos :: Arbol a -> Int
-- tal que (nNodos x) es el número de nodos del árbol x. Por ejemplo,
--    nNodos (N 9 (N 3 (H 2) (H 4)) (H 7))  ==  2
-- ---------------------------------------------------------------------

-- eliguivil roscargar paumacpar eledejim2 enrnarbej antmorper3 pabrabmon
-- albcercid josdeher ignareeva cescarde manruiber natmarmar2 cargonler
-- carmarcar5 belbenzam antbeacar josrodgal7 glovizcas margirmon alvfercen
-- marjimcom luimotmar monlagare albagucen migibagar felsuacor artmorfer
-- beagongon1 fatfervaz margarvil14 fraferpoy congomgom juaorture criortcar
-- joscasgom1 marlobrip marmerzaf margarflo5 natruipin josjimgon2
nNodos :: Arbol a -> Int
nNodos (H _)     = 0
nNodos (N _ a b) = 1 + nNodos a + nNodos b

-- ---------------------------------------------------------------------
-- Ejercicio 1.3. Comprobar con QuickCheck que en todo árbol binario el
-- número de sus hojas es igual al número de sus nodos más uno.
-- ---------------------------------------------------------------------

-- La propiedad es

-- eliguivil roscargar paumacpar eledejim2 enrnarbej antmorper3 pabrabmon
-- albcercid josdeher ignareeva cescarde manruiber natmarmar2 cargonler
-- carmarcar5 belbenzam antbeacar josrodgal7 glovizcas margirmon alvfercen
-- marjimcom luimotmar monlagare albagucen migibagar felsuacor
-- beagongon1 fatfervaz artmorfer margarvil14 fraferpoy congomgom
-- juaorture criortcar joscasgom1 marlobrip marmerzaf margarflo5
-- natruipin josjimgon2 
prop_nHojas :: Arbol Int -> Bool
prop_nHojas x = nHojas x == nNodos x +1

-- La comprobación es
--    λ> quickCheck prop_nHojas
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Definir la función
--    profundidad :: Arbol a -> Int
-- tal que (profundidad x) es la profundidad del árbol x. Por ejemplo,
--    profundidad (N 9 (N 3 (H 2) (H 4)) (H 7))              ==  2
--    profundidad (N 9 (N 3 (H 2) (N 1 (H 4) (H 5))) (H 7))  ==  3
--    profundidad (N 4 (N 5 (H 4) (H 2)) (N 3 (H 7) (H 4)))  ==  2
-- ---------------------------------------------------------------------

-- albcercid josdeher cescarde carmarcar5 antbeacar josrodgal7 margirmon
-- enrnarbej marjimcom luimotmar paumacpar albagucen migibagar josjimgon2
-- natmarmar2 felsuacor beagongon1 fatfervaz antmorper3 artmorfer margarvil14 
-- fraferpoy congomgom juaorture joscasgom1 criortcar marlobrip
-- margarflo5 natruipin 
profundidad :: Arbol a -> Int
profundidad (H _)     = 0
profundidad (N _ x y) = 1 + max (profundidad x) (profundidad y)

-- eliguivil roscargar 
profundidad2 :: Arbol a -> Int
profundidad2 (H _) = 0
profundidad2 (N _ i d) | ni == nd || ni > nd = 1 + profundidad2 i
                       | otherwise           = 1 + profundidad2 d
  where { ni = nNodos i ;
          nd = nNodos d }

-- Comentario: La definición profundidad2 es incorrecta. Para buscar un
-- contraejemplo, se considera la siguiente propiedad:
prop_profundidad2 :: Arbol Int -> Bool
prop_profundidad2 a =
  profundidad2 a == profundidad a

-- La comprobación es
--    λ> quickCheckWith (stdArgs {maxSize=9, maxSuccess=900}) prop_profundidad2
--    *** Failed! Falsifiable (after 22815 tests): 
--    N 2
--      (N 8
--         (N 3 (H 2) (H 0))
--         (N 2 (H 9) (H 7)))
--      (N 6
--         (N 6
--            (N 0 (H 4) (H 2))
--            (H 4))
--         (H 7))

-- El contraejemplo es
contraejemplo_profundidad2 :: Arbol Int
contraejemplo_profundidad2 =
  N 2
    (N 8
       (N 3 (H 2) (H 0))
       (N 2 (H 9) (H 7)))
    (N 6
       (N 6
          (N 0 (H 4) (H 2))
          (H 4))
       (H 7))

-- En efecto,
--    λ> profundidad contraejemplo_profundidad2
--    4
--    λ> profundidad2 contraejemplo_profundidad2
--    3

-- eledejim2 antmorper3 pabrabmon manruiber cargonler ignareeva
-- glovizcas alvfercen monlagare marmerzaf
profundidad3 :: Arbol a -> Int
profundidad3 (H _) = 0
profundidad3 (N _ x y) | xn >= yn  = 1 + profundidad3 x
                       | otherwise = 1 + profundidad3 y
  where { xn = nNodos x ;
          yn = nNodos y }

-- Comentario: La definición profundidad3 es incorrecta como se ve con
-- el contraejemplo anterior
--    λ> profundidad contraejemplo_profundidad2
--    4
--    λ> profundidad3 contraejemplo_profundidad2
--    3

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Comprobar con QuickCheck que para todo árbol binario
-- x, se tiene que
--    nNodos x <= 2^(profundidad x) - 1
-- ---------------------------------------------------------------------

-- La propiedad es

-- eliguivil roscargar eledejim2 enrnarbej antmorper3 pabrabmon albcercid
-- cescarde josdeher manruiber cargonler carmarcar5 antbeacar josrodgal7
-- ignareeva glovizcas margirmon alvfercen marjimcom luimotmar monlagare
-- paumacpar albagucen migibagar natmarmar2 felsuacor beagongon1 fatfervaz 
-- artmorfer margarvil14 congomgom fraferpoy juaorture joscasgom1 criortcar
-- marlobrip marmerzaf margarflo5 natruipin josjimgon2
prop_nNodosProfundidad :: Arbol Int -> Bool
prop_nNodosProfundidad x = nNodos x <= 2^(profundidad x) - 1

-- La comprobación es
-- λ> quickCheck prop_nNodosProfundidad
-- +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Definir la función
--    preorden :: Arbol a -> [a]
-- tal que (preorden x) es la lista correspondiente al recorrido
-- preorden del árbol x; es decir, primero visita la raíz del árbol, a
-- continuación recorre el subárbol izquierdo y, finalmente, recorre el
-- subárbol derecho. Por ejemplo,
--    preorden (N 9 (N 3 (H 2) (H 4)) (H 7))  ==  [9,3,2,4,7]
-- ---------------------------------------------------------------------

-- eliguivil roscargar paumacpar enrnarbej antmorper3 pabrabmon albcercid
-- cescarde josdeher manruiber natmarmar2 cargonler carmarcar5 antbeacar
-- josrodgal7 belbenzam ignareeva glovizcas eledejim2 margirmon alvfercen
-- marjimcom luimotmar monlagare albagucen migibagar felsuacor beagongon1
-- fatfervaz margarvil14 artmorfer fraferpoy juaorture congomgom joscasgom1
-- criortcar marlobrip marmerzaf margarflo5 natruipin josjimgon2
preorden :: Arbol a -> [a]
preorden (H a)     = [a]
preorden (N n i d) = n : preorden i ++ preorden d

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Comprobar con QuickCheck que la longitud de la lista
-- obtenida recorriendo un árbol en sentido preorden es igual al número
-- de nodos del árbol más el número de hojas.
-- ---------------------------------------------------------------------

-- eliguivil roscargar paumacpar enrnarbej antmorper3 pabrabmon albcercid
-- cescarde josdeher manruiber natmarmar2 cargonler carmarcar5 antbeacar
-- josrodgal7 belbenzam ignareeva glovizcas eledejim2 margirmon alvfercen
-- marjimcom luimotmar monlagare albagucen migibagar felsuacor beagongon1
-- fatfervaz margarvil14 artmorfer fraferpoy juaorture congomgom joscasgom1
-- criortcar marlobrip marmerzaf margarflo5 natruipin josjimgon2

-- La propiedad es
prop_length_preorden :: Arbol Int -> Bool
prop_length_preorden x = length (preorden x) == nNodos x + nHojas x

-- La comprobación es
--    λ> quickCheck prop_length_preorden
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 3.3. Definir la función
--    postorden :: Arbol a -> [a]
-- tal que (postorden x) es la lista correspondiente al recorrido
-- postorden del árbol x; es decir, primero recorre el subárbol
-- izquierdo, a continuación el subárbol derecho y, finalmente, la raíz
-- del árbol. Por ejemplo,
--    postorden (N 9 (N 3 (H 2) (H 4)) (H 7))  ==  [2,4,3,7,9]
-- ---------------------------------------------------------------------

-- eliguivil roscargar paumacpar enrnarbej antmorper3 pabrabmon albcercid
-- cescarde josdeher manruiber natmarmar2 cargonler antbeacar josrodgal7
-- migibagar belbenzam ignareeva glovizcas eledejim2 margirmon alvfercen
-- marjimcom luimotmar monlagare albagucen felsuacor beagongon1
-- fatfervaz margarvil14 artmorfer juaorture fraferpoy congomgom joscasgom1
-- criortcar marlobrip marmerzaf margarflo5 natruipin josjimgon2
postorden :: Arbol a -> [a]
postorden (H a)     = [a]
postorden (N n i d) = postorden i ++ postorden d ++ [n]

-- ---------------------------------------------------------------------
-- Ejercicio 3.4. Definir, usando un acumulador, la función
--    preordenIt :: Arbol a -> [a]
-- tal que (preordenIt x) es la lista correspondiente al recorrido
-- preorden del árbol x; es decir, primero visita la raíz del árbol, a
-- continuación recorre el subárbol izquierdo y, finalmente, recorre el
-- subárbol derecho. Por ejemplo,
--    preordenIt (N 9 (N 3 (H 2) (H 4)) (H 7))  ==  [9,3,2,4,7]
-- 
-- Nota: No usar (++) en la definición
-- ---------------------------------------------------------------------

-- enrnarbej pabrabmon albcercid cescarde josdeher manruiber cargonler
-- ignareeva eledejim2 marjimcom luimotmar albagucen natmarmar2 migibagar
-- beagongon1 antmorper3 josrodgal7 margarvil14 artmorfer congomgom
-- joscasgom1 natruipin criortcar josjimgon2  
preordenIt :: Arbol a -> [a]
preordenIt x = preordenItAux x []
  where
    preordenItAux (H a) xs     = a:xs
    preordenItAux (N a b c) xs = a : preordenItAux b (preordenItAux c xs)

-- eliguivil paumacpar 
preordenIt2 :: Arbol a -> [a]
preordenIt2 (H a) = [a]
preordenIt2 n = reverse (aux [] n)
  where aux :: [a] -> Arbol a -> [a]
        aux xs (H a)     = a:xs
        aux xs (N n i d) = aux (aux (n:xs) i) d

-- albcercid carmarcar5 glovizcas margirmon alvfercen felsuacor marmerzaf
preordenIt3 :: Arbol a -> [a]
preordenIt3 x = preAux [] x
  where preAux v (H x) = (x:v)
        preAux v (N x y z) = x:concat [preAux [] y, preAux [] z]

-- ---------------------------------------------------------------------
-- Ejercicio 3.5. Comprobar con QuickCheck que preordenIt es equivalente
-- a preorden. 
-- ---------------------------------------------------------------------

-- eliguivil enrnarbej pabrabmon albcercid cescarde josdeher manruiber
-- paumacpar cargonler carmarcar5 ignareeva antbeacar glovizcas migibagar
-- eledejim2 margirmon alvfercen marjimcom luimotmar albagucen natmarmar2
-- felsuacor beagongon1 antmorper3 fatfervaz josrodgal7 margarvil14 
-- artmorfer juaorture congomgom joscasgom1 criortcar marlobrip marmerzaf
-- natruipin margarflo5 josjimgon2
-- La propiedad es
prop_preordenIt :: Arbol Int -> Bool
prop_preordenIt x = preorden x == preordenIt x

-- La comprobación es
--    λ> quickCheck prop_preordenIt
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. Definir la función
--    espejo :: Arbol a -> Arbol a
-- tal que (espejo x) es la imagen especular del árbol x. Por ejemplo,
--    espejo (N 9 (N 3 (H 2) (H 4)) (H 7)) == N 9 (H 7) (N 3 (H 4) (H 2))
-- ---------------------------------------------------------------------

-- eliguivil roscargar enrnarbej pabrabmon albcercid cescarde josdeher
-- manruiber paumacpar natmarmar2 cargonler carmarcar5 belbenzam
-- antbeacar eledejim2 margirmon ignareeva alvfercen marjimcom luimotmar
-- glovizcas monlagare albagucen migibagar felsuacor beagongon1 antmorper3 
-- fatfervaz josrodgal7 artmorfer juaorture fraferpoy joscasgom1 congomgom
-- criortcar marlobrip marmerzaf natruipin margarflo5 josjimgon2
-- margarvil14 
espejo :: Arbol a -> Arbol a
espejo (H a) = H a
espejo (N n i d) = N n (espejo d) (espejo i)

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Comprobar con QuickCheck que para todo árbol x,
--    espejo (espejo x) = x
-- ---------------------------------------------------------------------

-- eliguivil roscargar enrnarbej pabrabmon albcercid cescarde josdeher
-- manruiber paumacpar natmarmar2 cargonler carmarcar5 belbenzam
-- antbeacar eledejim2 margirmon  ignareeva alvfercen marjimcom luimotmar
-- glovizcas monlagare albagucen migibagar felsuacor beagongon1 antmorper3
-- fatfervaz josrodgal7 margarvil14 artmorfer juaorture fraferpoy joscasgom1
-- congomgom criortcar marlobrip marmerzaf natruipin margarflo5 josjimgon2
-- La propiedad es
prop_espejo :: Arbol Int -> Bool
prop_espejo x = espejo (espejo x) == x

-- La comprobación es
--    λ> quickCheck prop_espejo
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 4.3. Comprobar con QuickCheck que para todo árbol binario
-- x, se tiene que
--    reverse (preorden (espejo x)) = postorden x
-- ---------------------------------------------------------------------

-- eliguivil roscargar enrnarbej pabrabmon albcercid cescarde josdeher
-- manruiber paumacpar natmarmar2 cargonler carmarcar5 belbenzam
-- antbeacar eledejim2 margirmon ignareeva alvfercen marjimcom luimotmar
-- glovizcas monlagare albagucen migibagar felsuacor beagongon1 antmorper3
-- fatfervaz josrodgal7 margarvil14 artmorfer juaorture fraferpoy joscasgom1
-- congomgom criortcar marlobrip marmerzaf natruipin margarflo5 josjimgon2
-- La propiedad es
prop_reverse_preorden_espejo :: Arbol Int -> Bool
prop_reverse_preorden_espejo x = 
  reverse (preorden (espejo x)) == postorden x

-- La comprobación es
--    λ> quickCheck prop_reverse_preorden_espejo
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 4.4. Comprobar con QuickCheck que para todo árbol x,
--    postorden (espejo x) = reverse (preorden x)
-- ---------------------------------------------------------------------

-- eliguivil roscargar enrnarbej pabrabmon albcercid cescarde josdeher
-- manruiber paumacpar natmarmar2 cargonler carmarcar5 belbenzam
-- antbeacar eledejim2 margirmon ignareeva alvfercen marjimcom luimotmar
-- glovizcas monlagare albagucen migibagar felsuacor beagongon1 antmorper3
-- fatfervaz josrodgal7 margarvil14 artmorfer juaorture fraferpoy joscasgom1
-- congomgom criortcar marlobrip marmerzaf natruipin margarflo5 josjimgon2

-- La propiedad es
prop_recorrido :: Arbol Int -> Bool
prop_recorrido x = postorden (espejo x) == reverse (preorden x)

-- La comprobación es
--    λ> quickCheck prop_recorrido
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 5.1. La función take está definida por
--    take :: Int -> [a] -> [a]
--    take 0            = []
--    take (n+1) []     = []
--    take (n+1) (x:xs) = x : take n xs
-- 
-- Definir la función 
--    takeArbol ::  Int -> Arbol a -> Arbol a
-- tal que (takeArbol n t) es el subárbol de t de profundidad n. Por
-- ejemplo,
--    takeArbol 0 (N 9 (N 3 (H 2) (H 4)) (H 7)) == H 9
--    takeArbol 1 (N 9 (N 3 (H 2) (H 4)) (H 7)) == N 9 (H 3) (H 7)
--    takeArbol 2 (N 9 (N 3 (H 2) (H 4)) (H 7)) == N 9 (N 3 (H 2) (H 4)) (H 7)
--    takeArbol 3 (N 9 (N 3 (H 2) (H 4)) (H 7)) == N 9 (N 3 (H 2) (H 4)) (H 7)
-- ---------------------------------------------------------------------
  
-- albcercid cescarde josdeher manruiber marjimcom luimotmar monlagare
-- albagucen migibagar beagongon1 margarvil14 juaorture fraferpoy
-- congomgom criortcar artmorfer antbeacar margarflo5 josjimgon2
takeArbol :: Int -> Arbol a -> Arbol a
takeArbol _ (H x)     = H x
takeArbol 0 (N x y z) = H x
takeArbol n (N x y z) = (N x (takeArbol (n-1) y) (takeArbol (n-1) z))

-- eliguivil roscargar enrnarbej pabrabmon paumacpar cargonler carmarcar5
-- eledejim2 margirmon alvfercen glovizcas felsuacor antmorper3 fatfervaz
-- josrodgal7 joscasgom1 marlobrip marmerzaf natruipin
takeArbol2 :: Int -> Arbol a -> Arbol a
takeArbol2 0 (N a _ _) = H a
takeArbol2 _ (H a)     = H a
takeArbol2 m (N n i d) = N n (takeArbol2 (m-1) i) (takeArbol2 (m-1) d)

-- ---------------------------------------------------------------------
-- Ejercicio 5.2. Comprobar con QuickCheck que la profundidad de 
-- (takeArbol n x) es menor o igual que n, para todo número natural n y
-- todo árbol x. 
-- ---------------------------------------------------------------------

-- eliguivil roscargar enrnarbej pabrabmon albcercid cescarde josdeher
-- manruiber paumacpar cargonler carmarcar5 belbenzam eledejim2 migibagar
-- margirmon alvfercen marjimcom luimotmar glovizcas monlagare albagucen 
-- felsuacor beagongon1 antmorper3 fatfervaz josrodgal7 margarvil14 
-- juaorture fraferpoy joscasgom1 congomgom criortcar artmorfer marlobrip
-- marmerzaf antbeacar natruipin margarflo5 josjimgon2
-- La propiedad es
prop_takeArbol:: Int -> Arbol Int -> Property
prop_takeArbol n x = n >= 0 ==> profundidad (takeArbol n x) <= n

-- La comprobación es
--    λ> quickCheck prop_takeArbol
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 6.1. La función
--    repeat :: a -> [a]
-- está definida de forma que (repeat x) es la lista formada por
-- infinitos elementos x. Por ejemplo,
--    repeat 3  ==  [3,3,3,3,3,3,3,3,3,3,3,3,3,...
-- La definición de repeat es
--    repeat x = xs where xs = x:xs
-- 
-- Definir la función
--    repeatArbol :: a -> Arbol a
-- tal que (repeatArbol x) es es árbol con infinitos nodos x. Por
-- ejemplo, 
--    takeArbol 0 (repeatArbol 3) == H 3
--    takeArbol 1 (repeatArbol 3) == N 3 (H 3) (H 3)
--    takeArbol 2 (repeatArbol 3) == N 3 (N 3 (H 3) (H 3)) (N 3 (H 3) (H 3))
-- ---------------------------------------------------------------------

-- enrnarbej pabrabmon albcercid cescarde josdeher manruiber cargonler 
-- carmarcar5 eledejim2 margirmon alvfercen luimotmar glovizcas monlagare
-- marjimcom albagucen natmarmar2 felsuacor beagongon1 antmorper3 
-- josrodgal7 margarvil14 juaorture fraferpoy joscasgom1 artmorfer
-- congomgom marlobrip marmerzaf antbeacar natruipin margarflo5
repeatArbol :: a -> Arbol a
repeatArbol x = N x rA rA
  where rA = repeatArbol x

-- eliguivil roscargar paumacpar 
repeatArbol2 :: a -> Arbol a
repeatArbol2 x = N x i d where { i = repeatArbol2 x ; d = repeatArbol2 x }

-- Comentario: La definición anterior se puede simplificar.

-- migibagar
repeatArbol3 :: a -> Arbol a
repeatArbol3 x = N x (N x y y) (N x y y)
  where y = repeatArbol3 x

-- ---------------------------------------------------------------------
-- Ejercicio 6.2. La función 
--    replicate :: Int -> a -> [a]
-- está definida por 
--    replicate n = take n . repeat
-- es tal que (replicate n x) es la lista de longitud n cuyos elementos
-- son x. Por ejemplo,
--    replicate 3 5  ==  [5,5,5]
-- 
-- Definir la función 
--    replicateArbol :: Int -> a -> Arbol a
-- tal que (replicate n x) es el árbol de profundidad n cuyos nodos son
-- x. Por ejemplo,
--    replicateArbol 0 5  ==  H 5
--    replicateArbol 1 5  ==  N 5 (H 5) (H 5)
--    replicateArbol 2 5  ==  N 5 (N 5 (H 5) (H 5)) (N 5 (H 5) (H 5))
-- ---------------------------------------------------------------------

-- eliguivil roscargar enrnarbej pabrabmon albcercid josdeher manruiber
-- cargonler paumacpar carmarcar5 eledejim2 margirmon alvfercen
-- luimotmar albagucen natmarmar2 beagongon1 antmorper3 josrodgal7 
-- fatfervaz margarvil14 juaorture fraferpoy joscasgom1 artmorfer
-- congomgom marlobrip antbeacar margarflo5
replicateArbol :: Int -> a -> Arbol a
replicateArbol n = takeArbol n . repeatArbol

-- cescarde
replicateArbol2 :: Int -> a -> Arbol a
replicateArbol2 0 a = H a
replicateArbol2 n a = N a (replicateArbol (n-1) a) (replicateArbol (n-1) a)

-- glovizcas monlagare marjimcom migibagar felsuacor marmerzaf natruipin
replicateArbol3 :: Int -> a -> Arbol a
replicateArbol3 n x = takeArbol n (repeatArbol x)

-- ---------------------------------------------------------------------
-- Ejercicio 6.3. Comprobar con QuickCheck que el número de hojas de 
-- (replicateArbol n x) es 2^n, para todo número natural n
--
-- Nota. Al hacer la comprobación limitar el tamaño de las pruebas como
-- se indica a continuación
--    quickCheckWith (stdArgs {maxSize=7}) prop_replicateArbol
-- ---------------------------------------------------------------------

-- eliguivil roscargar enrnarbej pabrabmon albcercid cescarde josdeher
-- manruiber cargonler carmarcar5 paumacpar eledejim2 margirmon alvfercen 
-- luimotmar glovizcas monlagare marjimcom albagucen migibagar natmarmar2
-- felsuacor beagongon1 antmorper3 josrodgal7 fatfervaz margarvil14
-- juaorture fraferpoy joscasgom1 artmorfer congomgom marmerzaf antbeacar 
-- natruipin margarflo5
 
-- La propiedad es
prop_replicateArbol :: Int -> Int -> Property
prop_replicateArbol n x = n >= 0 ==> nHojas (replicateArbol n x) == 2^n 

-- La comprobación es
--    λ> quickCheck prop_replicateArbol
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 7.1. Definir la función
--    mapArbol :: (a -> a) -> Arbol a -> Arbol a
-- tal que (mapArbol f x) es el árbol obtenido aplicándole a cada elemento
-- de x la función f. Por ejemplo,
--    ghci> mapArbol (*2) (N 9 (N 3 (H 2) (H 4)) (H 7)) 
--    N 18 (N 6 (H 4) (H 8)) (H 14)
-- ---------------------------------------------------------------------

-- eliguivil roscargar enrnarbej pabrabmon albcercid cescarde josdeher
-- manruiber cargonler carmarcar5 paumacpar eledejim2 margirmon alvfercen 
-- luimotmar glovizcas monlagare marjimcom albagucen migibagar natmarmar2
-- felsuacor beagongon1 antmorper3 antbeacar josrodgal7 fatfervaz margarvil14
-- juaorture fraferpoy joscasgom1 artmorfer congomgom marlobrip marmerzaf
-- natruipin margarflo5
mapArbol :: (a -> a) -> Arbol a -> Arbol a
mapArbol f (H a)     = H (f a)
mapArbol f (N n i d) = N (f n) (mapArbol f i) (mapArbol f d)

-- ---------------------------------------------------------------------
-- Ejercicio 7.2. Comprobar con QuickCheck que 
--    (mapArbol (1+)) . espejo = espejo . (mapArbol (1+))
-- ---------------------------------------------------------------------

-- eliguivil roscargar enrnarbej pabrabmon albcercid cescarde josdeher
-- manruiber cargonler carmarcar5 paumacpar eledejim2 margirmon alvfercen 
-- luimotmar glovizcas monlagare marjimcom albagucen migibagar natmarmar2
-- beagongon1 antmorper3 josrodgal7 fatfervaz margarvil14 joscasgom1
-- artmorfer congomgom felsuacor marlobrip marmerzaf natruipin
-- margarflo5 antbeacar 

-- La propiedad es
prop_mapArbol_espejo :: Arbol Int -> Bool
prop_mapArbol_espejo x = 
   ((mapArbol (1+)) . espejo) x == (espejo . mapArbol (1+)) x

-- La comprobación es
--    λ> quickCheck prop_mapArbol_espejo
--    +++ OK, passed 100 tests.

-- juaorture fraferpoy

-- La propiedad es
prop_mapArbol_espejo1 :: Arbol Int -> Bool
prop_mapArbol_espejo1 x =
  mapArbol (1+) (espejo x) == espejo (mapArbol (1+) x)

-- La comprobación es
-- *Main> quickCheck prop_mapArbol_espejo1
-- +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 7.3. Comprobar con QuickCheck que
--    (map (1+)) . preorden = preorden . (mapArbol (1+)) 
-- ---------------------------------------------------------------------

-- eliguivil roscargar enrnarbej pabrabmon albcercid cescarde josdeher 
-- manruiber cargonler paumacpar carmarcar5 eledejim2 margirmon alvfercen 
-- luimotmar glovizcas monlagare marjimcom albagucen migibagar natmarmar2
-- beagongon1 antmorper3 josrodgal7 fatfervaz margarvil14 joscasgom1
-- artmorfer congomgom felsuacor marlobrip marmerzaf natruipin
-- margarflo5 antbeacar 

-- La propiedad es
prop_map_preorden :: Arbol Int -> Bool
prop_map_preorden x = 
   ((map (1+)) . preorden) x == (preorden . (mapArbol (1+))) x 

-- La comprobación es
--    λ> quickCheck prop_map_preorden
--    +++ OK, passed 100 tests.

-- juaorture

-- La propiedad es
prop_map_preorden1 :: Arbol Int -> Bool
prop_map_preorden1 x = map (1+) (preorden x) == preorden (mapArbol (1+) x)

-- La comprobación es
-- *Main> quickCheck prop_map_preorden1
-- +++ OK, passed 100 tests.
-- ---------------------------------------------------------------------
-- Nota. Para comprobar propiedades de árboles con QuickCheck se
-- utilizará el siguiente generador.
-- ---------------------------------------------------------------------

instance Arbitrary a => Arbitrary (Arbol a) where
  arbitrary = sized arbol
    where
      arbol 0       = liftM H arbitrary 
      arbol n | n>0 = oneof [liftM H arbitrary,
                             liftM3 N arbitrary subarbol subarbol]
                      where subarbol = arbol (div n 2)
