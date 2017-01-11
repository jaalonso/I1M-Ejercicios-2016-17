-- I1M 2016-17: Relación 12 (15 de diciembre de 2016)
-- Aplicaciones de la programación funcional con listas infinitas.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- En esta relación se estudia distintas aplicaciones de la programación
-- funcional que usan listas infinitas
-- + enumeración de los números enteros,
-- + el problema de la bicicleta de Turing y
-- + la sucesión de Golomb,

import Test.QuickCheck

-- ---------------------------------------------------------------------
-- § Enumeración de los números enteros                               --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Los números enteros se pueden ordenar como sigue 
--    0, -1, 1, -2, 2, -3, 3, -4, 4, -5, 5, -6, 6, -7, 7, ...
-- Definir, por comprensión, la constante
--    enteros :: [Int]
-- tal que enteros es la lista de los enteros con la ordenación
-- anterior. Por ejemplo,
--    take 10 enteros  ==  [0,-1,1,-2,2,-3,3,-4,4,-5]
-- ---------------------------------------------------------------------

-- cescarde manruiber margarflo5 cargonler eledejim2 fatfervaz pabrabmon
-- migibagar natmarmar2 artmorfer josdeher monlagare alvfercen glovizcas
-- antmorper3 margirmon marjimcom margarvil14 belbenzam marlobrip
-- javcancif criortcar joscasgom1 ignareeva antbeacar
-- beagongon1 felsuacor carmarcar5 antdursan natruipin
enteros :: [Int]
enteros = 0 : concat [[-x,x] | x <- [1..]]

-- margarvil14
enteros2 :: [Int]
enteros2 = iterate siguiente 0
  where siguiente x | x >= 0    = -x -1
                    | otherwise = -x

-- fraferpoy cescarde pabrabmon
enteros3 :: [Int]
enteros3 = 0 : auxEnteros [1]
  where auxEnteros [x] = (-x) : x : auxEnteros [x+1] 

-- Comentario: La definición anterior se puede simplificar.

-- albcercid
enteros4 :: [Int]
enteros4 = 0 : [biyeccion x | x <- [1..]]
  where biyeccion x | odd x     = (-x) `div` 2
                    | otherwise = x `div` 2

-- paumacpar roscargar
enteros5 :: [Int]
enteros5 = doblar [0..]

doblar :: [Int] -> [Int]
doblar [] = []
doblar (x:xs) | x == 0    = 0 : doblar xs
              | otherwise = -x : x : doblar xs

-- enrnarbej
enteros6 :: [Int]
enteros6 = intercala positivos negativos
  where
    intercala (x:xs) (y:ys) = x:y:intercala xs ys
    positivos = 0   :[x+1 | x <- positivos]
    negativos = (-1):[x-1 | x <- negativos]

-- eliguivil
enteros7 :: [Int]
enteros7 = [if odd n then n `div` 2
                     else -(n `div` 2)
           | n <- [1..]]

enteros8 :: [Int]
enteros8 = 0 : concat ([par a | a <- [1..]])
  where par x = [-x,x]

-- juaorture
enteros9 :: [Int]
enteros9 =
  0 : concat [deParALista a | a <- zip [-a | a <- [1..]] [1..]]
  where deParALista :: (a,a) -> [a]
        deParALista (a,b) = [a,b]

-- Equivalencia
prop_equiv_enteros :: Int -> Property
prop_equiv_enteros k =
  k >= 0 ==>
  all (==x) [ enteros2 !! k
            , enteros3 !! k
            , enteros4 !! k
            , enteros5 !! k
            , enteros6 !! k
            , enteros7 !! k
            , enteros8 !! k
            , enteros9 !! k]
  where x = enteros !! k

-- Comprobación
--    λ> quickCheck prop_equiv_enteros
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Definir la función
--    posicion :: Int -> Int
-- tal que (posicion x) es la posición del entero x en la ordenación
-- anterior. Por ejemplo,
--    posicion 2  ==  4
-- ---------------------------------------------------------------------

-- margarvil14
posicion :: Int -> Int
posicion x = length (takeWhile (/=x) enteros)

-- paumacpar 
posicion2 :: Int -> Int
posicion2 y = posicionAux 0 y enteros
  where posicionAux n 0 (x:xs) = 0
        posicionAux n y (x:xs) | y == x    = n 
                               | otherwise = posicionAux (n+1) y xs 

-- fraferpoy fatfervaz monlagare glovizcas margarvil14 migibagar
-- felsuacor natruipin
posicion3 :: Int -> Int
posicion3 x = head [y | (x',y) <- zip enteros [0..]
                      , x == x']

-- albcercid enrnarbej manruiber cargonler margarflo5 eledejim2
-- pabrabmon roscargar natmarmar2 artmorfer josdeher alvfercen eliguivil
-- antmorper3 margirmon marjimcom belbenzam marlobrip criortcar
-- joscasgom1 ignareeva beagongon1 carmarcar5 antdursan antbeacar 
posicion4 :: Int -> Int
posicion4 x | x >= 0    = 2*x
            | otherwise = -2*x - 1

-- cescarde
posicion5 :: Int -> Int
posicion5 x | x >= 0 = 2*x+1
            | x <  0 = 2*(-x)

-- Comentario: La definición 5 es incorrecta, porque empieza a contar
-- las posiciones en 1 en lugar de empezar en 0.

-- margarvil14
posicion6 :: Int -> Int
posicion6 x = aux enteros 0
  where aux (y:ys) n | x == y    = n
                     | otherwise = aux ys (n+1)

-- javcancif
posicion7 :: Int -> Int
posicion7 x | x >= 0    = 2*x
            | otherwise = 2*(abs x) - 1

-- luimotmar
posicion8 :: Int -> Int
posicion8 x | x > 0 = length (takeWhile (/= (-x)) enteros8)
            | x < 0 = length (takeWhile (/= (-x)) enteros8) + 2
            | otherwise = 1

-- Comentario: La definición posicion8 es incorrecta. Por ejemplo,
--    λ> [posicion8 x | x <- take 9 enteros] 
--    [1,4,1,6,3,8,5,10,7]

-- juaorture
posicion9 :: Int -> Int
posicion9 x = head [a | a <- [0..]
                      , enteros!!a == x]

-- Comentario: La definición posicion9 se puede mejorar.

prop_equiv_posicion :: Int -> Bool
prop_equiv_posicion k =
  all (==x) [ posicion2 k
            , posicion3 k
            , posicion4 k
            -- , posicion5 k
            , posicion6 k
            , posicion7 k
            -- , posicion8 k
            , posicion9 k]
  where x = posicion k

-- Comparación de eficiencia
--    λ> posicion 30000
--    60000
--    (0.01 secs, 0 bytes)
--    λ> posicion2 30000
--    60000
--    (0.10 secs, 27,790,192 bytes)
--    λ> posicion3 30000
--    60000
--    (0.02 secs, 0 bytes)
--    λ> posicion4 30000
--    60000
--    (0.00 secs, 0 bytes)
--    λ> posicion6 30000
--    60000
--    (0.03 secs, 15,680,432 bytes)
--    λ> posicion7 30000
--    60000
--    (0.00 secs, 0 bytes)
--    λ> posicion9 30000
--    60000
--    (3.90 secs, 29,209,024 bytes)

-- ---------------------------------------------------------------------
-- § El problema de la bicicleta de Turing                            --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Cuentan que Alan Turing tenía una bicicleta vieja,
-- que tenía una cadena con un eslabón débil y además uno de los radios
-- de la rueda estaba doblado. Cuando el radio doblado coincidía con el
-- eslabón débil, entonces la cadena se rompía.   
--
-- La bicicleta se identifica por los parámetros (i,d,n) donde 
-- - i es el número del eslabón que coincide con el radio doblado al
--   empezar a andar,
-- - d es el número de eslabones que se desplaza la cadena en cada
--   vuelta de la rueda y  
-- - n es el número de eslabones de la cadena (el número n es el débil).
-- Si i=2 y d=7 y n=25, entonces la lista con el número de eslabón que 
-- toca el radio doblado en cada vuelta es 
--    [2,9,16,23,5,12,19,1,8,15,22,4,11,18,0,7,14,21,3,10,17,24,6,...
-- Con lo que la cadena se rompe en la vuelta número 14.
-- 
-- Definir la función
--    eslabones :: Int -> Int -> Int -> [Int]
-- tal que (eslabones i d n) es la lista con los números de eslabones 
-- que tocan el radio doblado en cada vuelta en una bicicleta de tipo 
-- (i,d,n). Por ejemplo, 
--    take 10 (eslabones 2 7 25)  ==  [2,9,16,23,5,12,19,1,8,15]
-- ---------------------------------------------------------------------

-- albcercid cargonler margarflo5 artmorfer fraferpoy alvfercen
-- antmorper3 migibagar roscargar margarvil14 fatfervaz joscasgom1
-- ignareeva beagongon1 carmarcar5 antbeacar natruipin
eslabones :: Int -> Int -> Int -> [Int]
eslabones i d n = [mod (d*a + i) n | a <- [0..]]

-- paumacpar pabrabmon margirmon
eslabones2 :: Int -> Int -> Int -> [Int]
eslabones2 i d n = [mod k n | k <- iterate (+d) i]

-- cescarde eledejim2 belbenzam felsuacor
eslabones3 :: Int -> Int -> Int -> [Int]
eslabones3 i d n | i >= n = eslabones3 (i-n) d n
                 | i <  n = i : eslabones3 (i+d) d n

-- enrnarbej manruiber
eslabones4 :: Int -> Int -> Int -> [Int]
eslabones4 i d n = i : [(x + d) `mod` n | x <- eslabones i d n]

-- josdeher marlobrip criortcar
eslabones5 :: Int -> Int -> Int -> [Int]
eslabones5 i d n | i+d <  n = i : eslabones5  (i+d)    d n
                 | i+d >= n = i : eslabones5 ((i+d)-n) d n

-- eliguivil
eslabones6 :: Int -> Int -> Int -> [Int]
eslabones6 i d n = i : eslabones ((i+d) `mod` n) d n

-- glovizcas
eslabones7 :: Int -> Int -> Int -> [Int]
eslabones7 i d n  | i + d < n = i : m
                  | otherwise = (i `mod` n) : m
  where m = eslabones (i+d) d n

-- marjimcom
eslabones8 :: Int -> Int -> Int -> [Int]
eslabones8 i d n = iterate f i
  where f x = (x + d) `mod` n 

-- luimotmar
eslabones9 :: Int -> Int -> Int -> [Int]
eslabones9 i d n | i < n     = i : eslabones9 (i + d) d n
                 | i == n    = eslabones9 0 d n
                 | otherwise = eslabones9 (rem i n) d n 

-- juaorture
eslabones10 :: Int -> Int -> Int -> [Int]
eslabones10 i d n = i `mod` n : eslabones (i+d) d n 

-- antdursan
eslabones11 :: Int -> Int -> Int -> [Int]
eslabones11 i d n = i : auxEslabones i d n

auxEslabones :: Int -> Int -> Int -> [Int]
auxEslabones i d n | i+d >= n  = (i+d-n) : auxEslabones (i+d-n) d n
                   | otherwise = (i+d) : auxEslabones (i+d) d n

-- Equivalencia
prop_equiv_eslabones :: Int -> Int -> Int -> Int -> Bool
prop_equiv_eslabones i d n k =
  all (==x) [ (f i1 d1 n1) !! k1
            | f <- [ eslabones2
                   , eslabones3
                   , eslabones4
                   , eslabones5
                   , eslabones6
                   , eslabones7
                   , eslabones8
                   , eslabones9
                   , eslabones10
                   , eslabones11
                   ]]
  where i1 = 1 + abs i
        d1 = 1 + i1 + abs d
        n1 = 1 + d1 + abs n
        k1 = abs k
        x  = (eslabones i1 d1 n1) !! k1
                        
-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Definir la función
--    numeroVueltas :: Int -> Int -> Int -> Int 
-- tal que (numeroVueltas i d n) es el número de vueltas que pasarán 
-- hasta que la cadena se rompa en una bicicleta de tipo (i,d,n). Por 
-- ejemplo,
--    numeroVueltas 2 7 25  ==  14
-- ---------------------------------------------------------------------

-- paumacpar cescarde enrnarbej cargonler manruiber eledejim2 pabrabmon 
-- margarflo5 artmorfer glovizcas margirmon marjimcom belbenzam margarvil14
-- marlobrip fatfervaz luimotmar ignareeva beagongon1 felsuacor
-- carmarcar5 antdursan  antbeacar natruipin
numeroVueltas :: Int -> Int -> Int -> Int
numeroVueltas i d n =
  length (takeWhile (/=0) (eslabones i d n)) 

-- albcercid fraferpoy alvfercen antmorper3 roscargar joscasgom1
numeroVueltas2 :: Int -> Int -> Int -> Int
numeroVueltas2 i d n =
  head [a | a <- [0..], mod (d*a + i) n == 0]

-- josdeher
numeroVueltas3 :: Int -> Int -> Int -> Int
numeroVueltas3 i d n =
  head [y | (x,y) <- zip (eslabones5 i d n) [0..], x == 0 ]

-- eliguivil migibagar
numeroVueltas4 :: Int -> Int -> Int -> Int
numeroVueltas4 i d n =
  length . takeWhile (/=0) $ eslabones i d n

-- juaorture
numeroVueltas5 :: Int -> Int -> Int -> Int
numeroVueltas5 i d n =
  head [a | a <- [0..] , (eslabones i d n)!!a == 0]

-- Equivalencia
prop_equiv_numeroVueltas :: Int -> Int -> Int -> Bool
prop_equiv_numeroVueltas i d n =
  all (== x) [ f i d n
             | f <- [ numeroVueltas2
                    , numeroVueltas3
                    , numeroVueltas4
                    , numeroVueltas5
                   ]]
  where x = numeroVueltas i d n

-- λ> prop_equiv_numeroVueltas 2 7 25
-- True

-- ---------------------------------------------------------------------
-- § La sucesión de Golomb                                            --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. [Basado en el problema 341 del proyecto Euler]. La
-- sucesión de Golomb {G(n)} es una sucesión auto descriptiva: es la
-- única sucesión no decreciente de números naturales tal que el número
-- n aparece G(n) veces en la sucesión. Los valores de G(n) para los
-- primeros números son los siguientes:
--    n       1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 ...
--    G(n)    1 2 2 3 3 4 4 4 5  5  5  6  6  6  6 ...
-- En los apartados de este ejercicio se definirá una función para
-- calcular los términos de la sucesión de Golomb. 
-- 
-- Definir la función
--    golomb :: Int -> Int
-- tal que (golomb n) es el n-ésimo término de la sucesión de Golomb. 
-- Por ejemplo,
--    golomb 5  ==  3
--    golomb 9  ==  5
-- Indicación: Se puede usar la función sucGolomb del apartado 2.
-- ---------------------------------------------------------------------

-- albcercid margarflo5 monlagare alvfercen roscargar marjimcom
-- eliguivil margarvil14 juaorture antbeacar 
golomb :: Int -> Int
golomb n = sucGolomb !! (n-1)

-- albcercid cescarde cargonler manruiber artmorfer josdeher paumacpar
-- eliguivil 
golomb2 1 = 1
golomb2 x = 1 + golomb2 (x - golomb2 (golomb2 (x-1)))

-- enrnarbej antmorper3 glovizcas belbenzam joscasgom1 beagongon1 antdursan
golomb3 :: Int -> Int
golomb3 1 = 1
golomb3 2 = 2
golomb3 n = sucGolomb3 !! (n-1)

--pabrabmon
golomb4 :: Int -> Int
golomb4 n = head (drop (n-1) sucGolomb)

-- margirmon
golombLista :: [(Int,Int)]
golombLista =
  [(1,1),(2,2),(2,3)]
  ++ zip (concat[replicate a b | (a,b) <- (drop 2 golombLista)]) [4..]

golomb5 :: Int -> Int
golomb5 x = head [fst (a,b) | (a,b) <- golombLista,
                  b == x]

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Definir la función
--    sucGolomb :: [Int]
-- tal que sucGolomb es la lista de los términos de la sucesión de
-- Golomb. Por ejemplo,
--    take 15 sucGolomb  ==  [1,2,2,3,3,4,4,4,5,5,5,6,6,6,6]
-- Indicación: Se puede usar la función subSucGolomb del apartado 3.
-- ---------------------------------------------------------------------

-- enrnarbej josdeher antmorper3 glovizcas margarvil14 joscasgom1
-- beagongon1 antdursan antbeacar
sucGolomb :: [Int]
sucGolomb = subSucGolomb 1

-- albcercid pabrabmon margarflo5 roscargar marjimcom eliguivil
sucGolomb2 :: [Int]
sucGolomb2 = 1:2:serie
serie = concat [replicate a b | (a,b) <- zip (1:serie) [2..]]

-- cescarde cargonler manruiber artmorfer belbenzam paumacpar natruipin
sucGolomb3 :: [Int]
sucGolomb3 = [golomb n | n <- [1..]]

-- monlagare alvfercen
sucGolomb4 :: [Int]
sucGolomb4 = 1 : xs
  where xs = concat [replicate a b | (a,b) <- zip [2..] [2..]]

-- comentario de glovizcas: sucGolomb4 es incorrecta, ya que 3 se repite
-- dos veces (no tres), 4 se repite tres veces, 5 tres veces, 6
-- cuatro veces... 

-- margirmon
sucGolomb5 :: [Int]
sucGolomb5 = map fst golombLista

-- juaorture
sucGolomb6 :: [Int]
sucGolomb6 = 1 : 2 : 2 : concat [replicate (golomb a) a | a <- [3..]] 

-- ---------------------------------------------------------------------
-- Ejercicio 3.3. Definir la función
--    subSucGolomb :: Int -> [Int]
-- tal que (subSucGolomb x) es la lista de los términos de la sucesión
-- de Golomb a partir de la primera ocurrencia de x. Por ejemplo,
--    take 10 (subSucGolomb 4)  ==  [4,4,4,5,5,5,6,6,6,6]
-- Indicación: Se puede usar la función golomb del apartado 1.
-- ---------------------------------------------------------------------

subSucGolomb :: Int -> [Int]
subSucGolomb 1 = [1] ++ subSucGolomb 2
subSucGolomb 2 = [2,2] ++ subSucGolomb 3
subSucGolomb x = (replicate (golomb x) x) ++ subSucGolomb (x+1) 

-- albcercid cescarde cargonler manruiber pabrabmon margarflo5 alvfercen
-- margirmon roscargar marjimcom eliguivil paumacpar 
subSucGolomb2 :: Int -> [Int]
subSucGolomb2 x = dropWhile (/= x) sucGolomb

-- enrnarbej josdeher antmorper3 joscasgom1 beagongon1 antdursan antbeacar
subSucGolomb3 :: Int -> [Int]
subSucGolomb3 n = concat [replicate (golomb3 x) x | x <- [n..]]

-- artmorfer glovizcas belbenzam natruipin
subSucGolomb4 :: Int -> [Int]
subSucGolomb4 x = filter (>= x) sucGolomb2

-- juaorture
subSucGolomb5 :: Int -> [Int]
subSucGolomb5 n = [golomb a | a <- [n..], golomb a >= n]
