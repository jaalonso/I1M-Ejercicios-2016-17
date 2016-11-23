-- I1M 2016-17: Rel_8.hs (9 de noviembre de 2016)
-- Funciones de orden superior y definiciones por plegados.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- Esta relación tiene contiene ejercicios con funciones de orden
-- superior y definiciones por plegado correspondientes al tema 7 
-- http://www.cs.us.es/~jalonso/cursos/i1m-16/temas/tema-7.html

-- ---------------------------------------------------------------------
-- Importación de librerías auxiliares                                --
-- ---------------------------------------------------------------------

import Test.QuickCheck
import Data.List

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    segmentos :: (a -> Bool) -> [a] -> [a]
-- tal que (segmentos p xs) es la lista de los segmentos de xs cuyos
-- elementos verifican la propiedad p. Por ejemplo,
--    segmentos even [1,2,0,4,9,6,4,5,7,2]  ==  [[2,0,4],[6,4],[2]]
--    segmentos odd  [1,2,0,4,9,6,4,5,7,2]  ==  [[1],[9],[5,7]]
-- ---------------------------------------------------------------------

-- enrnarbej roscargar margirmon
segmentos :: (a -> Bool) -> [a] -> [[a]]
segmentos _ [] = []
segmentos p (x:xs)
  | p x = takeWhile p (x:xs) : segmentos p (dropWhile p xs)
  | otherwise = segmentos p (dropWhile (\a -> not (p a)) xs)

-- antmorper3 cargonler ignareeva belbenzam pabrabmon joscasgom1 paumacpar 
-- glovizcas josrodgal7 antbeacar manruiber criortcar eliguivil beagongon1 
-- natmarmar2 josdeher felsuacor carmarcar5 fraferpoy cescarde fatfervaz 
-- albagucen marjimcom albcercid javcancif luimotmar congomgom eledejim2
-- natruipin monlagare margarflo5 marmerzaf antdursan josjimgon2
-- artmorfer margarvil14  juacasnie alvfercen antlopgom2 marlobrip
segmentos2 :: (a -> Bool) -> [a] -> [[a]]
segmentos2 p [] = []
segmentos2 p (x:xs)
  | p x = takeWhile p (x:xs) : segmentos2 p (dropWhile p xs)
  | otherwise = segmentos2 p xs

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Definir, por comprensión, la función
--    relacionadosC :: (a -> a -> Bool) -> [a] -> Bool
-- tal que (relacionadosC r xs) se verifica si para todo par (x,y) de
-- elementos consecutivos de xs se cumple la relación r. Por ejemplo,
--    relacionadosC (<) [2,3,7,9]                ==  True
--    relacionadosC (<) [2,3,1,9]                ==  False
-- ---------------------------------------------------------------------

-- enrnarbej antmorper3 cargonler ignareeva belbenzam margarflo5 juacasnie
-- pabrabmon joscasgom1 antbeacar paumacpar glovizcas josrodgal7 manruiber 
-- beagongon1 criortcar natmarmar2 josdeher felsuacor carmarcar5 fraferpoy
-- cescarde fatfervaz albagucen marjimcom albcercid javcancif migibagar 
-- luimotmar congomgom eledejim2 natruipin monlagare marmerzaf antdursan
-- josjimgon2 artmorfer margirmon margarvil14 alvfercen antlopgom2 marlobrip
relacionadosC :: (a -> a -> Bool) -> [a] -> Bool
relacionadosC r xs = and [r x y | (x,y) <- zip xs (tail xs)]

-- juaorture
relacionadosC1 :: (a -> a -> Bool) -> [a] -> Bool
relacionadosC1 r (x:xs) = and (map (\(x,y) -> r x y) (zip (x:xs) xs))

-- eliguivil roscargar
relacionadosC2 :: (a -> a -> Bool) -> [a] -> Bool
relacionadosC2 r xs =
  all (\(x,y) -> r x y) [(a,b) | (a,b) <- zip xs (tail xs)]

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Definir, por recursión, la función
--    relacionadosR :: (a -> a -> Bool) -> [a] -> Bool
-- tal que (relacionadosR r xs) se verifica si para todo par (x,y) de
-- elementos consecutivos de xs se cumple la relación r. Por ejemplo,
--    relacionadosR (<) [2,3,7,9]                ==  True
--    relacionadosR (<) [2,3,1,9]                ==  False
--    relacionadosR (/=) [1,5,1]                 ==  True
-- ---------------------------------------------------------------------

-- enrnarbej belbenzam margarflo5 josrodgal7 cescarde antbeacar migibagar
-- marmerzaf margirmon
relacionadosR :: (a -> a -> Bool) -> [a] -> Bool
relacionadosR _ []       = False
relacionadosR _ [x]      = True
relacionadosR r (x:y:xs) =  r x y && relacionadosR r (y:xs)

-- Comentario: Falta el caso de lista vacía.

-- juaorture, paumacpar carmarcar5 roscargar albcercid javcancif
-- luimotmar natruipin margarvil14 antlopgom2
relacionadosR2 :: (a -> a -> Bool) -> [a] -> Bool
relacionadosR2 _ []       = False
relacionadosR2 r [x]      = True
relacionadosR2 r (x:y:xs) | r x y     = relacionadosR2 r (y:xs)
                          | otherwise = False

-- Comentario: La definición relacionadosR2 se puede simplificar.

-- antmorper3 cargonler pabrabmon joscasgom1 felsuacor
relacionadosR3 :: (a -> a -> Bool) -> [a] -> Bool
relacionadosR3 r [x] = True
relacionadosR3 r (x:xs) | r x (head xs) = relacionadosR3 r xs
                        | otherwise = False

-- Comentario: La definición anterior se puede simplificar y completar
-- con el caso de la lista vacía.

-- glovizcas antmorper3 manruiber beagongon1 criortcar natmarmar2 josjimgon2
-- fatfervaz albagucen marjimcom congomgom fraferpoy eledejim2 ignareeva 
-- antdursan josdeher juacasnie alvfercen artmorfer marlobrip
relacionadosR4 r [] = False 
relacionadosR4 r [x] = True
relacionadosR4 r (x:y:xs) = r x y && relacionadosR r (y:xs)

-- eliguivil
relacionadosR5 :: Eq a => (a -> a -> Bool) -> [a] -> Bool
relacionadosR5 _ []                     = True
relacionadosR5 r (x:xs) | xs == []      = True
                        | r x (head xs) = relacionadosR r xs
                        | otherwise     = False

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Definir la función
--    agrupa :: Eq a => [[a]] -> [[a]]
-- tal que (agrupa xss) es la lista de las listas obtenidas agrupando
-- los primeros elementos, los segundos, ... Por ejemplo, 
--    agrupa [[1..6],[7..9],[10..20]]  ==  [[1,7,10],[2,8,11],[3,9,12]]
--    agrupa []                        ==  []
-- ---------------------------------------------------------------------

-- enrnarbej antmorper3 margarflo5 pabrabmon joscasgom1 paumacpar
-- glovizcas manruiber beagongon1 natmarmar2 josdeher fatfervaz
-- congomgom marmerzaf josjimgon2 artmorfer juacasnie margirmon josrodgal7
agrupa :: Eq a => [[a]] -> [[a]]
agrupa xss
  | all ( /= []) xss && xss /= [] = map (head) xss : agrupa (map tail xss)
  | otherwise                     = []

-- juaorture
agrupa1 :: Eq a => [[a]] -> [[a]]
agrupa1 []   = []
agrupa1 [[]] = [[]]
agrupa1 xss  = [aux n xss | n <- [0..minimum [length a | a <- xss]-1]]
  where aux :: Int -> [[a]] -> [a]
        aux n xss = [a!!n | a <- xss] 

-- cescarde eliguivil felsuacor carmarcar5 antbeacar roscargar belbenzam 
-- albagucen fraferpoy eledejim2 natruipin cargonler ignareeva
-- margarflo5 antdursan criortcar alvfercen antlopgom2 marlobrip
agrupa2 :: Eq a => [[a]] -> [[a]]
agrupa2 [] = []
agrupa2 xss | elem [] xss = []
            | otherwise = map head xss : agrupa (map tail xss)

-- eliguivil luimotmar marjimcom
agrupa3 :: Eq a => [[a]] -> [[a]]
agrupa3 []  = []
agrupa3 xss = [[xs !! n | xs <- xss]
              | n <- [0..minimum (map length xss) -1]]

-- albcercid
agrupa4 :: Eq a => [[a]] -> [[a]]
agrupa4 [] = []
agrupa4 xss = [map (!!x) xss | x <- [0..minimo xss - 1]]
   where minimo xss = minimum (map (length) xss)

-- margarvil14
agrupa5 :: Eq a => [[a]] -> [[a]]
agrupa5 [] = []
agrupa5 xss | [] `elem` xss = []
            | otherwise     = primeros xss : agrupa (restos xss)
  where primeros = map head
        restos   = map tail

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Comprobar con QuickChek que la longitud de todos los
-- elementos de (agrupa xs) es igual a la longitud de xs.
-- ---------------------------------------------------------------------
-- La propiedad es

-- paumacpar glovizcas margirmon natmarmar2 josdeher felsuacor cescarde
-- roscargar carmarcar5 belbenzam albagucen luimotmar fraferpoy eledejim2
-- congomgom natruipin cargonler margarflo5 ignareeva marmerzaf
-- beagongon1 josjimgon2 artmorfer juacasnie criortcar antlopgom2 josrodgal7
-- marlobrip
prop_agrupa1 :: [[Int]] -> Bool
prop_agrupa1 xss = length (agrupa xss) == length xss

-- Comentario: La propiedad prop_agrupa1 es incorrecta:
--    λ> quickCheck prop_agrupa1
--    *** Failed! Falsifiable (after 5 tests and 2 shrinks): 
--    [[]]
--    (0.08 secs, 33,689,688 bytes)

-- eliguivil albcercid margarvil14 alvfercen
prop_agrupa2 :: [[Int]] -> Bool
prop_agrupa2 xss = all (==(length xss)) (map length (agrupa xss))

-- eliguivil
-- La comprobación es
--    *Main> quickCheck prop_agrupa2
--    +++ OK, passed 100 tests.

-- juaorture antmorper3 manruiber antbeacar fatfervaz
-- La propiedad es
prop_agrupa3 :: [[Int]] -> Bool
prop_agrupa3 [] = True
prop_agrupa3 xss = minimum [length xs | xs <- xss] == length (agrupa xss)

-- Coemntario: La definición prop_agrupa3 es incorrecta.

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. Definir, por recursión, la función 
--    concatR :: [[a]] -> [a]
-- tal que (concatR xss) es la concatenación de las listas de xss. Por
-- ejemplo, 
--    concatR [[1,3],[2,4,6],[1,9]]  ==  [1,3,2,4,6,1,9]
-- ---------------------------------------------------------------------

-- enrnarbej cargonler belbenzam pabrabmon joscasgom1 paumacpar glovizcas
-- roscargar juaorture antmorper3 josrodgal7 margirmon manruiber beagongon1
-- eliguivil natmarmar2 criortcar josdeher felsuacor cescarde carmarcar5
-- antbeacar albagucen albcercid fatfervaz luimotmar fraferpoy natruipin
-- eledejim2 congomgom monlagare margarflo5 ignareeva marjimcom migibagar
-- marmerzaf antdursan josjimgon2 artmorfer juacasnie margarvil14 alvfercen
-- antlopgom2 marlobrip

concatR :: [[a]] -> [a]
concatR []       = []
concatR (xs:xss) = xs ++ concatR xss

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Definir, usando foldr, la función 
--    concatP :: [[a]] -> [a]
-- tal que (concatP xss) es la concatenación de las listas de xss. Por
-- ejemplo, 
--    concatP [[1,3],[2,4,6],[1,9]]  ==  [1,3,2,4,6,1,9]
-- ---------------------------------------------------------------------

-- enrnarbej cargonler belbenzam pabrabmon joscasgom1  roscargar marmerzaf
concatP :: [[a]] -> [a]
concatP xss = foldr (\xs ys -> xs ++ ys) [] xss

-- Comentario: La definición anterior se puede simplificar.

-- paumacpar glovizcas josrodgal7 margirmon manruiber beagongon1 juacasnie
-- josdeher felsuacor cescarde antbeacar albagucen albcercid luimotmar 
-- fraferpoy natruipin congomgom monlagare ignareeva marjimcom josjimgon2 
-- artmorfer margarvil14 antlopgom2 natmarmar2 marlobrip
concatP2 :: [[a]] -> [a]
concatP2 xss = foldr (++) [] xss 

-- Nota de margirmon: xss no es necesario en la funcion anterior

concatP2' :: [[a]] -> [a]
concatP2' = foldr (++) []

prop :: Eq a => [[a]] -> Bool
prop xss = concatP2 xss == concatP2' xss

-- λ> quickCheck prop
-- +++ OK, passed 100 tests.

-- juaorture antmorper3 migibagar
concatP3 :: [[a]] -> [a]
concatP3 = foldr (\x y -> x ++ y ) []

-- Comentario: La definición concatP3 se puede simplificar.

-- eliguivil criortcar carmarcar5 fatfervaz eledejim2 margarflo5
-- antdursan alvfercen
concatP4 :: [[a]] -> [a]
concatP4 = foldr (++) []

-- ---------------------------------------------------------------------
-- Ejercicio 4.3. Comprobar con QuickCheck que la funciones concatR,
-- concatP y concat son equivalentes.
-- ---------------------------------------------------------------------

-- enrnarbej cargonler belbenzam pabrabmon joscasgom1 paumacpar 
-- juaorture cescarde roscargar antbeacar luimotmar fraferpoy monlagare
-- marmerzaf 

-- La propiedad es
prop_concat1 :: [[Int]] -> Bool
prop_concat1 xss = concatR xss == concatP xss

-- Comentario: Falta la equivalencia con concat.

-- antmmorper3 glovizcas josrodgal7 manruiber beagongon1 criortcar
-- josdeher felsuacor carmarcar5 albagucen albcercid fatfervaz eledejim2 
-- natruipin congomgom margarflo5 ignareeva marjimcom migibagar artmorfer
-- antdursan josjimgon2 juacasnie margirmon alvfercen antlopgom2 natmarmar2
prop_concat2 :: [[Int]] -> Bool
prop_concat2 xss =
  concatR xss == concatP xss &&
  concatR xss == concat xss

-- eliguivil
prop_concat3 :: [[Int]] -> Bool
prop_concat3 xss =
  relacionadosR (==) [concatR xss, concatP xss, concat xss]

-- La comprobación es
-- Prelude> quickCheck prop_concat
-- +++ OK, passed 100 tests.

-- margarvil14
prop_concat4 :: [[Int]] -> Bool
prop_concat4 xss = concatR xss == y && concatP xss == y
  where y = concat xss

-- ---------------------------------------------------------------------
-- Ejercicio 4.4. Comprobar con QuickCheck que la longitud de 
-- (concatP xss) es la suma de las longitudes de los elementos de xss.
-- ---------------------------------------------------------------------

-- enrnarbej cargonler pabrabmon paumacpar antmorper3 josrodgal7 beagongon1
-- eliguivil carmarcar5 antbeacar albcercid fatfervaz monlagare marjimcom
-- marmerzaf antdursan josjimgon2 artmorfer margirmon margarvil14
-- criortcar natmarmar2 marlobrip
-- La propiedad es
prop_longConcat :: [[Int]] -> Bool
prop_longConcat xss = length (concatP xss) == sum (map length xss)

-- La comprobación es
-- Prelude> quickCheck prop_longConcat
-- +++ OK, passed 100 tests.

-- juaorture glovizcas manruiber josdeher felsuacor cescarde joscasgom1 
-- roscargar belbenzam albagucen luimotmar fraferpoy natruipin eledejim2 
-- congomgom margarflo5 ignareeva migibagar juacasnie alvfercen antlopgom2
-- La propiedad es
prop_longConcat1 :: [[Int]] -> Bool
prop_longConcat1 xss = length (concat xss) == sum [length xs | xs <- xss]

-- La comprobación es
--    *Main> quickCheck prop_longConcat
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 5.1. Definir, por comprensión, la función
--    filtraAplicaC :: (a -> b) -> (a -> Bool) -> [a] -> [b]
-- tal que (filtraAplicaC f p xs) es la lista obtenida aplicándole a los
-- elementos de xs que cumplen el predicado p la función f. Por ejemplo,
--    filtraAplicaC (4+) (<3) [1..7]  =>  [5,6]
-- ---------------------------------------------------------------------

-- enrnarbej pabrabmon joscasgom1 paumacpar juaorture antmorper3 josrodgal7
-- margirmon manruiber carmarcar5 roscargar belbenzam albcercid luimotmar
-- beagongon1 eliguivil josdeher felsuacor congomgom albagucen fatfervaz
-- fraferpoy natruipin glovizcas cargonler monlagare margarflo5 ignareeva
-- marjimcom migibagar marmerzaf antdursan josjimgon2 artmorfer juacasnie
-- margarvil14 criortcar alvfercen antlopgom2 marlobrip
filtraAplicaC :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filtraAplicaC f p xs = [f x | x <- xs, p x] 

-- cescarde antbeacar eledejim2
filtraAplicaC2 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filtraAplicaC2 f p xs = [f x | x <- filter p xs]

-- ---------------------------------------------------------------------
-- Ejercicio 5.2. Definir, usando map y filter, la función
--    filtraAplicaMF :: (a -> b) -> (a -> Bool) -> [a] -> [b]
-- tal que (filtraAplicaMF f p xs) es la lista obtenida aplicándole a los
-- elementos de xs que cumplen el predicado p la función f. Por ejemplo,
--    filtraAplicaMF (4+) (<3) [1..7]  =>  [5,6]
-- ---------------------------------------------------------------------

-- enrnarbej pabrabmon joscasgom1 paumacpar juaorture antmorper3 josrodgal7
-- margirmon manruiber beagongon1 eliguivil josdeher cescarde carmarcar5
-- roscargar antbeacar congomgom belbenzam albagucen albcercid fatfervaz
-- luimotmar eledejim2 natruipin glovizcas cargonler margarflo5 ignareeva
-- marjimcom migibagar marmerzaf antdursan josjimgon2 artmorfer juacasnie
-- margarvil14 criortcar alvfercen antlopgom2

filtraAplicaMF :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filtraAplicaMF f p xs = map f (filter p xs)

-- ---------------------------------------------------------------------
-- Ejercicio 5.3. Definir, por recursión, la función
--    filtraAplicaR :: (a -> b) -> (a -> Bool) -> [a] -> [b]
-- tal que (filtraAplicaR f p xs) es la lista obtenida aplicándole a los
-- elementos de xs que cumplen el predicado p la función f. Por ejemplo,
--    filtraAplicaR (4+) (<3) [1..7]  =>  [5,6]
-- ---------------------------------------------------------------------

-- enrnarbej pabrabmon joscasgom1 paumacpar juaorture antmorper3 josrodgal7
-- margirmon albcercid luimotmar manruiber beagongon1 eliguivil josdeher 
-- cescarde carmarcar5 roscargar congomgom antbeacar belbenzam albagucen
-- fraferpoy natruipin eledejim2 fatfervaz cargonler margarflo5 ignareeva
-- marjimcom migibagar marmerzaf antdursan josjimgon2 artmorfer juacasnie
-- margarvil14 criortcar alvfercen antlopgom2

filtraAplicaR :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filtraAplicaR _ _ [] = []
filtraAplicaR f p (x:xs) | p x = f x : filtraAplicaR f p xs
                         | otherwise = filtraAplicaR f p xs

-- glovizcas
filtraAplicaR1 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filtraAplicaR1 f p [] = []
filtraAplicaR1 f p (x:xs)
  | p x == True = [f x] ++ filtraAplicaR f p xs
  | otherwise   = filtraAplicaR f p xs

-- Comentario: La definición filtraAplicaR1 se puede simplificar.

-- ---------------------------------------------------------------------
-- Ejercicio 5.4. Definir, por plegado, la función
--    filtraAplicaP :: (a -> b) -> (a -> Bool) -> [a] -> [b]
-- tal que (filtraAplicaP f p xs) es la lista obtenida aplicándole a los
-- elementos de xs que cumplen el predicado p la función f. Por ejemplo,
--    filtraAplicaP (4+) (<3) [1..7]  =>  [5,6]
-- ---------------------------------------------------------------------

-- enrnarbej pabrabmon paumacpar antmorper3 josrodgal7 manruiber beagongon1
-- josdeher cescarde joscasgom1 carmarcar5 roscargar antbeacar belbenzam
-- albagucen congomgom eledejim2 natruipin fatfervaz glovizcas cargonler
-- ignareeva marjimcom marmerzaf margarflo5 antdursan josjimgon2
-- artmorfer alvfercen  juacasnie marlobrip criortcar
filtraAplicaP :: (a -> b) -> (a -> Bool) -> [a] -> [b] 
filtraAplicaP f p xs = foldr ( \y ys -> (f y):ys) [] (filter p xs)

-- eliguivil
filtraAplicaP2 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filtraAplicaP2 f p = foldr (\x xs -> if p x then (f x):xs else xs) []

-- albcercid
filtraAplicaP3 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filtraAplicaP3 f p xs = foldr (\x y -> (f x):y) [] (aux p [] xs)
  where
    aux p v [] = v
    aux p v (x:xs) | p x       = aux p (v++[x]) xs
                   | otherwise = aux p v xs

-- margarvil14
filtraAplicaP4 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filtraAplicaP4 f p = foldr g []
  where g x y | p x       = f x : y
              | otherwise = y

-- ---------------------------------------------------------------------
-- Ejercicio 6.1. Definir, mediante recursión, la función
--    maximumR :: Ord a => [a] -> a
-- tal que (maximumR xs) es el máximo de la lista xs. Por ejemplo,
--    maximumR [3,7,2,5]                  ==  7
--    maximumR ["todo","es","falso"]      ==  "todo"
--    maximumR ["menos","alguna","cosa"]  ==  "menos"
-- 
-- Nota: La función maximumR es equivalente a la predefinida maximum.
-- ---------------------------------------------------------------------

-- enrnarbej
maximumR :: Ord a => [a] -> a
maximumR (x:xs) = maxAux xs x
  where
    maxAux [] x = x
    maxAux (x:xs) n | x > n     = maxAux xs x
                    | otherwise = maxAux xs n

-- enrnarbej pabrabmon margirmon manruiber marjimcom
maximumR2 :: Ord a => [a] -> a
maximumR2 [x]      = x
maximumR2 (x:y:xs) = maximumR2 (maxi x y : xs)

maxi :: Ord a => a -> a -> a
maxi a b | a > b     = a
         | otherwise = b

-- pabrabmon antmorper3 beagongon1 antbeacar alvfercen josrodgal7
maximumR3 :: Ord a => [a] -> a
maximumR3 [x] = x
maximumR3 (x:xs) | x > head xs = maximumR3 (x:tail xs)
                 | otherwise   = maximumR3 xs

-- joscasgom1 manruiber josdeher albagucen albcercid josjimgon2
-- congomgom glovizcas margarflo5 marmerzaf antdursan marlobrip
maximumR4 :: Ord a => [a] -> a
maximumR4 [a] = a
maximumR4 (x:y:xs) | x < y     = maximumR4 (y:xs)
                   | otherwise = maximumR4 (x:xs)

-- juaorture carmarcar5 roscargar cargonler criortcar
maximumR5 :: Ord a => [a] -> a
maximumR5 [x] = x
maximumR5 (x:y:xs) | x > y     = maximumR5 (x:xs)
                   | otherwise = maximumR5 (y:xs)

-- paumacpar fatfervaz
maximumR6 :: Ord a => [a] -> a
maximumR6 [x] = x
maximumR6 (x:xs) | all (<x) xs = x
                 | otherwise   = maximumR6 xs


-- eliguivil
maximumR7 :: Ord a => [a] -> a
maximumR7 [] = error "¿Tiene supremo el conjunto vacío, illo?"
maximumR7 (x:xs) | xs == []  = x
                 | otherwise = max x (maximumR7 xs)

-- cescarde eledejim2 ignareeva
maximumR8 :: Ord a => [a] -> a
maximumR8 (x:xs) | x > maximumR8 xs = x
                 | otherwise        = maximumR8 xs

-- Comentario: La definición maximumR8 es incorrecta. Por ejemplo,
--    λ> maximumR8 [5]
--    *** Exception: Non-exhaustive patterns in function maximumR8

-- margarvil14
maximumR9 :: Ord a => [a] -> a
maximumR9 [x] = x
maximumR9 (x:y:ys) = max x (maximumR (y:ys))

-- ---------------------------------------------------------------------
-- Ejercicio 6.2. La función de plegado foldr1 está definida por 
--    foldr1 :: (a -> a -> a) -> [a] -> a
--    foldr1 _ [x]    =  x
--    foldr1 f (x:xs) =  f x (foldr1 f xs)
-- 
-- Definir, mediante plegado con foldr1, la función
--    maximumP :: Ord a => [a] -> a
-- tal que (maximumR xs) es el máximo de la lista xs. Por ejemplo,
--    maximumP [3,7,2,5]                  ==  7
--    maximumP ["todo","es","falso"]      ==  "todo"
--    maximumP ["menos","alguna","cosa"]  ==  "menos"
-- 
-- Nota: La función maximumP es equivalente a la predefinida maximum.
-- ---------------------------------------------------------------------

-- enrnarbej pabrabmon margirmon manruiber cargonler
maximumP :: Ord a => [a] -> a
maximumP xs = foldr1 maxi xs

-- antmorper3 eliguivil josdeher cescarde joscasgom1 roscargar albagucen
-- albcercid eledejim2 congomgom paumacpar antbeacar margarflo5
-- fatfervaz ignareeva  marjimcom beagongon1 antdursan josjimgon2
-- margarvil14 criortcar josrodgal7 
maximumP2 :: Ord a => [a] -> a
maximumP2 = foldr1 max
