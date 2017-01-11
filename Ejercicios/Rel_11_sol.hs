-- I1M 2016-17: Rel_11.hs (2 de diciembre de 2016)
-- Evaluación perezosa y listas infinitas.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- En esta relación se presentan ejercicios con listas infinitas y
-- evaluación perezosa. Estos ejercicios corresponden al tema 10 cuyas
-- transparencias se encuentran en  
--    http://www.cs.us.es/~jalonso/cursos/i1m-16/temas/tema-10.html

-- ---------------------------------------------------------------------
-- Importación de librerías auxiliares                                  
-- ---------------------------------------------------------------------

import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Definir, por recursión, la función 
--    repite :: a -> [a]
-- tal que (repite x) es la lista infinita cuyos elementos son x. Por
-- ejemplo, 
--    repite 5           ==  [5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,...
--    take 3 (repite 5)  ==  [5,5,5]
-- 
-- Nota: La función repite es equivalente a la función repeat definida
-- en el preludio de Haskell.
-- ---------------------------------------------------------------------

-- josrodgal7 enrnarbej fatfervaz paumacpar monlagare marjimcom congomgom
-- albcercid josdeher glovizcas eledejim2 joscasgom1 migibagar eliguivil
-- beagongon1 carmarcar5 belbenzam roscargar felsuacor cargonler pabrabmon
-- alvfercen marlobrip antmorper3 margarvil14 margirmon juaorture antbeacar
-- natruipin javcancif fraferpoy marmerzaf margarflo5 manruiber cescarde 
-- ignareeva artmorfer criortcar luimotmar albagucen
repite :: a -> [a]
repite x = x : repite x

-- Comentario: La definición anterior se puede mejorar.

-- antlopgom2
repite2 :: a -> [a]
repite2 x = [x] ++ repite2 x

-- Comentario: La definición anterior se puede mejorar.

-- La mejorada en clase (cescarde)
repite3 :: a -> [a]
repite3 x = xs
  where xs = x : xs

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Definir, por comprensión, la función 
--    repiteC :: a -> [a]
-- tal que (repiteC x) es la lista infinita cuyos elementos son x. Por
-- ejemplo, 
--    repiteC 5           ==  [5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,...
--    take 3 (repiteC 5)  ==  [5,5,5]
--
-- Nota: La función repiteC es equivalente a la función repeat definida
-- en el preludio de Haskell.
-- ---------------------------------------------------------------------

-- enrnarbej josrodgal7 fatfervaz paumacpar marjimcom congomgom albcercid
-- josdeher glovizcas eledejim2 joscasgom1 antlopgom2 eliguivil beagongon1
-- carmarcar5 belbenzam roscargar felsuacor cargonler pabrabmon alvfercen
-- marlobrip antmorper3 margirmon juaorture antbeacar natruipin
-- javcancif fraferpoy marmerzaf manruiber artmorfer luimotmar albagucen
repiteC :: a -> [a]
repiteC x = [x | n <- [1..]]

-- Comentario: La definición anterior se puede simplificar.

-- monlagare migibagar margarvil14 margarflo5
repiteC1 :: a -> [a]
repiteC1 x = [x | x <- (repiteC1 x)]

-- Comentario: La definición anterior se puede simplificar.

-- cescarde ignareeva criortcar
repiteC2 :: a -> [a]
repiteC2 x = [x | _ <- repite x]

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Definir, por recursión, la función 
--    repiteFinitaR :: Int-> a -> [a]
-- tal que (repiteFinitaR n x) es la lista con n elementos iguales a
-- x. Por ejemplo, 
--    repiteFinitaR 3 5  ==  [5,5,5]
--
-- Nota: La función repiteFinitaR es equivalente a la función replicate
-- definida en el preludio de Haskell.
-- ---------------------------------------------------------------------

-- josrodgal7 enrnarbej fatfervaz paumacpar monlagare marjimcom albcercid
-- joscasgom1 eliguivil beagongon1 carmarcar5 belbenzam roscargar
-- felsuacor antbeacar pabrabmon marlobrip antmorper3 margarvil14
-- eledejim2 cargonler margirmon juaorture fraferpoy marmerzaf natruipin
-- manruiber ignareeva artmorfer luimotmar
repiteFinitaR :: Int -> a -> [a]
repiteFinitaR 0 x = []
repiteFinitaR n x = x : repiteFinitaR (n-1) x

-- Comentario: La definición anterior se puede extender.

-- Comentario (juaorture): se puede sustituir la x por una variable
-- anónima en la primera ecuación. 

-- josdeher glovizcas congomgom migibagar alvfercen javcancif margarflo5 
-- cescarde criortcar albagucen
repiteFinitaR2 :: Int -> a -> [a]
repiteFinitaR2 n x | n <= 0    = []
                   | otherwise = x : repiteFinitaR (n-1) x

-- antlopgom2
repiteFinitaR3 :: Int -> a -> [a]
repiteFinitaR3 0 x = []
repiteFinitaR3 n x = [x] ++ repiteFinitaR3 (n-1) x

-- Comentario: La definición anterior se puede simplificar.

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Definir, por comprensión, la función 
--    repiteFinitaC :: Int-> a -> [a]
-- tal que (repiteFinitaC n x) es la lista con n elementos iguales a
-- x. Por ejemplo, 
--    repiteFinitaC 3 5  ==  [5,5,5]
--
-- Nota: La función repiteFinitaC es equivalente a la función replicate
-- definida en el preludio de Haskell.
-- ---------------------------------------------------------------------

-- enrnarbej josrodgal7 fatfervaz paumacpar monlagare marjimcom congomgom
-- albcercid josdeher glovizcas joscasgom1 eliguivil antlopgom2 beagongon1
-- carmarcar5 belbenzam roscargar felsuacor cargonler marlobrip antmorper3
-- margirmon juaorture antbeacar natruipin javcancif fraferpoy manruiber
-- artmorfer luimotmar albagucen
repiteFinitaC :: Int -> a -> [a]
repiteFinitaC n x = [x | k <-[1..n]]

-- Comentario: La definición anterior se puede simplificar.

-- migibagar pabrabmon alvfercen marmerzaf
repiteFinitaC2 :: Int -> a -> [a]
repiteFinitaC2 n x | n > 0      =  [x | y <- [1..n]]
                   | otherwise  =  []

-- Comentario: La definición anterior se puede simplificar.

-- margarvil14 eledejim2 margarflo5 cescarde ignareeva criortcar
repiteFinitaC3 :: Int -> a -> [a]
repiteFinitaC3 n x = [x | _ <- [1..n]]

-- cescarde
repiteFinitaC4 :: Int -> a -> [a]
repiteFinitaC4 n x = [x | _ <- replicate n x]

-- Comentario: No se debe de usar la función que se está definiendo.

-- ---------------------------------------------------------------------
-- Ejercicio 2.3. Definir, usando repite, la función 
--    repiteFinita :: Int-> a -> [a]
-- tal que (repiteFinita n x) es la lista con n elementos iguales a
-- x. Por ejemplo, 
--    repiteFinita 3 5  ==  [5,5,5]
--
-- Nota: La función repiteFinita es equivalente a la función replicate
-- definida en el preludio de Haskell.
-- ---------------------------------------------------------------------

-- enrnarbej josrodgal7 fatfervaz paumacpar marjimcom congomgom albcercid
-- josdeher glovizcas joscasgom1 eliguivil beagongon1 carmarcar5 roscargar 
-- pabrabmon alvfercen marlobrip antmorper3 cargonler margirmon antbeacar
-- natruipin margarflo5 manruiber criortcar luimotmar
repiteFinita :: Int -> a -> [a] 
repiteFinita n = take n . repite

-- monlagare antlopgom2 belbenzam felsuacor margarvil14 eledejim2 juaorture
-- javcancif fraferpoy marmerzaf cescarde ignareeva artmorfer albagucen
repiteFinita2 :: Int -> a -> [a] 
repiteFinita2 n x = take n (repite x)

-- migibagar pabrabmon
repiteFinita3 :: Int -> a -> [a]
repiteFinita3 n x | n > 0      =   take n (repite x)
                  | otherwise  =  []

-- Comentario: La definición anterior se puede simplificar.

-- cescarde
repiteFinita4 :: Int -> a -> [a]
repiteFinita4 n x = take n [x | _ <- repite x]

-- ---------------------------------------------------------------------
-- Ejercicio 2.4. Comprobar con QuickCheck que las funciones
-- repiteFinitaR, repiteFinitaC y repiteFinita son equivalentes a
-- replicate. 
--
-- Nota. Al hacer la comprobación limitar el tamaño de las pruebas como
-- se indica a continuación
--    quickCheckWith (stdArgs {maxSize=7}) prop_repiteFinitaEquiv
-- ---------------------------------------------------------------------

-- enrnarbej josrodgal7 paumacpar monlagare marjimcom albcercid glovizcas
-- congomgom joscasgom1 eliguivil antlopgom2 beagongon1 carmarcar5 
-- felsuacor cargonler antmorper3 belbenzam roscargar margirmon antbeacar
-- natruipin javcancif margarflo5 luimotmar albagucen 
-- La propiedad es
prop_repiteFinitaEquiv :: (Positive Int) -> Int -> Bool
prop_repiteFinitaEquiv (Positive n) x =
  all (== replicate n x) [ repiteFinitaR n x
                         , repiteFinitaC n x
                         , repiteFinita n x]

-- Comentario: La definición anterior se puede generalizar.


-- migibagar pabrabmon alvfercen marlobrip margarvil14 eledejim2
-- marmerzaf fraferpoy cescarde artmorfer
prop_repiteFinitaEquiv2 :: Int -> Int -> Bool
prop_repiteFinitaEquiv2 n x =
     (repiteFinitaR n x  ==  repiteFinitaC n x)
  && (repiteFinitaC n x  ==  repiteFinita n x)
  && (repiteFinita n x   ==  replicate n x)

-- Comentario: La definición anterior se puede mejorar.

-- josdeher manruiber
prop_repiteFinitaEquiv3 :: Int -> Int -> Bool
prop_repiteFinitaEquiv3 n x =
     repiteFinitaR2 n x == repiteFinitaC n x
  && repiteFinitaC  n x == repiteFinita n x

-- Comentario: La definición anterior está incompleta.

-- La comprobación es
--    *Main>  quickCheckWith (stdArgs {maxSize=7}) prop_repiteFinitaEquiv
--    +++ OK, passed 100 tests.

-- juaorture ignareeva criortcar

-- La propiedad es
prop_repiteFinitaEquiv4 :: Int -> Int -> Bool
prop_repiteFinitaEquiv4 n x =
     replicate n x     == r
  && repiteFinitaC n x == r
  && repiteFinita n x  == r
  where r = repiteFinitaR n x 

-- La comprobación es
-- *Main> quickCheck prop_repiteFinitaEquiv4
-- +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 2.5. Comprobar con QuickCheck que la longitud de
-- (repiteFinita n x) es n, si n es positivo y 0 si no lo es.
--
-- Nota. Al hacer la comprobación limitar el tamaño de las pruebas como
-- se indica a continuación
--    quickCheckWith (stdArgs {maxSize=30}) prop_repiteFinitaLongitud
-- ---------------------------------------------------------------------

-- enrnarbej josrodgal7 paumacpar fatfervaz monlagare marjimcom albcercid 
-- glovizcas congomgom joscasgom1 eliguivil migibagar beagongon1 carmarcar5 
-- belbenzam roscargar felsuacor cargonler pabrabmon antlopgom2 alvfercen
-- marlobrip antmorper3 margarvil14 eledejim2 margirmon antbeacar natruipin
-- javcancif marmerzaf josdeher margarflo5 manruiber cescarde artmorfer
-- luimotmar albagucen 
 
-- La propiedad es
prop_repiteFinitaLongitud :: Int -> Int -> Bool
prop_repiteFinitaLongitud n x
  | n > 0     = length (repiteFinita n x) == n
  | otherwise = length (repiteFinita n x) == 0

-- Comentario (juaorture) se puede definir "length (repiteFinita n x)"
-- como variable local para que solo la calcule una vez. 

-- La comprobación es
--   *Main> quickCheckWith (stdArgs {maxSize=30}) prop_repiteFinitaLongitud
--   +++ OK, passed 100 tests.

-- juaorture ignareeva criortcar

-- La propiedad es
prop_repiteFinitaLongitud2 :: Int -> Int -> Bool
prop_repiteFinitaLongitud2 n x | n > 0     = l == n
                               | otherwise = l == 0
  where l = length (repiteFinita n x)

-- La comprobación es
-- *Main> quickCheckWith (stdArgs {maxSize=30}) prop_repiteFinitaLongitud
-- +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 2.6. Comprobar con QuickCheck que todos los elementos de 
-- (repiteFinita n x) son iguales a x.
-- ---------------------------------------------------------------------

-- enrnarbej josordgal7 paumacpar fatfervaz marjimcom monlagare albcercid 
-- glovizcas congomgom joscasgom1 eliguivil migibagar beagongon1 carmarcar5 
-- belbenzam roscargar felsuacor cargonler pabrabmon antlopgom2 alvfercen
-- marlobrip antmorper3 margarvil14 eledejim2 margirmon antbeacar natruipin
-- javcancif marmerzaf fraferpoy josdeher margarflo5 manruiber cescarde
-- artmorfer criortcar luimotmar

-- La propiedad es
prop_repiteFinitaIguales :: Int -> Int -> Bool
prop_repiteFinitaIguales n x = all ( == x) $ repiteFinita n x

-- La comprobación es
--    *Main> quickCheck prop_repiteFinitaIguales
--    +++ OK, passed 100 tests.

-- juaorture (se puede simplificar a la anterior)

-- ignareeva albagucen 
prop_repiteFinitaIguales2 :: Int -> Int -> Bool
prop_repiteFinitaIguales2 n x =
  and [head xs == y | y <- tail xs]
  where xs = repiteFinita n x

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Definir, por comprensión, la función
--    ecoC :: String -> String
-- tal que (ecoC xs) es la cadena obtenida a partir de la cadena xs
-- repitiendo cada elemento tantas veces como indica su posición: el
-- primer elemento se repite 1 vez, el segundo 2 veces y así
-- sucesivamente. Por ejemplo, 
--    ecoC "abcd"  ==  "abbcccdddd"
-- ---------------------------------------------------------------------

-- enrnarbej josrodgal7 paumacpar fatfervaz marjimcom monlagare albcercid
-- joscasgom1 congomgom eliguivil beagongon1 carmarcar5 belbenzam roscargar
-- felsuacor cargonler pabrabmon antlopgom2 alvfercen antmorper3 glovizcas 
-- eledejim2 margarvil14 margirmon migibagar antbeacar marmerzaf josdeher 
-- margarflo5 manruiber fraferpoy cescarde artmorfer criortcar luimotmar
-- marlobrip albagucen ignareeva
ecoC :: String -> String
ecoC xs = concat [replicate n x | (x,n) <- zip xs [1..]]

-- juaorture natruipin
ecoC1 :: String -> String
ecoC1 xs = concat [repiteFinita p a | a <- xs
                                    , let p = posicion a xs]
     where posicion x (y:ys) | x == y    = 1
                             | otherwise = 1 + posicion x ys

-- Comentario: La definición ecoC1 es incorrecta. Por ejemplo,
--    λ> ecoC1 "aba"
--    "abba"

-- juaorture
ecoC2 :: String -> String
ecoC2 xs = concat [repiteFinita (p+1) a | a <- xs
                                        , p <- [0.. length xs - 1]
                                        , xs!!p == a]

-- Comentario: La definición ecoC2 es incorrecta. Por ejemplo,
--    λ> ecoC2 "aba"
--    "aaaabbaaaa"

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Definir, por recursión, la función
--    ecoR :: String -> String
-- tal que (ecoR xs) es la cadena obtenida a partir de la cadena xs
-- repitiendo cada elemento tantas veces como indica su posición: el
-- primer elemento se repite 1 vez, el segundo 2 veces y así
-- sucesivamente. Por ejemplo,  
--    ecoR "abcd"  ==  "abbcccdddd"
-- ---------------------------------------------------------------------

-- enrnarbej paumacpar monlagare albcercid joscasgom1 congomgom eliguivil
-- beagongon1 carmarcar5 belbenzam roscargar felsuacor cargonler pabrabmon
-- antlopgom2 alvfercen antmorper3 eledejim2 margirmon natruipin antbeacar
-- margarflo5 fraferpoy manruiber luimotmar albagucen ignareeva
ecoR :: String -> String
ecoR xs = ecoRaux xs 1

ecoRaux :: [a] -> Int -> [a]
ecoRaux [] _ = []
ecoRaux (x:xs) n = replicate n x ++ ecoRaux xs (n+1)

-- josrodgal7 fatfervaz marjimcom glovizcas margarvil14 marmerzaf
-- cescarde artmorfer marlobrip
ecoR2 :: String -> String
ecoR2 xs = aux 1 xs
  where aux n (x:xs) = replicate n x ++ aux (n+1) xs
        aux _ xs     = []

-- juaorture criortcar
ecoR3 :: String -> String
ecoR3 xs = aux2 xs 1
     where aux2 []     _ = []
           aux2 (x:xs) n = repiteFinita n x ++ aux2 xs (n+1)

-- migibagar
ecoR4 :: String -> String
ecoR4 xs = reverse (aux4 xs)

aux4 :: String -> String
aux4 [] = []
aux4 xs = replicate (length xs) (last xs) ++ aux4 (init xs)

-- Comentario: La definición anterior se puede mejorar. Por ejemplo,
--    λ> length (ecoR4 (show (10^10000)))
--    50015001
--    (9.49 secs, 9,546,126,992 bytes)
--    λ> length (ecoR (show (10^10000)))
--    50015001
--    (1.15 secs, 5,591,204,056 bytes)

-- josdeher
ecoR5 :: String -> String
ecoR5 xs = auxEcoR5 (length xs) (reverse xs)

auxEcoR5 :: Int -> [t] -> [t]
auxEcoR5 0 xs = []
auxEcoR5 y (x:xs) = auxEcoR5 (y-1) xs ++ repiteFinita y x

-- Comentario: La definición anterior se puede mejorar. Por ejemplo,
--    λ> length (ecoR5 (show (10^1000)))
--    501501
--    (2.28 secs, 9,445,549,664 bytes)
--    λ> length (ecoR (show (10^1000)))
--    501501
--    (0.03 secs, 57,797,640 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir, por recursión, la función
--    itera :: (a -> a) -> a -> [a]
-- tal que (itera f x) es la lista cuyo primer elemento es x y los
-- siguientes elementos se calculan aplicando la función f al elemento
-- anterior. Por ejemplo, 
--    ghci> itera (+1) 3
--    [3,4,5,6,7,8,9,10,11,12,{Interrupted!}
--    ghci> itera (*2) 1
--    [1,2,4,8,16,32,64,{Interrupted!}
--    ghci> itera (`div` 10) 1972
--    [1972,197,19,1,0,0,0,0,0,0,{Interrupted!}
-- 
-- Nota: La función repite es equivalente a la función iterate definida
-- en el preludio de Haskell.
-- ---------------------------------------------------------------------

-- enrnarbej josrodgal7 paumacpar fatfervaz monlagare marjimcom albcercid
-- joscasgom1 congomgom eliguivil migibagar beagongon1 carmarcar5 
-- felsuacor cargonler pabrabmon alvfercen antlopgom2 antmorper3
-- glovizcas margarvil14 obelbenzam roscargar eledejim2 margirmon
-- juaorture natruipin antbeacar marmerzaf josdeher margarflo5 fraferpoy
-- manruiber cescarde artmorfer criortcar luimotmar marlobrip albagucen 
-- ignareeva
itera :: (a -> a) -> a -> [a]
itera f x = x : itera f (f x)

-- ----------------------------------------------------------------------------
-- Ejercicio 5.1. Definir, por recursión, la función
--    agrupaR :: Int -> [a] -> [[a]]
-- tal que (agrupaR n xs) es la lista formada por listas de n elementos
-- consecutivos de la lista xs (salvo posiblemente la última que puede
-- tener menos de n elementos). Por ejemplo, 
--    ghci> agrupaR 2 [3,1,5,8,2,7]
--    [[3,1],[5,8],[2,7]]
--    ghci> agrupaR 2 [3,1,5,8,2,7,9] 
--    [[3,1],[5,8],[2,7],[9]]
--    ghci> agrupaR 5 "todo necio confunde valor y precio"
--    ["todo ","necio"," conf","unde ","valor"," y pr","ecio"]
-- ---------------------------------------------------------------------------- 

-- enrnarbej josrodgal7 paumacpar fatfervaz monlagare marjimcom albcercid 
-- joscasgom1 eliguivil beagongon1 carmarcar5 roscargar felsuacor
-- cargonler belbenzam pabrabmon alvfercen antlopgom2 antmorper3
-- margarvil14 eledejim2 margirmon migibagar natruipin antbeacar marmerzaf
-- margarflo5 fraferpoy manruiber cescarde juaorture criortcar luimotmar
-- marlobrip albagucen ignareeva
agrupaR :: Int -> [a] -> [[a]]
agrupaR _ [] = []
agrupaR n xs = take n xs : agrupaR n (drop n xs)

-- congomgom glovizcas josdeher     
agrupaR2 :: Int -> [a] -> [[a]]
agrupaR2 _ [] = []
agrupaR2 0 _  = []
agrupaR2 n xs = take n xs : agrupaR2 n (drop n xs) 

-- Comentario: Se supone que n > 0.

-- ----------------------------------------------------------------------------
-- Ejercicio 5.2. Definir, de manera no recursiva con iterate, la función
--    agrupa :: Int -> [a] -> [[a]]
-- tal que (agrupa n xs) es la lista formada por listas de n elementos
-- consecutivos de la lista xs (salvo posiblemente la última que puede
-- tener menos de n elementos). Por ejemplo, 
--    ghci> agrupa 2 [3,1,5,8,2,7]
--    [[3,1],[5,8],[2,7]]
--    ghci> agrupa 2 [3,1,5,8,2,7,9] 
--    [[3,1],[5,8],[2,7],[9]]
--    ghci> agrupa 5 "todo necio confunde valor y precio"
--    ["todo ","necio"," conf","unde ","valor"," y pr","ecio"]
-- ---------------------------------------------------------------------------- 

-- enrnarbej paumacpar marjimcom albcercid joscasgom1 eliguivil
-- beagongon1 carmarcar5 roscargar cargonler belbenzam pabrabmon
-- josrodgal7 alvfercen antlopgom2 antmorper3 margarvil14 eledejim2 
-- margirmon natruipin antbeacar marmerzaf josdeher congomgom margarflo5 
-- manruiber fraferpoy cescarde felsuacor criortcar luimotmar fatfervaz 
agrupa :: Int -> [a] -> [[a]]
agrupa n = takeWhile (not . null)
         . map (take n)
         . iterate (drop n)

-- Puede verse su funcionamiento en el siguiente ejemplo,
--    iterate (drop 2) [5..10]  
--    ==> [[5,6,7,8,9,10],[7,8,9,10],[9,10],[],[],...
--    map (take 2) (iterate (drop 2) [5..10])
--    ==> [[5,6],[7,8],[9,10],[],[],[],[],...
--    takeWhile (not . null) (map (take 2) (iterate (drop 2) [5..10]))
--    ==> [[5,6],[7,8],[9,10]]

-- juaorture (se puede simplificar a la anterior)
agrupa2 :: Eq a => Int -> [a] -> [[a]]
agrupa2 n xs = takeWhile (/= []) [take n a | a <- iterate (drop n) xs]

-- ----------------------------------------------------------------------------
-- Ejercicio 5.3. Comprobar con QuickCheck que todos los grupos de
-- (agrupa n xs) tienen longitud n (salvo el último que puede tener una
-- longitud menor). 
-- ---------------------------------------------------------------------------- 

-- enrnarbej

-- La propiedad es
prop_AgrupaLongitud :: Int -> [Int] -> Property
prop_AgrupaLongitud n xs =
  n > 0 && xs /= [] ==>
  all (\x -> length x == n) (init $ agrupa n xs)

-- paumacpar monlagare carmarcar5 congomgom beagongon1 roscargar
-- cargonler belbenzam pabrabmon josrodgal7 albcercid alvfercen
-- antlopgom2 margarvil14 antmorper3 glovizcas eledejim2 margirmon migibagar
-- natruipin antbeacar marmerzaf josdeher margarflo5 manruiber fraferpoy 
-- cescarde juaorture felsuacor criortcar luimotmar albagucen fatfervaz
prop_AgrupaLongitud2 :: Int -> [Int] -> Property
prop_AgrupaLongitud2 n xs =
  n > 0 && xs /= [] ==>
  all (==n) (map (length) (init (agrupa n xs)))

-- Comentario: La definición anterior se puede simplificar.

-- eliguivil
prop_AgrupaLongitud3 :: Int -> [Int] -> Property
prop_AgrupaLongitud3 n xs =
  n > 0 && xs /= [] ==>
  and $ init (map ((==n).length) (agrupa n xs))

-- La comprobación es
--    *Main> quickCheck prop_AgrupaLongitud
--    +++ OK, passed 100 tests.

-- ----------------------------------------------------------------------------
-- Ejercicio 5.4. Comprobar con QuickCheck que combinando todos los
-- grupos de (agrupa n xs) se obtiene la lista xs. 
-- ---------------------------------------------------------------------------- 

-- enrnarbej paumacpar fatfervaz monlagare eliguivil carmarcar5
-- congomgom beagongon1 roscargar cargonler belbenzam pabrabmon
-- josrodgal7 albcercid alvfercen antlopgom2 antmorper3 glovizcas
-- margarvil14 eledejim2 margirmon migibagar natruipin antbeacar marmerzaf
-- josdeher margarflo5 manruiber fraferpoy cescarde juaorture felsuacor
-- criortcar luimotmar albagucen 

-- La segunda propiedad es
prop_AgrupaCombina :: Int -> [Int] -> Property
prop_AgrupaCombina n xs =
  n > 0 && xs /= [] ==>
  concat (agrupa n xs) == xs

-- comentario (juaorture): la segunda condición no es necesaria

-- La comprobación es
--    *Main> quickCheck prop_AgrupaCombina
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 6.1. Sea la siguiente operación, aplicable a cualquier
-- número entero positivo:  
--    * Si el número es par, se divide entre 2.
--    * Si el número es impar, se multiplica por 3 y se suma 1.
-- Dado un número cualquiera, podemos considerar su órbita, es decir,
-- las imágenes sucesivas al iterar la función. Por ejemplo, la órbita
-- de 13 es
--    13, 40, 20, 10, 5, 16, 8, 4, 2, 1, 4, 2, 1,...
-- Si observamos este ejemplo, la órbita de 13 es periódica, es decir,
-- se repite indefinidamente a partir de un momento dado). La conjetura
-- de Collatz dice que siempre alcanzaremos el 1 para cualquier número
-- con el que comencemos. Ejemplos:  
--    * Empezando en n = 6 se obtiene 6, 3, 10, 5, 16, 8, 4, 2, 1.
--    * Empezando en n = 11 se obtiene: 11, 34, 17, 52, 26, 13, 40, 20,
--      10, 5, 16, 8, 4, 2, 1. 
--    * Empezando en n = 27, la sucesión tiene 112 pasos, llegando hasta
--      9232 antes de descender a 1:  27, 82, 41, 124, 62, 31, 94, 47,
--      142, 71, 214, 107, 322, 161, 484, 242, 121, 364, 182, 91, 274,
--      137, 412, 206, 103, 310, 155, 466, 233, 700, 350, 175, 526, 263,
--      790, 395, 1186, 593, 1780, 890, 445, 1336, 668, 334, 167, 502,
--      251, 754, 377, 1132, 566, 283, 850, 425, 1276, 638, 319, 958,
--      479, 1438, 719, 2158, 1079, 3238, 1619, 4858, 2429, 7288, 3644,
--      1822, 911, 2734, 1367, 4102, 2051, 6154, 3077, 9232, 4616, 2308,
--      1154, 577, 1732, 866, 433, 1300, 650, 325, 976, 488, 244, 122,
--      61, 184, 92, 46, 23, 70, 35, 106, 53, 160, 80, 40, 20, 10, 5,
--      16, 8, 4, 2, 1. 
-- 
-- Definir la función
--    siguiente :: Integer -> Integer
-- tal que (siguiente n) es el siguiente de n en la sucesión de
-- Collatz. Por ejemplo,
--    siguiente 13  ==  40
--    siguiente 40  ==  20
-- ---------------------------------------------------------------------

-- enrnarbej paumacpar fatfervaz albcercid joscasgom1 eliguivil migibagar
-- beagongon1 congomgom roscargar felsuacor cargonler pabrabmon josrodgal7 
-- antmorper3 eledejim2 margirmon natruipin antbeacar marmerzaf josdeher
-- fraferpoy glovizcas cescarde antlopgom2 luimotmar marlobrip alvfercen
-- criortcar albagucen 
siguiente :: Integer -> Integer
siguiente n | odd n     = 3*n + 1
            | otherwise = div n 2

-- monlagare carmarcar5 belbenzam marjimcom margarvil14 margarflo5 manruiber
siguiente1 :: Integer -> Integer
siguiente1 n | even n    = n `div` 2
             | otherwise = n * 3 + 1

-- juaorture
siguiente2 :: Integer -> Integer
siguiente2 n | n `mod` 2 == 0 = n `div`2
             | otherwise      = n*3 + 1
              
-- ---------------------------------------------------------------------
-- Ejercicio 6.2. Definir, por recursión, la función 
--    collatzR :: Integer -> [Integer]
-- tal que (collatzR n) es la órbita de CollatzR de n hasta alcanzar el
-- 1. Por ejemplo,
--    collatzR 13  ==  [13,40,20,10,5,16,8,4,2,1]
-- ---------------------------------------------------------------------

-- enrnarbej paumacpar fatfervaz monlagare albcercid joscasgom1 eliguivil 
-- migibagar beagongon1 congomgom roscargar carmarcar5 cargonler belbenzam
-- pabrabmon josrodgal7 marjimcom antmorper3 margarvil14 eledejim2 margirmon
-- natruipin antbeacar marmerzaf josdeher margarflo5 manruiber fraferpoy
-- glovizcas cescarde antlopgom2 felsuacor luimotmar marlobrip alvfercen
-- criortcar albagucen 
collatzR :: Integer -> [Integer]
collatzR 1 = [1]
collatzR n = n : collatzR (siguiente n)

-- juaorture
collatzR1 :: Integer -> [Integer]
collatzR1 n = n : aux n
  where aux :: Integer -> [Integer]
        aux n | n <= 1    = []
              | otherwise = sig : aux sig
          where sig = siguiente n

prop_collatz :: Integer -> Property
prop_collatz n =
  n > 0 ==>
  collatzR1 n == collatzR n

-- ---------------------------------------------------------------------
-- Ejercicio 6.3. Definir, sin recursión y con iterate, la función 
--    collatz :: Integer -> [Integer]
-- tal que (collatz n) es la órbita de Collatz d n hasta alcanzar el
-- 1. Por ejemplo,
--    collatz 13  ==  [13,40,20,10,5,16,8,4,2,1]
-- Indicación: Usar takeWhile e iterate.
-- ---------------------------------------------------------------------

-- enrnarbej paumacpar fatfervaz monlagare albcercid joscasgom1 migibagar
-- beagongon1 roscargar congomgom carmarcar5 cargonler pabrabmon josrodgal7
-- marjimcom antmorper3 margarvil14 eledejim2 margirmon natruipin antbeacar 
-- josdeher marmerzaf margarflo5 manruiber fraferpoy glovizcas cescarde
-- antlopgom2 felsuacor luimotmar alvfercen criortcar albagucen 
collatz :: Integer -> [Integer]
collatz n = takeWhile (/= 1) (iterate siguiente n) ++ [1]

-- eliguivil
collatz2 :: Integer -> [Integer]
collatz2 n = takeWhileInclusivo (/=1) (iterate (siguiente) n)

takeWhileInclusivo :: (a -> Bool) -> [a] -> [a]
takeWhileInclusivo _ [] = []
takeWhileInclusivo p (x:xs) =
  x : if p x
      then takeWhileInclusivo p xs
      else []

-- Fuente de TakeWhileInclusivo: http://bit.ly/2g7M6b8

-- juaorture
collatz3 :: Integer -> [Integer]
collatz3 n = takeWhile (>1) (iterate siguiente n) ++ [1]

-- ---------------------------------------------------------------------
-- Ejercicio 6.4. Definir la función
--    menorCollatzMayor :: Int -> Integer
-- tal que (menorCollatzMayor x) es el menor número cuya órbita de
-- Collatz tiene más de x elementos. Por ejemplo,
--    menorCollatzMayor 100  ==  27
-- ---------------------------------------------------------------------

-- albcercid carmarcar5 congomgom belbenzam pabrabmon marjimcom margarvil14
-- eledejim2 margirmon natruipin antbeacar josdeher margarflo5 manruiber
-- fraferpoy glovizcas cescarde juaorture luimotmar antlopgom2 felsuacor
-- alvfercen criortcar albagucen josrodgal7 
menorCollatzMayor :: Int -> Integer
menorCollatzMayor x = head [a | a <- [1..], length (collatz a) > x]

-- enrnarbej paumacpar monlagare joscasgom1 fatfervaz eliguivil migibagar
-- beagongon1 roscargar cargonler antmorper3 marmerzaf josdeher
menorCollatzMayor2 :: Int -> Integer
menorCollatzMayor2 x =
  head $ filter (\n -> length (collatz n) > x) [1..]

-- ---------------------------------------------------------------------
-- Ejercicio 6.5. Definir la función
--    menorCollatzSupera :: Integer -> Integer
-- tal que (menorCollatzSupera x) es el menor número cuya órbita de
-- Collatz tiene algún elemento mayor que x. Por ejemplo,
--    menorCollatzSupera 100  ==  15
-- ---------------------------------------------------------------------

-- migibagar congomgom marjimcom margirmon antbeacar glovizcas
-- juaorture alvfercen josrodgal7 
menorCollatzSupera :: Integer -> Integer
menorCollatzSupera x =
  head [n | n <- [1..], any (> x) (collatzR n)]

-- enrnarbej paumacpar monlagare joscasgom1 fatfervaz eliguivil
-- beagongon1 roscargar cargonler antmorper3 marmerzaf
menorCollatzSupera1 :: Integer -> Integer
menorCollatzSupera1 x =
  head $ filter (\n -> any (>x) (collatz n)) [1..]

-- albcercid carmarcar5 belbenzam pabrabmon margarvil14 eledejim2
-- natruipin josdeher margarflo5 manruiber fraferpoy cescarde antlopgom2
-- felsuacor luimotmar criortcar albagucen 

menorCollatzSupera2 :: Integer -> Integer
menorCollatzSupera2 x =
  head [ a | a <- [1..], maximum (collatzR a) > x ]

-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir, usando takeWhile y map, la función
--    potenciasMenores :: Int -> Int -> [Int]
-- tal que (potenciasMenores x y) es la lista de las potencias de x
-- menores que y. Por ejemplo,
--    potenciasMenores 2 1000  ==  [2,4,8,16,32,64,128,256,512]
-- ---------------------------------------------------------------------

-- margarvil14 eledejim2 marmerzaf margarflo5 glovizcas antlopgom2 
-- felsuacor alvfercen albagucen josrodgal7 
potenciasMenores :: Int -> Int -> [Int]
potenciasMenores x y = takeWhile (<y) (map (x^) [1..])

-- enrnarbej albcercid joscasgom1 fatfervaz beagongon1 roscargar
-- cargonler belbenzam pabrabmon marjimcom antmorper3 migibagar
-- antbeacar josdeher congomgom cescarde
potenciasMenores2 :: Int -> Int -> [Int]
potenciasMenores2 x y = takeWhile (<y) (map (\n -> x^n) [1..])

-- Comentario: La definición anterior se puede simplificar.

-- enrnarbej paumacpar eliguivil carmarcar5 margirmon natruipin
-- josdeher manruiber marjimcom
potenciasMenores3 :: Int -> Int -> [Int]
potenciasMenores3 x y = takeWhile (<y) (iterate (*x) x)

-- juaorture luimotmar
potenciasMenores4 :: Int -> Int -> [Int]
potenciasMenores4 x y = takeWhile (<y) [x^a | a <- [0..]]

-- ---------------------------------------------------------------------
-- Ejercicio 8.1. Definir, usando la criba de Eratóstenes, la constante
--    primos :: Integral a => [a]
-- cuyo valor es la lista de los números primos. Por ejemplo,
--    take 10 primos  ==  [2,3,5,7,11,13,17,19,23,29]
-- ---------------------------------------------------------------------

-- enrnarbej paumacpar albcercid joscasgom1 fatfervaz eliguivil beagongon1 
-- roscargar cargonler carmarcar belbenzam congomgom pabrabmon monlagare
-- antmorper3 eledejim2 migibagar natruipin antbeacar marmerzaf josdeher
-- margarflo5 manruiber fraferpoy glovizcas cescarde juaorture antlopgom2
-- felsuacor marjimcom luimotmar alvfercen albagucen josrodgal7 
primos :: Integral a => [a]
primos = criba [2..]

criba :: Integral a => [a] -> [a]
criba (p:xs) = p : criba [x | x <- xs, x `mod` p /= 0]

-- margarvil14
primos2 :: Integral a => [a]
primos2 = criba [2..]
   where criba []     = []
         criba (n:ns) = n : criba (elimina n ns)
         elimina n xs = [x | x <- xs, x `mod` n /= 0]

-- margirmon
divisible :: Int -> Int -> Bool
divisible x y = x `rem` y == 0

primos3 :: [Int]
primos3 = 2:3:[x | x <- [4..],
                   not (or (map (divisible x)
                                (takeWhile (<= (x `div` 2)) primos3)))]

-- ---------------------------------------------------------------------
-- Ejercicio 8.2. Definir, usando primos, la función
--    primo :: Integral a => a -> Bool
-- tal que (primo n) se verifica si n es primo. Por ejemplo,
--    primo 7  ==  True
--    primo 9  ==  False
-- ---------------------------------------------------------------------

-- margarvil14 eledejim2 margirmon glovizcas
primo :: Int -> Bool
primo n = n == head (dropWhile (<n) primos)

-- enrnarbej paumacpar joscasgom1 albcercid fatfervaz beagongon1 roscargar 
-- cargonler carmarcar5 belbenzam pabrabmon congomgom monlagare
-- antmorper3 migibagar natruipin antbeacar marmerzaf josdeher
-- margarflo5 manruiber fraferpoy antlopgom2 felsuacor albagucen josrodgal7 
primo2 :: Int -> Bool
primo2 1 = False
primo2 n = n == last (takeWhile (<=n) primos)

-- eliguivil
primo3 :: Int -> Bool
primo3 n = n == busca (>=n) primos
  where
    busca f (x:xs) | f x       = x
                   | otherwise = busca f xs

-- cescarde luimotmar
primo4 :: Int -> Bool
primo4 n = elem n (take n primos)

-- juaorture marjimcom alvfercen
primo5 :: Int -> Bool
primo5 n = n `elem` (takeWhile (<=n) primos)

-- ---------------------------------------------------------------------
-- Ejercicio 8.3. Definir la función
--    sumaDeDosPrimos :: Int -> [(Int,Int)]
-- tal que (sumaDeDosPrimos n) es la lista de las distintas
-- descomposiciones de n como suma de dos números primos. Por ejemplo, 
--    sumaDeDosPrimos 30  ==  [(7,23),(11,19),(13,17)]
--    sumaDeDosPrimos 10  ==  [(3,7),(5,5)]
-- Calcular, usando la función sumaDeDosPrimos, el menor número que
-- puede escribirse de 10 formas distintas como suma de dos primos.
-- ---------------------------------------------------------------------

-- albcercid carmarcar5 congomgom eledejim2 josdeher margarflo5
-- manruiber luimotmar josrodgal7 
sumaDeDosPrimos :: Int -> [(Int,Int)]
sumaDeDosPrimos n =
  [(a, n-a) | a <- takeWhile (n `div` 2 >=) primos
            , primo (n-a)]

-- enrnarbej paumacpar joscasgom1 eliguivil beagongon1 roscargar cargonler 
-- belbenzam monlagare antmorper3 marmerzaf
sumaDeDosPrimos2 :: Int -> [(Int,Int)]
sumaDeDosPrimos2 n =
  [(x,y) | (x,y) <- map (\p -> (p, n-p)) (takeWhile (<= (div n 2)) primos)
         , primo y]

-- pabrabmon natruipin antlopgom2 marjimcom alvfercen
sumaDeDosPrimos3 n =
  [ (x,y) | x <- takeWhile (< n) primos
          , y <- takeWhile (<= (n-x)) primos
          , x <= y
          , x + y == n]

-- margirmon
sumaDeDosPrimos4 :: Int -> [(Int,Int)]
sumaDeDosPrimos4 n = [ (x,(n-x))
                     | x <- (takeWhile (<= (n `div` 2)) primos),
                       primo (n-x)]

-- migibagar cescarde
sumaDeDosPrimos5 :: Int -> [(Int,Int)]
sumaDeDosPrimos5 n =
  [(x,y) | x <- [1..n]
         , y <- [x..n]
         , x+y == n
         , primo x
         , primo y]

-- Comentario: La definición anterior se puede mejorar. Por ejemplo,
--    λ> length (sumaDeDosPrimos5 1500)
--    67
--    (2.81 secs, 1,884,286,888 bytes)
--    λ> length (sumaDeDosPrimos 1500)
--    67
--    (1.21 secs, 821,659,512 bytes)


-- glovizcas felsuacor
sumaDeDosPrimos6 :: Int -> [(Int,Int)]
sumaDeDosPrimos6 n = [(a,b) | a <- takeWhile (<n) primos ,
                              b <- takeWhile (<n) primos ,
                              a + b == n , b>=a]

-- El cálculo es
--    *Main> head [ x | x<-[1..], length (sumaDeDosPrimos x) == 10]
--    114

-- juaorture
sumaDeDosPrimos7 :: Int -> [(Int,Int)]
sumaDeDosPrimos7 n = [(a,b) | a <- [2..n]
                            , primo a
                            , let b = n - a
                            , primo b
                            , b >= a]

-- El cálculo es
nSumas :: Int -> Int
nSumas n = head [a | a <- [0..]
                   , length (sumaDeDosPrimos7 a) >= n]

-- *Main> nSumas 10
-- 114

-- ---------------------------------------------------------------------
-- § La lista infinita de factoriales,                                --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 9.1. Definir, por comprensión, la función
--    factoriales1 :: [Integer]
-- tal que factoriales1 es la lista de los factoriales. Por ejemplo,
--    take 10 factoriales1  ==  [1,1,2,6,24,120,720,5040,40320,362880]
-- ---------------------------------------------------------------------

-- enrnarbej paumacpar joscasgom1 eliguivil albcercid roscargar cargonler
-- pabrabmon congomgom monlagare antmorper3 margarvil14 eledejim2
-- margirmon luimotmar natruipin antbeacar josdeher margarflo5 fatfervaz
-- manruiber glovizcas cescarde juaorture antlopgom2 felsuacor
-- beagongon1 alvfercen marmerzaf josrodgal7 
factoriales1 :: [Integer]
factoriales1 = 1 : [product [1..x] | x <- [1..]]

-- ---------------------------------------------------------------------
-- Ejercicio 9.2. Definir, usando zipWith, la función
--    factoriales2 :: [Integer]
-- tal que factoriales2 es la lista de los factoriales. Por ejemplo,
--    take 10 factoriales2  ==  [1,1,2,6,24,120,720,5040,40320,362880]
-- ---------------------------------------------------------------------

-- enrnarbej paumacpar joscasgom1 eliguivil roscargar cargonler pabrabmon
-- monlagare antmorper3 margarvil14 eledejim2 margirmon congomgom antbeacar
-- josdeher margarflo5 fatfervaz manruiber glovizcas juaorture antlopgom2
-- felsuacor beagongon1 luimotmar marmerzaf josrodgal7 
factoriales2 :: [Integer]
factoriales2 = 1:1:zipWith (*) [2..] (tail factoriales2)

-- Comentario: La definición anterior se puede simplificar como sigue
factoriales2' :: [Integer]
factoriales2' = 1 : zipWith (*) [1..] factoriales2'

-- El cálculo es
--    take 4 factoriales2
--    = take 4 (1 : zipWith (*) [1..] factoriales2)
--    = 1 : take 3 (zipWith (*) [1..] factoriales2)
--    = 1 : take 3 (zipWith (*) [1..] [1|R1])           {R1 es tail factoriales2}
--    = 1 : take 3 (1 : zipWith (*) [2..] [R1])      
--    = 1 : 1 : take 2 (zipWith (*) [2..] [1|R2])       {R2 es drop 2 factoriales2}  
--    = 1 : 1 : take 2 (2 : zipWith (*) [3..] [R2])
--    = 1 : 1 : 2 : take 1 (zipWith (*) [3..] [2|R3])    {R3 es drop 3 factoriales2}  
--    = 1 : 1 : 2 : take 1 (6 : zipWith (*) [4..] [R3])  
--    = 1 : 1 : 2 : 6 : take 0 (zipWith (*) [4..] [R3])  
--    = 1 : 1 : 2 : 6 : []
--    = [1, 1, 2, 6]

-- Comentario: Comparación de eficiencia
--    ghci> let xs = take 3000 factoriales1 in (sum xs - sum xs)
--    0
--    (17.51 secs, 5631214332 bytes)
--    ghci> let xs = take 3000 factoriales2 in (sum xs - sum xs)
--    0
--    (0.04 secs, 17382284 bytes)

-- albcercid
factoriales2b :: [Integer]
factoriales2b = 1:(listaConstruida [1..])

listaConstruida (x:xs) = x:listaConstruida (zipWith (*) xs [1..])

-- Comentario: La definición anterior se puede simplificar.

-- cescarde marjimcom
factoriales2c :: [Integer]
factoriales2c = zipWith (*) (factoriales1) (repeat 1)

-- Comentario: Se reduce a factoriales1

-- Una variante de esta misma función sería utilizar la suma y la lista
-- infinita de ceros
-- cescarde
factoriales2a :: [Integer]
factoriales2a = zipWith (+) (factoriales1) (repeat 0)

-- Comentario: Se reduce a factoriales1.

-- ---------------------------------------------------------------------
-- Ejercicio 9.3. Definir, por recursión, la función
--    factoriales3 :: [Integer]
-- tal que factoriales3 es la lista de los factoriales. Por ejemplo,
--    take 10 factoriales3  ==  [1,1,2,6,24,120,720,5040,40320,362880]
-- ---------------------------------------------------------------------

-- Comentario:
factoriales3 :: [Integer]
factoriales3 = 1 : aux 1 [1..]
  where aux x (y:ys) = z : aux z ys
          where z = x*y

-- El cálculo es
--    take 4 factoriales3
--    = take 4 (1 : aux 1 [1..])
--    = 1 : take 3 (aux 1 [1..])
--    = 1 : take 3 (1 : aux 1 [2..])
--    = 1 : 1 : take 2 (aux 1 [2..])
--    = 1 : 1 : take 2 (2 : aux 2 [3..])
--    = 1 : 1 : 2 : take 1 (aux 2 [3..])
--    = 1 : 1 : 2 : take 1 (6 : aux 6 [4..])
--    = 1 : 1 : 2 : 6 : take 0 (aux 6 [4..])
--    = 1 : 1 : 2 : 6 : []
--    = [1,1,2,6]

-- Comparación de eficiencia
--    ghci> let xs = take 3000 factoriales2 in (sum xs - sum xs)
--    0
--    (0.04 secs, 17382284 bytes)
--    ghci> let xs = take 3000 factoriales3 in (sum xs - sum xs)
--    0
--    (0.04 secs, 18110224 bytes)

-- enrnarbej paumacpar  roscargar luimotmar monlagare margirmon cescarde
-- antlopgom2 alvfercen marmerzaf josrodgal7 
factoriales3b :: [Integer]
factoriales3b = 1:1:map (\(x,y) -> x*y) (zip [2..] (tail factoriales3))

-- enrnarbej eliguivil pabrabmon antmorper3
factoriales3c :: [Integer]
factoriales3c = fact3bAux 2

fact3bAux :: Integer -> [Integer]
fact3bAux n = 1:1: map (*n) (tail $ fact3bAux (n+1))

-- albcercid pabrabmon margarvil14 eledejim2 cargonler antbeacar
-- congomgom manruiber glovizcas beagongon1 marjimcom margarflo5
factoriales3d :: [Integer]
factoriales3d = factorx 0

factorx x = factorial x:factorx (x+1)

factorial 0 = 1
factorial n = n*factorial (n-1)

-- josdeher
factoriales3e = auxFactoriale [1]

auxFactoriale xs =
  (last xs) : (auxFactoriale (xs ++ [last xs * length xs]))

-- Comentario: El tipo de factoriales3d es [Int] en lugar de [Integer].

-- ---------------------------------------------------------------------
-- Ejercicio 9.4. Definir, usando scanl1, la función
--    factoriales4 :: [Integer]
-- tal que factoriales4 es la lista de los factoriales. Por ejemplo,
--    take 10 factoriales4  ==  [1,1,2,6,24,120,720,5040,40320,362880]
-- ---------------------------------------------------------------------

-- enrnarbej paumacpar joscasgom1 eliguivil roscargar pabrabmon albcercid
-- monlagare margarvil14 antmorper3 eledejim2 cargonler margirmon migibagar
-- congomgom antbeacar josdeher margarflo5 fatfervaz manruiber glovizcas
-- cescarde juaorture antlopgom2 felsuacor beagongon1 marjimcom marmerzaf
-- josrodgal7 
factoriales4 :: [Integer]
factoriales4 = 1 : scanl1 (*) [1..]

-- Comparación de eficiencia
--    ghci> let xs = take 3000 factoriales3 in (sum xs - sum xs)
--    0
--    (0.04 secs, 18110224 bytes)
--    ghci> let xs = take 3000 factoriales4 in (sum xs - sum xs)
--    0
--    (0.03 secs, 11965328 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 9.5. Definir, usando iterate, la función
--    factoriales5 :: [Integer]
-- tal que factoriales5 es la lista de los factoriales. Por ejemplo,
--    take 10 factoriales5  ==  [1,1,2,6,24,120,720,5040,40320,362880]
-- ---------------------------------------------------------------------

-- albcercid eliguivil antmorper3 margarvil14 eledejim2 cargonler
-- margirmon natruipin josdeher margarflo5 manruiber glovizcas antlopgom2
-- beagongon1 luimotmar marmerzaf fatfervaz josrodgal7 
factoriales5 :: [Integer]
factoriales5 = map snd (iterate f (1,1))
  where f (a,b) = (a+1,b*a)

-- El cálculo es
--    take 4 factoriales5
--    = take 4 (map snd (iterate f (1,1)))
--    = take 4 (map snd [(1,1),(2,1),(3,2),(4,6),...])
--    = take 4 [1,1,2,6,...]
--    = [1,1,2,6]

-- Comparación de eficiencia
-- El cálculo es
--    ghci> let xs = take 3000 factoriales4 in (sum xs - sum xs)
--    0
--    (0.04 secs, 18110224 bytes)
--    ghci> let xs = take 3000 factoriales5 in (sum xs - sum xs)
--    0
--    (0.03 secs, 11965760 bytes)

-- enrnarbej paumacpar pabrabmon  cescarde
factoriales5b :: [Integer]
factoriales5b = map product $ iterate (\xs -> xs++[last xs+1]) [1]

-- ---------------------------------------------------------------------
-- § La sucesión de Fibonacci                                         --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 10.1. La sucesión de Fibonacci está definida por
--    f(0) = 0
--    f(1) = 1
--    f(n) = f(n-1)+f(n-2), si n > 1.
-- 
-- Definir la función
--    fib :: Integer -> Integer
-- tal que (fib n) es el n-ésimo término de la sucesión de Fibonacci. 
-- Por ejemplo,
--    fib 8  ==  21
-- ---------------------------------------------------------------------

-- enrnarbej paumacpar joscasgom1 albcercid roscargar pabrabmon monlagare
-- antmorper3 eledejim2 cargonler margirmon migibagar congomgom natruipin
-- antbeacar josdeher margarflo5 fatfervaz manruiber glovizcas cescarde
-- juaorture beagongon1 marjimcom antlopgom2 marmerzaf josrodgal7 
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- eliguivil
fib2 n = busca n fibs
  where
    busca 0 (x:xs) = x
    busca n (x:xs) = busca (n-1) xs

fibs = 0 : scanl (+) 1 fibs

-- ---------------------------------------------------------------------
-- Ejercicio 10.2. Definir, por comprensión, la función
--    fibs1 :: [Integer]
-- tal que fibs1 es la sucesión de Fibonacci. Por ejemplo,
--    take 10 fibs1  ==  [0,1,1,2,3,5,8,13,21,34]
-- ---------------------------------------------------------------------

-- enrnarbej paumacpar joscasgom1 albcercid roscargar pabrabmon monlagare
-- antmorper3 eledejim2 cargonler margirmon migibagar congomgom natruipin
-- antbeacar josdeher margarflo5 fatfervaz manruiber glovizcas cescarde 
-- beagongon1 marjimcom antlopgom2 marmerzaf josrodgal7 
fibs1 :: [Integer]
fibs1 = [fib n | n <- [0..]]

-- eliguivil
fibs1b :: [Integer]
fibs1b = 0:1:[n+m | (n,m) <- zip fibs1b (tail fibs1b)]

-- juaorture
fibs1c :: [Integer]
fibs1c = 0 : 1 : [fibs1!!a + fibs1!!(a-1) | a <- [1..]]

-- Comentario: La definición anterior se puede mejorar.

-- ---------------------------------------------------------------------
-- Ejercicio 10.3. Definir, por recursión, la función
--    fibs2 :: [Integer]
-- tal que fibs2 es la sucesión de Fibonacci. Por ejemplo,
--    take 10 fibs2  ==  [0,1,1,2,3,5,8,13,21,34]
-- ---------------------------------------------------------------------

fibs2 :: [Integer]
fibs2 = aux 0 1
  where aux x y = x : aux y (x+y)

-- Comparación de eficiencia
--    ghci> let xs = take 30 fibs1 in (sum xs - sum xs)
--    0
--    (6.02 secs, 421589672 bytes)
--    ghci> let xs = take 30 fibs2 in (sum xs - sum xs)
--    0
--    (0.01 secs, 515856 bytes)

-- enrnarbej paumacpar
fibs2a :: [Integer]
fibs2a = 0:1: map (\(x,y) -> x+y) (zip fibs2a (tail fibs3))

-- eliguivil
fibs2b :: [Integer]
fibs2b = 0 : scanl (+) 1 fibs2b

-- albcercid roscargar pabrabmon monlagare antmorper3 cargonler congomgom
-- josdeher manruiber glovizcas cescarde beagongon1 marjimcom antlopgom2
-- marmerzaf fatfervaz josrodgal7 
fibs2c :: [Integer]
fibs2c = listaF 0

listaF x = fib x:listaF (x+1)

-- ---------------------------------------------------------------------
-- Ejercicio 10.4. Definir, por recursión con zipWith, la función
--    fibs3 :: [Integer]
-- tal que fibs3 es la sucesión de Fibonacci. Por ejemplo,
--    take 10 fibs3  ==  [0,1,1,2,3,5,8,13,21,34]
-- ---------------------------------------------------------------------

-- enrnarbej paumacpar eliguivil roscargar pabrabmon albcercid monlagare
-- antmorper3 cargonler margirmon congomgom natruipin antbeacar josdeher
-- manruiber glovizcas cescarde beagongon1 marjimcom antlopgom2 marmerzaf
-- margarflo5 fatfervaz josrodgal7 
fibs3 :: [Integer]
fibs3 = 0 : 1 : zipWith (+) fibs3 (tail fibs3)

-- Comparación de eficiencia
--    ghci> let xs = take 40000 fibs2 in (sum xs - sum xs)
--    0
--    (0.90 secs, 221634544 bytes)
--    ghci> let xs = take 40000 fibs3 in (sum xs - sum xs)
--    0
--    (1.14 secs, 219448176 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 10.5. Definir, por recursión con acumuladores, la función 
--    fibs4 :: [Integer]y
-- tal que fibs4 es la sucesión de Fibonacci. Por ejemplo,
--    take 10 fibs4  ==  [0,1,1,2,3,5,8,13,21,34]
-- ---------------------------------------------------------------------

fibs4 :: [Integer]
fibs4 = fs
  where (xs,ys,fs) = (zipWith (+) ys fs, 1:xs, 0:ys)

-- El cálculo de fibs4 es 
--   +------------------------+-----------------+-------------------+
--   | xs = zipWith (+) ys fs | ys = 1:xs       | fs = 0:ys         |
--   +------------------------+-----------------+-------------------+
--   |                        | 1:...           | 0:...             |
--   |                        | ^               | ^                 |
--   | 1:...                  | 1:1:...         | 0:1:1:...         |
--   |                        |   ^             |   ^               |
--   | 1:2:...                | 1:1:2:...       | 0:1:1:2:...       |
--   |                        |     ^           |     ^             |
--   | 1:2:3:...              | 1:1:2:3:...     | 0:1:1:2:3:...     |
--   |                        |       ^         |       ^           |
--   | 1:2:3:5:...            | 1:1:2:3:5:...   | 0:1:1:2:3:5:...   |
--   |                        |         ^       |         ^         |
--   | 1:2:3:5:8:...          | 1:1:2:3:5:8:... | 0:1:1:2:3:5:8:... |
--   +------------------------+-----------------+-------------------+
-- En la tercera columna se va construyendo la sucesión.

-- Comparación de eficiencia
--    ghci> let xs = take 40000 fibs2 in (sum xs - sum xs)
--    0
--    (0.90 secs, 221634544 bytes)
--    ghci> let xs = take 40000 fibs4 in (sum xs - sum xs)
--    0
--    (0.84 secs, 219587064 bytes)

-- enrnarbej eliguivil pabrabmon albcercid antmorper3 cargonler congomgom
-- josdeher manruiber glovizcas cescarde paumacpar beagongon1 marjimcom
-- antlopgom2 marmerzaf fatfervaz josrodgal7 
fibs4a :: [Integer]
fibs4a = 0: 1: fibs4Aux [0,1]

fibs4Aux :: [Integer] -> [Integer]
fibs4Aux (x:y:xs) =  (x+y) : fibs4Aux ([y,x+y])

-- margirmon
fibs4b :: [Integer]
fibs4b = map head (iterate (\[x,y,z] -> [(x+y), x, y]) [0,1,1])

-- josdeher
fibs4c :: [Integer]
fibs4c = 0 : 1 : auxFibs4c [0,1]

auxFibs4c xs
  = y ++ auxFibs4c (xs++y)
    where y = [last xs + last (init xs)]

-- cescarde
fibs4d :: [Integer]
fibs4d = 0 : 1 : fiboAux
  where fiboAux = zipWith (+) fibs4d (tail fibs4d)

-- ---------------------------------------------------------------------
-- § El triángulo de Pascal                                           --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 11.1. El triángulo de Pascal es un triángulo de números
--          1
--         1 1
--        1 2 1
--      1  3 3  1
--     1 4  6  4 1
--    1 5 10 10 5 1
--   ...............
-- construido de la siguiente forma
-- + la primera fila está formada por el número 1;
-- + las filas siguientes se construyen sumando los números adyacentes
--   de la fila superior y añadiendo un 1 al principio y al final de la
--   fila. 
-- 
-- Definir, con iterate y zipWith, la función
--    pascal1 :: [[Integer]]
-- tal que pascal es la lista de las líneas del triángulo de Pascal. Por
-- ejemplo, 
--    ghci> take 6 pascal1
--    [[1],[1,1],[1,2,1],[1,3,3,1],[1,4,6,4,1],[1,5,10,10,5,1]]
-- ---------------------------------------------------------------------

pascal1 :: [[Integer]]
pascal1 = iterate f [1]
  where f xs = zipWith (+) (0:xs) (xs++[0])

-- Por ejemplo,
--    xs      = [1,2,1]
--    0:xs    = [0,1,2,1]
--    xs++[0] = [1,2,1,0]
--    +       = [1,3,3,1]

-- albcercid pabrabmon antmorper3 cargonler congomgom antbeacar josdeher
-- manruiber glovizcas cescarde beagongon1 antlopgom2 marmerzaf margarflo5
-- marjimcom
pascal1a :: [[Integer]]
pascal1a = iterate f [1]
  where f (x:xs) = x:zipWith (+) (x:xs) (xs ++ [0])

-- enrnarbej paumacpar  roscargar pabrabmon margirmon
pascal1b :: [[Integer]]
pascal1b = iterate (\xs -> 1:zipWith (+) xs (tail xs)++[1]) [1]

-- eliguivil
pascal1c :: [[Integer]]
pascal1c = [combinatorios n | n <- [0..]]
  where combinatorios n = [(fac n)`div`((fac k)*(fac (n-k)))| k <- [0..n]]

fac 0 = 1
fac k = k*(fac (k-1))

-- Comentario: No cumple la especificación.

-- ---------------------------------------------------------------------
-- Ejercicio 11.2. Definir, con map y zipWith, la función
--    pascal2 :: [[Integer]]
-- tal que pascal es la lista de las líneas del triángulo de Pascal. Por
-- ejemplo, 
--    ghci> take 6 pascal2
--    [[1],[1,1],[1,2,1],[1,3,3,1],[1,4,6,4,1],[1,5,10,10,5,1]]
-- ---------------------------------------------------------------------

pascal2 :: [[Integer]]
pascal2 = [1] : map f pascal2
  where f xs = zipWith (+) (0:xs) (xs++[0])

-- enrnarbej eliguivil roscargar pabrabmon antmorper3 cargonler
-- margirmon manruiber josdeher cescarde paumacpar beagongon1 antlopgom2
-- marjimcom
pascal2a :: [[Integer]]
pascal2a = [1] : map (\xs -> 1:zipWith (+) xs (tail xs)++[1]) pascal2

-- albcercid congomgom
pascal2b :: [[Integer]]
pascal2b = funcion [1]

funcion xs = xs:map (\(x:xs) -> x:zipWith (+) (x:xs) (xs++[0])) (funcion xs)

-- albcercid
pascal2c :: [[Integer]]
pascal2c = pascal [1]
  where pascal xs = xs:pascal (zipWith (+) (0:xs) (xs++[0]))

-- juaorture
pascal2d :: [[Integer]]
pascal2d = map pascalR [1..]

pascalR :: Int -> [Integer]
pascalR 1 = [1]
pascalR n = 1 : zipWith (+) p (tail p)++[1]
  where p = pascalR (n-1)

-- ---------------------------------------------------------------------
-- Ejercicio 11.3. Escribir la traza del cálculo de la expresión
--    take 4 pascal2
-- ---------------------------------------------------------------------

-- El cálculo es
--    take 4 pascal
--    = take 4 ([1] : map f pascal)
--    = [1] : (take 3 (map f pascal))    
--    = [1] : (take 3 (map f ([1]:R1pascal)))
--    = [1] : (take 3 ((f [1]) : map R1pascal)))
--    = [1] : (take 3 ((zipWith (+) (0:[1]) ([1]++[0]) : map R1pascal)))
--    = [1] : (take 3 ((zipWith (+) [0,1] [1,0]) : map R1pascal)))
--    = [1] : (take 3 ([1,1] : map R1pascal)))
--    = [1] : [1,1] : (take 2 (map R1pascal)))
--    = [1] : [1,1] : (take 2 (map ([1,1]:R2pascal)))
--    = [1] : [1,1] : (take 2 ((f [1,1]) : map R2pascal)))
--    = [1] : [1,1] : (take 2 ((zipWith (+) (0:[1,1]) ([1,1]++[0]) : map R2pascal)))
--    = [1] : [1,1] : (take 2 ((zipWith (+) [0,1,1] [1,1,0]) : map R2pascal)))
--    = [1] : [1,1] : (take 2 ([1,2,1] : map R2pascal)))
--    = [1] : [1,1] : [1,2,1] : (take 1 (map R2pascal)))
--    = [1] : [1,1] : [1,2,1] : (take 1 (map ([1,2,1]:R3pascal)))
--    = [1] : [1,1] : [1,2,1] : (take 1 ((f [1,2,1]) : map R3pascal)))
--    = [1] : [1,1] : [1,2,1] : (take 1 ((zipWith (+) (0:[1,2,1]) ([1,2,1]++[0]) : map R3pascal)))
--    = [1] : [1,1] : [1,2,1] : (take 1 ((zipWith (+) [0,1,2,1] [1,2,1,0]) : map R3pascal)))
--    = [1] : [1,1] : [1,2,1] : (take 1 ([1,3,3,1] : map R3pascal)))
--    = [1] : [1,1] : [1,2,1] : [1,3,3,1] : (take 0 (map R3pascal)))
--    = [1] : [1,1] : [1,2,1] : [1,3,3,1] : []
--    = [[1],[1,1],[1,2,1],[1,3,3,1]]
-- en el cálculo con R1pascal, R2pascal y R3pascal es el triángulo de
-- Pascal sin el primero, los dos primeros o los tres primeros elementos,
-- respectivamente. 
