-- I1M 2015-16: Rel_13.hs (27 de noviembre de 2015)
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
--    http://www.cs.us.es/~jalonso/cursos/i1m-14/temas/tema-10.pdf

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

-- juamorrom1 juanarcon alvalvdom1 josllagam ivaruicam manvermor rubvilval
-- carmengar silgongal erisancha marvilmor fracruzam
repite :: a -> [a]
repite x = x : repite x

-- Comentario: La definición anterior se puede mejorar.

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

-- fracruzam juamorrom1 juanarcon alvalvdom1 josllagam ivaruicam manvermor
-- rubvilval carmengar silgongal erisancha marvilmor

repiteC :: a -> [a]
repiteC x = [x | _ <- [1..]]

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

-- fracruzam alvalvdom1 josllagam ivaruicam manvermor carmengar silgongal
repiteFinitaR :: Int -> a -> [a]
repiteFinitaR 0 _ = []
repiteFinitaR n x = x : repiteFinitaR (n-1) x

-- juanarcon juamorrom1 rubvilval erisancha marvilmor
repiteFinitaR2 :: Int -> a -> [a]
repiteFinitaR2 n x  | n >= 1    = x : repiteFinitaR2 (n-1) x
                    | otherwise = []

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

-- fracruzam juamorrom1 juanarcon alvalvdom1 josllagam ivaruicam
-- manvermor rubvilval carmengar silgongal erisancha marvilmor

repiteFinitaC :: Int -> a -> [a]
repiteFinitaC n x = [x | _ <- [1..n]]

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

-- fracruzam juamorrom1 juanarcon alvalvdom1 josllagam ivaruicam
-- manvermor rubvilval carmengar silgongal erisancha marvilmor

repiteFinita :: Int -> a -> [a]
repiteFinita n = take n . repite

-- ---------------------------------------------------------------------
-- Ejercicio 2.4. Comprobar con QuickCheck que las funciones
-- repiteFinitaR, repiteFinitaC y repiteFinita son equivalentes a
-- replicate. 
--
-- Nota. Al hacer la comprobación limitar el tamaño de las pruebas como
-- se indica a continuación
--    quickCheckWith (stdArgs {maxSize=7}) prop_repiteFinitaEquiv
-- ---------------------------------------------------------------------

-- fracruzam alvalvdom1 manvermor

-- La propiedad es

-- prop_repiteFinitaEquiv :: Int -> Int -> Property 
prop_repiteFinitaEquiv n x = 
    n > 0 ==>
      r == repiteFinitaR n x && r == repiteFinitaC n x && r == repiteFinita n x
    where r = replicate n x

-- La comprobación es
--    *Main> quickCheckWith (stdArgs {maxSize=7}) prop_repiteFinitaEquiv
--    +++ OK, passed 100 tests.

-- juamorrom1 juanarcon josllagam  ivaruicam rubvilval carmengar silgongal
-- erisancha marvilmor

-- La propiedad es
prop_repiteFinitaEquiv2 :: Int -> Int -> Bool
prop_repiteFinitaEquiv2 n x = a == b && b == c && c == d
    where a = replicate n x
          b = repiteFinita n x
          c = repiteFinitaR2 n x
          d = repiteFinitaC n x

-- La comprobación es
--    *Main> quickCheckWith (stdArgs {maxSize=7}) prop_repiteFin-- itaEquiv
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 2.5. Comprobar con QuickCheck que la longitud de
-- (repiteFinita n x) es n, si n es positivo y 0 si no lo es.
--
-- Nota. Al hacer la comprobación limitar el tamaño de las pruebas como
-- se indica a continuación
--    quickCheckWith (stdArgs {maxSize=30}) prop_repiteFinitaLongitud
-- ---------------------------------------------------------------------

-- fracruzam juamorrom1 juanarcon alvalvdom1 josllagam ivaruicam
-- manvermor rubvilval carmengar silgongal erisancha marvilmor

-- La propiedad es
prop_repiteFinitaLongitud :: Int -> Int -> Bool
prop_repiteFinitaLongitud n x | n > 0     = longitud == n
                              | otherwise = longitud == 0
    where longitud = length $ repiteFinita n x

-- La comprobación es
--    *Main> quickCheckWith (stdArgs {maxSize=30}) prop_repiteFinitaLongitud
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 2.6. Comprobar con QuickCheck que todos los elementos de 
-- (repiteFinita n x) son iguales a x.
-- ---------------------------------------------------------------------

-- fracruzam juamorrom1 juanarcon alvalvdom1 josllagam ivaruicam
-- manvermor rubvilval silgongal erisancha marvilmor

-- La propiedad es
prop_repiteFinitaIguales :: Int -> Int -> Bool
prop_repiteFinitaIguales n x = all (x ==) $ repiteFinita n x

-- La comprobación es
--    *Main> quickCheck prop_repiteFinitaIguales
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Definir, por comprensión, la función
--    ecoC :: String -> String
-- tal que (ecoC xs) es la cadena obtenida a partir de la cadena xs
-- repitiendo cada elemento tantas veces como indica su posición: el
-- primer elemento se repite 1 vez, el segundo 2 veces y así
-- sucesivamente. Por ejemplo, 
--    ecoC "abcd"  ==  "abbcccdddd"
-- ---------------------------------------------------------------------

-- fracruzam
ecoC xs = concatMap replicate' (zip [1..] xs)
    where replicate' :: (Int,Char) -> [Char]
          replicate' (x,y) = replicate x y

-- juanarcon josllagam juamorrom1 manvermor alvalvdom1 silgongal erisancha
-- marvilmor
ecoC2 :: String -> String
ecoC2 xs = concat [replicate n x | (n,x) <- zip [1..] xs]

-- ivaruicam
ecoC3 :: String -> String
ecoC3 xs = concat [x | x <- zipWith (replicate) [1..] xs]

-- Comentario: La definición anterior se puede simplificar.

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Definir, por recursión, la función
--    ecoR :: String -> String
-- tal que (ecoR xs) es la cadena obtenida a partir de la cadena xs
-- repitiendo cada elemento tantas veces como indica su posición: el
-- primer elemento se repite 1 vez, el segundo 2 veces y así
-- sucesivamente. Por ejemplo, 
--    ecoR "abcd"  ==  "abbcccdddd"
-- ---------------------------------------------------------------------

-- fracruzam juamorrom1 juanarcon josllagam ivaruicam alvalvdom1 silgongal
-- erisancha marvilmor

ecoR :: String -> String
ecoR = ecoR_aux 1
    where ecoR_aux :: Int -> String -> String
          ecoR_aux _ []    = []
          ecoR_aux n (x:xs) = replicate n x ++ ecoR_aux (n+1) xs

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

-- fracruzam juamorrom1 juanarcon alvalvdom1 josllagam ivaruicam
-- manvermor silgongal erisancha marvilmor

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

-- fracruzam juamorrom1 juanarcon alvalvdom1 ivaruicam josllagam silgongal
-- manvermor erisancha

agrupaR :: Int -> [a] -> [[a]]
agrupaR _ [] = []
agrupaR n xs = take n xs : agrupaR n (drop n xs)

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

-- fracruzam erisancha

agrupa :: Int -> [a] -> [[a]]
agrupa n = takeWhile (not.null) . map (take n) . iterate (drop n) 

-- ----------------------------------------------------------------------------
-- Ejercicio 5.3. Comprobar con QuickCheck que todos los grupos de
-- (agrupa n xs) tienen longitud n (salvo el último que puede tener una
-- longitud menor). 
-- ---------------------------------------------------------------------------- 

-- fracruzam

-- La propiedad es
prop_AgrupaLongitud :: Int -> [Int] -> Property
prop_AgrupaLongitud n xs =
    n > 0 && xs /= [] ==> all (\x -> length x == n) (init agrupanxs) &&
                          length (last $ agrupanxs) <= n
    where agrupanxs = agrupa n xs

-- La comprobación es
--    *Main> quickCheck prop_AgrupaLongitud
--    +++ OK, passed 100 tests.

-- ivaruicam erisancha

prop_AgrupaLongitud2 :: Int -> [Int] -> Property
prop_AgrupaLongitud2 n xs = n > 0 ==> all (<= n) (map (length) (agrupaR n xs))

-- ----------------------------------------------------------------------------
-- Ejercicio 5.4. Comprobar con QuickCheck que combinando todos los
-- grupos de (agrupa n xs) se obtiene la lista xs. 
-- ---------------------------------------------------------------------------- 

-- fracruzam ivaruicam juamorrom1 manvermor

-- La segunda propiedad es
prop_AgrupaCombina :: Int -> [Int] -> Property
prop_AgrupaCombina n xs = 
    n > 0 && xs /= []  ==> xs == foldr1 (++) (agrupa n xs) 

-- La comprobación es
--    *Main> quickCheck prop_AgrupaCombina
--    +++ OK, passed 100 tests.

-- erisancha
prop_AgrupaCombina2 :: Int -> [Int] -> Property
prop_AgrupaCombina2 n xs = n > 0 ==> concat (agrupa n xs) == xs

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

-- fracruzam juamorrom1 alvalvdom1 ivaruicam josllagam manvermor
-- erisancha silgongal 
siguiente :: Integer -> Integer
siguiente n | even n    = div n 2
            | otherwise = 3 * n + 1

-- ---------------------------------------------------------------------
-- Ejercicio 6.2. Definir, por recursión, la función 
--    collatzR :: Integer -> [Integer]
-- tal que (collatzR n) es la órbita de CollatzR de n hasta alcanzar el
-- 1. Por ejemplo,
--    collatzR 13  ==  [13,40,20,10,5,16,8,4,2,1]
-- ---------------------------------------------------------------------

-- fracruzam juamorrom1 alvalvdom1 ivaruicam manvermor erisancha
-- silgongal 
collatzR :: Integer -> [Integer]
collatzR 1 = [1]
collatzR n = n : collatzR (siguiente n)

-- josllagam
collatzR2 :: Integer -> [Integer]
collatzR2 1 = [1]
collatzR2 n | even n     = [n] ++ collatzR2 (n `div` 2)
            | otherwise  = [n] ++ collatzR2 (n * 3 + 1)

-- Comentario: La definición anterior se puede mejorar.

-- ---------------------------------------------------------------------
-- Ejercicio 6.3. Definir, sin recursión y con iterate, la función 
--    collatz :: Integer -> [Integer]
-- tal que (collatz n) es la órbita de Collatz d n hasta alcanzar el
-- 1. Por ejemplo,
--    collatz 13  ==  [13,40,20,10,5,16,8,4,2,1]
-- Indicación: Usar takeWhile e iterate.
-- ---------------------------------------------------------------------

-- fracruzam juamorrom1 alvalvdom1 ivaruicam manvermor josllagam erisancha
--silgongal

collatz :: Integer -> [Integer]
collatz n = takeWhile (/=1) (iterate siguiente n) ++ [1]

-- ---------------------------------------------------------------------
-- Ejercicio 6.4. Definir la función
--    menorCollatzMayor :: Int -> Integer
-- tal que (menorCollatzMayor x) es el menor número cuya órbita de
-- Collatz tiene más de x elementos. Por ejemplo,
--    menorCollatzMayor 100  ==  27
-- ---------------------------------------------------------------------

-- fracruzam
menorCollatzMayor :: Int -> Integer
menorCollatzMayor x = 
    fromIntegral $ length (takeWhile (<x) (map length (map collatz [1..]))) + 1

-- Comentario: La definición anterior se puede simplificar.

-- juamorrom1 alvalvdom1 ivaruicam manvermor josllagam silgongal erisancha
menorCollatzMayor1 :: Int -> Integer
menorCollatzMayor1 x = head [n | n <- [1..], length (collatz n) > x]

-- ---------------------------------------------------------------------
-- Ejercicio 6.5. Definir la función
--    menorCollatzSupera :: Integer -> Integer
-- tal que (menorCollatzSupera x) es el menor número cuya órbita de
-- Collatz tiene algún elemento mayor que x. Por ejemplo,
--    menorCollatzSupera 100  ==  15
-- ---------------------------------------------------------------------

-- juamorrom1 alvalvdom1 ivaruicam manvermor josllagam silgongal erisancha
menorCollatzSupera :: Integer -> Integer
menorCollatzSupera x = head [n | n <- [1..], any (>x) (collatz n)]

-- fracruzam
menorCollatzSupera2 :: Integer -> Integer
menorCollatzSupera2 x = 
    (fromIntegral $ 
     length $ 
     takeWhile not $ 
     map (any (>x)) $ 
     map collatz [1..]) + 1

-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir, usando takeWhile y map, la función
--    potenciasMenores :: Int -> Int -> [Int]
-- tal que (potenciasMenores x y) es la lista de las potencias de x
-- menores que y. Por ejemplo,
--    potenciasMenores 2 1000  ==  [2,4,8,16,32,64,128,256,512]
-- ---------------------------------------------------------------------

-- fracruzam juamorrom1 alvalvdom1 ivaruicam josllagam silgongal erisancha
potenciasMenores :: Int -> Int -> [Int]
potenciasMenores x y = takeWhile (<y) $ map (x^) [1..]

-- ---------------------------------------------------------------------
-- Ejercicio 8.1. Definir, usando la criba de Eratóstenes, la constante
--    primos :: Integral a => [a]
-- cuyo valor es la lista de los números primos. Por ejemplo,
--    take 10 primos  ==  [2,3,5,7,11,13,17,19,23,29]
-- ---------------------------------------------------------------------

-- fracruzam juamorrom1 alvalvdom1 ivaruicam manvermor josllagam silgongal
-- erisancha

primos :: Integral a => [a]
primos = criba [2..]
  where criba :: Integral a => [a] -> [a]
        criba (p:xs) = p : criba [x | x <- xs, x `mod` p /= 0]

-- ---------------------------------------------------------------------
-- Ejercicio 8.2. Definir, usando primos, la función
--    primo :: Integral a => a -> Bool
-- tal que (primo n) se verifica si n es primo. Por ejemplo,
--    primo 7  ==  True
--    primo 9  ==  False
-- ---------------------------------------------------------------------

-- juamorrom1 josllagam silgongal
primo :: Int -> Bool
primo n = elem n (takeWhile (<=n) primos)

-- Comentario: La definición anterior se puede mejorar.

-- ivaruicam manvermor erisancha
primo1 :: Int -> Bool
primo1 n = elem n (take n primos) 

-- Comentario: La definición anterior se puede mejorar.

-- alvalvdom1 
primo2 :: Integral a => a -> Bool
primo2 n = head [x | x <- primos, x >= n] == n

-- fracruzam
primo3 :: Int -> Bool
primo3 n = n == last (takeWhile (<=n) primos)

-- Comentario: La definición anterior se puede mejorar.

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

-- ivaruicam juamorrom1 manvermor josllagam silgongal fracruzam
sumaDeDosPrimos :: Int -> [(Int,Int)]
sumaDeDosPrimos n = 
    [(x,y) | x <- primos', y <- primos', x+y == n, x <= y]
    where primos' = takeWhile (<n) primos

-- Comentario: La definición anterior se puede mejorar.

-- erisancha
sumaDeDosPrimos2 :: Int -> [(Int,Int)]
sumaDeDosPrimos2 n = [(x,n-x)| x <- primoh, x <= n-x, (n-x) `elem` primoh]
    where primoh = takeWhile (<=n) primos

-- Comentario: La definición anterior se puede mejorar.

-- El cálculo es
menorSumaDeDosPrimos = 
    head [n | n <- [1..], length (sumaDeDosPrimos n) == 10]

-- El menor número que puede escribirse de 10 formas distintas como 
-- suma de dos primos es el 114.

-- ---------------------------------------------------------------------
-- § La lista infinita de factoriales,                                --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 9.1. Definir, por comprensión, la función
--    factoriales1 :: [Integer]
-- tal que factoriales1 es la lista de los factoriales. Por ejemplo,
--    take 10 factoriales1  ==  [1,1,2,6,24,120,720,5040,40320,362880]
-- ---------------------------------------------------------------------

-- juamorrom1
factoriales1 :: [Integer]
factoriales1 = map (factorial) [0..]

-- Comentario: La definición anterior se puede simplificar.

factorial n = if n < 2 then 1 else n * factorial (n-1)

-- ivaruicam
factoriales11:: [Integer]
factoriales11 = map (factorial1) [0..]
    where factorial1 n = product [1..n]

-- Comentario: La definición anterior se puede simplificar.

-- manvermor erisancha josllagam

factoriales12 :: [Integer]
factoriales12 = [factorial2 n | n <- [0..]]

factorial2 n = product [1..n]

-- fracruzam
factoriales13 :: [Integer]
factoriales13 = [product [1..x] | x <- 1:[1..]]

-- Comentario: La definición anterior se puede simplificar.

-- ---------------------------------------------------------------------
-- Ejercicio 9.2. Definir, usando zipWith, la función
--    factoriales2 :: [Integer]
-- tal que factoriales2 es la lista de los factoriales. Por ejemplo,
--    take 10 factoriales2  ==  [1,1,2,6,24,120,720,5040,40320,362880]
-- ---------------------------------------------------------------------

-- erisancha josllagam

factoriales2 :: [Integer]
factoriales2 = 1 : zipWith (*) [1..] factoriales2

-- fracruzam
factoriales22 :: [Integer]
factoriales22 = zipWith (\x y -> product [x..y]) [1,1..] (1:[1..])

-- ---------------------------------------------------------------------
-- Ejercicio 9.3. Definir, por recursión, la función
--    factoriales3 :: [Integer]
-- tal que factoriales3 es la lista de los factoriales. Por ejemplo,
--    take 10 factoriales3  ==  [1,1,2,6,24,120,720,5040,40320,362880]
-- ---------------------------------------------------------------------

-- juamorrom1
factoriales3 :: [Integer]
factoriales3 = 1 : aux 1 [1..]
    where aux x (y:ys) = (x*y) : aux (x*y) ys

-- Comentario: La definición anterior se puede mejorar.

-- manvermor erisancha
factoriales3a :: [Integer]
factoriales3a = aux 0
    where  aux n = factorial2 n : aux (n+1)

-- Comentario: La definición anterior se puede mejorar.

-- fracruzam
factoriales3b :: [Integer]
factoriales3b = 1 : auxFactoriales3b 1 1
  where auxFactoriales3b :: Integer -> Integer -> [Integer]
        auxFactoriales3b n y = z : auxFactoriales3b (n+1) z
            where z = n*y

-- ---------------------------------------------------------------------
-- Ejercicio 9.4. Definir, usando scanl1, la función
--    factoriales4 :: [Integer]
-- tal que factoriales4 es la lista de los factoriales. Por ejemplo,
--    take 10 factoriales4  ==  [1,1,2,6,24,120,720,5040,40320,362880]
-- ---------------------------------------------------------------------

-- erisancha fracruzam josllagam

factoriales4 :: [Integer]
factoriales4 = 1 : scanl1 (*) [1..]

-- ---------------------------------------------------------------------
-- Ejercicio 9.5. Definir, usando iterate, la función
--    factoriales5 :: [Integer]
-- tal que factoriales5 es la lista de los factoriales. Por ejemplo,
--    take 10 factoriales5  ==  [1,1,2,6,24,120,720,5040,40320,362880]
-- ---------------------------------------------------------------------

factoriales5 :: [Integer]
factoriales5 = undefined

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

-- ivaruicam juamorrom1 erisancha fracruzam
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- ---------------------------------------------------------------------
-- Ejercicio 10.2. Definir, por comprensión, la función
--    fibs1 :: [Integer]
-- tal que fibs1 es la sucesión de Fibonacci. Por ejemplo,
--    take 10 fibs1  ==  [0,1,1,2,3,5,8,13,21,34]
-- ---------------------------------------------------------------------

-- juamorrom1 ivaruicam
fibs1 :: [Integer]
fibs1 = map (fib) [0..]

-- Comentario: La definición anterior se puede simplificar

-- erisancha fracruzam
fibs1a :: [Integer]
fibs1a = map fib [0..]

-- erisancha
fibs1b :: [Integer]
fibs1b = [fib x | x<- [0..]]

-- fracruzam
fibs1c :: [Integer]
fibs1c = 0:1:[fibs1b !! n + fibs1b !! (n+1) | n <- [0..]]

-- ---------------------------------------------------------------------
-- Ejercicio 10.3. Definir, por recursión, la función
--    fibs2 :: [Integer]
-- tal que fibs2 es la sucesión de Fibonacci. Por ejemplo,
--    take 10 fibs2  ==  [0,1,1,2,3,5,8,13,21,34]
-- ---------------------------------------------------------------------

-- juamorrom1
fibs2 :: [Integer]
fibs2 = aux xs
      where aux (x:xs) = fib x:(aux xs)
            xs = [0..]

-- Comentario: La definición anterior se puede mejorar sin usar la
-- función fib.

-- ivaruicam erisancha fracruzam
fibs21 :: [Integer]
fibs21 = aux 0
    where aux n = fib n : aux (n+1)

-- Comentario: La definición anterior se puede mejorar.

-- fracruzam
fibs2b :: [Integer]
fibs2b = 0 : 1 : auxfibs2b 0
    where auxfibs2b :: Int -> [Integer]
          auxfibs2b n = fibs2b !! n + fibs2b !! (n+1) : auxfibs2b (n+1)

-- Comentario: La definición anterior se puede mejorar.

-- ---------------------------------------------------------------------
-- Ejercicio 10.4. Definir, por recursión con zipWith, la función
--    fibs3 :: [Integer]
-- tal que fibs3 es la sucesión de Fibonacci. Por ejemplo,
--    take 10 fibs3  ==  [0,1,1,2,3,5,8,13,21,34]
-- ---------------------------------------------------------------------

-- erisancha fracruzam
fibs3 :: [Integer]
fibs3 = 0 : 1 : zipWith (+) fibs3 (tail fibs3)

-- ---------------------------------------------------------------------
-- Ejercicio 10.5. Definir, por recursión con acumuladores, la función 
--    fibs4 :: [Integer]
-- tal que fibs4 es la sucesión de Fibonacci. Por ejemplo,
--    take 10 fibs4  ==  [0,1,1,2,3,5,8,13,21,34]
-- ---------------------------------------------------------------------

-- fracruzam
fibs4 :: [Integer]
fibs4 = 0 : 1 : auxFibs4 [0,1]
    where auxFibs4 :: [Integer] -> [Integer]
          auxFibs4 v@[_,b] = sumv : auxFibs4 [b,sumv]
              where sumv = sum v

-- Comentario: La definición anterior se puede simplificar.

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
-- * la primera fila está formada por el número 1;
-- * las filas siguientes se construyen sumando los números adyacentes
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

-- erisancha

pascal1 :: [[Integer]]
pascal1 = iterate z [1]
    where z n = zipWith (+) (0:n) (n++[0])

-- fracruzam
pascal12 :: [[Integer]]
pascal12 = 
    iterate (\xs -> 1:(zipWith (\x y -> x+y) xs (tail xs)) ++ [1]) [1]

-- ---------------------------------------------------------------------
-- Ejercicio 11.2. Definir, con map y zipWith, la función
--    pascal2 :: [[Integer]]
-- tal que pascal es la lista de las líneas del triángulo de Pascal. Por
-- ejemplo, 
--    ghci> take 6 pascal2
--    [[1],[1,1],[1,2,1],[1,3,3,1],[1,4,6,4,1],[1,5,10,10,5,1]]
-- ---------------------------------------------------------------------

-- erisancha
pascal2 :: [[Integer]]
pascal2 = [1] : map z pascal2
    where z n = zipWith (+) (0:n) (n++[0])

-- ---------------------------------------------------------------------
-- Ejercicio 11.3. Escribir la traza del cálculo de la expresión
--    take 4 pascal2
-- ---------------------------------------------------------------------

-- erisancha

-- take 4 pascal2 = [1] : []
-- take 4 pascal2 = [[1]]
-- take (4-1) pascal2 = [1] : [[1] : [1]] : []
-- take 3 pascal2 = [[1],[1,1],[]]
-- take (3-1) pascal2 = [1] : [1,1] : [[1] : [2,1]] : []
-- take 2 pascal2 = [[1],[1,1],[1,2,1],[]]
-- take (2-1) pascal2 = [1] : [1,1] : [1,2,1] : [[1] : [3,3,1]] : []
-- take 1 pascal2 = [[1],[1,1],[1,2,1],[1,3,3,1],[]]
-- take (1-1) pascal2 = [1] : [1,1] : [1,2,1] : [1,3,3,1] : []
-- take 0 pascal2 = [[1],[1,1],[1,2,1],[1,3,3,1]]
