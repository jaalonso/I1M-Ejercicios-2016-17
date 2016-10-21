-- I1M 2016-17: Rel_4.hs (18 de octubre de 2015)
-- Definiciones por recursión (1)
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- En esta relación se presentan ejercicios con definiciones por
-- recursión correspondientes al tema 6 cuyas transparencias se 
-- encuentran en  
--    http://www.cs.us.es/~jalonso/cursos/i1m-16/temas/tema-6.html
 
-- ---------------------------------------------------------------------
-- Importación de librerías auxiliares                                --
-- ---------------------------------------------------------------------

import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Definir por recursión la función
--    potencia :: Integer -> Integer -> Integer
-- tal que (potencia x n) es x elevado al número natural n. Por ejemplo,  
--    potencia 2 3  ==  8
-- ---------------------------------------------------------------------

-- enrnarbej artmorfer manruiber eliguivil eledejim2 albagucen pabrabmon
-- migibagar paumacpar javcancif cargonler antmorper3 juacasnie
-- congomgom josjimgon2 margirmon margarflo5 albcercid margarvil14
-- josdeher roscargar glovizcas fatfervaz juaorture joscasgom1 ignareeva
-- marjimcom beagongon1 marmerzaf carmarcar5 felsuacor fraferpoy
-- belbenzam natruipin criortcar
potencia :: Integer -> Integer -> Integer
potencia _ 0 = 1
potencia x n = x * potencia x (n-1)

-- luimotmar marlobrip
potencia2 :: Integer -> Integer -> Integer
potencia2 1 n = 1
potencia2 x 0 = 1
potencia2 x n = x * potencia2 x (n-1)

-- Comentario: La definición potencia2 se puede simplificar.

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Comprobar con QuickCheck que la función potencia es
-- equivalente a la predefinida (^).
-- ---------------------------------------------------------------------

-- La propiedad es

-- enrnarbej manruiber artmorfer eliguivil eledejim2 pabrabmon paumacpar  
-- javcancif cargonler antmorper3 juacasnie congomgom josjimgon2
-- margarflo5 albcercid josdeher roscargar glovizcas juaorture
-- joscasgom1 migibagar ignareeva fatfervaz luimotmar marjimcom
-- beagongon1 marmerzaf carmarcar5 margirmon felsuacor fraferpoy
-- marlobrip belbenzam natruipin criortcar
prop_potencia :: Integer -> Integer -> Property
prop_potencia x n = n >= 0 ==> potencia x n == x ^ n

-- Prelude> quickCheck prop_potencia
-- +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Dados dos números naturales, a y b, es posible
-- calcular su máximo común divisor mediante el Algoritmo de
-- Euclides. Este algoritmo se puede resumir en la siguiente fórmula:
--    mcd(a,b) = a,                   si b = 0
--             = mcd (b, a módulo b), si b > 0
-- 
-- Definir la función 
--    mcd :: Integer -> Integer -> Integer
-- tal que (mcd a b) es el máximo común divisor de a y b calculado
-- mediante el algoritmo de Euclides. Por ejemplo,
--    mcd 30 45  ==  15
-- ---------------------------------------------------------------------

-- enrnarbej manruiber eliguivil artmorfer eledejim2 pabrabmon paumacpar
-- cargonler antmorper3 josjimgon2 margarflo5 albcercid margarvil14 
-- josdeher congomgom glovizcas fatfervaz joscasgom1 migibagar ignareeva
-- beagongon1 marmerzaf margirmon felsuacor marlobrip fraferpoy
mcd :: Integer -> Integer -> Integer
mcd a 0 = a
mcd a b = mcd b (mod a b)

-- juacasnie roscargar juaorture belbenzam
mcd2 :: Integer -> Integer -> Integer
mcd2 a b | b == 0 = a
         | b > 0  = mcd2 b (a `mod` b)

-- Comentario: La definición mcd2 se puede simplificar.

-- luimotmar marjimcom
mcd3 :: Integer -> Integer -> Integer
mcd3 a 0 = a
mcd3 0 b = b
mcd3 a b = mcd b (mod a b)

-- Comentario: La definición mcd3 se puede simplificar.

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Definir y comprobar la propiedad prop_mcd según la
-- cual el máximo común divisor de dos números a y b (ambos mayores que
-- 0) es siempre mayor o igual que 1 y además es menor o igual que el
-- menor de los números a  y b. 
-- ---------------------------------------------------------------------

-- La propiedad es

-- enrnarbej manruiber artmorfer pabrabmon cargonler antmorper3
-- joscasgom1 ignareeva luimotmar margirmon belbenzam 
prop_mcd :: Integer -> Integer -> Property
prop_mcd a b =
  a > 0 && b > 0 ==>
  mcd a b >= 1 && mcd a b <= minimum [a,b]

-- Comentario: La definición anterior se puede mejorar.

-- eliguivil paumacpar 
prop_mcd2 :: Integer -> Integer -> Property
prop_mcd2 a b =
  a > 0 && b > 0 ==>
  m >= 1 && m <= min a b
  where m = mcd a b

-- eledejim2 josjimgon2 margarflo5 albcercid josdeher congomgom roscargar
-- juaorture glovizcas fatfervaz migibagar beagongon1 marjimcom
-- marmerzaf felsuacor natruipin
prop_mcd3 :: Integer -> Integer -> Property
prop_mcd3 a b =
  a > 0 &&  b > 0 ==>
  mcd a b >= 1 && mcd a b <= min a b

-- Comentario: La definición prop_mcd3 se puede mejorar.

-- juacasnie
prop_mcd4 :: Integer -> Integer -> Property
prop_mcd4 a b =
  (a > 0) && (b > 0) && (a < b) ==> mcd a b >= 1 && mcd a b <= a

-- La definición prop_mcd4 es incompleta.

-- Su comprobación es
--    Prelude> quickCheck prop_mcd 
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 2.3. Teniendo en cuenta que buscamos el máximo común
-- divisor de a y b, sería razonable pensar que el máximo común divisor
-- siempre sería igual o menor que la mitad del máximo de a y b. Definir
-- esta propiedad y comprobarla.  
-- ---------------------------------------------------------------------

-- La propiedad es

-- enrnarbej manruiber artmorfer eledejim2 pabrabmon paumacpar cargonler
-- roscargar antmorper3 albcercid joscasgom1 luimotmar beagongon1
-- marjimcom margirmon felsuacor belbenzam 
prop_mcd_div :: Integer -> Integer -> Property
prop_mcd_div a b =
  a > 0 && b > 0 && a /= b ==>
  mcd a b <= div (maximum [a,b]) 2

-- Comentario: La condición (a /= b) no está en el enunciado.

-- Prelude> quickCheck prop_mcd_div
-- +++ OK, passed 100 tests.

-- juacasnie juaorture fatfervaz ignareeva marmerzaf natruipin
prop_mcd_div2 :: Integer -> Integer -> Property
prop_mcd_div2 a b =
  (a > 0) && (b > 0) ==>
  mcd a b <= (max a b) `div` 2

-- La comprobación es
--    λ> quickCheck prop_mcd_div2
--    *** Failed! Falsifiable (after 1 test): 
--    1
--    1

-- prop_mcd_div3 :: Integer -> Integer -> Property
prop_mcd_div3 a b =
  a > 1 && b > 1 ==>
  mcd a b <= (max a b) `div` 2

-- La comprobación es
--    λ> quickCheck prop_mcd_div3
--    *** Failed! Falsifiable (after 1 test): 
--    1
--    1

-- josjimgon2 josdeher congomgom margarflo5 glovizcas 
prop_mcd_div4 :: Integer -> Integer -> Property
prop_mcd_div4 a b =
  a > 0 && b > 0 && a /= b ==>
  mcd a b <= div (max a b) 2

-- Comentario: La condición (a /= b) no está en el enunciado.

-- migibagar
prop_mcd_div5 :: Integer -> Integer -> Property
prop_mcd_div5 a b =
  a > 0 && b > 0  ==>
  mcd a b <= (div (max a b) 2)

-- La comprobación es:
--    λ> quickCheck prop_mcd_div5
--    *** Failed! Falsifiable (after 1 test): 
--    1
--    1

-- ---------------------------------------------------------------------
-- Ejercicio 3.1, Definir por recursión la función
--    pertenece :: Eq a => a -> [a] -> Bool
-- tal que (pertenece x xs) se verifica si x pertenece a la lista xs. Por
-- ejemplo, 
--    pertenece 3 [2,3,5]  ==  True
--    pertenece 4 [2,3,5]  ==  False
-- ---------------------------------------------------------------------

-- enrnarbej manruiber artmorfer eledejim2 antmorper3 fatfervaz
pertenece :: Eq a => a -> [a] -> Bool
pertenece _ []     = False
pertenece n (x:xs) = n == x || pertenece n xs

-- eliguivil pabrabmon paumacpar cargonler javcancif juacasnie josjimgon2
-- albcercid margarvil14 josdeher congomgom roscargar margarflo5 juaorture
-- glovizcas joscasgom1 ignareeva beagongon1 marjimcom margirmon
-- felsuacor fraferpoy belbenzam natruipin
pertenece2 :: Eq a => a -> [a] -> Bool
pertenece2 n [] = False
pertenece2 n (x:xs) | n == x    = True
                    | otherwise = pertenece2 n xs

-- Comentario: La definición anterior se puede simplificar.

-- luimotmar
pertenece3 :: Eq a => a -> [a] -> Bool
pertenece3 x [] = False
pertenece3 x xs = if head xs == x
                  then True
                  else pertenece x (tail xs)

-- Comentario: La definición anterior se puede simplificar.

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Comprobar con quickCheck que pertenece es equivalente
-- a elem. 
-- ---------------------------------------------------------------------

-- enrnarbej eliguivil manruiber josdeher margarflo5 juaorture glovizcas
-- joscasgom1 margirmon belbenzam

-- La propiedad es
prop_pertenece :: Eq a => a -> [a] -> Bool
prop_pertenece x xs = (pertenece x xs) == (x `elem` xs)

-- La comprobación es
-- Prelude> quickCheck prop_pertenece
-- +++ OK, passed 100 tests.

-- artmorfer eledejim2 pabrabmon paumacpar cargonler antmorper3 javcancif
-- juacasnie josjimgon2 albcercid margarvil14 congomgom roscargar
-- fatfervaz ignareeva luimotmar fraferpoy beagongon1 marjimcom felsuacor
--natruipin
prop_pertenece2 :: Eq a => a -> [a] -> Bool
prop_pertenece2 x xs = pertenece x xs == elem x xs

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. Definir por recursión la función
--    concatenaListas :: [[a]] -> [a]
-- tal que (concatenaListas xss) es la lista obtenida concatenando las listas de
-- xss. Por ejemplo,
--    concatenaListas [[1..3],[5..7],[8..10]]  ==  [1,2,3,5,6,7,8,9,10]
-- ---------------------------------------------------------------------
 
-- enrnarbej manruiber artmorfer pabrabmon eledejim2 cargonler paumacpar 
-- antmorper3 juacasnie javcancif josjimgon2 albcercid margarvil14
-- roscargar josdeher congomgom margarflo5 glovizcas joscasgom1
-- fatfervaz ignareeva luimotmar beagongon1 marjimcom margirmon
-- felsuacor fraferpoy juaorture belbenzam natruipin
concatenaListas :: [[a]] -> [a]
concatenaListas [] = []
concatenaListas (x:xs) = x ++ concatenaListas xs

-- eliguivil
concatenaListas2 :: Eq a => [[a]] -> [a]
concatenaListas2 [] = []
concatenaListas2 ((x:xs):xss) | xs /= [] = x:concatenaListas (xs:xss)
                              | xs == [] = x:concatenaListas xss

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Comprobar con QuickCheck que concatenaListas es
-- equivalente a concat. 
-- ---------------------------------------------------------------------

-- enrnarbej eliguivil manruiber artmorfer pabrabmon eledejim2 cargonler 
-- paumacpar juacasnie antmorper3 javcancif josjimgon2 albcercid
-- margarvil14 josdeher congomgom margarflo5 juaorture glovizcas
-- joscasgom1 fatfervaz ignareeva luimotmar beagongon1 marjimcom
-- margirmon felsuacor fraferpoy roscargar belbenzam natruipin

-- La propiedad es
prop_concat :: Eq a => [[a]] -> Bool
prop_concat xss = concatenaListas xss == concat xss

-- La comprobación es
-- Prelude> quickCheck prop_concat
-- +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 5.1. Definir por recursión la función
--    coge :: Int -> [a] -> [a]
-- tal que (coge n xs) es la lista de los n primeros elementos de
-- xs. Por ejemplo, 
--    coge   3  [4..12]  ==  [4,5,6]
--    coge (-3) [4..12]  ==  []
-- ---------------------------------------------------------------------

-- enrnarbej manruiber artmorfer pabrabmon eledejim2 cargonler paumacpar  
-- antmorper3 javcancif josjimgon2 margarflo5 glovizcas joscasgom1
-- ignareeva beagongon1 marjimcom felsuacor fraferpoy roscargar
-- natruipin belbenzam
coge :: Int -> [a] -> [a]
coge _ []                 = []
coge n (x:xs) | n <= 0    = []
              | otherwise = x : coge (n-1) xs

-- eliguivil albcercid josdeher congomgom fatfervaz
coge2 :: Int -> [a] -> [a]
coge2 _ []                 = []
coge2 n (x:xs) | n > 0     = x : coge2 (n-1) xs
               | otherwise = []

-- juacasnie
coge3 :: Int -> [a] -> [a]
coge3 n []              = []
coge3 n (x:xs) | n <= 0 = []
               | n > 0 = [x] ++ coge3 (n-1) xs

-- juaorture
coge4 :: Int -> [a] -> [a]
coge4 1 (x:xs) = [x]
coge4 n (x:xs) | n > 0 = x : (coge4 (n-1) xs)
               | otherwise = []
coge4 _ [] = []

-- Comentario: La definición coge4 se puede simplificar.

-- luimotmar
coge5 :: Int -> [a] -> [a]
coge5 _ [] = []
coge5 0 xs = []
coge5 n xs = head xs : coge5 (n-1) (tail xs)

-- Comentario: La definición coge5 se puede simplificar.

-- margirmon
coge6 :: Int -> [a] -> [a]
coge6 _ []     = []
coge6 0 xs     = []
coge6 n (x:xs) = x: coge6 (n-1) xs

-- ---------------------------------------------------------------------
-- Ejercicio 5.2. Comprobar con QuickCheck que coge es equivalente a
-- take. 
-- ---------------------------------------------------------------------

-- La propiedad es

-- enrnarbej eliguivil manruiber pabrabmon eledejim2 cargonler paumacpar
-- roscargar antmorper3 juacasnie javcancif josjimgon2 albcercid
-- josdeher congomgom margarflo5 juaorture glovizcas joscasgom1
-- ignareeva fatfervaz beagongon1 marjimcom felsuacor fraferpoy
-- belbenzam margarvil14 natruipin
prop_coge :: Int -> [Int] -> Bool
prop_coge n xs = coge n xs == take n xs

-- luimotmar margirmon
prop_coge2 :: Int -> [Int] -> Property
prop_coge2 n xs = n > 1 ==> coge n xs == take n xs

-- La comprobación es
-- Prelude> quickCheck prop_coge
-- +++ OK, passed 100 tests.
