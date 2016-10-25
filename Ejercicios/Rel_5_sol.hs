-- I1M 2016-17: Rel_5.hs (21 de octubre de 2016)
-- Definiciones por recursión (2).
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
import Data.List
import Data.Numbers.Primes

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Definir, por recursión, la función 
--    sumaCuadradosR :: Integer -> Integer
-- tal que (sumaCuadradosR n) es la suma de los cuadrados de los números
-- de 1 a n. Por ejemplo, 
--    sumaCuadradosR 4  ==  30 
-- ---------------------------------------------------------------------

-- enrnarbej albcercid eliguivil josrodgal7 fatfervaz manruiber
-- juaorture glovizcas roscargar antdursan eledejim2 paumacpar marjimcom 
-- carmarcar5 natmarmar2 belbenzam pabrabmon alvfercen cargonler margirmon
-- antmorper3 antbeacar artmorfer cescarde marmerzaf criortcar ignareeva 
-- javcancif juacasnie josdeher margarvil14 natruipin margarflo5
sumaCuadradosR :: Integer -> Integer
sumaCuadradosR 1 = 1
sumaCuadradosR n = n^2 + sumaCuadradosR (n-1)

--fraferpoy
--sumaCuadradosR2 :: Integer -> Integer
sumaCuadradosR2 0 = 0
sumaCuadradosR2 1 = 1
sumaCuadradosR2 n = n^2 + sumaCuadradosR2 (n-1)
  
-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Comprobar con QuickCheck si sumaCuadradosR n es igual a
-- n(n+1)(2n+1)/6. 
-- ---------------------------------------------------------------------

-- enrnarbej eliguivil josrodgal7 fatfervaz manruiber glovizcas antdursan 
-- marjimcom marlobrip natmarmar2 artmorfer belbenzam alvfercen
-- cargonler antbeacar antmorper3 albcercid paumacpar carmarcar5 fraferpoy 
-- marmerzaf criortcar ignareeva margarflo5 javcancif margirmon josdeher
-- natruipin cescarde
-- La propiedad es 
prop_SumaCuadrados :: Integer -> Property
prop_SumaCuadrados n =
  n > 0 ==> sumaCuadradosR n == n*(n+1)*(2*n+1) `div` 6

-- La comprobación es
--    Prelude> quickCheck prop_SumaCuadrados
--    +++ OK, passed 100 tests.

-- juaorture roscargar eledejim2 pabrabmon juacasnie
-- La propiedad es
prop_SumaCuadrados2 :: Integer -> Property
prop_SumaCuadrados2 n =
  n >= 1 ==> sumaCuadradosR n == n * (n+1) * (2*n + 1) `div` 6

-- La comprobación es
--    *Main> quickCheck prop_SumaCuadrados2
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 1.3. Definir, por comprensión, la función 
--    sumaCuadradosC :: Integer --> Integer
-- tal que (sumaCuadradosC n) es la suma de los cuadrados de los números
-- de 1 a n. Por ejemplo, 
--    sumaCuadradosC 4  ==  30 
-- ---------------------------------------------------------------------

-- enrnarbej josrodgal7 fatfervaz manruiber juaorture glovizcas
-- roscargar antbeacar antdursan eledejim2 paumacpar marjimcom carmarcar5
-- marlobrip belbenzam pabrabmon artmorfer alvfercen cargonler
-- antmorper3 albcercid fraferpoy cescarde marmerzaf criortcar
-- ignareeva margarflo5 eliguivil javcancif margirmon juacasnie josdeher
-- margarvil14 natruipin
sumaCuadradosC :: Integer -> Integer 
sumaCuadradosC n = sum [ x^2 | x<-[1..n]]

-- ---------------------------------------------------------------------
-- Ejercicio 1.4. Comprobar con QuickCheck que las funciones
-- sumaCuadradosR y sumaCuadradosC son equivalentes sobre los números
-- naturales. 
-- ---------------------------------------------------------------------

-- enrnarbej josrodgal7 fatfervaz manruiber glovizcas roscargar antdursan
-- eledejim2 paumacpar antbeacar marjimcom carmarcar5 natmarmar2 belbenzam
-- artmorfer alvfercen cargonler antmorper3 albcercid fraferpoy cescarde
-- marmerzaf criortcar ignareeva margarflo5 eliguivil javcancif margirmon
-- josdeher margarvil14 natruipin
-- La propiedad es
prop_sumaCuadradosR :: Integer -> Property
prop_sumaCuadradosR n =
  n > 0 ==> sumaCuadradosR n == sumaCuadradosC n

-- La comprobación es
--    Prelude> quickCheck prop_sumaCuadradosR
--    +++ OK, passed 100 tests.

-- juaorture pabrabmon juacasnie
-- La propiedad es
prop_sumaCuadradosR1 :: Integer -> Property
prop_sumaCuadradosR1 n =
  n >= 1 ==> sumaCuadradosR n == sumaCuadradosC n

-- La comprobación es
--    *Main> quickCheck prop_sumaCuadradosR
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Definir, por recursión, la función
--    digitosR :: Integer -> [Integer]
-- tal que (digitosR n) es la lista de los dígitos del número n. Por
-- ejemplo, 
--    digitosR 320274  ==  [3,2,0,2,7,4]
-- ---------------------------------------------------------------------

-- enrnarbej antdursan
digitosR :: Integer -> [Integer]
digitosR 0 = []
digitosR n = digitosR ((n-(n `mod` 10)) `div` 10) ++ [n `mod` 10]

-- Comentario: La definición digitosR se puede mejorar.

-- manruiber glovizcas eledejim2 marjimcom carmarcar5 natmarmar2
-- belbenzam albcercid fraferpoy marmerzaf criortcar ignareeva
-- margarflo5 eliguivil margirmon josdeher fatfervaz natruipin
digitosR1 0 = []
digitosR1 n = digitosR1 (n `div` 10) ++ [mod n 10]

-- juaorture pabrabmon alvfercen cargonler javcancif juacasnie
digitosR2 :: Integer -> [Integer]
digitosR2 0 = []
digitosR2 n = digitosR2 (n `div` 10) ++ [n `mod` 10]

-- roscargar
digitosR3 :: Integer -> [Integer]
digitosR3 0 = []
digitosR3 n = digitosR3 (fst (x,y)) ++ [snd (x,y)]
  where (x,y) = quotRem n 10 

-- Comentario: La definición digitosR3 se puede simplificar.

-- antmorper3 paumacpar 
digitosR4 :: Integer -> [Integer]
digitosR4 0 = []
digitosR4 n = digitosR4 (div n 10) ++ [rem n 10]

-- cescarde margarvil14
digitosR5 :: Integer -> [Integer]
digitosR5 n = reverse (digitosR5a n)

digitosR5a n | n < 10    = [n]
             | otherwise = (rem n 10) : digitosR5a (div n 10) 

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Definir, por comprensión, la función
--    digitosC :: Integer -> [Integer]
-- tal que (digitosC n) es la lista de los dígitos del número n. Por
-- ejemplo, 
--    digitosC 320274  ==  [3,2,0,2,7,4]
-- Indicación: Usar las funciones show y read.
-- ---------------------------------------------------------------------

-- enrnarbej manruiber glovizcas roscargar antdursan eledejim2 marjimcom
-- carmarcar5 natmarmar2 pabrabmon belbenzam alvfercen cargonler antmorper3
-- cescarde fraferpoy marmerzaf criortcar ignareeva margarflo5 eliguivil
-- javcancif juacasnie josdeher fatfervaz margarvil14 natruipin  margirmon
digitosC :: Integer -> [Integer]
digitosC n = [read [x] | x <- show n]

-- juaorture albcercid paumacpar 
digitosC1 :: Integer -> [Integer]
digitosC1 n = [(read [a]):: Integer | a <- show n]

-- Comentario: La definición digitosC1 se puede simplificar.

-- ---------------------------------------------------------------------
-- Ejercicio 2.3. Comprobar con QuickCheck que las funciones digitosR y
-- digitosC son equivalentes.
-- ---------------------------------------------------------------------

-- enrnarbej manruiber juaorture glovizcas roscargar antdursan eledejim2
-- marjimcom carmarcar5 natmarmar2 pabrabmon belbenzam alvfercen cargonler
-- antmorper3 albcercid cescarde fraferpoy marmerzaf criortcar paumacpar 
-- ignareeva margarflo5 eliguivil javcancif margirmon juacasnie josdeher
-- fatfervaz margarvil14 natruipin
-- La propiedad es
prop_digitos :: Integer -> Property
prop_digitos n = n > 0 ==> digitosR n == digitosC n
  
-- La comprobación es
--    Prelude> quickCheck prop_digitos
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Definir, por recursión, la función 
--    sumaDigitosR :: Integer -> Integer
-- tal que (sumaDigitosR n) es la suma de los dígitos de n. Por ejemplo,
--    sumaDigitosR 3     ==  3
--    sumaDigitosR 2454  == 15
--    sumaDigitosR 20045 == 11
-- ---------------------------------------------------------------------

-- juaorture eledejim2 carmarcar5 natmarmar2 manruiber natruipin
sumaDigitosR :: Integer -> Integer
sumaDigitosR 0 = 0
sumaDigitosR n = last (digitosC n) + sumaDigitosR (n `div` 10)

-- manruiber roscargar natruipin
sumaDigitosR2 :: Integer -> Integer
sumaDigitosR2 n = sum (digitosR n)

-- Comentario: La definición de sumaDigitosR2 no es recursiva.

-- antdursan
sumaDigitosR3 :: Integer -> Integer
sumaDigitosR3 0 = 0
sumaDigitosR3 a = a `mod` 10 + sumaDigitosR3 ((a- (a `mod` 10)) `div` 10)

-- Comentario: La definición sumaDigitosR3 se puede mejorar.

-- marjimcom glovizcas belbenzam antmorper3 marmerzaf fraferpoy paumacpar 
-- josdeher fatfervaz
sumaDigitosR4 :: Integer -> Integer
sumaDigitosR4 0 = 0
sumaDigitosR4 n = n `rem` 10 + sumaDigitosR4 (n `div` 10)

-- pabrabmon enrnarbej alvfercen cargonler albcercid criortcar
-- margarflo5 eliguivil javcancif margirmon juacasnie
sumaDigitosR5 :: Integer -> Integer
sumaDigitosR5 0 = 0
sumaDigitosR5 n = sumaDigitosR (n `div` 10) + (n `mod` 10) 

-- cescarde
sumaDigitosR6 :: Integer -> Integer
sumaDigitosR6 0 = 0
sumaDigitosR6 n | n < 10    = n
                | otherwise = (rem n 10) + sumaDigitosR (div n 10)

-- margarvil14
sumaDigitosR7 :: Integer -> Integer
sumaDigitosR7 n | n < 10    = n
                | otherwise = n `rem` 10 + sumaDigitosR7 (n `div`10)

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Definir, sin usar recursión, la función 
--    sumaDigitosNR :: Integer -> Integer
-- tal que (sumaDigitosNR n) es la suma de los dígitos de n. Por ejemplo,
--    sumaDigitosNR 3     ==  3
--    sumaDigitosNR 2454  == 15
--    sumaDigitosNR 20045 == 11
-- ---------------------------------------------------------------------

-- juaorture manruiber roscargar antdursan eledejim2 carmarcar5
-- marjimcom glovizcas natmarmar2 pabrabmon belbenzam enrnarbej
-- alvfercen antmorper3 albcercid marmerzaf fraferpoy criortcar 
-- paumacpar ignareeva margarflo5 eliguivil javcancif margirmon 
-- juacasnie josdeher margarvil14 natruipin fatfervaz
sumaDigitosNR :: Integer -> Integer
sumaDigitosNR n = sum (digitosC n)

-- cargonler cescarde
sumaDigitosNR2 :: Integer -> Integer
sumaDigitosNR2 n = sum [ read [x] | x <- show n ]

-- Comentario: La definición anterior se puede simplificar.

-- ---------------------------------------------------------------------
-- Ejercicio 3.3. Comprobar con QuickCheck que las funciones sumaDigitosR
-- y sumaDigitosNR son equivalentes.
-- ---------------------------------------------------------------------

-- juaorture manruiber roscargar antdursan eledejim2 carmarcar5
-- marjimcom glovizcas natmarmar2 pabrabmon belbenzam enrnarbej juacasnie
-- alvfercen cargonler antmorper3 albcercid cescarde marmerzaf fraferpoy 
-- criortcar ignareeva margarflo5 eliguivil paumacpar javcancif  margirmon
-- josdeher margarvil14 natruipin fatfervaz
-- La propiedad es
prop_sumaDigitos :: Integer -> Property
prop_sumaDigitos n =
  n >= 0 ==> sumaDigitosR n == sumaDigitosNR n

-- La comprobación es
--    *Main> quickCheck prop_sumaDigitos
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. Definir, por recursión, la función 
--    listaNumeroR :: [Integer] -> Integer
-- tal que (listaNumeroR xs) es el número formado por los dígitos xs. Por
-- ejemplo, 
--    listaNumeroR [5]        == 5
--    listaNumeroR [1,3,4,7]  == 1347
--    listaNumeroR [0,0,1]    == 1
-- ---------------------------------------------------------------------

-- juaorture 
listaNumeroR :: [Integer] -> Integer
listaNumeroR [] = 0
listaNumeroR xs = listaNumeroR (10 `por` (init xs)) + last xs

por :: Integer -> [Integer] -> [Integer]
por n xs = [n*a | a <- xs]

-- Comentario: La definición listaNumeroR se puede mejorar. Por ejemplo,
--    λ> listaNumeroR (replicate 2000 5) `mod` 10
--    5
--    (1.95 secs, 1,217,124,376 bytes)
--    λ> listaNumeroR' (replicate 2000 5) `mod` 10
--    5
--    (0.00 secs, 0 bytes)

-- manruiber carmarcar5 antmorper3 eliguivil margirmon
listaNumeroR2 :: [Integer] -> Integer
listaNumeroR2 [] = 0
listaNumeroR2 xs = last xs + (10 * listaNumeroR (init xs))

-- Comentario: La definición listaNumeroR2 se puede mejorar. Por ejemplo,
--    λ> listaNumeroR2 (replicate 2000 5) `mod` 10
--    5
--    (1.96 secs, 1,219,960,088 bytes)
--    λ> listaNumeroR' (replicate 2000 5) `mod` 10
--    5
--    (0.00 secs, 0 bytes)

-- joscasgom1 carmarcar5 pabrabmon marjimcom enrnarbej belbenzam
-- cargonler roscargar albcercid  marmerzaf margarflo5 eliguivil
-- javcancif josdeher
listaNumeroR3 :: [Integer] -> Integer
listaNumeroR3 [] = 0
listaNumeroR3 (x:xs) = x*10^(length xs) + listaNumeroR xs

-- Comentario: La definición listaNumeroR3 se puede mejorar. Por ejemplo,
--    λ> listaNumeroR3 (replicate 2000 5) `mod` 10
--    5
--    (1.92 secs, 1,216,697,576 bytes)
--    λ> listaNumeroR' (replicate 2000 5) `mod` 10
--    5
--    (0.00 secs, 0 bytes)

-- antdursan juacasnie
listaNumeroR4 :: [Integer] -> Integer
listaNumeroR4 [] = 0
listaNumeroR4 (x:xs) = x*(10^n) + listaNumeroR xs
  where n = length xs

-- Comentario: La definición listaNumeroR4 se puede mejorar. Por ejemplo,
--    λ> listaNumeroR4 (replicate 2000 5) `mod` 10
--    5
--    (1.95 secs, 1,216,642,536 bytes)
--    λ> listaNumeroR' (replicate 2000 5) `mod` 10
--    5
--    (0.00 secs, 0 bytes)

-- glovizcas
listaNumeroR5 :: [Integer] -> Integer
listaNumeroR5 xs = read (cadenaNumeroR xs) :: Integer

cadenaNumeroR [] = []
cadenaNumeroR xs = show (xs !! 0) ++ cadenaNumeroR (tail xs)

-- Comentario: La definición anterior se puede simplificar.

-- cescarde
listaNumeroR6 :: [Integer] -> Integer
listaNumeroR6 []  = 0
listaNumeroR6 [n] = n
listaNumeroR6 xs  = head xs * 10^((length xs)-1) + listaNumeroR (tail xs)

-- Comentario: La definición listaNumeroR4 se puede mejorar. Por ejemplo,
--    λ> listaNumeroR6 (replicate 2000 5) `mod` 10
--    5
--    (1.92 secs, 1,213,310,096 bytes)
--    λ> listaNumeroR' (replicate 2000 5) `mod` 10
--    5
--    (0.00 secs, 0 bytes)

-- paumacpar
listaNumeroR7 :: [Integer] -> Integer
listaNumeroR7 (x:xs) = (x*(10^(length (x:xs)-1))) + listaNumeroR xs
listaNumeroR7 []     = 0

-- Comentario: La definición listaNumeroR4 se puede mejorar. Por ejemplo,
--    λ> listaNumeroR7 (replicate 2000 5) `mod` 10
--    5
--    (1.91 secs, 1,219,463,312 bytes)
--    λ> listaNumeroR' (replicate 2000 5) `mod` 10
--    5
--    (0.00 secs, 0 bytes)

-- margarvil14
listaNumeroR8 :: [Integer] -> Integer
listaNumeroR8 xs = listaNumeroR8' (reverse xs)

listaNumeroR8' :: [Integer] -> Integer
listaNumeroR8' [x]    = x
listaNumeroR8' (x:xs) = x + 10*(listaNumeroR8' xs)


-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Definir, por comprensión, la función 
--    listaNumeroC :: [Integer] -> Integer
-- tal que (listaNumeroC xs) es el número formado por los dígitos xs. Por
-- ejemplo, 
--    listaNumeroC [5]        == 5
--    listaNumeroC [1,3,4,7]  == 1347
--    listaNumeroC [0,0,1]    == 1
-- ---------------------------------------------------------------------

-- juaorture
listaNumeroC :: [Integer] -> Integer
listaNumeroC xs =
  sum [fst a * snd a
      | a <- (zip xs (reverse [10^b | b <- [0..(length xs -1)]]))]

-- Comentario: La definición anterior se puede simplificar.

-- marjimcom cargonler antmorper3 josdeher
listaNumeroC2 :: [Integer] -> Integer
listaNumeroC2 xs =
  sum[x*10^y | (x,y) <- zip xs (reverse [0..(length xs)-1])]

-- enrnarbej glovizcas roscargar cescarde marmerzaf manruiber margarflo5 
-- javcancif juacasnie margirmon
listaNumeroC3 :: [Integer] -> Integer
listaNumeroC3 [] = 0
listaNumeroC3 xs = read (concat [ show x | x <- xs])

-- albcercid natruipin
listaNumeroC4 :: [Integer] -> Integer
listaNumeroC4 xs =
  sum [x * 10^(y-1) | (x,y) <- zip (reverse xs) [1..]]

-- paumacpar
listaNumeroC5 :: [Integer] -> Integer 
listaNumeroC5 xs = sum [a*b | (a,b) <- zip (reverse xs) ys]
  where ys = [10^x | x <- [0..]]

-- eliguivil
listaNumeroC6 :: [Integer] -> Integer
listaNumeroC6 xs = sum [x*10^(-a) | (x,a) <- zip xs [-(length xs)+1..0]]

-- margarvil14
listaNumeroC7 :: [Integer] -> Integer
listaNumeroC7 xs = sum [y*10^n | (y,n) <- zip (reverse xs) [0..]]

-- ---------------------------------------------------------------------
-- Ejercicio 4.3. Comprobar con QuickCheck que las funciones
-- listaNumeroR y listaNumeroC son equivalentes.
-- ---------------------------------------------------------------------

-- juaorture pabrabmon marjimcom cargonler glovizcas roscargar manruiber
-- antmorper3 albcercid cescarde marmerzaf paumacpar margarflo5 margirmon
-- juacasnie josdeher margarvil14 natruipin
-- La propiedad es
prop_listaNumero :: [Integer] -> Bool
prop_listaNumero xs = listaNumeroR xs == listaNumeroC xs

-- La comprobación es
--    *Main> quickCheck prop_listaNumero
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 5.1. Definir, por recursión, la función 
--    mayorExponenteR :: Integer -> Integer -> Integer 
-- tal que (mayorExponenteR a b) es el exponente de la mayor potencia de
-- a que divide b. Por ejemplo,
--    mayorExponenteR 2 8    ==  3
--    mayorExponenteR 2 9    ==  0
--    mayorExponenteR 5 100  ==  2
--    mayorExponenteR 2 60   ==  2
-- ---------------------------------------------------------------------

-- juaorture pabrabmon
mayorExponenteR :: Integer -> Integer -> Integer
mayorExponenteR a b
  | a `elem` factores b =
    1 + mayorExponenteR a (b `div` minimum [x | x <- factores b
                                              , x `mod`a == 0])
  | otherwise = 0

factores:: Integer -> [Integer]
factores n = [a | a <- [1..n]
                , n `mod`a == 0]

-- enrnarbej marjimcom antmorper3 albcercid marmerzaf paumacpar
-- cargonler manruiber margirmon carmarcar5 juacasnie josdeher margarvil14
-- natruipin
mayorExponenteR2 :: Integer -> Integer -> Integer
mayorExponenteR2 a b
  | mod b a /= 0 = 0
  | otherwise    = 1 + mayorExponenteR2 a (div b a)

-- ---------------------------------------------------------------------
-- Ejercicio 5.2. Definir, por comprensión, la función 
--    mayorExponenteC :: Integer -> Integer -> Integer 
-- tal que (mayorExponenteC a b) es el exponente de la mayor potencia de
-- a que divide a b. Por ejemplo,
--    mayorExponenteC 2 8    ==  3
--    mayorExponenteC 5 100  ==  2
--    mayorExponenteC 5 101  ==  0
-- ---------------------------------------------------------------------

-- juaorture roscargar
mayorExponenteC :: Integer -> Integer -> Integer
mayorExponenteC a b =
  maximum ([n | n <- [1..b `div` a]
              , b `rem` a^n == 0]
           ++ [0])

-- Comentario: La definición anterior se puede mejorar. Por ejemplo,
--    λ> mayorExponenteC 2 (2^16)
--    16
--    (0.65 secs, 379,071,136 bytes)
--    λ> mayorExponenteC' 2 (2^16)
--    16
--    (0.01 secs, 23,570,056 bytes)

-- mejorada (mayorExponenteC5)

-- pabrabmon marjimcom antmorper3 marmerzaf cargonler margirmon
mayorExponenteC2 :: Integer -> Integer -> Integer
mayorExponenteC2 a b =
  last [x | x <- [0..b]
          , b `mod` a^x == 0]

-- Comentario: La definición anterior se puede mejorar. Por ejemplo,
--    λ> mayorExponenteC2 2 (2^16)
--    16
--    (3.26 secs, 1,166,111,640 bytes)
--    λ> mayorExponenteC' 2 (2^16)
--    16
--    (0.01 secs, 23,570,056 bytes)

-- enrnarbej manruiber
mayorExponenteC3 :: Integer -> Integer -> Integer
mayorExponenteC3 a b = sum [1 | p <- primeFactors b
                              , a == p]

-- albcercid
mayorExponenteC4 :: Integer -> Integer -> Integer
mayorExponenteC4 a b =
  last (0 : [x | x <- [0..c]
               , mod b (a^x) == 0
               , a^x <= b
               ])
  where c = head [x | x <- [1..]
                    , b <= a^x]

-- Comentario: La definición mayorExponenteC4 se puede simplificar (eliminando
-- una condición).

-- juaorture (mejorada)
mayorExponenteC5 :: Integer -> Integer -> Int
mayorExponenteC5 a b =
  length (filter (==a) (descomposicion b))

descomposicion :: Integer -> [Integer]
descomposicion n
  | n > 1     = head (primos n):(descomposicion (n `div` (head (primos n))))
  | otherwise = []

primos:: Integer -> [Integer]
primos n = [a | a <- factores n
              , factores a == [1,a]]

-- paumacpar carmarcar5 josdeher natruipin
mayorExponenteC6 :: Integer -> Integer -> Integer
mayorExponenteC6 a b
  | mod b a /= 0 = 0
  | otherwise    = last [k | k <- [1..b]
                           , a^k <= b
                           , mod b a^k == 0]

-- Comentario: La definición anterior se puede mejorar. Por ejemplo,
--    λ> mayorExponenteC6 2 (2^16)
--    16
--    (3.22 secs, 1,156,340,768 bytes)
--    λ> mayorExponenteC' 2 (2^16)
--    16
--    (0.01 secs, 23,570,056 bytes)

-- juacasnie
mayorExponenteC7 :: Integer -> Integer -> Integer
mayorExponenteC7 a b = maximum ([x | x <- [0..b], a^x == b] ++ [0])

-- Comentario: La definición mayorExponenteC7 se puede mejorar. Por ejemplo,
--    λ> mayorExponenteC7 2 (2^16)
--    17
--    (3.22 secs, 1,158,510,544 bytes)
--    λ> mayorExponenteC' 2 (2^16)
--    16
--    (0.01 secs, 23,570,056 bytes)

-- margarvil14
mayorExponenteC8 :: Integer -> Integer -> Integer
mayorExponenteC8 a b = head [n-1 | n <- [0..], b `mod`a^n /= 0]
