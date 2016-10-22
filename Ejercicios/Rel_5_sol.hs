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

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Definir, por recursión, la función 
--    sumaCuadradosR :: Integer -> Integer
-- tal que (sumaCuadradosR n) es la suma de los cuadrados de los números
-- de 1 a n. Por ejemplo, 
--    sumaCuadradosR 4  ==  30 
-- ---------------------------------------------------------------------

-- enrnarbej albcercid eliguivil josrodgal7 fatfervaz manruiber
-- juaorture glovizcas roscargar antdursan 
sumaCuadradosR :: Integer -> Integer
sumaCuadradosR 1 = 1
sumaCuadradosR n = n^2 + sumaCuadradosR (n-1)

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Comprobar con QuickCheck si sumaCuadradosR n es igual a
-- n(n+1)(2n+1)/6. 
-- ---------------------------------------------------------------------

-- enrnarbej eliguivil josrodgal7 fatfervaz manruiber glovizcas antdursan
-- La propiedad es
prop_SumaCuadrados :: Integer -> Property
prop_SumaCuadrados n =
  n > 0 ==> sumaCuadradosR n == n*(n+1)*(2*n+1) `div` 6

-- La comprobación es
--    Prelude> quickCheck prop_SumaCuadrados
--    +++ OK, passed 100 tests.

-- juaorture roscargar
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
-- roscargar antdursan 
sumaCuadradosC :: Integer -> Integer
sumaCuadradosC n = sum [ x^2 | x<-[1..n]]

-- ---------------------------------------------------------------------
-- Ejercicio 1.4. Comprobar con QuickCheck que las funciones
-- sumaCuadradosR y sumaCuadradosC son equivalentes sobre los números
-- naturales. 
-- ---------------------------------------------------------------------

-- enrnarbej josrodgal7 fatfervaz manruiber glovizcas roscargar antdursan
-- La propiedad es
prop_sumaCuadradosR :: Integer -> Property
prop_sumaCuadradosR n =
  n > 0 ==> sumaCuadradosR n == sumaCuadradosC n

-- La comprobación es
--    Prelude> quickCheck prop_sumaCuadradosR
--    +++ OK, passed 100 tests.

-- juaorture
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

-- manruiber glovizcas
digitosR1 0 = []
digitosR1 n = digitosR1 (n `div` 10) ++ [mod n 10]

digitosR2 :: Integer -> [Integer]
digitosR2 0 = []
digitosR2 n = digitosR2 (n `div` 10) ++ [n `mod` 10]

-- roscargar
digitosR3 :: Integer -> [Integer]
digitosR3 0 = []
digitosR3 n = digitosR3 (fst (x,y)) ++ [snd (x,y)]
  where (x,y) = quotRem n 10 

-- Comentario: La definición digitosR3 se puede simplificar.

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Definir, por comprensión, la función
--    digitosC :: Integer -> [Integer]
-- tal que (digitosC n) es la lista de los dígitos del número n. Por
-- ejemplo, 
--    digitosC 320274  ==  [3,2,0,2,7,4]
-- Indicación: Usar las funciones show y read.
-- ---------------------------------------------------------------------

--enrnarbej manruiber glovizcas roscargar antdursan
digitosC :: Integer -> [Integer]
digitosC n = [read [x] | x <- show n]

-- juaorture 
digitosC1 :: Integer -> [Integer]
digitosC1 n = [(read [a]):: Integer | a <- show n]

-- Comentario: La definición digitosC1 se puede simplificar.

-- ---------------------------------------------------------------------
-- Ejercicio 2.3. Comprobar con QuickCheck que las funciones digitosR y
-- digitosC son equivalentes.
-- ---------------------------------------------------------------------

-- enrnarbej manruiber juaorture glovizcas roscargar antdursan
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

-- juaorture
sumaDigitosR :: Integer -> Integer
sumaDigitosR 0 = 0
sumaDigitosR n = last (digitosC n) + sumaDigitosR (n `div` 10)

-- manruiber roscargar
sumaDigitosR2 :: Integer -> Integer
sumaDigitosR2 n = sum (digitosR n)

-- Comentario: La definición de sumaDigitosR2 no es recursiva.

-- antdursan
sumaDigitosR3 :: Integer -> Integer
sumaDigitosR3 0 = 0
sumaDigitosR3 a = a `mod` 10 + sumaDigitosR3 ((a- (a `mod` 10)) `div` 10)

-- Comentario: La definición sumaDigitosR3 se puede mejorar.

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Definir, sin usar recursión, la función 
--    sumaDigitosNR :: Integer -> Integer
-- tal que (sumaDigitosNR n) es la suma de los dígitos de n. Por ejemplo,
--    sumaDigitosNR 3     ==  3
--    sumaDigitosNR 2454  == 15
--    sumaDigitosNR 20045 == 11
-- ---------------------------------------------------------------------

-- juaorture manruiber roscargar antdursan
sumaDigitosNR :: Integer -> Integer
sumaDigitosNR n = sum (digitosC n)

-- ---------------------------------------------------------------------
-- Ejercicio 3.3. Comprobar con QuickCheck que las funciones sumaDigitosR
-- y sumaDigitosNR son equivalentes.
-- ---------------------------------------------------------------------

-- juaorture manruiber roscargar antdursan
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

-- manruiber
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

-- joscasgom1
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

-- antdursan
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

-- ---------------------------------------------------------------------
-- Ejercicio 4.3. Comprobar con QuickCheck que las funciones
-- listaNumeroR y listaNumeroC son equivalentes.
-- ---------------------------------------------------------------------

-- juaorture
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

mayorExponenteR :: Integer -> Integer -> Integer 
mayorExponenteR a b = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 5.2. Definir, por comprensión, la función 
--    mayorExponenteC :: Integer -> Integer -> Integer 
-- tal que (mayorExponenteC a b) es el exponente de la mayor potencia de
-- a que divide a b. Por ejemplo,
--    mayorExponenteC 2 8    ==  3
--    mayorExponenteC 5 100  ==  2
--    mayorExponenteC 5 101  ==  0
-- ---------------------------------------------------------------------

-- juaorture
mayorExponenteC :: Integer -> Integer -> Integer
mayorExponenteC a b =
  maximum ([n | n <- [1..b]
              , b `rem` a^n == 0]
           ++ [0])

-- Comentario: La definición anterior se puede mejorar. Por ejemplo,
--    λ> mayorExponenteC 2 (2^16)
--    16
--    (3.27 secs, 1,177,928,440 bytes)
--    λ> mayorExponenteC' 2 (2^16)
--    16
--    (0.01 secs, 23,570,056 bytes)

