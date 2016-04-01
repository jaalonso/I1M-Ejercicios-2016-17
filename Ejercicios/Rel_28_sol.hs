-- I1M 2015-16: Relación 28 (29 de marzo de 2016)
-- Ecuación con factoriales.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- El objetivo de esta relación de ejercicios es resolver la ecuación
--    a! * b! = a! + b! + c!
-- donde a, b y c son números naturales.

-- ---------------------------------------------------------------------
-- Importación de librerías auxiliares                                --
-- ---------------------------------------------------------------------

import Test.QuickCheck 
import Data.Maybe

-- ---------------------------------------------------------------------
-- Introducción: El objetivo de esta relación de ejercicios es resolver
-- la ecuación
--    a! * b! = a! + b! + c!
-- donde a, b y c son números naturales.
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    factorial :: Integer -> Integer
-- tal que (factorial n) es el factorial de n. Por ejemplo,
--    factorial 5  ==  120
-- ---------------------------------------------------------------------

-- silgongal jespergue abrdelrod rubvilval fracruzam juamorrom1 manpende
-- josllagam erisancha juanarcon lucgamgal
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n-1)

-- isrbelnun manvermor 
factorial2 :: Integer -> Integer
factorial2 n = product $ [1..n]

-- Comentario: La definición anterior se puede simplificar.

-- alvalvdom1 ivaruicam
factorial3 :: Integer -> Integer
factorial3 n = product [1..n]

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la constante
--    factoriales :: [Integer]
-- tal que factoriales es la lista de los factoriales de los números
-- naturales. Por ejemplo,
--    take 7 factoriales  ==  [1,1,2,6,24,120,720]
-- ---------------------------------------------------------------------

-- silgongal isrbelnun jespergue manvermor lucgamgal josllagam
factoriales :: [Integer]
factoriales = [factorial n | n <- [0..]]

-- Comentario: La definición anterior se puede mejorar.

-- abrdelrod rubvilval juamorrom1 alvalvdom1 manpende erisancha ivaruicam
-- juanarcon
factoriales2 :: [Integer]
factoriales2 = map factorial [0..]

-- Comentario: La definición anterior se puede mejorar.

-- fracruzam
factoriales3 :: [Integer]
factoriales3 = scanl1 (*) (1:[1..])

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir, usando factoriales, la función
--    esFactorial :: Integer -> Bool
-- tal que (esFactorial n) se verifica si existe un  número natural m
-- tal que n es m!. Por ejemplo,
--    esFactorial 120  ==  True
--    esFactorial  20  ==  False
-- ---------------------------------------------------------------------

-- silgongal jespergue fracruzam alvalvdom1 erisancha juanarcon lucgamgal
esFactorial :: Integer -> Bool
esFactorial n = head (dropWhile (< n) factoriales) == n

-- isrbelnun abrdelrod rubvilval manvermor juamorrom1 manpende josllagam 
esFactorial2 :: Integer -> Bool
esFactorial2 n = elem n (takeWhile (<=n) factoriales)

-- Comentario: La definición anterior se puede mejorar.

-- ivaruicam
esFactorial3 :: Integer -> Bool
esFactorial3 n = aux 2 n
  where aux v n | v > n            = False
                | q == 1 && r == 0 = True 
                | r == 0           = aux (v+1) q
                | otherwise        = False
          where (q,r) = divMod n v 

-- Comentario: La definición anterior es casi correcta. Un contraejemplo
-- es
--    λ> esFactorial3 1
--    False

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la constante
--    posicionesFactoriales :: [(Integer,Integer)]
-- tal que posicionesFactoriales es la lista de los factoriales con su
-- posición. Por ejemplo,
--    ghci> take 7 posicionesFactoriales
--    [(0,1),(1,1),(2,2),(3,6),(4,24),(5,120),(6,720)]
-- ---------------------------------------------------------------------

-- silgongal jespergue manvermor
posicionesFactoriales :: [(Integer,Integer)]
posicionesFactoriales = [(x,y) | (x,y) <- zip [0..] factoriales]

-- Comentario: La definición anterior se puede simplificar.

-- isrbelnun rubvilval fracruzam juamorrom1 manpende josllagam erisancha
-- ivaruicam juanarcon lucgamgal
posicionesFactoriales2 :: [(Integer,Integer)]
posicionesFactoriales2 = zip [0..] factoriales

-- abrdelrod
posicionesFactoriales3 :: [(Integer,Integer)]
posicionesFactoriales3 =
  (0,1) : iterate (\(x,y) -> (x+1,y*(x+1))) (1,1)

-- alvalvdom1
posicionesFactoriales4 :: [(Integer,Integer)]
posicionesFactoriales4 = aux 0 factoriales
  where aux a (x:xs) = (a,x) : aux (a+1) xs

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la función
--    invFactorial :: Integer -> Maybe Integer
-- tal que (invFactorial x)  es (Just n) si el factorial de n es x y es
-- Nothing, en caso contrario. Por ejemplo,
--    invFactorial 120  == Just 5
--    invFactorial 20   == Nothing
-- ---------------------------------------------------------------------

-- isrbelnun jespergue lucgamgal
invFactorial :: Integer -> Maybe Integer
invFactorial x
  | esFactorial x = Just (posicion x factoriales)
  | otherwise     = Nothing
  where posicion n (x:xs) | n == x    = 0
                          | otherwise = 1 + posicion n xs

-- abrdelrod manvermor alvalvdom1 manpende josllagam erisancha silgongal
-- juanarcon
invFactorial2 :: Integer -> Maybe Integer
invFactorial2 x
  | esFactorial x = Just $ head [y | (y,z) <- posicionesFactoriales, z == x]
  | otherwise = Nothing

-- rubvilval
invFactorial3 :: Integer -> Maybe Integer
invFactorial3 x
  | esFactorial x =
      Just $ fst $ head (filter (\(a,b) -> x == b) posicionesFactoriales)
  | otherwise     = Nothing

-- fracruzam 
invFactorial4 :: Integer -> Maybe Integer
invFactorial4 x | n == x    = Just m
                | otherwise = Nothing
  where (m,n) = head $ dropWhile (\(_,y) -> y < x) posicionesFactoriales

-- juamorrom1
invFactorial5 :: Integer -> Maybe Integer
invFactorial5 x = aux x posicionesFactoriales
  where aux :: Ord a1 => a1 -> [(a, a1)] -> Maybe a
        aux x [] = Nothing
        aux x ((y1,y2):ys) | x == y2   = Just y1
                           | y2 > x    = Nothing
                           | otherwise = aux x ys

-- ivaruicam
invFactorial6 :: Integer -> Maybe Integer
invFactorial6 x =  aux 2 x
  where aux v x | v > x            = Nothing
                | q == 1 && r == 0 = Just v
                | r == 0           = aux (v+1) q
                | otherwise        = Nothing
          where (q,r) = divMod x v 

-- Esta definición es igual a la de esFactorial3, pero en lugar de
-- devolver True devuelve el valor

-- Comentario: La definición anterior es casi correcta. Contraejemplo,
--    λ> invFactorial6 1
--    Nothing

-- ---------------------------------------------------------------------
-- Ejercicio 6. Definir la constante
--    pares :: [(Integer,Integer)]
-- tal que pares es la lista de todos los pares de números naturales. Por
-- ejemplo, 
--    ghci> take 11 pares
--    [(0,0),(0,1),(1,1),(0,2),(1,2),(2,2),(0,3),(1,3),(2,3),(3,3),(0,4)]
-- ---------------------------------------------------------------------

-- silgongal
pares :: [(Integer,Integer)]
pares = pares' [0..]
    where pares' xs = [(a,b) | n <- [1..],
                               ns <- [take n xs], 
                               (a,b) <- zip ns (repeat (last ns))]

-- Comentario: La definición anterior se puede simplificar.

-- isrbelnun jespergue rubvilval manvermor fracruzam juamorrom1 manpende
-- erisancha ivaruicam juanarcon
pares2 :: [(Integer,Integer)]
pares2 = [(x,y) | y <- [0..], x <- [0..y]]

-- abrdelrod josllagam
pares3 :: [(Integer,Integer)]
pares3 = iterate f (0,0)
   where f (x,y) | x == y    = (0,x+1)
                 | otherwise = (x+1,y)

-- alvalvdom1 lucgamgal
pares4 :: [(Integer,Integer)]
pares4 = [(x-n,x) | x <- [0..], n <- [x,x-1..0]]

-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir la constante
--    solucionFactoriales :: (Integer,Integer,Integer)
-- tal que solucionFactoriales es una terna (a,b,c) que es una solución
-- de la ecuación 
--    a! * b! = a! + b! + c!
-- Calcular el valor de solucionFactoriales.
-- ---------------------------------------------------------------------

-- abrdelrod ivaruicam isrbelnun juanarcon josllagam
solucionFactoriales :: (Integer,Integer,Integer)
solucionFactoriales =
  head [(a,b,f z)
       | (a,b) <- pares,
         let (x,y,z) = (factorial a, factorial b, invFactorial (x*y-x-y)),
         z /= Nothing]
   where f (Just a) = a

-- fracruzam erisancha (fromJust está definido en la librería Data.Maybe)
solucionFactoriales2 :: (Integer,Integer,Integer)
solucionFactoriales2 =
  head [(a,b,fromJust c)
       | (a,b) <- pares,
         let fa = factorial a,
         let fb = factorial b,
         let fc = fa * fb - fa - fb,
         esFactorial fc,
         let c = invFactorial fc]

-- El cálculo es
--    ghci> solucionFactoriales
--    (3,3,4)

-- ---------------------------------------------------------------------
-- Ejercicio 8. Comprobar con QuickCheck que solucionFactoriales es la
-- única solución de la ecuación
--    a! * b! = a! + b! + c!
-- con a, b y c números naturales
-- ---------------------------------------------------------------------

-- abrdelrod ivaruicam isrbelnun juanarcon josllagam
prop_solucionFactoriales :: Integer -> Integer -> Integer -> Property
prop_solucionFactoriales a b c = (a,b,c) /= (3,3,4) ==> x*y /= x+y+z
    where (x,y,z) = (factorial a, factorial b, factorial c)

-- fracruzam erisancha
prop_solucionFactoriales2 :: (Positive Integer) -> (Positive Integer) ->
                            (Positive Integer) -> Bool
prop_solucionFactoriales2 (Positive x) (Positive y) (Positive z) =
    (fx * fy == fx + fy + fz) == ((x,y,x) == (3,3,4))
  where fx = factorial x
        fy = factorial y
        fz = factorial z

-- ---------------------------------------------------------------------
-- Nota: El ejercicio se basa en el artículo "Ecuación con factoriales"
-- del blog Gaussianos publicado en
--    http://gaussianos.com/ecuacion-con-factoriales
-- ---------------------------------------------------------------------
