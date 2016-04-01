-- I1M 2015-16: Relación 29 (29 de marzo de 2016)
-- Números de Lychrel.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- Un número de Lychrel es un número natural para el que nunca se
-- obtiene un capicúa mediante el proceso de invertir las cifras y sumar
-- los dos números. Por ejemplo, los siguientes números no son números
-- de Lychrel: 
--    * 56, ya que en un paso se obtiene un capicúa: 56+65=121.
--    * 57, ya que en dos pasos se obtiene un capicúa: 57+75=132,
--      132+231=363
--    * 59, ya que en dos pasos se obtiene un capicúa: 59+95=154,
--      154+451=605, 605+506=1111
--    * 89, ya que en 24 pasos se obtiene un capicúa.
-- En esta relación vamos a buscar el primer número de Lychrel.

-- ---------------------------------------------------------------------
-- Librerías auxiliares                                               --
-- ---------------------------------------------------------------------

import Test.QuickCheck
import Data.List

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    esCapicua :: Integer -> Bool
-- tal que (esCapicua x) se verifica si x es capicúa. Por ejemplo,
--    esCapicua 252  ==  True
--    esCapicua 253  ==  False
-- ---------------------------------------------------------------------

-- isrbelnun jespergue silgongal rubvilval manvermor juamorrom1 erisancha
-- juanarcon manpende
esCapicua :: Integer -> Bool
esCapicua x = show x == reverse (show x)

-- abrdelrod 
esCapicua2 :: Integer -> Bool
esCapicua2 x = x == (read.reverse.show) x

-- fracruzam alvalvdom1
esCapicua3 :: Integer -> Bool
esCapicua3 x = sx == reverse sx
  where sx = show x

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    inverso :: Integer -> Integer
-- tal que (inverso x) es el número obtenido escribiendo las cifras de x
-- en orden inverso. Por ejemplo,
--    inverso 253  ==  352
-- ---------------------------------------------------------------------

-- isrbelnun jespergue silgongal abrdelrod rubvilval fracruzam manvermor
-- juamorrom1 alvalvdom1 erisancha juanarcon manpende
inverso :: Integer -> Integer
inverso = read . reverse . show

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--    siguiente :: Integer -> Integer
-- tal que (siguiente x) es el número obtenido sumándole a x su
-- inverso. Por ejemplo,
--    siguiente 253  ==  605
-- ---------------------------------------------------------------------

-- isrbelnun jespergue silgongal abrdelrod rubvilval fracruzam manvermor
-- juamorrom1 alvalvdom1 erisancha juanarcon manpende
siguiente :: Integer -> Integer
siguiente x = x + inverso x

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función
--    busquedaDeCapicua :: Integer -> [Integer]
-- tal que (busquedaDeCapicua x) es la lista de los números tal que el
-- primero es x, el segundo es (siguiente de x) y así sucesivamente
-- hasta que se alcanza un capicúa. Por ejemplo,
--    busquedaDeCapicua 253  ==  [253,605,1111]
-- ---------------------------------------------------------------------

-- isrbelnun jespergue silgongal juanarcon
busquedaDeCapicua :: Integer -> [Integer]
busquedaDeCapicua x = takeUntil esCapicua (iterate siguiente x)

takeUntil p []     = []
takeUntil p (x:xs) | p x       = [x]
                   | otherwise = x : takeUntil p xs

-- abrdelrod 
busquedaDeCapicua2 :: Integer -> [Integer]
busquedaDeCapicua2 0 = [0]
busquedaDeCapicua2 n = (takeWhile (/= 0).iterate f) n
   where f x | esCapicua x = 0
             | otherwise   = siguiente x

-- rubvilval erisancha
busquedaDeCapicua3 :: Integer -> [Integer]
busquedaDeCapicua3 x = aux [x]
  where aux xs | esCapicua $ last xs = xs
               | otherwise           = aux (xs ++ [siguiente $ last xs])

-- fracruzam
busquedaDeCapicua4 :: Integer -> [Integer]
busquedaDeCapicua4 = takeWhileCapicua' . iterate siguiente
  where takeWhileCapicua' :: [Integer] -> [Integer]
        takeWhileCapicua' (x:xs) | esCapicua x = [x]
                                 | otherwise   = x: takeWhileCapicua' xs

-- manvermor
busquedaDeCapicua5 :: Integer -> [Integer]
busquedaDeCapicua5 x = takeWhile p xs ++ [head (dropWhile p xs)]
       where p = not . esCapicua
             xs = iterate (siguiente) x

-- juamorrom1 alvalvdom1 manpende
busquedaDeCapicua6 :: Integer -> [Integer]
busquedaDeCapicua6 x | esCapicua x = [x]
                     | otherwise   = x:(busquedaDeCapicua6 (siguiente x))

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la función
--    capicuaFinal :: Integer -> Integer
-- tal que (capicuaFinal x) es la capicúa con la que termina la búsqueda
-- de capicúa a partir de x. Por ejemplo,
--    capicuaFinal 253  ==  1111
-- ---------------------------------------------------------------------

-- isrbelnun jespergue silgongal rubvilval manvermor juamorrom1 
-- alvalvdom1 erisancha juanarcon manpende
capicuaFinal :: Integer -> Integer
capicuaFinal = last . busquedaDeCapicua

-- abrdelrod
capicuaFinal2 :: Integer -> Integer
capicuaFinal2 = until esCapicua siguiente

-- fracruzam
capicuaFinal3 :: Integer -> Integer
capicuaFinal3 = head . dropWhile (not.esCapicua) . iterate siguiente

-- ---------------------------------------------------------------------
-- Ejercicio 6. Definir la función
--    orden :: Integer -> Integer
-- tal que (orden x) es el número de veces que se repite el proceso de
-- calcular el inverso a partir de x hasta alcanzar un número
-- capicúa. Por ejemplo,
--    orden 253  ==  2
-- ---------------------------------------------------------------------

-- isrbelnun 
orden :: Integer -> Integer
orden = nIteraciones . busquedaDeCapicua
  where nIteraciones []     = -1
        nIteraciones (x:xs) = 1 + nIteraciones xs

-- silgongal jespergue juamorrom1 alvalvdom1 erisancha juanarcon
-- manpende
orden2 :: Integer -> Integer
orden2 x  = genericLength (busquedaDeCapicua x) - 1

-- abrdelrod fracruzam manvermor
orden3 :: Integer -> Integer
orden3 x | esCapicua x = 0
         | otherwise   = 1 + orden3 (siguiente x)

--rubvilval
orden4 :: Integer -> Integer
orden4 x = aux' [x] 0
  where aux' xs n | esCapicua $ last xs = n
                  | otherwise = aux' (xs ++ [siguiente $ last xs]) (n+1)

-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir la función
--    ordenMayor :: Integer -> Integer -> Bool
-- tal que (ordenMayor x n) se verifica si el orden de x es mayor o
-- igual que n. Dar la definición sin necesidad de evaluar el orden de
-- x. Por ejemplo,
--    ghci> ordenMayor 1186060307891929990 2
--    True
--    ghci> orden 1186060307891929990
--    261
-- ---------------------------------------------------------------------

-- abrdelrod rubvilval fracruzam jespergue juamorrom1 alvalvdom1
-- erisancha silgongal isrbelnun juanarcon manpende
ordenMayor :: Integer -> Integer -> Bool
ordenMayor x 0 = True
ordenMayor x n = not (esCapicua x) && ordenMayor (siguiente x) (n-1)

-- manvermor
ordenMayor2 :: Integer -> Integer -> Bool
ordenMayor2 x n | n <= 0 = True
                | esCapicua x = n == 0
                | otherwise = ordenMayor2 (siguiente x) (n-1)

-- ---------------------------------------------------------------------
-- Ejercicio 8. Definir la función
--    ordenEntre :: Integer -> Integer -> [Integer]
-- tal que (ordenEntre m n) es la lista de los elementos cuyo orden es
-- mayor o igual que m y menor que n. Por ejemplo,
--    take 5 (ordenEntre 10 11)  ==  [829,928,9059,9149,9239]
-- ---------------------------------------------------------------------

-- abrdelrod manpende
ordenEntre :: Integer -> Integer -> [Integer]
ordenEntre m n = [x | x <- [1..], ordenMayor x m, ordenMenor x n]
      where ordenMenor x 0 = False
            ordenMenor x n = esCapicua x || ordenMenor (siguiente x) (n-1)

-- rubvilval
ordenEntre2 :: Integer -> Integer -> [Integer]
ordenEntre2 m n = filter (\a -> ordenMayor a m && ordenMenor a n) [1..]
  where ordenMenor y c =
          any esCapicua (take (fromIntegral c) (iterate siguiente y))

-- manvermor juamorrom1 alvalvdom1 erisancha silgongal isrbelnun juanarcon
ordenEntre3 :: Integer -> Integer -> [Integer]
ordenEntre3 m n = [x | x <- [1..], ordenMayor2 x m, not (ordenMayor2 x n)]

-- ---------------------------------------------------------------------
-- Ejercicio 9. Definir la función
--    menorDeOrdenMayor :: Integer -> Integer
-- tal que (menorDeOrdenMayor n) es el menor elemento cuyo orden es
-- mayor que n. Por ejemplo,
--    menorDeOrdenMayor 2   ==  19
--    menorDeOrdenMayor 20  ==  89
-- ---------------------------------------------------------------------

-- abrdelrod jespergue manvermor alvalvdom1 silgongal juanarcon
-- manpende
menorDeOrdenMayor :: Integer -> Integer
menorDeOrdenMayor n = head [x | x <- [1..], ordenMayor x n]

-- rubvilval fracruzam juamorrom1 erisancha
menorDeOrdenMayor2 :: Integer -> Integer
menorDeOrdenMayor2 n = head $ filter (\a -> ordenMayor a n) [1..]

-- isrbelnun
menorDeOrdenMayor3 :: Integer -> Integer
menorDeOrdenMayor3 n = aux [1..] n
  where aux (x:xs) n | ordenMayor x n == True = x
                     | otherwise              = aux xs n

-- ---------------------------------------------------------------------
-- Ejercicio 10. Definir la función 
--    menoresdDeOrdenMayor :: Integer -> [(Integer,Integer)]
-- tal que (menoresdDeOrdenMayor m) es la lista de los pares (n,x) tales
-- que n es un número entre 1 y m y x es el menor elemento de orden
-- mayor que n. Por ejemplo,
--    menoresdDeOrdenMayor 5  ==  [(1,10),(2,19),(3,59),(4,69),(5,79)]
-- ---------------------------------------------------------------------

-- abrdelrod erisancha isrbelnun manpende
menoresdDeOrdenMayor :: Integer -> [(Integer,Integer)]
menoresdDeOrdenMayor m = zip [1..m] (map menorDeOrdenMayor [1..m])

-- rubvilval jespergue manvermor juamorrom1 alvalvdom1 silgongal juanarcon
menoresdDeOrdenMayor2 :: Integer -> [(Integer,Integer)]
menoresdDeOrdenMayor2 m = [(n,menorDeOrdenMayor n) | n <- [1..m]] 

-- ---------------------------------------------------------------------
-- Ejercicio 11. A la vista de los resultados de (menoresdDeOrdenMayor 5)
-- conjeturar sobre la última cifra de menorDeOrdenMayor.
-- ---------------------------------------------------------------------

-- A primera vista, parece que para todo n mayor que 1, la última cifra
-- del menor número de orden mayor que n es 9


-- ---------------------------------------------------------------------
-- Ejercicio 12. Decidir con QuickCheck la conjetura.
-- ---------------------------------------------------------------------

-- abrdelrod jespergue manvermor alvalvdom1 erisancha silgongal isrbelnun
-- juanarcon manpende

-- La conjetura es
prop_menorDeOrdenMayor :: Integer -> Property
prop_menorDeOrdenMayor n =
  n > 1 ==> (last.show) (menorDeOrdenMayor n) == '9'

-- La comprobación es
--    *Main> quickCheck prop_menorDeOrdenMayor
--    *** Failed! Falsifiable (after 28 tests and 2 shrinks): 
--    25

-- ---------------------------------------------------------------------
-- Ejercicio 13. Calcular (menoresdDeOrdenMayor 50)
-- ---------------------------------------------------------------------

-- Solución: El cálculo es 
--    *Main> menoresdDeOrdenMayor 50
--    [(1,10),(2,19),(3,59),(4,69),(5,79),(6,79),(7,89),(8,89),(9,89),
--     (10,89),(11,89),(12,89),(13,89),(14,89),(15,89),(16,89),(17,89),
--     (18,89),(19,89),(20,89),(21,89),(22,89),(23,89),(24,89),(25,196),
--     (26,196),(27,196),(28,196),(29,196),(30,196),(31,196),(32,196),
--     (33,196),(34,196),(35,196),(36,196),(37,196),(38,196),(39,196),
--     (40,196),(41,196),(42,196),(43,196),(44,196),(45,196),(46,196),
--     (47,196),(48,196),(49,196),(50,196)]

-- ---------------------------------------------------------------------
-- Ejercicio 14. A la vista de (menoresdDeOrdenMayor 50), conjeturar el
-- orden de 196. 
-- ---------------------------------------------------------------------

-- Parece que 196 es el primer número de Lychrel

-- ---------------------------------------------------------------------
-- Ejercicio 15. Comprobar con QuickCheck la conjetura sobre el orden de
-- 196. 
-- ---------------------------------------------------------------------

-- abrdelrod jespergue manvermor alvalvdom1 erisancha silgongal isrbelnun
-- juanarcon manpende

-- La propiedad es
prop_ordenDe196 :: Integer -> Property
prop_ordenDe196 n = n >= 0 ==> ordenMayor 196 n

-- La comprobación es
--    *Main> quickCheck prop_ordenDe196
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Nota. En el ejercicio anterior sólo se ha comprobado la conjetura de
-- que 196 es un número de Lychrel. Otra cuestión distinta es
-- probarla. Hasta la fecha, no se conoce ninguna demostración ni
-- refutación de la conjetura 196.
-- ---------------------------------------------------------------------
