-- I1M 2015-16: Relaci�n 29 (29 de marzo de 2016)
-- N�meros de Lychrel.
-- Departamento de Ciencias de la Computaci�n e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducci�n                                                       --
-- ---------------------------------------------------------------------

-- Un n�mero de Lychrel es un n�mero natural para el que nunca se
-- obtiene un capic�a mediante el proceso de invertir las cifras y sumar
-- los dos n�meros. Por ejemplo, los siguientes n�meros no son n�meros
-- de Lychrel: 
--    * 56, ya que en un paso se obtiene un capic�a: 56+65=121.
--    * 57, ya que en dos pasos se obtiene un capic�a: 57+75=132,
--      132+231=363
--    * 59, ya que en dos pasos se obtiene un capic�a: 59+95=154,
--      154+451=605, 605+506=1111
--    * 89, ya que en 24 pasos se obtiene un capic�a.
-- En esta relaci�n vamos a buscar el primer n�mero de Lychrel.

-- ---------------------------------------------------------------------
-- Librer�as auxiliares                                               --
-- ---------------------------------------------------------------------

import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la funci�n
--    esCapicua :: Integer -> Bool
-- tal que (esCapicua x) se verifica si x es capic�a. Por ejemplo,
--    esCapicua 252  ==  True
--    esCapicua 253  ==  False
-- ---------------------------------------------------------------------

esCapicua :: Integer -> Bool
esCapicua x = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la funci�n
--    inverso :: Integer -> Integer
-- tal que (inverso x) es el n�mero obtenido escribiendo las cifras de x
-- en orden inverso. Por ejemplo,
--    inverso 253  ==  352
-- ---------------------------------------------------------------------

inverso :: Integer -> Integer
inverso = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la funci�n
--    siguiente :: Integer -> Integer
-- tal que (siguiente x) es el n�mero obtenido sum�ndole a x su
-- inverso. Por ejemplo,
--    siguiente 253  ==  605
-- ---------------------------------------------------------------------

siguiente :: Integer -> Integer
siguiente x = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la funci�n
--    busquedaDeCapicua :: Integer -> [Integer]
-- tal que (busquedaDeCapicua x) es la lista de los n�meros tal que el
-- primero es x, el segundo es (siguiente de x) y as� sucesivamente
-- hasta que se alcanza un capic�a. Por ejemplo,
--    busquedaDeCapicua 253  ==  [253,605,1111]
-- ---------------------------------------------------------------------

busquedaDeCapicua :: Integer -> [Integer]
busquedaDeCapicua = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la funci�n
--    capicuaFinal :: Integer -> Integer
-- tal que (capicuaFinal x) es la capic�a con la que termina la b�squeda
-- de capic�a a partir de x. Por ejemplo,
--    capicuaFinal 253  ==  1111
-- ---------------------------------------------------------------------

capicuaFinal :: Integer -> Integer
capicuaFinal x = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 6. Definir la funci�n
--    orden :: Integer -> Integer
-- tal que (orden x) es el n�mero de veces que se repite el proceso de
-- calcular el inverso a partir de x hasta alcanzar un n�mero
-- capic�a. Por ejemplo,
--    orden 253  ==  2
-- ---------------------------------------------------------------------

orden :: Integer -> Integer
orden = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir la funci�n
--    ordenMayor :: Integer -> Integer -> Bool
-- tal que (ordenMayor x n) se verifica si el orden de x es mayor o
-- igual que n. Dar la definici�n sin necesidad de evaluar el orden de
-- x. Por ejemplo,
--    ghci> ordenMayor 1186060307891929990 2
--    True
--    ghci> orden 1186060307891929990
--    261
-- ---------------------------------------------------------------------

ordenMayor :: Integer -> Integer -> Bool
ordenMayor x n = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 8. Definir la funci�n
--    ordenEntre :: Integer -> Integer -> [Integer]
-- tal que (ordenEntre m n) es la lista de los elementos cuyo orden es
-- mayor o igual que m y menor que n. Por ejemplo,
--    take 5 (ordenEntre 10 11)  ==  [829,928,9059,9149,9239]
-- ---------------------------------------------------------------------

ordenEntre :: Integer -> Integer -> [Integer]
ordenEntre m n = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 9. Definir la funci�n
--    menorDeOrdenMayor :: Integer -> Integer
-- tal que (menorDeOrdenMayor n) es el menor elemento cuyo orden es
-- mayor que n. Por ejemplo,
--    menorDeOrdenMayor 2   ==  19
--    menorDeOrdenMayor 20  ==  89
-- ---------------------------------------------------------------------

menorDeOrdenMayor :: Integer -> Integer
menorDeOrdenMayor n = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 10. Definir la funci�n 
--    menoresdDeOrdenMayor :: Integer -> [(Integer,Integer)]
-- tal que (menoresdDeOrdenMayor m) es la lista de los pares (n,x) tales
-- que n es un n�mero entre 1 y m y x es el menor elemento de orden
-- mayor que n. Por ejemplo,
--    menoresdDeOrdenMayor 5  ==  [(1,10),(2,19),(3,59),(4,69),(5,79)]
-- ---------------------------------------------------------------------

menoresdDeOrdenMayor :: Integer -> [(Integer,Integer)]
menoresdDeOrdenMayor m = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 11. A la vista de los resultados de (menoresdDeOrdenMayor 5)
-- conjeturar sobre la �ltima cifra de menorDeOrdenMayor.
-- ---------------------------------------------------------------------


-- ---------------------------------------------------------------------
-- Ejercicio 12. Decidir con QuickCheck la conjetura.
-- ---------------------------------------------------------------------

-- La conjetura es
prop_menorDeOrdenMayor :: Integer -> Property
prop_menorDeOrdenMayor n = undefined

-- La comprobaci�n es

-- ---------------------------------------------------------------------
-- Ejercicio 13. Calcular (menoresdDeOrdenMayor 50)
-- ---------------------------------------------------------------------

-- Soluci�n: El c�lculo es

-- ---------------------------------------------------------------------
-- Ejercicio 14. A la vista de (menoresdDeOrdenMayor 50), conjeturar el
-- orden de 196. 
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 15. Comprobar con QuickCheck la conjetura sobre el orden de
-- 196. 
-- ---------------------------------------------------------------------

-- La propiedad es
prop_ordenDe196 n = undefined

-- La comprobaci�n es