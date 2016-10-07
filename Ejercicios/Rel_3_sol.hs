-- I1M 2016-17: Rel_4.hs (5 de Octubre de 2016)
-- Definiciones por comprensión (Ejercicios resueltos)
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- En esta relación se presentan ejercicios con definiciones por
-- comprensión correspondientes al tema 5 que se encuentra
--    http://www.cs.us.es/~jalonso/cursos/i1m-16/temas/tema-5.html

import Test.QuickCheck
-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir, por comprensión, la función 
--    sumaDeCuadrados :: Integer -> Integer 
-- tal que (sumaDeCuadrados n) es la suma de los cuadrados de los
-- primeros n números; es decir, 1^2 + 2^2 + ... + n^2. Por ejemplo,
--    sumaDeCuadrados 3    ==  14
--    sumaDeCuadrados 100  ==  338350
-- ---------------------------------------------------------------------

-- fatfervaz eledejim2 enrnarbej antmorper3 roscargar paumacpar 
sumaDeCuadrados :: Integer -> Integer 
sumaDeCuadrados n = sum [x^2 | x <- [1..n]]

-- enrnarbej
sumaDeCuadrados2 :: Integer -> Integer 
sumaDeCuadrados2 n = n*(n+1)*(2*n+1) `div` 6

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir por comprensión la función 
--    replica :: Int -> a -> [a]
-- tal que (replica n x) es la lista formada por n copias del elemento
-- x. Por ejemplo,  
--    replica 4 7     ==  [7,7,7,7]
--    replica 3 True  ==  [True, True, True]
-- Nota: La función replica es equivalente a la predefinida replicate.
-- ---------------------------------------------------------------------

-- enrnarbej antmorper3
replica :: Int -> a -> [a]
replica n x = [x | k <- [1..n]]

-- paumacpar
replica2 :: Int -> a -> [a]
replica2 n x = [ x | n <- [1..n]]

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Definir la función 
--    suma :: Integer -> Integer
-- tal (suma n) es la suma de los n primeros números. Por ejemplo,
--    suma 3  ==  6
-- ---------------------------------------------------------------------

-- fatfervaz enrnarbej antmorper3 eledejim2 roscargar paumacpar 
suma :: Integer -> Integer
suma n = sum [x | x <- [1..n]]

-- enrnarbej
suma2 :: Integer -> Integer
suma2 n = n*(n+1) `div` 2

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Los triángulos aritméticos se forman como sigue
--     1
--     2  3
--     4  5  6
--     7  8  9 10
--    11 12 13 14 15
--    16 17 18 19 20 21
-- Definir la función
--    linea :: Integer -> [Integer]
-- tal que (linea n) es la línea n-ésima de los triángulos
-- aritméticos. Por ejemplo,  
--    linea 4  ==  [7,8,9,10]
--    linea 5  ==  [11,12,13,14,15]
-- ---------------------------------------------------------------------

-- enrnarbej antmorper3
linea :: Integer -> [Integer]
linea n = [ x | x <- [k..k+n-1]]
  where k = suma (n-1) + 1 

-- Comentario: La definición linea se puede simplificar.

-- paumacpar 
linea2 :: Integer -> [Integer]
linea2 n = [x | x <- [y+1..y+n]]
  where y = sum [n-1, n-2..1]

-- Comentario: La definición linea2 se puede simplificar.

-- ---------------------------------------------------------------------
-- Ejercicio 3.3. Definir la función 
--    triangulo :: Integer -> [[Integer]]
-- tal que (triangulo n) es el triángulo aritmético de altura n. Por
-- ejemplo, 
--    triangulo 3  ==  [[1],[2,3],[4,5,6]]
--    triangulo 4  ==  [[1],[2,3],[4,5,6],[7,8,9,10]]
-- ---------------------------------------------------------------------

-- enrnarbej antmorper3 paumacpar 
triangulo :: Integer -> [[Integer]]
triangulo n = [linea n | n <- [1..n]]

-- Comentario: La definición anterior se puede mejorar.

-- ---------------------------------------------------------------------
-- Ejercicio 4. Un entero positivo es perfecto si es igual a la suma de
-- sus factores, excluyendo el propio número. 
-- 
-- Definir por comprensión la función 
--    perfectos :: Int -> [Int]
-- tal que (perfectos n) es la lista de todos los números perfectos
-- menores que n. Por ejemplo,  
--    perfectos 500  ==  [6,28,496]
-- Indicación: Usar la función factores del tema 5.
-- ---------------------------------------------------------------------

-- enrnarbej antmorper3 
perfectos :: Int -> [Int]
perfectos n = [x | x <- [1..n], sum (factores x) == 2*x]

factores :: Int -> [Int]
factores n = [x | x <- [1..n], n `mod` x == 0]

-- ---------------------------------------------------------------------
-- Ejercicio 5.1. Un número natural n se denomina abundante si es menor
-- que la suma de sus divisores propios. Por ejemplo, 12 y 30 son
-- abundantes pero 5 y 28 no lo son.
-- 
-- Definir la función 
--    numeroAbundante :: Int -> Bool
-- tal que (numeroAbundante n) se verifica si n es un número
-- abundante. Por ejemplo,  
--    numeroAbundante 5  == False
--    numeroAbundante 12 == True
--    numeroAbundante 28 == False
--    numeroAbundante 30 == True
-- ---------------------------------------------------------------------

-- enrnarbej antmorper3 
numeroAbundante :: Int -> Bool
numeroAbundante n = sum (factores n) > 2*n

-- ---------------------------------------------------------------------
-- Ejercicio 5.2. Definir la función  
--    numerosAbundantesMenores :: Int -> [Int]
-- tal que (numerosAbundantesMenores n) es la lista de números
-- abundantes menores o iguales que n. Por ejemplo,
--    numerosAbundantesMenores 50  ==  [12,18,20,24,30,36,40,42,48]
-- ---------------------------------------------------------------------

-- enrnarbej antmorper3
numerosAbundantesMenores :: Int -> [Int]
numerosAbundantesMenores n =
  [x | x <- [1..n], numeroAbundante x]

-- ---------------------------------------------------------------------
-- Ejercicio 5.3. Definir la función 
--    todosPares :: Int -> Bool
-- tal que (todosPares n) se verifica si todos los números abundantes
-- menores o iguales que n son pares. Por ejemplo,
--    todosPares 10    ==  True
--    todosPares 100   ==  True
--    todosPares 1000  ==  False
-- ---------------------------------------------------------------------

-- enrnarbej
todosPares :: Int -> Bool
todosPares n = and [even x | x <- numerosAbundantesMenores n]

-- antmorper3
todosPares2 :: Int -> Bool
todosPares2 n = all even (numerosAbundantesMenores n)

-- ---------------------------------------------------------------------
-- Ejercicio 5.4. Definir la constante 
--    primerAbundanteImpar :: Int
-- que calcule el primer número natural abundante impar. Determinar el
-- valor de dicho número.
-- ---------------------------------------------------------------------

-- enrnarbej
primerAbundanteImpar :: Int
primerAbundanteImpar = head [x | x <- [1..] , numeroAbundante x,  odd x]

-- Su cálculo es
--    ghci> primerAbundanteImpar
--    945

-- ---------------------------------------------------------------------
-- Ejercicio 6 (Problema 1 del proyecto Euler) Definir la función 
--    euler1 :: Int -> Int
-- tal que (euler1 n) es la suma de todos los múltiplos de 3 ó 5 menores
-- que n. Por ejemplo,
--    euler1 10  ==  23
-- 
-- Calcular la suma de todos los múltiplos de 3 ó 5 menores que 1000.
-- ---------------------------------------------------------------------

-- enrnarbej
euler1 :: Int -> Int
euler1 n =
  sum [x | x <- [1..n-1], x `mod` 3  == 0] +
  sum [x | x <- [1..n-1], x `mod` 5  == 0] -
  sum [x | x <- [1..n-1], x `mod` 15 == 0]

-- Cálculo:
--    Prelude> euler1 1000
--    233168

-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir la función 
--    circulo :: Int -> Int
-- tal que (circulo n) es el la cantidad de pares de números naturales
-- (x,y) que se encuentran dentro del círculo de radio n. Por ejemplo, 
--    circulo 3  ==  9
--    circulo 4  ==  15
--    circulo 5  ==  22
-- ---------------------------------------------------------------------

-- enrnarbej
circulo :: Int -> Int
circulo n =
  length [(x,y) | x <- [ 0..n] , y <- [ 0..n] , x^2 + y^2 < n^2]

-- ---------------------------------------------------------------------
-- Ejercicio 8.1. Definir la función 
--    aproxE :: Double -> [Double]
-- tal que (aproXE n) es la lista cuyos elementos son los términos de la
-- sucesión (1+1/m)**m desde 1 hasta n. Por ejemplo, 
--    aproxE 1 == [2.0]
--    aproxE 4 == [2.0,2.25,2.37037037037037,2.44140625]
-- ---------------------------------------------------------------------

-- enrnarbej
aproxE :: Double -> [Double]
aproxE n = [(1 + 1/m)**m | m <- [1..n]]

-- ---------------------------------------------------------------------
-- Ejercicio 8.2. ¿Cuál es el límite de la sucesión (1+1/m)**m ?
-- ---------------------------------------------------------------------

-- enrnarbej
-- El número e.

-- ---------------------------------------------------------------------
-- Ejercicio 8.3. Definir la función 
--    errorAproxE :: Double -> Double
-- tal que (errorE x) es el menor número de términos de la sucesión
-- (1+1/m)**m necesarios para obtener su límite con un error menor que
-- x. Por ejemplo, 
--    errorAproxE 0.1    ==  13.0
--    errorAproxE 0.01   ==  135.0
--    errorAproxE 0.001  ==  1359.0
-- Indicación: En Haskell, e se calcula como (exp 1).
-- ---------------------------------------------------------------------

-- enrnarbej
errorAproxE :: Double -> Double
errorAproxE x = head [m | m <- [1..] , abs (exp 1 - (1+1/m)**m) < x]

-- ---------------------------------------------------------------------
-- Ejercicio 9.1. Definir la función
--    aproxLimSeno :: Double -> [Double]
-- tal que (aproxLimSeno n) es la lista cuyos elementos son los términos
-- de la sucesión  
--    sen(1/m) 
--    --------
--      1/m 
-- desde 1 hasta n. Por ejemplo,
--    aproxLimSeno 1 == [0.8414709848078965]
--    aproxLimSeno 2 == [0.8414709848078965,0.958851077208406]
-- ---------------------------------------------------------------------

-- enrnarbej
aproxLimSeno :: Double -> [Double]
aproxLimSeno n = [ sin (1/m)*m | m <- [ 1..n]]

-- ---------------------------------------------------------------------
-- Ejercicio 9.2. ¿Cuál es el límite de la sucesión sen(1/m)/(1/m) ?
-- ---------------------------------------------------------------------

-- enrnarbej
-- 1

-- ---------------------------------------------------------------------
-- Ejercicio 9.3. Definir la función 
--    errorLimSeno :: Double -> Double
-- tal que (errorLimSeno x) es el menor número de términos de la sucesión 
-- sen(1/m)/(1/m) necesarios para obtener su límite con un error menor
-- que x. Por ejemplo, 
--    errorLimSeno 0.1     ==   2.0
--    errorLimSeno 0.01    ==   5.0
--    errorLimSeno 0.001   ==  13.0
--    errorLimSeno 0.0001  ==  41.0
-- ---------------------------------------------------------------------

-- enrnarbej
errorLimSeno :: Double -> Double
errorLimSeno x = head [m | m <- [1..] , abs (1 - sin (1/m)*m  ) < x]

-- ---------------------------------------------------------------------
-- Ejercicio 10.1. Definir la función 
--    calculaPi :: Double -> Double
-- tal que (calculaPi n) es la aproximación del número pi calculada
-- mediante la expresión 
--    4*(1 - 1/3 + 1/5 - 1/7 + ...+ (-1)**n/(2*n+1))
-- Por ejemplo,
--    calculaPi 3    ==  2.8952380952380956
--    calculaPi 300  ==  3.1449149035588526
-- ---------------------------------------------------------------------

-- enrnarbej
calculaPi :: Double -> Double
calculaPi n = 4 * sum [(-1)**m/(2*m+1)| m <- [0..n]]

-- ---------------------------------------------------------------------
-- Ejercicio 10.2. Definir la función 
--    errorPi :: Double -> Double
-- tal que (errorPi x) es el menor número de términos de la serie
--    4*(1 - 1/3 + 1/5 - 1/7 + ...+ (-1)**n/(2*n+1))
-- necesarios para obtener pi con un error menor que x. Por ejemplo,
--    errorPi 0.1    ==    9.0
--    errorPi 0.01   ==   99.0
--    errorPi 0.001  ==  999.0
-- ---------------------------------------------------------------------

-- enrnarbej
errorPi :: Double -> Double
errorPi x =
  head [k | (p,k) <- [(calculaPi n,n) | n <- [0..]]
          , abs (pi - p) < x]

-- ---------------------------------------------------------------------
-- Ejercicio 11.1. Una terna (x,y,z) de enteros positivos es pitagórica
-- si x^2 + y^2 = z^2. 
-- 
-- Definir, por comprensión, la función 
--    pitagoricas :: Int -> [(Int,Int,Int)]
-- tal que (pitagoricas n) es la lista de todas las ternas pitagóricas
-- cuyas componentes están entre 1 y n. Por ejemplo, 
--    pitagoricas 10  ==  [(3,4,5),(4,3,5),(6,8,10),(8,6,10)]
-- ---------------------------------------------------------------------

-- enrnarbej
pitagoricas :: Int -> [(Int,Int,Int)]
pitagoricas n =
  [(x,y,z) | x <- [1..n]
           , y <- [1..n]
           , z <- [1..n]
           , x^2 + y^2 == z^2]

-- ---------------------------------------------------------------------
-- Ejercicio 11.2. Definir la función 
--    numeroDePares :: (Int,Int,Int) -> Int
-- tal que (numeroDePares t) es el número de elementos pares de la terna
-- t. Por ejemplo,
--    numeroDePares (3,5,7)  ==  0
--    numeroDePares (3,6,7)  ==  1
--    numeroDePares (3,6,4)  ==  2
--    numeroDePares (4,6,4)  ==  3
-- ---------------------------------------------------------------------

-- enrnarbej
numeroDePares :: (Int,Int,Int) -> Int
numeroDePares (x,y,z) = sum [ 1 | k <- [x,y,z] , even k ]

-- ---------------------------------------------------------------------
-- Ejercicio 11.3. Definir la función
--    conjetura :: Int -> Bool
-- tal que (conjetura n) se verifica si todas las ternas pitagóricas
-- cuyas componentes están entre 1 y n tiene un número impar de números
-- pares. Por ejemplo,
--    conjetura 10  ==  True
-- ---------------------------------------------------------------------

-- enrnarbej
conjetura :: Int -> Bool
conjetura n = and [odd (numeroDePares x) | x <- pitagoricas n]

-- ---------------------------------------------------------------------
-- Ejercicio 11.4. Demostrar la conjetura para todas las ternas
-- pitagóricas. 
-- ---------------------------------------------------------------------

-- enrnarbej
-- Prelude> quickCheck conjetura 
-- +++ OK, passed 100 tests.

-- Comentario: Se pide la demostración matemática.

-- ---------------------------------------------------------------------
-- Ejercicio 12.1. (Problema 9 del Proyecto Euler). Una terna pitagórica
-- es una terna de números naturales (a,b,c) tal que a<b<c y
-- a^2+b^2=c^2. Por ejemplo (3,4,5) es una terna pitagórica. 
-- 
-- Definir la función 
--    ternasPitagoricas :: Integer -> [[Integer]]
-- tal que (ternasPitagoricas x) es la lista de las ternas pitagóricas
-- cuya suma es x. Por ejemplo,
--    ternasPitagoricas 12  ==  [(3,4,5)]
--    ternasPitagoricas 60  ==  [(10,24,26),(15,20,25)]
-- ---------------------------------------------------------------------

-- enrnarbej
ternasPitagoricas :: Integer -> [(Integer,Integer,Integer)]
ternasPitagoricas x =
  [(a,b,c) | a <- [0..l]
           , b <- [a+1..l]
           , c <- [b+1..l]
           , sum [a,b,c] == x
           , a^2 + b^2 == c^2]
  where
    l = div x 2 +1

-- Comentario: La definición anterior se puede mejorar.

-- ---------------------------------------------------------------------
-- Ejercicio 12.2. Definir la constante 
--    euler9 :: Integer
-- tal que euler9 es producto abc donde (a,b,c) es la única terna
-- pitagórica tal que a+b+c=1000.  
--
-- Calcular el valor de euler9.
-- ---------------------------------------------------------------------

-- enrnarbej
euler9 :: Integer
euler9 = producto (head (ternasPitagoricas 1000))

producto :: (Integer,Integer,Integer) -> Integer
producto (a,b,c) = a*b*c

-- El cálculo del valor de euler9 es 
-- 31875000 

-- ---------------------------------------------------------------------
-- Ejercicio 13. El producto escalar de dos listas de enteros xs y ys de
-- longitud n viene dado por la suma de los productos de los elementos
-- correspondientes. 
-- 
-- Definir por comprensión la función 
--    productoEscalar :: [Int] -> [Int] -> Int
-- tal que (productoEscalar xs ys) es el producto escalar de las listas
-- xs e ys. Por ejemplo,
--    productoEscalar [1,2,3] [4,5,6]  ==  32
-- ---------------------------------------------------------------------

-- enrnarbej
productoEscalar :: [Int] -> [Int] -> Int
productoEscalar xs ys =
  sum [xs!!x * ys!!x | x <- [0..length xs-1] ]

-- Comentario: La definición anterior se puede mejorar.

-- ---------------------------------------------------------------------
-- Ejercicio 14. Definir, por comprensión, la función
--    sumaConsecutivos :: [Int] -> [Int]
-- tal que (sumaConsecutivos xs) es la suma de los pares de elementos
-- consecutivos de la lista xs. Por ejemplo,
--    sumaConsecutivos [3,1,5,2]  ==  [4,6,7]
--    sumaConsecutivos [3]        ==  []
-- ---------------------------------------------------------------------

-- enrnarbej
sumaConsecutivos :: [Int] -> [Int]
sumaConsecutivos xs = [x+y | (x,y) <- zip xs (tail xs)]

-- ---------------------------------------------------------------------
-- Ejercicio 15. Los polinomios pueden representarse de forma dispersa o
-- densa. Por ejemplo, el polinomio 6x^4-5x^2+4x-7 se puede representar
-- de forma dispersa por [6,0,-5,4,-7] y de forma densa por
-- [(4,6),(2,-5),(1,4),(0,-7)].  
-- 
-- Definir la función 
--    densa :: [Int] -> [(Int,Int)]
-- tal que (densa xs) es la representación densa del polinomio cuya
-- representación dispersa es xs. Por ejemplo, 
--   densa [6,0,-5,4,-7]  ==  [(4,6),(2,-5),(1,4),(0,-7)]
--   densa [6,0,0,3,0,4]  ==  [(5,6),(2,3),(0,4)]
-- ---------------------------------------------------------------------

-- enrnarbej
densa :: [Int] -> [(Int,Int)]
densa xs =
  [(x,y) | (x,y) <- zip (reverse [0.. length xs -1]) xs, y /= 0]

-- ---------------------------------------------------------------------
-- Ejercicio 16.0. La bases de datos sobre actividades de personas pueden
-- representarse mediante listas de elementos de la forma (a,b,c,d),
-- donde a es el nombre de la persona, b su actividad, c su fecha de
-- nacimiento y d la de su fallecimiento. Un ejemplo es la siguiente que
-- usaremos a lo largo de este ejercicio,
-- ---------------------------------------------------------------------

personas :: [(String,String,Int,Int)]
personas = [("Cervantes","Literatura",1547,1616),
            ("Velazquez","Pintura",1599,1660),
            ("Picasso","Pintura",1881,1973),
            ("Beethoven","Musica",1770,1823),
            ("Poincare","Ciencia",1854,1912),
            ("Quevedo","Literatura",1580,1654),
            ("Goya","Pintura",1746,1828),
            ("Einstein","Ciencia",1879,1955),
            ("Mozart","Musica",1756,1791),
            ("Botticelli","Pintura",1445,1510),
            ("Borromini","Arquitectura",1599,1667),
            ("Bach","Musica",1685,1750)]

-- ---------------------------------------------------------------------
-- Ejercicio 16.1. Definir la función
--    nombres :: [(String,String,Int,Int)] -> [String]
-- tal que (nombres bd) es la lista de los nombres de las personas de la
-- base de datos bd. Por ejemplo,  
--    ghci> nombres personas
--     ["Cervantes","Velazquez","Picasso","Beethoven","Poincare",
--      "Quevedo","Goya","Einstein","Mozart","Botticelli","Borromini","Bach"]
-- ---------------------------------------------------------------------

-- enrnarbej
nombres :: [(String,String,Int,Int)] -> [String]
nombres bd = [a | (a,b,c,d) <- bd ]

-- ---------------------------------------------------------------------
-- Ejercicio 16.2. Definir la función
--    musicos :: [(String,String,Int,Int)] -> [String]
-- tal que (musicos bd) es la lista de los nombres de los músicos de la
-- base de datos bd. Por ejemplo,  
--    musicos personas  ==  ["Beethoven","Mozart","Bach"]
-- ---------------------------------------------------------------------

-- enrnarbej
musicos :: [(String,String,Int,Int)] -> [String]
musicos bd = [a | (a,b,c,d) <- bd , b == "Musica"]

-- ---------------------------------------------------------------------
-- Ejercicio 16.3. Definir la función 
--    seleccion :: [(String,String,Int,Int)] -> String -> [String]
-- tal que (seleccion bd m) es la lista de los nombres de las personas
-- de la base de datos bd cuya actividad es m. Por ejemplo,  
--    ghci> seleccion personas "Pintura"
--    ["Velazquez","Picasso","Goya","Botticelli"]
--    ghci> seleccion personas "Musica"
--    ["Beethoven","Mozart","Bach"]
-- ---------------------------------------------------------------------

-- enrnarbej
seleccion :: [(String,String,Int,Int)] -> String -> [String]
seleccion bd m = [ a | (a,b,c,d) <- bd , b == m]

-- ---------------------------------------------------------------------
-- Ejercicio 16.4. Definir, usando el apartado anterior, la función
--    musicos' :: [(String,String,Int,Int)] -> [String]
-- tal que (musicos' bd) es la lista de los nombres de los músicos de la
-- base de datos bd. Por ejemplo,   
--    ghci> musicos' personas
--    ["Beethoven","Mozart","Bach"]
-- ---------------------------------------------------------------------

-- enrnarbej
musicos' :: [(String,String,Int,Int)] -> [String]
musicos' bd = seleccion bd "Musica"

-- ---------------------------------------------------------------------
-- Ejercicio 16.5. Definir la función 
--    vivas :: [(String,String,Int,Int)] -> Int -> [String]
-- tal que (vivas bd a) es la lista de los nombres de las personas de la
-- base de datos bd  que estaban vivas en el año a. Por ejemplo,  
--    ghci> vivas personas 1600
--    ["Cervantes","Velazquez","Quevedo","Borromini"]
-- ---------------------------------------------------------------------

-- enrnarbej
vivas :: [(String,String,Int,Int)] -> Int -> [String]
vivas bd y = [ a | (a,b,c,d) <- bd , c <= y , d >= y]
