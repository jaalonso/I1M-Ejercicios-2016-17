-- I1M 2015-16: Rel_3.hs (2 de Octubre de 2015)
-- Definiciones por comprensión (Ejercicios resueltos)
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- En esta relación se presentan ejercicios con definiciones por
-- comprensión correspondientes al tema 5 que se encuentra
--    http://www.cs.us.es/~jalonso/cursos/i1m-15/temas/tema-5.html

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir, por comprensión, la función 
--    sumaDeCuadrados :: Integer -> Integer 
-- tal que (sumaDeCuadrados n) es la suma de los cuadrados de los
-- primeros n números; es decir, 1^2 + 2^2 + ... + n^2. Por ejemplo,
--    sumaDeCuadrados 3    ==  14
--    sumaDeCuadrados 100  ==  338350
-- ---------------------------------------------------------------------

-- guache carmengar manvermor erisancha carruirui3 lucgamgal josllagam
-- alvalvdom1 pabmorgar fracruzam jespergue juanarcon blaruiher
-- manvazbar1 rubvilval ivaruicam isrbelnun javperlag abrdelrod
-- silgongal carboncar irecasmat manpende fatvilpiz juamorrom1 marcamde3
-- paocabper alelobcan 
sumaDeCuadrados :: Integer -> Integer 
sumaDeCuadrados n = sum [x^2 | x <- [1..n]]

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir por comprensión la función 
--    replica :: Int -> a -> [a]
-- tal que (replica n x) es la lista formada por n copias del elemento
-- x. Por ejemplo,  
--    replica 4 7     ==  [7,7,7,7]
--    replica 3 True  ==  [True, True, True]
-- Nota: La función replica es equivalente a la predefinida replicate.
-- ---------------------------------------------------------------------

-- manvermor erisancha carruirui3 alvalvdom1 carmengar pabmorgar
-- alelobcan josllagam jespergue juanarcon blaruiher rubvilval isrbelnun
-- javperlag fracruzam silgongal carboncar manvazbar1 manpende
-- juamorrom1 paocabper 
replica :: Int -> a -> [a]
replica n x = [x | _ <- [1..n]]

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Definir la función 
--    suma :: Integer -> Integer
-- tal (suma n) es la suma de los n primeros números. Por ejemplo,
--    suma 3  ==  6
-- ---------------------------------------------------------------------

-- guache carmengar manvermor erisancha carruirui3 lucgamgal alvalvdom1 
-- pedjaecar pabmorgar josllagam fracruzam jespergue juanarcon blaruiher
-- ivaruicam isrbelnun javperlag abrdelrod carboncar manvazbar1 manpende
-- silgongal rubvilval juamorrom1 marcamde3 paocabper alelobcan
suma :: Integer -> Integer
suma n = sum [x | x <- [1..n]]

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

-- carmengar manvermor carruirui3 lucgamgal alvalvdom1 josllagam
-- abrdelrod juanarcon manpende paocabper alelobcan
linea :: Integer -> [Integer]
linea n =  [x | x <- [(1 + suma (n-1))..suma n]]

-- Comentario: La definición anterior se puede simplificar.

-- erisancha pabmorgar blaruiher ivaruicam silgongal rubvilval
-- juamorrom1 
linea2 :: Integer -> [Integer]
linea2 n =  [x | x <- [1 + suma (n-1)..suma n]]

-- Comentario: La definición anterior se puede simplificar.

-- isrbelnun
linea3 :: Integer -> [Integer]
linea3 n = [suma n - (n-1) .. suma n]

-- Comentario: La definición anterior se puede mejorar.

-- javperlag 
linea4 :: Integer -> [Integer] 
linea4 x = [suma(x-1) +1 .. suma x]

-- Comentario: La definición anterior se puede mejorar.

-- fracruzam irecasmat
linea5 :: Integer -> [Integer] 
linea5 n = [x | x <- [z + 1..z + n]]
    where z = div (n*(n-1)) 2

--manvazbar1
linea6 :: Integer -> [Integer] 
linea6 n = [x | x <- [sum [1..n-1] + 1..sum [1..n-1] + n]]

-- marvilmor
linea7 :: Integer -> [Integer]
linea7 n = take (fromIntegral n) [sum [1..n-1]+1..]

-- ---------------------------------------------------------------------
-- Ejercicio 3.3. Definir la función 
--    triangulo :: Integer -> [[Integer]]
-- tal que (triangulo n) es el triángulo aritmético de altura n. Por
-- ejemplo, 
--    triangulo 3  ==  [[1],[2,3],[4,5,6]]
--    triangulo 4  ==  [[1],[2,3],[4,5,6],[7,8,9,10]]
-- ---------------------------------------------------------------------

-- carmengar manvermor erisancha carruirui3 lucgamgal alvalvdom1 josllagam
-- pabmorgar jespergue juanarcon blaruiher isrbelnun javperlag fracruzam
-- abrdelrod silgongal irecasmat rubvilval manvazbar1 juamorrom1
-- marcamde3 manpende paocabper marvilmor alelobcan
triangulo :: Integer -> [[Integer]]
triangulo n = [linea x | x <- [1..n]]

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

-- carmengar erisancha pabmorgar ivaruicam blaruiher abrdelrod silgongal
-- rubvilval irecasmat manvazbar1 manpende isrbelnun
perfectos :: Int -> [Int]
perfectos n = [x | x <- [1..n], sum (factores x) - x == x]

factores n = [x | x <- [1..n], mod n x == 0]

-- guache carruirui3 lucgamgal juanarcon paocabper juamorrom1
perfectos2 :: Int -> [Int]
perfectos2 n = [x | x <- [1..n-1], x == sum (init (factores x))]

-- alvalvdom1
perfectos2' :: Int -> [Int]
perfectos2' n = [x | x <- [1..n-1], x == sum (init (factores x))]

-- manvermor javperlag
perfectos3 :: Int -> [Int]
perfectos3 n = [x | x <- [1..n], sum (factores x) == 2*x]

-- josllagam
perfectos4 :: Int -> [Int]
perfectos4 n = [x | x <- [1..n], x == sum (factores4 x)]

factores4 n = [x | x <- [1..n], (mod n x)==0, x/=n]

-- Comentario: La definición de factores4 se puede simplificar.

-- fracruzam
perfectos5 :: Int -> [Int]
perfectos5 n = [x | x <- [1..n] , x == sum (factores x)]
            
factores5 :: Int -> [Int]
factores5 n = [x | x <- [1..n-1] , rem n x == 0]


-- marcamde3
perfectos7 :: Int -> [Int]
perfectos7 n = [x | x <- [1..n], sum (factores x) - x == x]
    where factores n = [x | x <- [1..n], mod n x == 0]
--alelobcan
perfectos8 :: Int -> [Int]
perfectos8 n = [ x | x <- [6..n], sum(factores x)-x==x]
factores8 n = [x | x <- [1..n], mod n x ==0]

-- ---------------------------------------------------------------------
-- Ejercicio 5.1. Un número natural n se denomina abundante si es menor
-- que la suma de sus divisores propios. Por ejemplo, 12 y 30 son
-- abundantes pero 5 y 28 no lo son.
-- 
-- Definir la función 
--    numeroAbundante :: Int -> Bool
-- tal que (numeroAbundante n) se verifica si n es un número
-- abundante. Por ejemplo,  
--    numeroAbundante 6  == False
--    numeroAbundante 12 == True
--    numeroAbundante 28 == False
--    numeroAbundante 30 == True
-- ---------------------------------------------------------------------

-- carmengar erisancha pabmorgar alvalvdom1 ivaruicam blaruiher
-- abrdelrod manvazbar1 manpende 
numeroAbundante :: Int -> Bool
numeroAbundante n = sum (factores n) - n > n

-- Pregunta: ¿divisores propios incluye al 1?

-- guache
numeroAbundante2 :: Int -> Bool
numeroAbundante2 n = and [n < sum (init (factores n))]

-- Comentario: La definición anterior se puede mejorar.

-- carruirui3 manvermor lucgamgal juanarcon silgongal
numeroAbundante3 :: Int -> Bool
numeroAbundante3 n = sum (factores n) > 2*n

-- josllagam paocabper
numeroAbundante4 :: Int -> Bool
numeroAbundante4 n = n < sum (factores n)

-- fracruzam 
numeroAbundante5 :: Int -> Bool
numeroAbundante5 n = n < sum [x | x <- [2..(n-1)] , mod n x == 0]

-- Comentario: La definición anterior se puede simplificar.

-- rubvilval juamorrom1 isrbelnun
numeroAbundante6 :: Int -> Bool
numeroAbundante6 n = sum(init(factores n)) > n

--marcamde3
numeroAbundante7 :: Int -> Bool
numeroAbundante7 n = n < sum [x | x <- [1..n-1], mod n x == 0]

-- ---------------------------------------------------------------------
-- Ejercicio 5.2. Definir la función  
--    numerosAbundantesMenores :: Int -> [Int]
-- tal que (numerosAbundantesMenores n) es la lista de números
-- abundantes menores o iguales que n. Por ejemplo,
--    numerosAbundantesMenores 50  ==  [12,18,20,24,30,36,40,42,48]
-- ---------------------------------------------------------------------

-- carmengar carruirui3 erisancha manvermor lucgamgal pabmorgar
-- josllagam alvalvdom1 josllagam fracruzam ivaruicam abrdelrod
-- juanarcon blaruiher silgongal rubvilval manvazbar1 juamorrom1
-- manpende paocabper isrbelnun
numerosAbundantesMenores :: Int -> [Int]
numerosAbundantesMenores n = [x | x <- [1..n], numeroAbundante x]

-- guache
numerosAbundantesMenores2 :: Int -> [Int]
numerosAbundantesMenores2 n = 
    [x | x <- [1..n], x < sum (init(factores x))]

-- Comentario: La definición anterior se puede simplificar.

-- ---------------------------------------------------------------------
-- Ejercicio 5.3. Definir la función 
--    todosPares :: Int -> Bool
-- tal que (todosPares n) se verifica si todos los números abundantes
-- menores o iguales que n son pares. Por ejemplo,
--    todosPares 10    ==  True
--    todosPares 100   ==  True
--    todosPares 1000  ==  False
-- ---------------------------------------------------------------------

-- carmengar guache carruirui3 manvermor erisancha lucgamgal pabmorgar
-- alvalvdom1 josllagam fracruzam juanarcon blaruiher silgongal
-- rubvilval juamorrom1 manpende paocabper isrbelnun
todosPares :: Int -> Bool
todosPares n = all even (numerosAbundantesMenores n)

-- guache 
todosPares2 :: Int -> Bool
todosPares2 n = 
    and [even x | x <- [1..n], x < sum (init(factores x))]

-- abrdelrod manvazbar1
todosPares3 :: Int -> Bool
todosPares3 n = [x | x <- xs, even x] == xs
    where xs = numerosAbundantesMenores n

-- ---------------------------------------------------------------------
-- Ejercicio 5.4. Definir la constante 
--    primerAbundanteImpar :: Int
-- que calcule el primer número natural abundante impar. Determinar el
-- valor de dicho número.
-- ---------------------------------------------------------------------

-- guache carmengar manpende
primerAbundanteImpar :: Int
primerAbundanteImpar = 
    head [x | x <- [1,3..], x< sum (init(factores x))]

-- Comentario: La definición anterior se puede simplificar.

-- guache carruirui3 manvermor erisancha lucgamgal pabmorgar alvalvdom1
-- fracruzam josllagam juanarcon blaruiher silgongal rubvilval
-- manvazbar1 juamorrom1 paocabper 
primerAbundanteImpar3 :: Int
primerAbundanteImpar3 = head [x | x<- [1,3..], numeroAbundante x]

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

-- carmengar erisancha guache carruirui3 carmengar josllagam abrdelrod
-- paocabper carboncar blaruiher manpende silgongal 
euler1 :: Int -> Int
euler1 n = sum [x | x <- [1..n-1], rem x 3 == 0 || rem x 5 == 0]

-- manvermor pabmorgar
euler1b :: Int -> Int
euler1b n = sum [x | x <- [1..n-1], multiplo x 3 ||  multiplo x 5]
    where multiplo x y = x `mod` y == 0

-- alvalvdom1 lucgamgal fracruzam juanarcon manvazbar1 ivaruicam juamorrom1
euler1c :: Int -> Int
euler1c n = sum [x | x <- [1..n-1], x `mod` 3 == 0 || x `mod` 5 == 0]

-- rubvilval
euler1d :: Int -> Int
euler1d n = sum [x | x <- [3..n-1], gcd x 3 == 3 || gcd x 5 == 5]


-- Cálculo: euler1 1000 = 233168

-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir la función
--    circulo :: Int -> Int
-- tal que (circulo n) es el la cantidad de pares de números naturales
-- (x,y) que se encuentran dentro del círculo de radio n. Por ejemplo, 
--    circulo 3  ==  9
--    circulo 4  ==  15
--    circulo 5  ==  22
-- ---------------------------------------------------------------------

-- carmengar guache carruirui3 manvermor erisancha lucgamgal pabmorgar
-- fracruzam abrdelrod juanarcon carboncar blaruiher silgongal rubvilval
-- manvazbar1 manpende ivaruicam josllagam juamorrom1
circulo :: Int -> Int
circulo n = length [(x,y) | x <- [0..n], y <- [0..n], x^2+y^2 < n^2]
 
-- manvazbar1: ¿consideramos el 0 natural?; ¿no sería <= n^2 porque si
-- se encuentra en el perímetro estaría dentro? 

-- ---------------------------------------------------------------------
-- Ejercicio 8.1. Definir la función 
--    aproxE :: Double -> [Double]
-- tal que (aproXE n) es la lista cuyos elementos son los términos de la
-- sucesión (1+1/m)**m desde 1 hasta n. Por ejemplo, 
--    aproxE 1 == [2.0]
--    aproxE 4 == [2.0,2.25,2.37037037037037,2.44140625]
-- ---------------------------------------------------------------------

-- carmengar guache manvermor carruirui3 alvalvdom1 erisancha lucgamgal
-- pabmorgar fracruzam josllagam abrdelrod juanarcon blaruiher carboncar
-- rubvilval ivaruicam manvazbar1 juamorrom1
aproxE :: Double -> [Double]
aproxE n = [(1+1/m)**m | m <- [1..n]]

-- ---------------------------------------------------------------------
-- Ejercicio 8.2. ¿Cuál es el límite de la sucesión (1+1/m)**m ?
-- ---------------------------------------------------------------------

-- carmengar carruirui3 erisancha lucgamgal pabmorgar fracruzam 
-- alvalvdom1 josllagam abrdelrod juanarcon blaruiher carboncar
-- rubvilval manvazbar1 juamorrom1

-- Sol.: El límite es el número e
 
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

-- carmengar carruirui3 erisancha josllagam abrdelrod blaruiher manvazbar1
errorAproxE :: Double -> Double
errorAproxE x = head [n | n <- [1..], last(aproxE n) > (exp 1) - x]

-- Comentario: La definición anterior se puede mejorar.

-- pabmorgar lucgamgal fracruzam juanarcon rubvilval juamorrom1
errorAproxE2 :: Double -> Double
errorAproxE2 x = head [m | m <- [1..], abs ((1+1/m)**m - exp(1)) < x]

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

-- carmengar manvermor carruirui3 erisancha lucgamgal pabmorgar
-- fracruzam josllagam alvalvdom1 juanarcon blaruiher rubvilval manvazbar1
-- abrdelrod ivaruicam juamorrom1 silgongal
aproxLimSeno :: Double -> [Double]
aproxLimSeno n = [sin (1/m)/(1/m)| m <- [1..n]]

-- ---------------------------------------------------------------------
-- Ejercicio 9.2. ¿Cuál es el límite de la sucesión sen(1/m)/(1/m) ?
-- ---------------------------------------------------------------------

-- carmengar carruirui3 erisancha lucgamgal pabmorgar fracruzam:
-- alvalvdom1 josllagam juanarcon blaruiher rubvilval abrdelrod manvazbar1
-- juamorrom1 silgongal

-- Sol.: El límite es 1

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

-- carmengar carruirui3 erisancha josllagam blaruiher rubvilval
-- abrdelrod manvazbar1
errorLimSeno :: Double -> Double
errorLimSeno x = head [n | n <- [1..], last (aproxLimSeno n) > 1 - x]

-- Comentario: La definición anterior se puede mejorar.

-- pabmorgar lucgamgal fracruzam juanarcon juamorrom1 silgongal
errorLimSeno2 :: Double -> Double
errorLimSeno2 x = head [n | n <- [1..], abs (sin(1/n)/(1/n)- 1) < x]

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

-- carmengar manvermor carruirui3 erisancha lucgamgal pabmorgar
-- fracruzam josllagam juanarcon blaruiher rubvilval abrdelrod manpende
-- ivaruicam manvazbar1 juamorrom1
calculaPi :: Double -> Double
calculaPi n = 4*sum [(-1)**m/(2*m+1)| m <- [0..n]]

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

-- carmengar carruirui3 erisancha pabmorgar lucgamgal fracruzam
-- juanarcon blaruiher rubvilval abrdelrod manvazbar1 juamorrom1
errorPi :: Double -> Double
errorPi x = head [n | n <- [1..] , abs (pi - (calculaPi n)) < x]

-- Comentario: La definición anterior se puede mejorar.

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

-- carruirui3 manvermor carmengar erisancha lucgamgal pabmorgar
-- alvalvdom1 fracruzam josllagam juanarcon blaruiher rubvilval
-- abrdelrod manpende ivaruicam manvazbar1 juamorrom1 silgongal
pitagoricas :: Int -> [(Int,Int,Int)] 
pitagoricas n = [(x,y,z) | x <- [1..n],
                           y <- [1..n],
                           z <- [1..n],
                           x^2 + y^2 == z^2]

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

-- guache carmengar carruirui3 manvermor lucgamgal pabmorgar fracruzam
-- alvalvdom1 blaruiher rubvilval 
numeroDePares :: (Int,Int,Int) -> Int
numeroDePares (x,y,z) = length (filter even [x,y,z])

-- pedestara carruirui3 erisancha josllagam juanarcon abrdelrod manpende
-- ivaruicam manvazbar1 juamorrom1 silgongal
numeroDePares2 :: (Int,Int,Int)->Int
numeroDePares2 (x,y,z) = length [1 | n <- [x, y, z], even n]

-- ---------------------------------------------------------------------
-- Ejercicio 11.3. Definir la función
--    conjetura :: Int -> Bool
-- tal que (conjetura n) se verifica si todas las ternas pitagóricas
-- cuyas componentes están entre 1 y n tiene un número impar de números
-- pares. Por ejemplo,
--    conjetura 10  ==  True
-- ---------------------------------------------------------------------

-- carmengar blaruiher rubvilval josllagam manvazbar1
conjetura :: Int -> Bool
conjetura n = and (map odd (map numeroDePares (pitagoricas n)))

-- carruirui3 erisancha pabmorgar fracruzam juanarcon alvalvdom1
-- juamorrom1 silgongal 
conjetura2 :: Int -> Bool
conjetura2 n = and [odd (numeroDePares terna) | terna <- pitagoricas n]

-- carruirui3 lucgamgal
-- Similar a la versión de carmengar, pero aprovechando la composición
-- de funciones 
conjetura3 :: Int -> Bool
conjetura3 n = and (map (odd . numeroDePares) (pitagoricas n))

-- ---------------------------------------------------------------------
-- Ejercicio 11.4. Demostrar la conjetura para todas las ternas
-- pitagóricas. 
-- ---------------------------------------------------------------------

-- carruirui3 erisancha silgongal
-- Todas las ternas pitagóricas tienen un número impar de números pares.
-- Demostración por reducción al absurdo: supongamos que hay una terna
-- pitagórica con un número impar de números pares.
--
-- Caso 1: {a, b} pares; c impar
-- a y b pueden ser expresados como el doble de otros naturales a' y b'.
-- Entonces:
-- a^2 + b^2 = c^2 \iff (2*a')^2 + (2*b')^2 = c^2
-- Sacando factor común:
-- 4*(a'^2 + b^2) = c^2
-- a'^2 + b'^2 = (c/2)^2
-- Dado que la suma y el producto de naturales devuelven
-- naturales, (c/2) es natural, por lo que c es par. CONTRADICCIÓN
--
-- Caso 2: a impar; {b, c} pares (equivalente a {a, c} pares y b impar)
-- De forma similar al caso anterior:
-- a^2 + b^2 = c^2 \iff a^2 + (2*b')^2 = (2*c')^2
-- Reordenamos la ecuación:
-- a^2 = (2*c')^2 - (2*b')^2
-- Sacando factor común:
-- a^2 = 4*(c'^2 - b'^2)
-- (a/2)^2 = c'^2 - b'^2
-- Dado que la suma y el producto de naturales devuelven
-- naturales, (a/2) es natural, por lo que a es par. CONTRADICCIÓN
-- QED

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

-- carruirui3
-- Esta versión es más legible y eficiente. La definición de x ahorra 
-- un generador y una guarda, mientras que el uso de z `div` 2 como 
-- límite inferior de y genera automáticamente ternas ordenadas
-- gratuitamente ternas ordenadas reduciendo a la mitad la lista de ys.
ternasPitagoricas :: Integer -> [(Integer,Integer,Integer)]
ternasPitagoricas n = [(x,y,z) | z <- [1..n],
                                 y <- [z `div` 2..z],
                                 let x = n-z-y,
                                 x^2 + y^2 == z^2]

-- pabmorgar erisancha juanarcon blaruiher rubvilval juamorrom1 silgongal
ternasPitagoricas2 :: Integer -> [(Integer,Integer,Integer)]
ternasPitagoricas2 x = [(a,b,c) | a <- [1..x],
                                  b <- [a+1..x],
                                  c <- [x-a-b],
                                  a^2 + b^2 == c^2]
-- fracruzam manpende alvalvdom1 abrdelrod josllagam manvazbar1
ternasPitagoricas3 :: Integer -> [(Integer,Integer,Integer)]
ternasPitagoricas3 x = [(a,b,c) | a <- [1..x] , b <- [1..x] , c <- [1..x] ,
                                  a < b , b < c  , a^2 + b^2 == c^2 ,
                                  a+b+c == x]

-- Comentario: La definición anterior se puede mejorar.

-- ---------------------------------------------------------------------
-- Ejercicio 12.2. Definir la constante 
--    euler9 :: Integer
-- tal que euler9 es producto abc donde (a,b,c) es la única terna
-- pitagórica tal que a+b+c=1000.  
--
-- Calcular el valor de euler9.
-- ---------------------------------------------------------------------

-- carruirui3 manvermor lucgamgal pabmorgar fracruzam erisancha josllagam
-- juanarcon manpende alvalvdom1 rubvilval blaruiher abrdelrod manvazbar1
-- juamorrom1 silgongal
euler9 :: Integer
euler9 = a*b*c where (a,b,c) = head (ternasPitagoricas 1000)

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

-- guache carmengar
productoEscalar :: [Int] -> [Int] -> Int
productoEscalar xs ys = sum [x | x<- zipWith (*) xs ys]

-- Comentario: La definición anterior se puede simplificar.

-- guache carmengar manvermor carruirui3 lucgamgal pabmorgar fracruzam
-- alvalvdom1 rubvilval juanarcon abrdelrod manpende juamorrom1 silgongal
productoEscalar2 :: [Int] -> [Int] -> Int 
productoEscalar2 xs ys = sum [x*y | (x,y) <- zip xs ys ]

-- carruirui3 erisancha blaruiher manvazbar1
productoEscalar3 :: [Int] -> [Int] -> Int
productoEscalar3 xs ys = sum (zipWith (*) xs ys)

-- ---------------------------------------------------------------------
-- Ejercicio 14. Definir, por comprensión, la función
--    sumaConsecutivos :: [Int] -> [Int]
-- tal que (sumaConsecutivos xs) es la suma de los pares de elementos
-- consecutivos de la lista xs. Por ejemplo,
--    sumaConsecutivos [3,1,5,2]  ==  [4,6,7]
--    sumaConsecutivos [3]        ==  []
-- ---------------------------------------------------------------------

-- guache
sumaConsecutivos :: [Int] -> [Int]
sumaConsecutivos xs = zipWith (+) xs (tail xs)

-- guache carruirui3 manvermor fracruzam alvalvdom1 manpende
sumaConsecutivos2 :: [Int] -> [Int]
sumaConsecutivos2 [_] = []
sumaConsecutivos2 xs = [x+y | (x,y) <- zip xs (tail xs)]

-- Comentario: La definición anterior se puede simplificar.

-- pabmorgar lucgamgal erisancha juanarcon rubvilval abrdelrod manvazbar1
-- juamorrom1 silgongal
-- La definición anterior sin el caso del lista de un solo elemento
sumaConsecutivos3 :: [Int] -> [Int]
sumaConsecutivos3 xs = [x+y | (x,y) <- zip xs (tail xs)]

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

-- guache juanarcon abrdelrod manpende fracruzam silgongal
densa :: [Int] -> [(Int,Int)]
densa xs = 
    [(a,b) | (a,b) <- zip (reverse [0..(length xs -1)]) xs, b /= 0]

-- Comentario: La definición anterior se puede mejorar.

-- manvermor pabmorgar erisancha
densa2 :: [Int] -> [(Int,Int)]
densa2 xs = [(x,y) | (x,y) <- zip ys xs]
    where ys = reverse [0..length xs-1]

-- Comentario: La definición anterior se puede mejorar.

-- carruirui3 lucgamgal rubvilval
densa3 :: [Int] -> [(Int, Int)]
densa3 xs = [y | y <- zip [length xs - 1, length xs - 2..] xs,
                 snd y /= 0]

-- Comentario: La definición anterior se puede mejorar.

-- fracruzam manvazbar1
-- Igual que la definición 1, pero con menos paréntesis.
densa4 :: [Int] -> [(Int,Int)]
densa4 xs = 
    [(a,b) | (a,b) <- zip (reverse [0..length xs -1]) xs , b /= 0]

-- josllagam juamorrom1
densa5 :: [Int] -> [(Int,Int)]
densa5 xs = [(x,y)| (x,y) <- zip [length xs-1,length xs-2..0] xs, y/=0]

-- Comentario: La definición anterior se puede mejorar.

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

-- guache carruirui3 manvermor lucgamgal pabmorgar alvalvdom1 erisancha
-- juanarcon blaruiher rubvilval abrdelrod manpende fracruzam josllagam
-- manvazbar1 silgongal 
nombres :: [(String,String,Int,Int)] -> [String]
nombres bd = [a | (a,_,_,_) <- bd]

-- ---------------------------------------------------------------------
-- Ejercicio 16.2. Definir la función
--    musicos :: [(String,String,Int,Int)] -> [String]
-- tal que (musicos bd) es la lista de los nombres de los músicos de la
-- base de datos bd. Por ejemplo,  
--    musicos personas  ==  ["Beethoven","Mozart","Bach"]
-- ---------------------------------------------------------------------

-- guache carruirui3 manvermor lucgamgal pabmorgar alvalvdom1 juanarcon
-- blaruiher abrdelrod manpende manvazbar1
musicos :: [(String,String,Int,Int)] -> [String]
musicos bd = [a | (a,c,_,_) <- bd, c == "Musica"]

-- erisancha rubvilval fracruzam josllagam silgongal
musicos2 :: [(String,String,Int,Int)] -> [String]
musicos2 bd = [x| (x, "Musica",_,_) <- bd]

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

-- guache carruirui3 manvermor lucgamgal pabmorgar alvalvdom1 erisancha
-- juanarcon rubvilval abrdelrod manpende fracruzam josllagam manvazbar1
-- silgongal 
seleccion :: [(String,String,Int,Int)] -> String -> [String]
seleccion bd m = [a | (a,c,_,_) <- bd, c == m]

-- ---------------------------------------------------------------------
-- Ejercicio 16.4. Definir, usando el apartado anterior, la función
--    musicos' :: [(String,String,Int,Int)] -> [String]
-- tal que (musicos' bd) es la lista de los nombres de los músicos de la
-- base de datos bd. Por ejemplo,   
--    ghci> musicos' personas
--    ["Beethoven","Mozart","Bach"]
-- ---------------------------------------------------------------------

-- guache carruirui3 manvermor lucgamgal pabmorgar alvalvdom1 erisancha
-- juanarcon blaruiher rubvilval abrdelrod fracruzam josllagam
-- manvazbar1 silgongal 
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

-- guache manvermor lucgamgal pabmorgar alvalvdom1 erisancha juanarcon
-- blaruiher rubvilval abrdelrod manpende fracruzam josllagam manvazbar1
-- silgongal 
vivas :: [(String,String,Int,Int)] -> Int -> [String]
vivas ps a = [c | (c,_,u,v) <- ps, u<=a && v>= a]

-- carruirui3
-- el cambio de orden ayuda a visualizar la relación nac <= a <= def
-- Además, las variables usan nombres más comprensibles.
vivas2 :: [(String,String,Int,Int)] -> Int -> [String]
vivas2 bd a = [n | (n,_,nac,def) <- bd, nac <= a && a <= def]
