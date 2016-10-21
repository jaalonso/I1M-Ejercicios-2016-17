-- I1M 2016-17: Rel_3.hs (5 de Octubre de 2016)
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
import Data.List

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir, por comprensión, la función 
--    sumaDeCuadrados :: Integer -> Integer 
-- tal que (sumaDeCuadrados n) es la suma de los cuadrados de los
-- primeros n números; es decir, 1^2 + 2^2 + ... + n^2. Por ejemplo,
--    sumaDeCuadrados 3    ==  14
--    sumaDeCuadrados 100  ==  338350
-- ---------------------------------------------------------------------

-- fatfervaz eledejim2 enrnarbej antmorper3 roscargar paumacpar
-- cargonler beagongon1 pabrabmon artmorfer margarflo5 albcercid
-- monlagare ignareeva natmarmar2 glovizcas margarvil14 marmerzaf
-- mardelrui marjimcom antbeacar joscasgom1 felsuacor eliguivil belbenzam
-- migibagar fraferpoy congomgom carmarcar5 antdursan albagucen
-- juacasnie juaorture manruiber natruipin luimotmar josjimgon2 josdeher
-- josrodgal7 javleilor antlopgom2 margirmon alvfercen marlobrip cescarde
-- javcancif
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

-- enrnarbej antmorper3 cargonler beagongon1 pabrabmon monlagare mardelrui
-- roscargar josdeher josrodgal7 javleilor marjimcom margirmon marlobrip
replica :: Int -> a -> [a]
replica n x = [x | k <- [1..n]]

-- paumacpar artmorfer margarflo5 ignareeva natmarmar2 fatfervaz glovizcas
-- marmerzaf joscasgom1 felsuacor eledejim2 belbenzam fraferpoy
-- congomgom antdursan albagucen antbeacar luimotmar manruiber
-- josjimgon2 alvfercen antlopgom2 javcancif
replica2 :: Int -> a -> [a]
replica2 n x = [ x | n <- [1..n]]

-- pabrabmon
replica3 :: Int -> a -> [a]
replica3 n x = [x | (x,n) <- zip (repeat x) [1..n]]

-- albcercid
replica4 :: Int -> a -> [a]
replica4 n x = primeros [ (x,n) | n <- [1..n] ]
  where primeros xs = [x | (x,_) <- xs]

-- eliguivil
replica5 :: Int -> a -> [a]
replica5 n x = [x | (x,_) <- [(x,y) | y <- [1..n]]]

-- Comentario: La definición replica5 se puede simplificar.

--juacasnie cescarde
replica6 :: Int -> a -> [a]
replica6 n x = [x | _  <- [1..n]]

-- juaorture
replica7 :: Int -> a -> [a]
replica7 n x = [fst (a,b) | (a,b) <- (zip (repeat x) [1..n])]

-- Comentario: La definición de replica7 se puede simplificar.

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Definir la función 
--    suma :: Integer -> Integer
-- tal (suma n) es la suma de los n primeros números. Por ejemplo,
--    suma 3  ==  6
-- ---------------------------------------------------------------------

-- fatfervaz enrnarbej antmorper3 eledejim2 roscargar paumacpar margarflo5
-- cargonler beagongon1 albcercid monlagare natmarmar2 marjimcom joscasgom1
-- eliguivil belbenzam migibagar fraferpoy carmarcar5 antdursan
-- albagucen manruiber natruipin josjimgon2 juacasnie javleilor  antlopgom2
suma :: Integer -> Integer
suma n = sum [x | x <- [1..n]]

-- Comentario: La definición anterior se puede simplificar.

-- enrnarbej
suma2 :: Integer -> Integer
suma2 n = n*(n+1) `div` 2

-- pabrabmon artmorfer antbeacar glovizcas luimotmar ignareeva margarvil14
-- marmerzaf mardelrui felsuacor congomgom albagucen josdeher josrodgal7
-- juaorture margirmon alvfercen cescarde javcancif
suma3 :: Integer -> Integer 
suma3 n = sum [1..n]

-- juaorture
suma4 :: Integer -> Integer
suma4 1 = 1
suma4 n = suma (n-1) + n

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

-- enrnarbej antmorper3 cargonler ignareeva
linea :: Integer -> [Integer]
linea n = [ x | x <- [k..k+n-1]]
  where k = suma (n-1) + 1 

-- Comentario: La definición linea se puede simplificar.

-- paumacpar monlagare congomgom
linea2 :: Integer -> [Integer]
linea2 n = [x | x <- [y+1..y+n]]
  where y = sum [n-1, n-2..1]

-- Comentario: La definición linea2 se puede simplificar.

-- joscasgom1 pabrabmon antbeacar margarflo5 natmarmar2 fatfervaz marjimcom
-- eliguivil beagongon1 artmorfer antdursan manruiber josjimgon2
-- roscargar margirmon antlopgom2
linea3 :: Integer -> [Integer]
linea3 n = [x | x <- [suma n +1-n..suma n]]

-- albcercid migibagar fraferpoy alvfercen javcancif
linea4 :: Integer -> [Integer]
linea4 n = [x | x <- [sum [1..n-1]+1..sum [1..n]]]

-- glovizcas marmerzaf carmarcar5
linea5 :: Integer -> [Integer]
linea5 n = [suma n -n+1 .. suma n]

-- margarvil14
linea6 :: Integer -> [Integer]
linea6 n = [x,x+1..x+(n-1)]
  where x = sum [(n-n)..(n-1)]+1

-- Comentario: La definición linea6 se puede simplificar.

-- mardelrui felsuacor eledejim2 belbenzam juacasnie albagucen 
linea7 :: Integer -> [Integer]
linea7 n = [x + suma(n-1) | x <- [1..n]]

-- juaorture cescarde
linea8 :: Integer -> [Integer]
linea8 n = [suma (n-1)+1..suma n]

-- josdeher
linea9 :: Integer -> [Integer]
linea9 n = [(sum[1..n-1]+1)..(sum[1..n])]

-- natruipin
linea10 :: Integer-> [Integer]
linea10 n = [sum [1..n]-n+1..sum[1..n]]

-- ---------------------------------------------------------------------
-- Ejercicio 3.3. Definir la función 
--    triangulo :: Integer -> [[Integer]]
-- tal que (triangulo n) es el triángulo aritmético de altura n. Por
-- ejemplo, 
--    triangulo 3  ==  [[1],[2,3],[4,5,6]]
--    triangulo 4  ==  [[1],[2,3],[4,5,6],[7,8,9,10]]
-- ---------------------------------------------------------------------

-- enrnarbej antmorper3 paumacpar cargonler pabrabmon albcercid
-- margarflo5 ignareeva monlagare fatfervaz glovizcas mardelrui
-- marmerzaf marjimcom joscasgom1 felsuacor margarvil14 eledejim2
-- eliguivil belbenzam beagongon1 artmorfer migibagar congomgom
-- antdursan juaorture manruiber josjimgon2 josdeher josrodgal7
-- juacasnie roscargar natruipin fraferpoy alvfercen natmarmar2
-- antlopgom2 carmarcar5 javcancif
triangulo :: Integer -> [[Integer]]
triangulo n = [linea n | n <- [1..n]]

-- Comentario: La definición anterior se puede mejorar.

-- cescarde albagucen 
triangulo2 :: Integer -> [[Integer]]
triangulo2 n =  [linea x | x <- [1..n]]

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

-- enrnarbej antmorper3 monlagare antbeacar joscasgom1 eledejim2 artmorfer
-- margirmon carmarcar5 ignareeva manruiber natruipin fatfervaz
-- antlopgom2 
perfectos :: Int -> [Int]
perfectos n = [x | x <- [1..n]
                 , sum (factores x) == 2*x]

factores :: Int -> [Int]
factores n = [x | x <- [1..n]
                , n `mod` x == 0]

-- paumacpar cargonler margarflo5 glovizcas pabrabmon mardelrui
-- marjimcom natmarmar2 roscargar belbenzam fraferpoy marmerzaf juacasnie
-- beagongon1 antdursan felsuacor luimotmar albagucen josjimgon2 josrodgal7
-- marlobrip
perfectos2 :: Int -> [Int]
perfectos2 n = [x | x <- [1..n]
                  , x == (sum (factores x))-x]

-- albcercid
perfectos4 :: Int -> [Int]
perfectos4 n = filter factorPerfecto [x | x <- [1..n]]

factorPerfecto n = sum (factores n) - n == n

-- margarvil14 congomgom
perfectos5 :: Int -> [Int]
perfectos5 n = [x | x <- [1..n-1]
                  , sum (factores5 x) == x]

factores5 x = [y | y <- [1..x-1]
                 , x `mod` y == 0]

-- eliguivil
perfectos6 :: Int -> [Int]
perfectos6 n = [m | m <- [1..n]
                  , m == sum [x | x <- [1..m-1], m `mod` x == 0]]

-- migibagar juaorture
perfectos7 :: Int -> [Int]
perfectos7 n = [x | x <- [1..n]
                  , x == sum (factores x \\ [x])]

-- josdeher
perfectos8 :: Int -> [Int]
perfectos8 n = [x | x <- [1..n]
                  , sum (factores8 x) == x]

factores8 n = [x | x <- [1..n]
                 , n `mod` x == 0
                 , x /=n ]

-- Comentario: La definición de factores8 se puede mejorar.

-- cescarde
perfectos9 :: Int -> [Int]
perfectos9 n = [x | x <- [1..n]
                  , sum (init (factores x)) == x]
  where factores n = [x | x <- [1..n]
                        , mod n x == 0]

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

-- enrnarbej antmorper3 artmorfer ignareeva carmarcar5 albagucen
-- margirmon manruiber luimotmar 
numeroAbundante :: Int -> Bool
numeroAbundante n = sum (factores n) > 2*n

-- paumacpar cargonler pabrabmon antbeacar albcercid margarflo5 monlagare 
-- glovizcas marjimcom joscasgom1 natmarmar2 roscargar belbenzam fraferpoy
-- marmerzaf beagongon1 antdursan felsuacor josjimgon2 josrodgal7 juacasnie
-- antlopgom2 marlobrip fatfervaz 
numeroAbundante2 :: Int -> Bool
numeroAbundante2 n = n < (sum (factores n) -n)

-- Comentario: La definición anterior se puede simplificar.

-- fatfervaz
numeroAbundante3 :: Int -> Bool
numeroAbundante3 n
 | n < sum (factores n) -1 = True
 | otherwise               = False

-- Comentario: La definición numeroAbundantes3 se puede simplificar.

-- mardelrui
numeroAbundante4 :: Int -> Bool
numeroAbundante4 n = n < divprop
  where divprop = sum (factores n) - n

-- margarvil14 eledejim2 josdeher congomgom
numeroAbundante5 :: Int -> Bool
numeroAbundante5 n = sum (factores n) >  n 

-- eliguivil
numeroAbundante6 :: Int -> Bool
numeroAbundante6 n = n < sum [x | x <- [1..n-1]
                                , n `mod` x == 0]

-- migibagar juaorture natruipin
numeroAbundante7 :: Int -> Bool
numeroAbundante7 n = sum (factores n \\ [n]) > n

-- cescarde
numeroAbundante8 :: Int -> Bool
numeroAbundante8 n = n < sum (divisores n)
  where divisores n = [x | x <- [1..n-1]
                         , mod n x == 0]

---------------------------------------------------------------------
-- Ejercicio 5.2. Definir la función  
--    numerosAbundantesMenores :: Int -> [Int]
-- tal que (numerosAbundantesMenores n) es la lista de números
-- abundantes menores o iguales que n. Por ejemplo,
--    numerosAbundantesMenores 50  ==  [12,18,20,24,30,36,40,42,48]
-- ---------------------------------------------------------------------

-- enrnarbej antmorper3 paumacpar cargonler pabrabmon margarflo5
-- glovizcas fatfervaz monlagare mardelrui joscasgom1 natmarmar2
-- eledejim2 eliguivil roscargar migibagar marmerzaf marjimcom 
-- beagongon1 artmorfer antbeacar ignareeva carmarcar5 felsuacor albagucen
-- juaorture manruiber fraferpoy josjimgon2 josdeher josrodgal7 juacasnie
-- antdursan margirmon congomgom cescarde marlobrip
numerosAbundantesMenores :: Int -> [Int]
numerosAbundantesMenores n =
  [x | x <- [1..n], numeroAbundante x]

-- albcercid natruipin
numerosAbundantesMenores2 :: Int -> [Int]
numerosAbundantesMenores2 n =
  filter numeroAbundante [x | x <- [1..n]]

-- margarvil14
numerosAbundantesMenores3 :: Int -> [Int]
numerosAbundantesMenores3 n = [x | x <- [1..n-1]
                                 , numeroAbundante x == True]

-- Comentario: La definición numerosAbundantesMenores3 se puede simplificar.

-- luimotmar
numerosAbundantesMenores4 :: Int -> [Int]
numerosAbundantesMenores4 n = [x | x <- [1..n]
                                 , numeroAbundante x == True]

-- Comentario: La definición anterior se puede simplificar.

-- antlopgom2 
numerosAbundantesMenores5 :: Int -> [Int]
numerosAbundantesMenores5 n =
  [x | x <- [1..n]
     , sum (factores x) - x > x]


-- ---------------------------------------------------------------------
-- Ejercicio 5.3. Definir la función 
--    todosPares :: Int -> Bool
-- tal que (todosPares n) se verifica si todos los números abundantes
-- menores o iguales que n son pares. Por ejemplo,
--    todosPares 10    ==  True
--    todosPares 100   ==  True
--    todosPares 1000  ==  False
-- ---------------------------------------------------------------------

-- enrnarbej paumacpar margarflo5 glovizcas fatfervaz mardelrui
-- joscasgom1 eliguivil carmarcar5 manruiber natruipin josrodgal7
-- juacasnie josjimgon2 antdursan marigmon cescarde
todosPares :: Int -> Bool
todosPares n = and [even x | x <- numerosAbundantesMenores n]

-- antmorper3 cargonler pabrabmon monlagare natmarmar2 margarvil14
-- eledejim2 roscargar marmerzaf marjimcom beagongon1 migibagar
-- margarflo5 artmorfer antbeacar ignareeva felsuacor albagucen fraferpoy 
-- luimotmar congomgom antlopgom2 josdeher
todosPares2 :: Int -> Bool 
todosPares2 n = all even (numerosAbundantesMenores n)

-- albcercid
todosPares3 :: Int -> Bool 
todosPares3 n =
  and [numerosAbundantesMenores n == filter even (numerosAbundantesMenores n)]

-- Comentario: La definición anterior se puede simplificar.

-- juaorture
todosPares4 :: Int -> Bool
todosPares4 n = filter odd (numerosAbundantesMenores n) == []

-- ---------------------------------------------------------------------
-- Ejercicio 5.4. Definir la constante 
--    primerAbundanteImpar :: Int
-- que calcule el primer número natural abundante impar. Determinar el
-- valor de dicho número.
-- ---------------------------------------------------------------------

-- enrnarbej paumacpar antmorper3 cargonler pabrabmon albcercid
-- margarflo5 glovizcas antbeacar mardelrui joscasgom1 eliguivil marjimcom
-- beagongon1 monlagare belbenzam carmarcar5 felsuacor manruiber
-- josdeher margirmon antbeacar antlopgom2
primerAbundanteImpar :: Int
primerAbundanteImpar = head [x | x <- [1..]
                               , numeroAbundante x
                               ,  odd x]

-- margarvil14 eledejim2 roscargar marmerzaf artmorfer ignareeva
-- albagucen fraferpoy fatfervaz josjimgon2 antdursan natmarmar2
-- congomgom
primerAbundanteImpar2 :: Int
primerAbundanteImpar2 = head [x | x <- [1,3..]
                                , numeroAbundante x]

-- migibagar natruipin juacasnie juaorture
primerAbundanteImpar3 :: Int
primerAbundanteImpar3 = head [x | x <- (numerosAbundantesMenores 1000)
                                , odd x]

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

-- enrnarbej antmorper3 pabrabmon joscasgom1 manruiber fatfervaz
euler1 :: Int -> Int
euler1 n =
  sum [x | x <- [1..n-1], x `mod` 3  == 0] +
  sum [x | x <- [1..n-1], x `mod` 5  == 0] -
  sum [x | x <- [1..n-1], x `mod` 15 == 0]

-- albcercid margarflo5 cargonler paumacpar margarvil14 roscargar
-- eledejim2 belbenzam beagongon1 albagucen manruiber josdeher
-- josrodgal7 josjimgon2 fraferpoy artmorfer ignareeva antdursan
-- natmarmar2 congomgom antlopgom2 marlobrip
euler1b :: Int -> Int
euler1b n = sum [x | x <- [1..n-1]
                   , (x `mod` 3 == 0) || (x `mod`5 == 0)]

-- glovizcas marmerzaf antbeacar marjimcom monlagare felsuacor natruipin
euler1c :: Int -> Int
euler1c n = sum [x | x <- [1..n-1]
                   ,  rem x 3 == 0 || rem x 5 == 0]
-- mardelrui
euler1d :: Int -> Int
euler1d n = sum[ x | x <- [1..n-1]
                   , or [ x `mod` 3 == 0, x `mod`5 == 0]]

-- eliguivil
euler1e :: Int -> Int
euler1e n = sum [3*m | m <- [1..n], 3*m < n] +
            sum [5*m | m <- [1..n], 5*m < n] -
            sum [15*m | m <- [1..n], 15*m < n]

-- luimotmar cescarde
euler1f :: Int -> Int
euler1f n = sum [x | x <- [1..n-1]
                   , mod x 3 == 0 ||  mod x 5 == 0]

--juacasnie
euler1g :: Int -> Int
euler1g n = sum (nub ([3*x | x <- [1..(n `div` 3)], 3*x < n] ++
                      [5*x | x <- [1..(n `div` 5)], 5*x < n]))

-- Comentario: La definición euler1g se puede mejorar.

-- juaorture
euler1h :: Int -> Int
euler1h n =
  sum ((multiplos 5 n ++ multiplos 3 n) \\ (multiplos 15 n))

multiplos :: Int -> Int -> [Int]
multiplos a n = [x | x <- [1..(n-1)], (x `mod` a == 0)]

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

-- enrnarbej antmorper3 pabrabmon albcercid cargonler margarflo5
-- joscasgom1 roscargar eliguivil belbenzam paumacpar eledejim2
-- beagongon1 monlagare migibagar carmarcar5 manruiber natruipin josdeher
-- fatfervaz josjimgon2 juacasnie fraferpoy artmorfer marjimcom ignareeva
-- antdursan margirmon margarvil14 antlopgom2 albagucen cescarde antlopgom2
-- marlobrip
circulo :: Int -> Int
circulo n =
  length [(x,y) | x <- [ 0..n]
                , y <- [ 0..n]
                , x^2 + y^2 < n^2]

-- juaorture
circulo2 :: Double -> Int
circulo2 n = length (a \\ [(x,y) | (x,y) <- a
                                 , (sqrt(x^2+y^2)>=n)])
  where a = [(x,y) | x <- [0..(n-1)]
                   , y <- [0..(n-1)]]

-- ---------------------------------------------------------------------
-- Ejercicio 8.1. Definir la función 
--    aproxE :: Double -> [Double]
-- tal que (aproXE n) es la lista cuyos elementos son los términos de la
-- sucesión (1+1/m)**m desde 1 hasta n. Por ejemplo, 
--    aproxE 1 == [2.0]
--    aproxE 4 == [2.0,2.25,2.37037037037037,2.44140625]
-- ---------------------------------------------------------------------

-- enrnarbej antmorper3 pabrabmon albcercid glovizcas mardelrui margirmon
-- cargonler margarflo5 joscasgom1 paumacpar fatfervaz eliguivil roscargar
-- eledejim2 beagongon1 marjimcom monlagare migibagar belbenzam margarvil14
-- carmarcar5 felsuacor juaorture manruiber marmerzaf fraferpoy josdeher
-- josrodgal7 josjimgon2 juacasnie natruipin artmorfer ignareeva
-- antdursan natmarmar2 albagucen cescarde antlopgom2
aproxE :: Double -> [Double]
aproxE n = [(1 + 1/m)**m | m <- [1..n]]

-- ---------------------------------------------------------------------
-- Ejercicio 8.2. ¿Cuál es el límite de la sucesión (1+1/m)**m ?
-- ---------------------------------------------------------------------

-- enrnarbej antmorper3 pabrabmon albcercid glovizcas mardelrui
-- margarflo5 joscasgom1 paumacpar fatfervaz eliguivil eledejim2
-- beagongon1 marjimcom migibagar felsuacor juaorture manruiber
-- marmerzaf natruipin josdeher josjimgon2 roscargar juacasnie fraferpoy
-- artmorfer antdursan margirmon natmarmar2 josrodgal7 albagucen cescarde
-- El número e 

-- margarvil14 antlopgom2
limAproxE = last (aproxE n)
  where n = 10^10

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

-- enrnarbej antmorper3 pabrabmon glovizcas mardelrui margarflo5 roscargar
-- joscasgom1 eliguivil beagongon1 marjimcom cargonler manruiber
-- josdeher josjimgon2 natruipin artmorfer antdursan margirmon natmarmar2
-- ignareeva josrodgal7 congomgom
errorAproxE :: Double -> Double
errorAproxE x = head [m | m <- [1..]
                        , abs (exp 1 - (1+1/m)**m) < x]

-- paumacpar eliguivil eledejim2 fatfervaz juacasnie fraferpoy antlopgom2
errorAproxE2 :: Double -> Double
errorAproxE2 n = head [ m | m <- [1..]
                          , (exp 1 - (1+(1/m))**m) < n]

-- albcercid
errorAproxE3 :: Double -> Double
errorAproxE3 x = head [fst n | n <- parejasE
                             , snd n < x]

parejasE = [(a,b) | (a,b) <- zip [1..] [ abs ((1+1/n)**n-exp 1) | n <- [1..]]]

-- Comentario: La definición anterior se puede simplificar.

-- margarvil14
errorAproxE4 :: Double -> Double
errorAproxE4 x = head [n | n <- [1..], last (aproxE n) > exp 1 - x]

-- juaorture
errorAproxE5 :: Double -> Double
errorAproxE5 x = head [n | n <- [1..]
                         , abs (last (aproxE n ) - exp 1 ) < x ]

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

-- enrnarbej antmorper3 pabrabmon albcercid margarflo5 joscasgom1
-- paumacpar fatfervaz eliguivil roscargar eledejim2 beagongon1 belbenzam
-- carmarcar5 felsuacor antbeacar manruiber josjimgon2 juacasnie fraferpoy 
-- antdursan margirmon ignareeva natmarmar2 artmorfer
aproxLimSeno :: Double -> [Double]
aproxLimSeno n = [ sin (1/m)*m | m <- [ 1..n]]

-- glovizcas mardelrui migibagar marjimcom margarvil14 juaorture marmerzaf
-- josdeher natruipin congomgom cescarde antlopgom2 marlobrip
aproxLimSeno2 :: Double -> [Double]
aproxLimSeno2 n = [sin(1/m)/(1/m) | m <- [1..n]]

-- josrodgal7
aproxLimSeno3 :: Double -> [Double]
aproxLimSeno3 n = [m*sin(1/m) | m <- [1..n]]

-- ---------------------------------------------------------------------
-- Ejercicio 9.2. ¿Cuál es el límite de la sucesión sen(1/m)/(1/m) ?
-- ---------------------------------------------------------------------

-- enrnarbej antmorper3 pabrabmon albcercid glovizcas margarflo5
-- joscasgom1 paumacpar mardelrui fatfervaz eliguivil eledejim2
-- beagongon1 antbeacar migibagar marjimcom carmcarcar5 felsuacor juaorture 
-- marmerzaf josdeher josjimgon2 roscargar juacasnie fraferpoy natruipin
-- artmorfer antdursan margirmon josrodgal7 natmarmar2 congomgom cescarde
-- marlobrip manruiber
-- 1

-- margarvil14 antlopgom2
limAproxLimSeno = last (aproxLimSeno n)
  where n = 10^6

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

-- enrnarbej antmorper3 pabrabmon margarflo5 joscasgom1 eliguivil
-- beagongon1 marjimcom antbeacar manruiber josjimgon2 roscargar natruipin
-- antdursan margirmon ignareeva josrodgal7 congomgom
errorLimSeno :: Double -> Double
errorLimSeno x = head [m | m <- [1..]
                         , abs (1 - sin (1/m)*m  ) < x]

-- paumacpar eledejim2 belbenzam fatfervaz juacasnie artmorfer fraferpoy 
-- antlopgom2
errorLimSeno2 :: Double -> Double
errorLimSeno2 x = head [m | m <- [1..]
                          , (1 - (sin (1/m)*m)) < x] 

-- albcercid
errorLimSeno3 :: Double -> Double
errorLimSeno3 x = head [fst n | n <- parejasSeno
                              , snd n < x]
  
parejasSeno = [(a,b) | (a,b) <- zip [1..] [(1-sin(1/n)/(1/n)) | n <- [1..]]]

-- Comentario: La definición errorLimSen2 se puede simplificar.

-- glovizcas mardelrui 
errorLimSeno4 :: Double -> Double
errorLimSeno4 x = head [m | m <- [1..]
                          , abs(1 - sin(1/m)/(1/m)) < x]

-- margarvil14 josdeher
errorLimSeno5 :: Double -> Double
errorLimSeno5 x = head [n | n <- [1..]
                          , last (aproxLimSeno n) > 1 - x]

-- juaorture
errorLimSeno6 :: Double -> Double
errorLimSeno6 x =
  head [n | n <- [1..]
          , abs (last (aproxLimSeno n) - 1) < x]
               
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

-- enrnarbej antmorper3 pabrabmon albcercid glovizcas joscasgom1
-- mardelrui margarflo5 paumacpar  roscargar eliguivil eledejim2
-- beagongon1 marjimcom belbenzam carmarcar5 felsuacor margarvil14
-- juaorture manruiber marmerzaf josdeher fatfervaz josjimgon2 juacasnie
-- fraferpoy natruipin artmorfer antdursan margirmon ignareeva
-- josrodgal7 natmarmar2 albagucen cescarde congomgom
calculaPi :: Double -> Double
calculaPi n = 4 * sum [(-1)**m/(2*m+1)| m <- [0..n]]

-- migibagar
calculaPi2 :: Double -> Double
calculaPi2 n = 4*(1 + sum (pii n))

pii :: Double -> [Double]
pii n = [(-1)**x/(2*x + 1) | x <- [1..n]]

-- antlopgom2
calculaPi3 :: Double -> Double
calculaPi3 n = sum [4*(((-1)**m)/(2*m+1)) | m <- [0..n]]

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

-- enrnarbej pabrabmon joscasgom1 manruiber
errorPi :: Double -> Double
errorPi x =
  head [k | (p,k) <- [(calculaPi n,n) | n <- [0..]]
          , abs (pi - p) < x]

-- albcercid
errorPi2 :: Double -> Double
errorPi2 x = head [fst n | n <- parejasPi
                         , snd n<x]

parejasPi = [(a,b) | (a,b) <- zip [1..] [ abs ((calculaPi n)-pi) | n <- [1..]]]

-- Comentario: La definición errorPi2 se puede simplificar.

-- albcercid antmorper3 mardelrui margarflo5 paumacpar roscargar eliguivil
-- eledejim2 beagongon1 marjimcom belbenzam margarvil14 josdeher
-- fatfervaz josjimgon2 juacasnie natruipin artmorfer antdursan
-- margirmon fraferpoy ignareeva josrodgal7 natmarmar2 albagucen congomgom
-- antlopgom2 marmerzaf
errorPi3 :: Double -> Double
errorPi3 x = head [m | m <- [1..]
                     , abs (pi - calculaPi m) < x]

-- juaorture
errorPi4 :: Double -> Double
errorPi4 x = head [n | n <- [1..]
                     , (errorPii n) < x]

errorPii:: Double -> Double
errorPii x = abs ((calculaPi x)-pi)
          
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

-- enrnarbej pabrabmon antmorper3 albcercid glovizcas joscasgom1
-- mardelrui margarflo5 paumacpar fatfervaz roscargar eliguivil 
-- beagongon1 migibagar marjimcom belbenzam carmarcar5 felsuacor juacasnie
-- juaorture manruiber marmerzaf fraferpoy josdeher luimotmar margarvil14
-- natruipin artmorfer antdursan margirmon ignareeva josrodgal7
-- natmarmar2 josjimgon2 albagucen cescarde congomgom marlobrip antlopgom2
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

-- enrnarbej pabrabmon joscasgom1 mardelrui roscargar manruiber
-- margarflo5 josdeher fatfervaz fraferpoy margirmon ignareeva
-- congomgom
numeroDePares :: (Int,Int,Int) -> Int
numeroDePares (x,y,z) = sum [ 1 | k <- [x,y,z] , even k ]

-- paumacpar eledejim2 marjimcom belbenzam carmarcar5 natruipin
-- artmorfer natmarmar2 josjimgon2 cescarde marlobrip antlopgom2
numeroDePares2 :: (Int,Int,Int) -> Int
numeroDePares2 (x,y,z) = length [ m | m <- [x,y,z], even m] 

-- pabrabmon manruiber albagucen 
numeroDePares3 :: (Int,Int,Int) -> Int
numeroDePares3 (x,y,z)
  | even x == True  && even y == True && even z == True   = 3
  | even x == True  && even y == True                     = 2
  | even y == True  && even z == True                     = 2
  | even x == True  && even z == True                     = 2
  | even x == False && even y == False && even z == False = 0
  | otherwise = 1

-- Comentario: La definición numeroDePares2 se puede simplificar.

-- albcercid glovizcas beagongon1 felsuacor juaorture marmerzaf
-- antdursan josrodgal7 
numeroDePares4 :: (Int,Int,Int) -> Int
numeroDePares4 (x,y,z) = length (filter even [x,y,z])

-- antmorper3 margarflo5
numeroDePares5 :: (Int,Int,Int) -> Int
numeroDePares5 (x,y,z) = length [ n | n <- [x,y,z], even n == True]

-- Comentario: La definición numerosDePares4 se puede simplificar.

-- migibagar
numeroDePares6 :: (Int,Int,Int) -> Int
numeroDePares6 (x,y,z) = length [n | n <- [x,y,z], even n]

-- eliguivil
numeroDePares7 :: (Int,Int,Int) -> Int
numeroDePares7 (x,y,z) =
  length ([x' | x' <- [1..maximum [x,y,z]]
              , x' == x, even x'] ++
          [y' | y' <- [1..maximum [x,y,z]]
              , y' == y
              , even y'] ++
          [z' | z' <- [1..maximum [x,y,z]]
              , z' == z
              , even z'])

-- luimotmar
numeroDePares8 :: (Int,Int,Int) -> Int
numeroDePares8 (x,y,z) =
  if even x == True && even y == True && even z == True
  then 3
  else if even x  == True && even y == True
       then 2
       else if even y  == True && even z == True
            then 2
            else if even x  == True && even z == True
                 then 2                                             
                 else if even x == True
                      then 1
                      else if even y == True
                           then 1
                           else if even z == True
                                then 1
                                else 0

-- Comentario: La definición anterior se puede simplificar.

-- margarvil14
numeroDePares9 :: (Int,Int,Int) -> Int
numeroDePares9 (x,y,z) = length (filter even [x,y,z])

-- juacasnie
numeroDePares10 :: (Int,Int,Int) -> Int
numeroDePares10 (x,y,z) 
   | even x && even y && even z               = 3
   | (even x && even y) || (even x && even z) = 2
   | even y && even z                         = 2
   | even x || even y || even z               = 1
   | otherwise                                = 0

-- Comentario: La definición numeroDePares10 se puede simplificar.

-- ---------------------------------------------------------------------
-- Ejercicio 11.3. Definir la función
--    conjetura :: Int -> Bool
-- tal que (conjetura n) se verifica si todas las ternas pitagóricas
-- cuyas componentes están entre 1 y n tiene un número impar de números
-- pares. Por ejemplo,
--    conjetura 10  ==  True
-- ---------------------------------------------------------------------

-- enrnarbej antmorper3 roscargar beagongon1 juaorture manruiber josdeher
-- fatfervaz natruipin artmorfer antdursan margirmon margarflo5
-- ignareeva josrodgal7 josjimgon2 albagucen cescarde congomgom antlopgom2
conjetura :: Int -> Bool
conjetura n = and [odd (numeroDePares x) | x <- pitagoricas n]

-- pabrabmon joscasgom1
conjetura2 :: Int -> Bool
conjetura2 n =
  [(x,y,z) | (x,y,z) <- pitagoricas n
           , numeroDePares (x,y,z) `mod` 2 == 1] ==
  pitagoricas n

-- Comentario: La definición conjetura2 se puede simplificar.

-- albcercid
conjetura3 :: Int -> Bool
conjetura3 n = pitagoricas n == filter (imparesPares) (pitagoricas n)

imparesPares n = 1 == numeroDePares n || 3 == numeroDePares n

-- Comentario: La definición imparesPares se puede simplificar.

-- paumacpar glovizcas eledejim2 eliguivil belbenzam felsuacor marmerzaf
-- fraferpoy 
conjetura5 :: Int -> Bool
conjetura5 n = and [odd (numeroDePares (x,y,z)) | (x,y,z) <- pitagoricas n]

-- marjimcom carmarcar5
conjetura6 :: Int -> Bool
conjetura6 n = all odd [numeroDePares m | m <- pitagoricas n]

-- margarvil14
conjetura7 :: Int -> Bool
conjetura7 n = and (map (odd . numeroDePares) (pitagoricas n))

-- ---------------------------------------------------------------------
-- Ejercicio 11.4. Demostrar la conjetura para todas las ternas
-- pitagóricas. 
-- ---------------------------------------------------------------------

-- enrnarbej pabrabmon antmorper3 margarflo5 mardelrui roscargar paumacpar 
-- eledejim2 manruiber marmerzaf josdeher fatfervaz antdursan josrodgal7
-- margarvil14 josjimgon2 paumacpar cescarde antlopgom2
-- Prelude> quickCheck conjetura  
-- +++ OK, passed 100 tests.

-- Comentario: Se pide la demostración matemática.


-- juacasnie
-- Procedemos por reducción al absurdo. Supongamos que tenemos una terna
-- pitagórica (x,y,z) cualquiera y distingamos dos casos:
--
-- En el primer caso, suponemos que x e y son pares y z impar, es decir:
--    x = 2k
--    y = 2k'
--    z = 2k'' + 1
-- donde k, k' y k'' son naturales.
-- Sustituyendo en la igualdad x² + y² = z² tenemos que:
--       (2k)² + (2k')² = (2k'' +1)²
--    => 4k² + 4k'² = 4k''² + 4k'' +1
--    => 2(2k² + 2k'²) = 2(2k''² + 2k'') + 1
-- Llamando n y m a los segundos factores del primer y segundo miembro
-- de la última desigualdad: 
--    2n = 2m + 1
-- Es decir, un par es igual a un impar. Pero como esto nunca es cierto,
-- la hipótesis inicial es falsa. 
-- 
-- En el segundo caso, suponemos que x y z son pares pero y es impar, es decir:
--    x = 2k
--    y = 2k' + 1
--    z = 2k'' 
-- donde k, k' y k'' son naturales.
-- Sustituyendo en la igualdad x² + y² = z² tenemos que:
--       (2k)² + (2k' + 1)² = (2k'')²
--    => 4k² + 4k'² + 4k' + 1 = 4k''²
--    => 2(2k'² + 2k') + 1 = 2(2k''² - 2k²)
-- Llamando n y m a los segundos factores del primer y segundo miembro
-- de la última desigualdad: 
--    2n + 1 =  2m
-- Si m = 0 (si k'' = k), tenemos que un impar es igual a 0, lo cual es
-- una contradicción. 
-- Si m /= 0, entonces volvemos al caso 1 en el que un par es igual a un
-- impar, lo cual nunca ocurre. 
--  Como la última igualdad nunca se cumple, la hipótesis inicial es falsa.
-- 
-- El segundo caso es análogo a cuando son pares tanto z como y. Por
-- tanto, queda demostrado que en una terna pitagórica nunca habrá un
-- número par de números pares, que es equivalente a que siempre habrá
-- un número impar de números pares.

-- eliguivil
--
-- Si a es par, por definición a es múltiplo de 2, y por definición de múltiplo
-- (x es múltiplo de un número y si x contiene a y un número entero de veces,
-- por lo que tiene x = p*y, donde p es un entero), se
-- tiene así entonces que si a es par podrá expresarse como a = 2*k (k <- Z).
--
-- Si b es impar, b = 2*j + 1 (donde j es un entero), porque por
-- definición de múltiplo, si dividimos b/2 = j + 1/2, vemos que b no
-- contiene a dos un número entero de veces a 2 (j <- Z), lo cual es la
-- definición de impar. 

-- Veamos ahora las propiedades de la suma de pares e impares:
-- Si a es par y b es impar: a + b = 2*k + 2*j + 1 = 2(k+j) + 1, como el
-- producto y la suma son operaciones internas en los enteros, se tiene
-- que si k,j <- Z, entonces 2(k+j) <- Z, y volviendo a la definición de
-- imparidad vemos que por ello 2(k+j) + 1 es impar, y esto demuestra
-- que par + impar = impar. Si se tiene que d es par, i <- Z:
-- a + d = 2*k + 2*i = 2(k+i) ==> par + par = par. (Suma y producto
-- cerrados en los enteros)
-- Por último, si b es impar y d también, se tendrá que
-- b + d = 2*k + 1 + 2*i + 1 = 2(k+i) + 2. Comprobamos, ahora, que b + d
-- contiene a 2 un número entero de veces: (b + d)/2 = 2(k+i) + 1. Se
-- tiene así que impar + impar = par 
--
-- Por el Teorema fundamental de la aritmética, se tiene que todo natural puede
-- expresarse como único producto de primos. Si a es par, entonces tiene
-- a 2 entre sus factores primos a = 2*a1*...*an (donde los a1,a2,...,an
-- son primos). Por ello, a^2 lo seguirá teniendo entre sus factores
-- también: a^2 = 2*2*(a1*...*an)^2  por lo que a^2 será par. Si b no 
-- tiene a 2 entre sus factores, b^2 tampoco, por lo que b^2 será impar.
--
-- Se tiene así que en la igualdad x^2 + y^2 = z^2 (x, y, z <- N) se
-- tienen solo tres posibilidades en lo que a paridad se refiere:
--    par + impar = impar
--    impar + impar = par
--    par + par = par
-- por lo que siempre habrá o una o tres componentes pares en una terna
-- pitagórica cualquiera.

---------------------------------------------------------------------
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

-- enrnarbej pabrabmon joscasgom1 manruiber 
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

-- antmorper3 mardelrui margarflo5 roscargar eledejim2 belbenzam
-- felsuacor artmorfer carmarcar5 marjimcom ignareeva josjimgon2
-- albagucen 
ternasPitagoricas2 :: Integer -> [(Integer,Integer,Integer)]
ternasPitagoricas2 x =
  [(a,b,c) | a <- [1..x]
           , b <- [a+1..x]
           , c <- [a+2..x]
           , a^2+b^2 == c^2
           , a+b+c==x]

-- Comentario: La definición ternasPitagoricas2 se puede mejorar.

-- albcercid 
ternasPitagoricas3 :: Integer -> [(Integer,Integer,Integer)]
ternasPitagoricas3 x =
  [(a,b,c) | a <- [1..x `div` 3]
           , b <- [a..x]
           , c <- [b..x]
           , a^2+b^2 == c^2
           , a+b+c==x]

-- Comentario: La definición ternasPitagoricas3 se puede mejorar.

-- fatfervaz paumacpar glovizcas beagongon1 monlagare
ternasPitagoricas4 :: Integer -> [(Integer,Integer,Integer)]
ternasPitagoricas4 x = [(a,b,c) | c <- [1..x]
                                , b <- [1..c]
                                , a <- [1..b]
                                , a^2 + b^2 == c^2
                                , sum [a,b,c] == x]

-- Comentario: La definición ternasPitagoricas4 se puede mejorar.

-- migibagar juacasnie cescarde marlobrip
ternasPitagoricas5 :: Integer -> [(Integer,Integer,Integer)]
ternasPitagoricas5 n =
  [(x,y,z) | x <- [1..n]
           , y <- [1..n]
           , z <- [1..n]
           , x<y && y<z
           , x^2+y^2==z^2
           , x+y+z == n]

-- Comentario: La definición anterior se puede mejorar.

-- eliguivil
ternasPitagoricas6 :: Integer -> [(Integer,Integer,Integer)]
ternasPitagoricas6 x =
  [(fromIntegral a,fromIntegral b,fromIntegral c)
  | a <- [1..x]
  , b <- [1..x]
  , c <- [1..x]
  , a^2+b^2==c^2
  , a+b+c == x]

-- Comentario: La definición anterior se puede mejorar.

-- juaorture natruipin marmerzaf 
ternasPitagoricas7 :: Integer -> [(Integer,Integer,Integer)]
ternasPitagoricas7 x =
  [(a,b,c) | a <- [1..x]
           , b <- [1..x]
           , c <- [1..x]
           , a^2 + b^2 == c^2
           , a < b
           , b < c
           , a+b+c==x]

-- Comentario: La definición anterior se puede mejorar.

-- josdeher fraferpoy josrodgal7
ternasPitagoricas8 :: Integer -> [(Integer,Integer,Integer)]
ternasPitagoricas8 n =
  [(x,y,z) | x <- [0..n]
           , y <- [x..n]
           , z <- [y..n]
           , x<y && y<z
           , x^2 + y^2 == z^2
           , sum[x,y,z] == n]

-- Comentario: La definición anterior se puede mejorar.

-- margarvil14 antlopgom2
ternasPitagoricas10 :: Integer -> [(Integer,Integer,Integer)]
ternasPitagoricas10 x =
  [(a,b,c) | a <- [1..x]
           , b <- [a..x]
           , c <- [b..x]
           , a + b + c == x
           , a^2 + b^2 == c^2]
                              
-- Comentario: La definición anterior se puede mejorar.

-- antdursan
ternasPitagoricas11 :: Integer -> [(Integer,Integer,Integer)]
ternasPitagoricas11 x =
  [(a,b,c) | a <- [0..x-2]
           , b <- [a+1..x-1]
           , c <- [b+1..x]
           , sum[a,b,c] == x
           , a^2 + b^2 == c^2] 

-- Comentario: La definición anterior se puede mejorar.

-- margirmon
ternasPitagoricas12 :: Integer -> [(Integer,Integer,Integer)]
ternasPitagoricas12 x =
  [(a,b,x-a-b) | a <- [1..x `div` 3]
               , b <- [a..x-a]
               , a^2 + b^2 == (x-a-b)^2]
  
-- ---------------------------------------------------------------------
-- Ejercicio 12.2. Definir la constante 
--    euler9 :: Integer
-- tal que euler9 es producto abc donde (a,b,c) es la única terna
-- pitagórica tal que a+b+c=1000.  
--
-- Calcular el valor de euler9.
-- ---------------------------------------------------------------------

-- enrnarbej pabrabmon antmorper3 joscasgom1 migibagar manruiber
-- marjimcom margirmon josjimgon2
euler9 :: Integer
euler9 = producto (head (ternasPitagoricas 1000))

producto :: (Integer,Integer,Integer) -> Integer
producto (a,b,c) = a*b*c

-- El cálculo del valor de euler9 es 
-- 31875000 

-- albcercid mardelrui fatfervaz paumacpar glovizcas margarflo5 roscargar
-- beagongon1 eledejim2 belbenzam felsuacor josdeher eliguivil juacasnie
-- natruipin artmorfer carmarcar5 antdursan ignareeva josrodgal7
-- albagucen cescarde marmerzaf antlopgom2
euler9b :: Integer
euler9b = head [a*b*c | (a,b,c) <- ternasPitagoricas 1000]

-- El cálculo del valor de euler9 es 
-- 31875000 (567.11 secs, 360,484,325,448 bytes)

-- margarvil14 fraferpoy
euler9c :: Integer
euler9c = a*b*c
  where (a,b,c) = head (ternasPitagoricas10 1000)

-- juaorture
euler9d :: Integer
euler9d = productoTerna (head (ternasPitagoricas 1000))
        
productoTerna:: (Integer,Integer,Integer) -> Integer
productoTerna (a,b,c) = a*b*c

pri::(a,b,c)->a
pri (a,b,c) = a

seg::(a,b,c)->b
seg (a,b,c) = b

ter::(a,b,c)->c
ter (a,b,c) = c

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
--    productoEscalar [1,2] [3,4]      ==  11
-- ---------------------------------------------------------------------

-- enrnarbej josrodgal7
productoEscalar :: [Int] -> [Int] -> Int
productoEscalar xs ys =
  sum [xs!!x * ys!!x | x <- [0..length xs-1] ]

-- Comentario: La definición productoEscalar se puede mejorar

-- pabrabmon joscasgom1 manruiber antdursan
productoEscalar2 :: [Int] -> [Int] -> Int
productoEscalar2 xs ys =
  sum [producto2 (y,z) | (y,z) <- (zip xs ys)]

producto2 (y,z) = y*z

-- enrnarbej antmorper3 albcercid fatfervaz paumacpar  mardelrui belbenzam
-- margarflo5 beagongon1 eledejim2 glovizcas felsuacor josdeher
-- roscargar juacasnie marjimcom natruipin artmorfer carmarcar5 margirmon 
-- fraferpoy josrodgal7 margarvil14 josjimgon2 albagucen cescarde ignareeva
-- marmerzaf antlopgom2
productoEscalar3 :: [Int] -> [Int] -> Int
productoEscalar3 xs ys = sum [x*y | (x,y)<- zip xs ys]

-- migibagar
productoEscalar4 :: [Int] -> [Int] -> Int
productoEscalar4 xs ys =
  sum [producto4 ((zip xs ys) !! y) | y <- [0..length xs-1]]

producto4 :: Num a => (a,a) -> a
producto4 (x,y) = x*y

-- Comentario: La definición anterior se puede mejorar.

-- eliguivil
productoEscalar5 :: [Int] -> [Int] -> Int
productoEscalar5 xs ys = sum [a*b | (a,b) <- zip xs ys ]

-- juaorture 
productoEscalar6 :: [Int] -> [Int] -> Int
productoEscalar6 xs ys = sum (zipWith (*) xs ys)

-- ---------------------------------------------------------------------
-- Ejercicio 14. Definir, por comprensión, la función
--    sumaConsecutivos :: [Int] -> [Int]
-- tal que (sumaConsecutivos xs) es la suma de los pares de elementos
-- consecutivos de la lista xs. Por ejemplo,
--    sumaConsecutivos [3,1,5,2]  ==  [4,6,7]
--    sumaConsecutivos [3]        ==  []
-- ---------------------------------------------------------------------

-- enrnarbej pabrabmon albcercid joscasgom1 fatfervaz beagongon1
-- antmorper3 belbenzam felsuacor manruiber josdeher glovizcas juacasnie
-- marjimcom artmorfer carmarcar5 margirmon josrodgal7 josjimgon2
-- albagucen ignareeva antdursan antlopgom2
sumaConsecutivos :: [Int] -> [Int]
sumaConsecutivos xs = [x+y | (x,y) <- zip xs (tail xs)]

-- paumacpar margarflo5 mardelrui migibagar eliguivil margarvil14
-- cescarde marmerzaf
sumaConsecutivos2 :: [Int] -> [Int]
sumaConsecutivos2 xs = [ a+b | (a,b) <- adyacentes xs]

adyacentes :: [a] -> [(a,a)]
adyacentes xs = zip xs (tail xs)

-- juaorture roscargar natruipin
sumaConsecutivos3 :: [Int] -> [Int]
sumaConsecutivos3 xs = zipWith (+) xs (tail xs)

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

-- enrnarbej pabrabmon antmorper3 joscasgom1 mardelrui margarflo5 belbenzam
-- beagongon1 cargonler eliguivil manruiber glovizcas juacasnie margarvil14
-- marjimcom natruipin artmorfer carmarcar5 margirmon josrodgal7
-- josjimgon2 cescarde ignareeva marmerzaf antdursan antlopgom2
densa :: [Int] -> [(Int,Int)]
densa xs =
  [(x,y) | (x,y) <- zip (reverse [0.. length xs -1]) xs
         , y /= 0]

-- albcercid
densa2 :: [Int] -> [(Int,Int)]
densa2 xs = [n | n <- (zip (reverse [0..length xs-1]) xs)
               , snd n /= 0]

-- fatfervaz paumacpar josdeher roscargar
densa3 :: [Int] -> [(Int,Int)]
densa3 xs = [(x,y) | (x,y) <- zip ([(length xs)-1,(length xs)-2..0]) xs, y/=0]

-- migibagar juaorture
densa4 :: [Int] -> [(Int,Int)]
densa4 xs = (zip (reverse [0..length xs - 1]) xs) \\
            [(x,0) | x <- [0..length xs - 1]]

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

-- enrnarbej pabrabmon antmorper3 albcercid joscasgom1 fatfervaz
-- paumacpar mardelrui beagongon1 migibagar margarflo5 eliguivil manruiber
-- josdeher belbenzam glovizcas roscargar juacasnie natruipin artmorfer
-- margirmon carmarcar5 fraferpoy josjimgon2 cescarde ignareeva marmerzaf
-- antdursan
nombres :: [(String,String,Int,Int)] -> [String]
nombres bd = [a | (a,b,c,d) <- bd ]

-- juaorture
nombres2 :: [(String,String,Int,Int)] -> [String]
nombres2 bd = [first (x)| x<-bd]

first:: (a,b,c,d) -> a
first (a,b,c,d) = a

-- margarvil14 marjimcom josrodgal7
nombres3 :: [(String,String,Int,Int)] -> [String]
nombres3 personas = [a | (a,_,_,_) <- personas]

-- ---------------------------------------------------------------------
-- Ejercicio 16.2. Definir la función
--    musicos :: [(String,String,Int,Int)] -> [String]
-- tal que (musicos bd) es la lista de los nombres de los músicos de la
-- base de datos bd. Por ejemplo,  
--    musicos personas  ==  ["Beethoven","Mozart","Bach"]
-- ---------------------------------------------------------------------

-- enrnarbej pabrabmon antmorper3 albcercid joscasgom1 fatfervaz
-- paumacpar mardelrui beagongon1 migibagar margarflo5 eliguivil manruiber 
-- glovizcas roscargar juacasnie natruipin artmorfer margirmon
-- carmarcar5 fraferpoy josjimgon2 cescarde ignareeva marmerzaf antdursan
musicos :: [(String,String,Int,Int)] -> [String]
musicos bd = [a | (a,b,c,d) <- bd , b == "Musica"]

-- juaorture
musicos2 :: [(String,String,Int,Int)] -> [String]
musicos2 bd = [first a | a<-bd, second a == "Musica"]

second:: (a,b,c,d) -> b
second (a,b,c,d) = b

-- josdeher
musicos3 :: [(String,String,Int,Int)] -> [String]
musicos3 bd = [x | (x,"Musica",z,u) <- bd]

-- margarvil14 marjimcom josrodgal7
musicos4 :: [(String,String,Int,Int)] -> [String]
musicos4 personas = [a | (a,"Musica",_,_) <- personas]

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

-- enrnarbej pabrabmon antmorper3 albcercid joscasgom1 fatfervaz
-- paumacpar mardelrui beagongon1 migibagar margarflo5 eliguivil manruiber
-- josdeher glovizcas roscargar juacasnie natruipin artmorfer margirmon
-- carmarcar5 fraferpoy josjimgon2 cescarde ignareeva marmerzaf antdursan
seleccion :: [(String,String,Int,Int)] -> String -> [String]
seleccion bd m = [ a | (a,b,c,d) <- bd , b == m]

-- juaorture
seleccion2 :: [(String,String,Int,Int)] -> String -> [String]
seleccion2 bd m = [first a | a <- bd, second a == m]

-- margarvil14 marjimcom
seleccion3 :: [(String,String,Int,Int)] -> String -> [String]
seleccion3 personas m = [a | (a,b,_,_) <- personas, b == m]

-- josrodgal7 
seleccion4 :: [(String,String,Int,Int)] -> String -> [String]
seleccion4 bd m = [a | (a,b,_,_) <- bd, b == m]

-- Duda: ¿Puede ponerse en lugar de b el parámetro m? (Y que siga
-- funcionando, claro) 

-- ---------------------------------------------------------------------
-- Ejercicio 16.4. Definir, usando el apartado anterior, la función
--    musicos' :: [(String,String,Int,Int)] -> [String]
-- tal que (musicos' bd) es la lista de los nombres de los músicos de la
-- base de datos bd. Por ejemplo,   
--    ghci> musicos' personas
--    ["Beethoven","Mozart","Bach"]
-- ---------------------------------------------------------------------

-- enrnarbej pabrabmon antmorper3 albcercid joscasgom1 fatfervaz
-- paumacpar mardelrui beagongon1 migibagar margarflo5 eliguivil
-- juaorture manruiber josdeher glovizcas roscargar juacasnie natruipin
-- artmorfer marjimcom margirmon carmarcar5 fraferpoy josrodgal7 josjimgon2
-- cescarde ignareeva marmerzaf antdursan
musicos' :: [(String,String,Int,Int)] -> [String]
musicos' bd = seleccion bd "Musica"

-- margarvil14
musicos'2 :: [(String,String,Int,Int)] -> [String]
musicos'2 personas = seleccion personas "Musica"

-- ---------------------------------------------------------------------
-- Ejercicio 16.5. Definir la función 
--    vivas :: [(String,String,Int,Int)] -> Int -> [String]
-- tal que (vivas bd a) es la lista de los nombres de las personas de la
-- base de datos bd  que estaban vivas en el año a. Por ejemplo,  
--    ghci> vivas personas 1600
--    ["Cervantes","Velazquez","Quevedo","Borromini"]
-- ---------------------------------------------------------------------

-- enrnarbej pabrabmon antmorper3 albcercid joscasgom1 fatfervaz
-- paumacpar eliguivil beagongon1 migibagar margarflo5 manruiber
-- josdeher glovizcas roscargar juacasnie natruipin artmorfer margirmon
-- carmarcar5 fraferpoy josrodgal7 josjimgon2 cescarde ignareeva marmerzaf
-- antdursan
vivas :: [(String,String,Int,Int)] -> Int -> [String]
vivas bd y = [a | (a,b,c,d) <- bd
                , c <= y
                , d >= y]

-- mardelrui
vivas2 :: [(String,String,Int,Int)] -> Int -> [String]
vivas2 ps a = [xs | (xs,ys,x,y) <- ps
                  , and [not (x > a), not (y < a)]]

-- juaorture
vivas3 :: [(String,String,Int,Int)] -> Int -> [String]
vivas3 bd a = [first b | b<-bd, fourth b > a, third b < a]

fourth :: (a,b,c,d) -> d
fourth (a,b,c,d) = d

third :: (a,b,c,d) -> c
third (a,b,c,d) = c


-- margarvil14 marjimcom
vivas4 :: [(String,String,Int,Int)] -> Int -> [String]
vivas4 personas av = [a | (a,_,c,d) <- personas
                        , c < av
                        , av < d]
  
-- ---------------------------------------------------------------------
-- Ejercicio 17. Definir la función
--    todosIguales :: Eq a => [a] -> Bool
-- tal que (todosIguales xs) se verifica si todos los elementos de xs
-- son iguales. Por ejemplo,
--    todosIguales [2,2,2,2]  ==  True
--    todosIguales "oso"      ==  False
--    todosIguales []         ==  True
-- ---------------------------------------------------------------------

-- enrnarbej antmorper3 pabrabmon joscasgom1 manruiber roscargar
-- artmorfer fraferpoy albagucen 
todosIguales :: Eq a => [a] -> Bool
todosIguales []     = True
todosIguales (x:xs) = and [x == y | y <- xs]

-- albcercid glovizcas beagongon1 cargonler eliguivil josdeher juacasnie
-- natruipin marjimcom margirmon carmarcar5 josjimgon2 antdursan
todosIguales2 :: Eq a => [a] -> Bool
todosIguales2 xs = and [x == y | (x,y) <- zip xs (tail xs)]

-- paumacpar mardelrui eledejim2 migibagar margarflo5 marmerzaf
todosIguales3 :: Eq a => [a] -> Bool
todosIguales3 xs = and [x == y | (x,y) <- adyacentes3 xs]

adyacentes3 :: [a] -> [(a,a)]
adyacentes3 xs= zip xs (tail xs) 

-- juaorture
todosIguales4 :: (Eq a)=> [a] -> Bool
todosIguales4 xs =
  (length xs)^2 == length [(a,b) | a <- xs
                                 , b <- xs
                                 , a == b ]

-- josrodgal7
todosIguales5 :: Eq a => [a] -> Bool
todosIguales5 xs = and [a == b | a <- xs, b <- xs ]
