-- I1M 2015-16: Relación 34 (22 de abril de 2016)
-- División y factorización de polinomios mediante la regla de Ruffini.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- El objetivo de esta relación de ejercicios es implementar la regla de
-- Ruffini y sus aplicaciones utilizando las implementaciones del TAD de
-- polinomio estudiadas en el tema 21 que se pueden descargar desde 
--    http://www.cs.us.es/~jalonso/cursos/i1m-15/temas/tema-21.html
-- 
-- Para realizar los ejercicios hay que tener instalada la librería I1M
-- que contiene la implementación de TAD de los polinomios. Los pasos
-- para instalarla son los siguientes:
-- + Descargar el paquete I1M desde http://bit.ly/1pbnDqm
-- + Descomprimirlo (y se crea el directorio I1M-master.zip).
-- + Cambiar al directorio I1M-master.
-- + Ejecutar cabal install I1M.cabal
-- 
-- Otra forma es descargar, en el directorio de ejercicios, la
-- implementación del TAD de polinomios: 
-- + PolRepTDA      que está en http://bit.ly/1WJnS93
-- + PolRepDispersa que está en http://bit.ly/1WJnUO8
-- + PolRepDensa    que está en http://bit.ly/1WJnV4E 
-- + PolOperaciones que está en http://bit.ly/1WJnTd7

-- ---------------------------------------------------------------------
-- Importación de librerías                                           --
-- ---------------------------------------------------------------------

import Test.QuickCheck

-- Hay que elegir una librería 
import I1M.PolOperaciones 
-- import PolOperaciones 

-- ---------------------------------------------------------------------
-- Ejemplos                                                           --
-- ---------------------------------------------------------------------

-- Además de los ejemplos de polinomios (ejPol1, ejPol2 y ejPol3) que se
-- encuentran en PolOperaciones, usaremos el siguiente ejemplo.
ejPol4 :: Polinomio Int
ejPol4 = consPol 3 1 
                 (consPol 2 2 
                          (consPol 1 (-1) 
                                   (consPol 0 (-2) polCero)))

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    divisores :: Int -> [Int]
-- tal que (divisores n) es la lista de todos los divisores enteros de
-- n. Por ejemplo,
--    divisores 4     ==  [1,2,4,-1,-2,-4]
--    divisores (-6)  ==  [1,-1,2,-2,3,-3,6,-6]
-- ---------------------------------------------------------------------

-- manvermor: Faltaba el absoluto de n

-- silgongal manvermor josllagam lucgamgal
divisores :: Int -> [Int]
divisores n = divisores' n ++ map (*(-1)) (divisores' n)

divisores' :: Int -> [Int]
divisores' n = [x | x <-[1..abs n], mod n x == 0]

-- isrbelnun
divisores2 :: Int -> [Int]
divisores2 n = [x  | x <- [1..abs n], rem n x == 0] ++ 
               [-y | y <- [1..abs n], rem n y == 0]

-- Comentario: La definición anterior se puede mejorar.

-- abrderod
divisores3 :: Int -> [Int]
divisores3 n = xs ++ map (0-) xs
     where xs = filter (\x -> rem n x == 0) [1..abs n]

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    coeficiente :: (Num a, Eq a) => Int -> Polinomio a -> a
-- tal que (coeficiente k p) es el coeficiente del término de grado k en
-- p. Por ejemplo:
--     coeficiente 4 ejPol1 == 3
--     coeficiente 3 ejPol1 == 0
--     coeficiente 2 ejPol1 == -5
--     coeficiente 5 ejPol1 == 0
-- ---------------------------------------------------------------------

-- silgongal manvermor josllagam abrdelrod lucgamgal
coeficiente :: (Num a, Eq a) => Int -> Polinomio a -> a
coeficiente k p 
    | esPolCero p  = 0
    | grado p == k = coefLider p
    | otherwise    = coeficiente k (restoPol p)

-- isrbelnun
coeficiente2 :: (Num a, Eq a) => Int -> Polinomio a -> a
coeficiente2 k p 
    | k >  grado p = 0
    | k == grado p = coefLider p
    | otherwise    = coeficiente2 k (restoPol p)

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función 
--    terminoIndep :: (Num a, Eq a) => Polinomio  a -> a
-- tal que (terminoIndep p) es el término independiente del polinomio
-- p. Por ejemplo,
--    terminoIndep ejPol1 == 3
--    terminoIndep ejPol2 == 0
--    terminoIndep ejPol4 == -2
-- ---------------------------------------------------------------------

-- silgongal manvermor isrbelnun josllagam abrdelrod lucgamgal
terminoIndep :: (Num a, Eq a) => Polinomio  a -> a
terminoIndep = coeficiente 0 

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función 
--    coeficientes :: (Num a, Eq a) => Polinomio a -> [a]
-- tal que (coeficientes p) es la lista de coeficientes de p, ordenada
-- según el grado. Por ejemplo,
--     coeficientes ejPol1 == [3,0,-5,0,3]
--     coeficientes ejPol4 == [1,2,-1,-2]
--     coeficientes ejPol2 == [1,0,0,5,4,0]
-- ---------------------------------------------------------------------

-- manvermor
coeficientes :: (Num a, Eq a) => Polinomio a -> [a]
coeficientes p = [coeficiente k p | k <- (reverse [0..(grado p)])]

-- Comentario: La definición anterior se puede simplificar eliminando
-- paréntesis. 

-- isrbelnun josllagam
coeficientes3 :: (Num a, Eq a) => Polinomio a -> [a]
coeficientes3 p = map (`coeficiente` p) [(grado p),(grado p)-1..0]

-- Comentario: La definición anterior se puede mejorar reduciendo el
-- cálculo del grado.

-- abrdelrod
coeficientes4 :: (Num a, Eq a) => Polinomio a -> [a]
coeficientes4 p = map (flip coeficiente p) [grado p, grado p-1..0]

-- Comentario: La definición anterior se puede mejorar reduciendo el
-- cálculo del grado y simplificar eliminando flip.

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la función 
--    creaPol :: (Num a, Eq a) => [a] -> Polinomio a
-- tal que (creaPol cs) es el polinomio cuya lista de coeficientes es
-- cs. Por ejemplo,
--     creaPol [1,0,0,5,4,0] == x^5 + 5*x^2 + 4*x
--     creaPol [1,2,0,3,0]   == x^4 + 2*x^3 + 3*x
-- ---------------------------------------------------------------------

-- silgongal josllagam lucgamgal
creaPol :: (Num a, Eq a) => [a] -> Polinomio a
creaPol []                 = polCero
creaPol (x:cs) | x /= 0    = consPol (length cs) x (creaPol cs)
               | otherwise = creaPol cs 

-- manvermor
creaPol2 :: (Num a, Eq a) => [a] -> Polinomio a
creaPol2 cs = aux 0 (reverse cs)
    where aux n []     = polCero
          aux n (c:cs) = consPol n c (aux (n+1) cs)

-- isrbelnun
creaPol3 :: (Num a, Eq a) => [a] -> Polinomio a
creaPol3 []     = polCero
creaPol3 (x:xs) = consPol (length xs) x (creaPol xs)

-- abrdelrod
creaPol4 :: (Num a, Eq a) => [a] -> Polinomio a
creaPol4 xs = 
    foldr (\(x,y) -> consPol x y) 
          polCero 
          (zip [length xs - 1, length xs - 2..0] xs)

-- Comentario: La definición anterior se puede mejorar reduciendo el
-- cálculo de longitud y usando uncurry.

prop_creaPol :: [Int] -> Bool
prop_creaPol xs =
    creaPol2 xs == p &&
    creaPol3 xs == p &&
    creaPol4 xs == p 
    where p = creaPol xs

-- ---------------------------------------------------------------------
-- Ejercicio 6. Comprobar con QuickCheck que, dado un polinomio p, el
-- polinomio obtenido mediante creaPol a partir de la lista de
-- coeficientes de p coincide con p.
-- ---------------------------------------------------------------------

-- manvermor isrbelnun josllagam abrdelrod lucgamgal
-- La propiedad es
prop_coef :: Polinomio Int -> Bool
prop_coef p = creaPol (coeficientes p) == p

-- La comprobación es
--    ghci> quickCheck prop_coef
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir una función 
--    pRuffini:: Int -> [Int] -> [Int]
-- tal que (pRuffini r cs) es la lista que resulta de aplicar un paso
-- del regla de Ruffini al número entero r y a la lista de coeficientes
-- cs. Por ejemplo,
--    pRuffini 2 [1,2,-1,-2] == [1,4,7,12]
--    pRuffini 1 [1,2,-1,-2] == [1,3,2,0]
-- ya que
--      | 1  2  -1  -2           | 1  2  -1  -2
--    2 |    2   8  14         1 |    1   3   2
--    --+--------------        --+-------------
--      | 1  4   7  12           | 1  3   2   0
-- ---------------------------------------------------------------------

-- manvermor abrdelrod
pRuffini :: Int -> [Int] -> [Int]
pRuffini r cs = scanl1 (\x y -> r*x+y) cs

-- isrbelnun josllagam
pRuffini2 :: Int -> [Int] -> [Int]
pRuffini2 _ [] = []
pRuffini2 r (c:cs) = c : aux c r cs
    where aux _ _ []     = []
          aux n r (c:cs) = 
              ((head (c:cs)) + n*r) : aux ((head (c:cs)) + n*r) r cs

-- Equivalencia
prop_pRuffini :: Int -> [Int] -> Bool
prop_pRuffini r cs =
    pRuffini r cs == pRuffini2 r cs

-- ---------------------------------------------------------------------
-- Ejercicio 8. Definir la función 
--    cocienteRuffini:: Int -> Polinomio Int -> Polinomio Int
-- tal que (cocienteRuffini r p) es el cociente de dividir el polinomio
-- p por el polinomio x-r. Por ejemplo:
--     cocienteRuffini 2 ejPol4    == x^2 + 4*x + 7
--     cocienteRuffini (-2) ejPol4 == x^2 + -1
--     cocienteRuffini 3 ejPol4    == x^2 + 5*x + 14
-- ---------------------------------------------------------------------

-- manvermor isrbelnun josllagam
cocienteRuffini:: Int -> Polinomio Int -> Polinomio Int
cocienteRuffini r p = creaPol $ init $ pRuffini r $ coeficientes p

-- abrdelrod
cocienteRuffini2 :: Int -> Polinomio Int -> Polinomio Int
cocienteRuffini2 r = creaPol . pRuffini r . init . coeficientes

-- Equivalencia
prop_cocienteRuffini :: Int -> Polinomio Int -> Bool
prop_cocienteRuffini r p = 
    cocienteRuffini r p == cocienteRuffini2 r p

-- ---------------------------------------------------------------------
-- Ejercicio 9. Definir la función 
--    restoRuffini:: Int -> Polinomio Int -> Int
-- tal que (restoRuffini r p) es el resto de dividir el polinomio p por
-- el polinomio x-r. Por ejemplo, 
--     restoRuffini 2 ejPol4    == 12
--     restoRuffini (-2) ejPol4 == 0
--     restoRuffini 3 ejPol4    == 40
-- ---------------------------------------------------------------------

-- manvermor isrbelnun josllagam
restoRuffini:: Int -> Polinomio Int -> Int
restoRuffini r p = last $ pRuffini r (coeficientes p)

-- abrdelrod
restoRuffini2 :: Int -> Polinomio Int -> Int
restoRuffini2 r = last . pRuffini r . coeficientes

-- Equivalencia
prop_restoRuffini :: Int -> Polinomio Int -> Bool
prop_restoRuffini r p = 
    restoRuffini r p == restoRuffini2 r p

-- ---------------------------------------------------------------------
-- Ejercicio 10. Comprobar con QuickCheck que, dado un polinomio p y un
-- número entero r, las funciones anteriores verifican la propiedad de
-- la división euclídea.
-- ---------------------------------------------------------------------

-- manvermor isrbelnun josllagam abrdelrod
-- La propiedad es
prop_diviEuclidea:: Int -> Polinomio Int -> Bool
prop_diviEuclidea r p = p == sumaPol (multPol d q) s
    where d = creaPol [1,-r]
          q = cocienteRuffini r p
          s = creaPol [restoRuffini r p]

-- La comprobación es
--    ghci> quickCheck prop_diviEuclidea
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 11. Definir la función
--     esRaizRuffini:: Int -> Polinomio Int -> Bool 
-- tal que (esRaizRuffini r p) se verifica si r es una raiz de p, usando
-- para ello el regla de Ruffini. Por ejemplo,
--     esRaizRuffini 0 ejPol3 == True
--     esRaizRuffini 1 ejPol3 == False
-- ---------------------------------------------------------------------

-- manvermor isrbelnun josllagam abrdelrod
esRaizRuffini :: Int -> Polinomio Int -> Bool 
esRaizRuffini r p = restoRuffini r p == 0

-- ---------------------------------------------------------------------
-- Ejercicio 12. Definir la función 
--    raicesRuffini :: Polinomio Int -> [Int]
-- tal que (raicesRuffini p) es la lista de las raices enteras de p,
-- calculadas usando el regla de Ruffini. Por ejemplo,
--     raicesRuffini ejPol1              == []
--     raicesRuffini ejPol2              == [0]
--     raicesRuffini ejPol3              == [0]
--     raicesRuffini ejPol4              == [-2,-1,1]
--     raicesRuffini (creaPol [1,-2,1])  == [1,1]
-- ---------------------------------------------------------------------

-- manvermor
raicesRuffini :: Polinomio Int -> [Int]
raicesRuffini p = 
    if esPolCero p 
    then [] 
    else aux (0:divisores t)
    where aux [] = []
          aux (x:xs) 
              | esRaizRuffini x p = x : raicesRuffini (cocienteRuffini x p)
              | otherwise         = aux xs 
          t = terminoIndep p                

-- Comentario: La definición anterior se puede simplificar usando guardas.

-- ---------------------------------------------------------------------
-- Ejercicio 13. Definir la función
--    factorizacion :: Polinomio Int -> [Polinomio Int]
-- tal que (factorizacion p) es la lista de la descomposición del
-- polinomio p en factores obtenida mediante el regla de Ruffini. Por
-- ejemplo, 
--    ghci> factorizacion (creaPol [1,0,0,0,-1])
--    [x^2 + 1,1*x + 1,1*x + -1]
-- ---------------------------------------------------------------------

-- manvermor josllagam abrdelrod
factorizacion :: Polinomio Int -> [Polinomio Int]
factorizacion p = 
    if esPolCero p 
    then [p] 
    else aux (raicesRuffini p)
    where aux []     = [p]
          aux (x:xs) = creaPol [1,-x] : factorizacion (cocienteRuffini x p)

-- isrbelnun
factorizacion2 :: Polinomio Int -> [Polinomio Int]
factorizacion2 p = reverse (aux p (raicesRuffini p))
    where aux p []     = [p]
          aux p (x:xs) = creaPol [1,-x] : aux (cocienteRuffini x p) xs

-- ---------------------------------------------------------------------
-- Generador de polinomios                                            --
-- ---------------------------------------------------------------------

-- (genPol n) es un generador de polinomios. Por ejemplo,
--    ghci> sample (genPol 1)
--    7*x^9 + 9*x^8 + 10*x^7 + -14*x^5 + -15*x^2 + -10
--    -4*x^8 + 2*x
--    -8*x^9 + 4*x^8 + 2*x^6 + 4*x^5 + -6*x^4 + 5*x^2 + -8*x
--    -9*x^9 + x^5 + -7
--    8*x^10 + -9*x^7 + 7*x^6 + 9*x^5 + 10*x^3 + -1*x^2
--    7*x^10 + 5*x^9 + -5
--    -8*x^10 + -7
--    -5*x
--    5*x^10 + 4*x^4 + -3
--    3*x^3 + -4
--    10*x
genPol :: (Arbitrary a, Num a, Eq a) => Int -> Gen (Polinomio a)
genPol 0 = return polCero
genPol n = do n <- choose (0,10)
              b <- arbitrary
              p <- genPol (div n 2)
              return (consPol n b p) 

instance (Arbitrary a, Num a, Eq a) => Arbitrary (Polinomio a) where
    arbitrary = sized genPol

