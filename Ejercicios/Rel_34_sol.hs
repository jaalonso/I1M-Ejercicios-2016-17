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

-- silgongal manvermor josllagam lucgamgal jespergue alvalvdom1
-- rubvilval javperlag manpende 
divisores :: Int -> [Int]
divisores n = divisores' n ++ map (*(-1)) (divisores' n)

divisores' :: Int -> [Int]
divisores' n = [x | x <-[1..abs n], mod n x == 0]

-- isrbelnun
divisores2 :: Int -> [Int]
divisores2 n = [x  | x <- [1..abs n], rem n x == 0] ++ 
               [-y | y <- [1..abs n], rem n y == 0]

-- Comentario: La definición anterior se puede mejorar.

-- abrderod fracruzam erisancha juanarcon
divisores3 :: Int -> [Int]
divisores3 n = xs ++ map (0-) xs
     where xs = filter (\x -> rem n x == 0) [1..abs n]

-- carruirui3
divisores4 :: Int -> [Int]
divisores4 0 = []
divisores4 n = xs ++ n : map (0-) xs ++ [-n]
    where xs = filter (\x -> n `mod` x == 0) [1..(abs n) `div` 2]

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

-- silgongal manvermor josllagam abrdelrod lucgamgal jespergue
-- alvalvdom1 rubvilval manpende juanarcon
coeficiente :: (Num a, Eq a) => Int -> Polinomio a -> a
coeficiente k p 
    | esPolCero p  = 0
    | grado p == k = coefLider p
    | otherwise    = coeficiente k (restoPol p)

-- isrbelnun fracruzam carruirui3 javperlag erisancha
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

-- silgongal manvermor isrbelnun josllagam abrdelrod lucgamgal fracruzam
-- jespergue carruirui3 alvalvdom1 rubvilval manpende erisancha juanarcon
terminoIndep :: (Num a, Eq a) => Polinomio  a -> a
terminoIndep = coeficiente 0 

-- javperlag
terminoIndep2 :: (Num a, Eq a) => Polinomio  a -> a
terminoIndep2 p | r == polCero = coefLider p
                | otherwise    = terminoIndep2 r
    where r = restoPol p

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función 
--    coeficientes :: (Num a, Eq a) => Polinomio a -> [a]
-- tal que (coeficientes p) es la lista de coeficientes de p, ordenada
-- según el grado. Por ejemplo,
--     coeficientes ejPol1 == [3,0,-5,0,3]
--     coeficientes ejPol4 == [1,2,-1,-2]
--     coeficientes ejPol2 == [1,0,0,5,4,0]
-- ---------------------------------------------------------------------

-- manvermor jespergue silgongal juanarcon
coeficientes :: (Num a, Eq a) => Polinomio a -> [a]
coeficientes p = [coeficiente k p | k <- (reverse [0..(grado p)])]

-- Comentario: La definición anterior se puede simplificar eliminando
-- paréntesis. 

-- isrbelnun josllagam rubvilval manpende
coeficientes3 :: (Num a, Eq a) => Polinomio a -> [a]
coeficientes3 p = map (`coeficiente` p) [(grado p),(grado p)-1..0]

-- Comentario: La definición anterior se puede mejorar reduciendo el
-- cálculo del grado.

-- abrdelrod alvalvdom1 erisancha
coeficientes4 :: (Num a, Eq a) => Polinomio a -> [a]
coeficientes4 p = map (`coeficiente` p) [g, g-1..0]
    where g = grado p

-- fracruzam
coeficientes5 :: (Num a, Eq a) => Polinomio a -> [a]
coeficientes5 p = rastreaCoef [n,n-1..0] p
  where n = grado p
        rastreaCoef :: (Num a, Eq a) => [Int] -> Polinomio a -> [a]
        rastreaCoef (n:ns) p | esPolCero p = []
                             | otherwise   = coeficiente n p: 
                                             rastreaCoef ns p
        rastreaCoef [] _ = []

-- carruirui3
coeficientes6 :: (Num a, Eq a) => Polinomio a -> [a]
coeficientes6 p | esPolCero p = []
                | otherwise   = aux (grado p) p
  where aux 0 q = [coefLider q]
        aux n q | grado q == n = coefLider q : aux (n-1) (restoPol q)
                | otherwise    = 0 : aux (n-1) q

-- javperlag (dispersa es la función descrita en la relación anterior)
coeficientes7 :: (Num a, Eq a) => Polinomio a -> [a]
coeficientes7 = dispersa

dispersa p | esPolCero p = []
           | esPolCero r = c :(replicate g 0)
           | otherwise   = (coefLider p): (replicate tal 0)++(dispersa r)
    where g   = grado p
          c   = coefLider p
          r   = restoPol p
          tal = g - (grado r) - 1

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la función 
--    creaPol :: (Num a, Eq a) => [a] -> Polinomio a
-- tal que (creaPol cs) es el polinomio cuya lista de coeficientes es
-- cs. Por ejemplo,
--     creaPol [1,0,0,5,4,0] == x^5 + 5*x^2 + 4*x
--     creaPol [1,2,0,3,0]   == x^4 + 2*x^3 + 3*x
-- ---------------------------------------------------------------------

-- silgongal josllagam lucgamgal jespergue rubvilval 
creaPol :: (Num a, Eq a) => [a] -> Polinomio a
creaPol []                 = polCero
creaPol (x:cs) | x /= 0    = consPol (length cs) x (creaPol cs)
               | otherwise = creaPol cs 

-- manvermor erisancha
creaPol2 :: (Num a, Eq a) => [a] -> Polinomio a
creaPol2 cs = aux 0 (reverse cs)
    where aux n []     = polCero
          aux n (c:cs) = consPol n c (aux (n+1) cs)

-- isrbelnun alvalvdom1 manpende juanarcon
creaPol3 :: (Num a, Eq a) => [a] -> Polinomio a
creaPol3 []     = polCero
creaPol3 (x:xs) = consPol (length xs) x (creaPol xs)

-- abrdelrod carruirui3
creaPol4 :: (Num a, Eq a) => [a] -> Polinomio a
creaPol4 xs = 
    foldr (uncurry consPol) 
          polCero 
          (zip [l-1, l-2..0] xs)
    where l = length xs

-- fracruzam
creaPol5 :: (Num a, Eq a) => [a] -> Polinomio a
creaPol5 [] = polCero
creaPol5 xs = construye (length xs - 1) xs
    where construye :: (Num a, Eq a) => Int -> [a] -> Polinomio a
          construye k (x:xs) = consPol k x (construye (k-1) xs)
          construye _  _     = polCero

-- javperlag
creaPol6 :: (Num a, Eq a) => [a] -> Polinomio a
creaPol6 = creaPolDispersa 

creaPolDispersa :: (Num a, Eq a) => [a] -> Polinomio a
creaPolDispersa xs|null xs     = polCero
                  |h== 0       = creaPolDispersa t
                  |otherwise   = consPol (length t) h (creaPolDispersa t)
                  where h= head xs
                        t= tail xs

-- Equivalencia
prop_creaPol :: [Int] -> Bool
prop_creaPol xs =
    creaPol2 xs == p &&
    creaPol3 xs == p &&
    creaPol4 xs == p &&
    creaPol5 xs == p &&
    creaPol6 xs == p 
    where p = creaPol xs

-- Comprobación
--    λ> quickCheck prop_creaPol
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 6. Comprobar con QuickCheck que, dado un polinomio p, el
-- polinomio obtenido mediante creaPol a partir de la lista de
-- coeficientes de p coincide con p.
-- ---------------------------------------------------------------------

-- manvermor isrbelnun josllagam abrdelrod lucgamgal fracruzam jespergue
-- alvalvdom1 rubvilval javperlag manpende silgongal erisancha juanarcon
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

-- manvermor abrdelrod fracruzam carruirui3 alvalvdom1 manpende
-- silgongal erisancha
pRuffini :: Int -> [Int] -> [Int]
pRuffini r cs = scanl1 (\x y -> r*x+y) cs

-- isrbelnun josllagam jespergue rubvilval javperlag juanarcon
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

-- manvermor isrbelnun josllagam alvalvdom1 rubvilval juanarcon manpende
cocienteRuffini:: Int -> Polinomio Int -> Polinomio Int
cocienteRuffini r p = creaPol $ init $ pRuffini r $ coeficientes p

-- abrdelrod fracruzam jespergue carruirui3 javperlag silgongal erisancha
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

-- manvermor isrbelnun josllagam alvalvdom1 juanarcon manpende
restoRuffini:: Int -> Polinomio Int -> Int
restoRuffini r p = last $ pRuffini r (coeficientes p)

-- abrdelrod fracruzam jespergue carruirui3 rubvilval silgongal
restoRuffini2 :: Int -> Polinomio Int -> Int
restoRuffini2 r = last . pRuffini r . coeficientes

-- javperlag  erisancha
restoRuffini3:: Int -> Polinomio Int -> Int
restoRuffini3 r p = valor p r

-- Equivalencia
prop_restoRuffini :: Int -> Polinomio Int -> Bool
prop_restoRuffini r p = 
    restoRuffini2 r p == x &&
    restoRuffini3 r p == x 
    where x = restoRuffini r p 

-- Comprobación
--    λ> quickCheck prop_restoRuffini
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 10. Comprobar con QuickCheck que, dado un polinomio p y un
-- número entero r, las funciones anteriores verifican la propiedad de
-- la división euclídea.
-- ---------------------------------------------------------------------

-- manvermor isrbelnun josllagam abrdelrod fracruzam jespergue carruirui3
-- alvalvdom1 rubvilval  javperlag silgongal erisancha juanarcon manpende
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

-- manvermor isrbelnun josllagam abrdelrod fracruzam jespergue carruirui3
-- alvalvdom1 rubvilval javperlag silgongal erisancha juanarcon manpende
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

-- manvermor rubvilval silgongal juanarcon abrdelrod
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

-- fracruzam carruirui3 javperlag alvalvdom1 erisancha manpende
raicesRuffini2 :: Polinomio Int -> [Int]
raicesRuffini2 p = filter (flip esRaizRuffini p)
                          (divisores $ terminoIndep p)

-- Equivalencia:
prop_raicesRuffini :: Polinomio Int -> Bool
prop_raicesRuffini p =
    raicesRuffini p == raicesRuffini2 p

-- Comprobación
--    λ> quickCheck prop_raicesRuffini
--    *** Failed! Falsifiable (after 2 tests): 
--    1*x

-- Comentario: Las definiciones no son equivalentes. Por ejemplo,
--    λ> raicesRuffini (consPol 1 1 polCero)
--    [0]
--    λ> raicesRuffini2 (consPol 1 1 polCero)
--    []

-- ---------------------------------------------------------------------
-- Ejercicio 13. Definir la función
--    factorizacion :: Polinomio Int -> [Polinomio Int]
-- tal que (factorizacion p) es la lista de la descomposición del
-- polinomio p en factores obtenida mediante el regla de Ruffini. Por
-- ejemplo, 
--    ghci> factorizacion (creaPol [1,0,0,0,-1])
--    [x^2 + 1,1*x + 1,1*x + -1]
-- ---------------------------------------------------------------------

-- manvermor josllagam abrdelrod jespergue rubvilval alvalvdom1 silgongal
-- erisancha juanarcon manpende
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

-- fracruzam 
factorizacion3 :: Polinomio Int -> [Polinomio Int]
factorizacion3 p | esPolCero p = [polCero]
                 | otherwise   = descompon p (raicesRuffini p)
    where descompon :: Polinomio Int -> [Int] -> [Polinomio Int]
          descompon p []     = [p]
          descompon p (a:as) = q : descompon r as
              where q = consPol 1 1 $ consPol 0 (-a) polCero
                    r = cocienteRuffini a p

--javperlag
{-
factorizacion4 :: Polinomio Int -> [Polinomio Int]
factorizacion4 p = aux p [] (raicesRuffini p)
aux p xs []= p:xs
aux p xs (y:ys)=aux (cocienteRuffini y p)((poli y):xs)ys
-}

-- Comentario: No está definida la función poli. 

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
