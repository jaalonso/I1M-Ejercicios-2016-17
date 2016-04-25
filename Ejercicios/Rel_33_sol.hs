-- I1M 2015-16: Relación 33 (22 de abril de 2016)
-- Operaciones con el TAD de polinomios.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- El objetivo de esta relación es ampliar el conjunto de operaciones
-- sobre polinomios definidas utilizando las implementaciones del TAD de
-- polinomio estudiadas en el tema 21 
--    http://www.cs.us.es/~jalonso/cursos/i1m-15/temas/tema-21.html
-- 
-- Además, en algunos ejemplos de usan polinomios con coeficientes
-- racionales. En Haskell, el número racional x/y se representa por
-- x%y. El TAD de los números racionales está definido en el módulo
-- Data.Ratio.   
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
import Data.Ratio

-- Hay que elegir una librería 
import I1M.PolOperaciones 
-- import PolOperaciones 

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    creaPolDispersa :: (Num a, Eq a) => [a] -> Polinomio a
-- tal que (creaPolDispersa xs) es el polinomio cuya representación
-- dispersa es xs. Por ejemplo,
--    creaPolDispersa [7,0,0,4,0,3]  ==  7*x^5 + 4*x^2 + 3
-- ---------------------------------------------------------------------

-- pabmorgar ivaruicam fatvilpiz silgongal jespergue manvermor marvilmor
-- isrbelnun josllagam alvalvdom1 lucgamgal javperlag
creaPolDispersa :: (Num a, Eq a) => [a] -> Polinomio a
creaPolDispersa []     = polCero
creaPolDispersa (x:xs) = consPol (length xs) x (creaPolDispersa xs)

-- abrdelrod
creaPolDispersa2 :: (Num a, Eq a) => [a] -> Polinomio a
creaPolDispersa2 xs = 
    foldr (\(x,y) -> consPol x y) polCero (zip [m-1,m-2..] xs)
    where m = length xs

-- Comentario: La definición anterior se puede mejorar usando uncurry.

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    creaPolDensa :: (Num a, Eq a) => [(Int,a)] -> Polinomio a
-- tal que (creaPolDensa xs) es el polinomio cuya representación
-- densa es xs. Por ejemplo,
--    creaPolDensa [(5,7),(4,2),(3,0)]  ==  7*x^5 + 2*x^4
-- ---------------------------------------------------------------------

-- pabmorgar ivaruicam fatvilpiz silgongal jespergue marvilmor
-- isrbelnun josllagam javperlag
creaPolDensa :: (Num a, Eq a) => [(Int,a)] -> Polinomio a
creaPolDensa []     = polCero 
creaPolDensa (x:xs) = consPol (fst x) (snd x) (creaPolDensa xs)

-- Comentario: La definición anterior se puede simplificar sin usar fst
-- ni snd.

-- abrdelrod
creaPolDensa2 :: (Num a, Eq a) => [(Int,a)] -> Polinomio a
creaPolDensa2 = foldr (\(x,y) -> consPol x y) polCero

-- Comentario: La definición anterior se puede mejorar usando uncurry.

-- manvermor alvalvdom1
creaPolDensa3 :: (Num a, Eq a) => [(Int,a)] -> Polinomio a
creaPolDensa3 []         = polCero
creaPolDensa3 ((x,y):xs) = consPol x y (creaPolDensa3 xs)

-- ---------------------------------------------------------------------
-- Nota. En el resto de la relación se usará en los ejemplos los
-- los polinomios que se definen a continuación.
-- ---------------------------------------------------------------------

pol1, pol2, pol3 :: (Num a, Eq a) => Polinomio a
pol1 = creaPolDensa [(5,1),(2,5),(1,4)]
pol2 = creaPolDispersa [2,3]
pol3 = creaPolDensa [(7,2),(4,5),(2,5)]

pol4, pol5, pol6 :: Polinomio Rational 
pol4 = creaPolDensa [(4,3),(2,5),(0,3)]
pol5 = creaPolDensa [(2,6),(1,2)]
pol6 = creaPolDensa [(2,8),(1,14),(0,3)]

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--    densa :: (Num a, Eq a) => Polinomio a -> [(Int,a)]
-- tal que (densa p) es la representación densa del polinomio p. Por
-- ejemplo, 
--    pol1        ==  x^5 + 5*x^2 + 4*x
--    densa pol1  ==  [(5,1),(2,5),(1,4)]
-- ---------------------------------------------------------------------

-- pabmorgar ivaruicam silgongal jespergue marvilmor isrbelnun josllagam
-- lucgamgal
densa :: (Num a, Eq a) => Polinomio a -> [(Int,a)]
densa p | p == polCero = []
        | otherwise =  [(grado p,coefLider p)] ++ densa (restoPol p)

-- Comentario: La definición anterior se puede mejorar sin usar (++)

-- fatvilpiz
densa2 :: (Num a, Eq a) => Polinomio a -> [(Int,a)]
densa2 p = zip (grados p) (coef p)

coef p | esPolCero p = []
       | otherwise   = coefLider p : coef (restoPol p)

grados p | esPolCero p = []
         | otherwise   = grado p : grados (restoPol p)

-- abrdelrod manvermor alvalvdom1 javperlag
densa3 :: (Num a, Eq a) => Polinomio a -> [(Int,a)]
densa3 p | esPolCero p = []
         | otherwise   = (grado p,coefLider p) : densa3 (restoPol p)

---------------------------------------------------------------------
-- Ejercicio 4. Definir la función
--    densaAdispersa :: (Num a, Eq a) => [(Int,a)] -> [a]
-- tal que (densaAdispersa ps) es la representación dispersa del
-- polinomio cuya representación densa es ps. Por ejemplo,
--    densaAdispersa [(5,1),(2,5),(1,4)]  ==  [1,0,0,5,4,0]
-- ---------------------------------------------------------------------

-- ivaruicam
densaAdispersa :: (Num a, Eq a) => [(Int,a)] -> [a]
densaAdispersa ys@((n,b):xs) = aux n ys
    where aux (-1) [] = []
          aux v [] = 0 : aux (v-1) []
          aux v ys@((n,b):xs) | v == n = b : aux  (v-1)  xs
                              | otherwise = 0 :aux (v-1) ys
 
-- silgongal jespergue abrdelrod marvilmor josllagam lucgamgal javperlag  
densaAdispersa2 :: (Num a, Eq a) => [(Int,a)] -> [a]
densaAdispersa2 = dispersa . creaPolDensa

-- manvermor
densaAdispersa3 :: (Num a, Eq a) => [(Int,a)] -> [a]
densaAdispersa3 ps = aux (map fst ps) (reverse [0..d])
    where d = fst (head ps)
          aux [] zs = []
          aux ys [] = []
          aux ys (z:zs) | notElem z ys = 0 : aux ys zs
                        | otherwise = busca z ps : aux ys zs
                
busca :: Eq a1 => a1 -> [(a1, a)] -> a
busca z xs = head [y | (x,y) <- xs, x == z]

-- isrbelnun
densaAdispersa4 :: (Num a, Eq a) => [(Int,a)] -> [a]
densaAdispersa4 (p:ps) 
    | (p:ps) == []  = []
    | (p:ps) == [p] = snd p : add (fst p) 0
    | otherwise     = snd p : add (fst p - (1 + fst (head ps))) 0 ++ 
                              densaAdispersa4 ps

add :: (Eq a, Num a) => a -> t -> [t]
add 0 _ = []
add n x = x : add (n-1) x

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la función
--    dispersa :: (Num a, Eq a) => Polinomio a -> [a]
-- tal que (dispersa p) es la representación dispersa del polinomio
-- p. Por ejemplo,
--    pol1           ==  x^5 + 5*x^2 + 4*x
--    dispersa pol1  ==  [1,0,0,5,4,0]
-- ---------------------------------------------------------------------

-- ivaruicam 
dispersa :: (Num a, Eq a) => Polinomio a -> [a]
dispersa p = aux (grado p) p
    where aux (-1) polCero = []
          aux v p | esPolCero p = 0: aux (v-1) p
                  | v == grado p = coefLider p :aux  (v-1) (restoPol p)
                  | otherwise = 0 :aux (v-1) p

-- silgongal jespergue josllagam
dispersa2 :: (Num a, Eq a) => Polinomio a -> [a]
dispersa2 p = coefLider p : aux p
    where aux p | esPolCero p = []
                | (grado p - grado r) == 1 = dispersa r
                | otherwise = replicate (grado p - grado r - 1) 0 ++
                              dispersa r
          r = restoPol p   

-- abrdelrod
dispersa3 :: (Num a, Eq a) => Polinomio a -> [a]
dispersa3 p = aux (grado p) p
    where aux 0 p | grado p == 0 = [coefLider p]
                  | otherwise = aux 0 (restoPol p)
          aux n p | grado p == n = coefLider p : aux (n-1) p
                  | grado (restoPol p) == n = aux n (restoPol p)
                  | otherwise = 0: aux (n-1) p

-- manvermor marvilmor isrbelnun
dispersa4 :: (Num a, Eq a) => Polinomio a -> [a]
dispersa4 p = densaAdispersa (densa p)

-- ---------------------------------------------------------------------
-- Ejercicio 6. Definir la función
--    coeficiente :: (Num a, Eq a) => Int -> Polinomio a -> a
-- tal que (coeficiente k p) es el coeficiente del término de grado k
-- del polinomio p. Por ejemplo,
--    pol1                ==  x^5 + 5*x^2 + 4*x
--    coeficiente 2 pol1  ==  5
--    coeficiente 3 pol1  ==  0
-- ---------------------------------------------------------------------

-- ivaruicam silgongal jespergue abrdelrod josllagam alvalvdom1
-- lucgamgal 
coeficiente :: (Num a, Eq a) => Int -> Polinomio a -> a
coeficiente k p | esPolCero p = 0
                | k == n      = coefLider p
                | otherwise   = coeficiente k r
    where r = restoPol p
          n = grado p

-- manvermor marvilmor
coeficiente2 :: (Num a, Eq a) => Int -> Polinomio a -> a
coeficiente2 k p =
    head [ x | (x,y) <- zip (dispersa p) (reverse [0..d]), k == y]
    where d = grado p

-- isrbelnun
coeficiente3 :: (Num a, Eq a) => Int -> Polinomio a -> a
coeficiente3 k p 
    | k <= (l-1) = head (drop (l-k-1) (dispersa p))
    | otherwise  = 0
    where l = length (dispersa p)

-- javperlag 
coeficiente4 :: (Num a, Eq a) => Int -> Polinomio a -> a
coeficiente4 k p 
    | g > k     = coeficiente4 k (restoPol p)
    | g < k     = 0
    | otherwise = coefLider p
    where g = grado p

-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir la función
--    coeficientes :: (Num a, Eq a) => Polinomio a -> [a]
-- tal que (coeficientes p) es la lista de los coeficientes del
-- polinomio p. Por ejemplo,
--    pol1               ==  x^5 + 5*x^2 + 4*x
--    coeficientes pol1  ==  [1,0,0,5,4,0]
-- ---------------------------------------------------------------------

-- pabmorgar ivaruicam silgongal jespergue manvermor marvilmor isrbelnun 
-- josllagam lucgamgal javperlag
coeficientes :: (Num a, Eq a) => Polinomio a -> [a]
coeficientes = dispersa 

-- abrdelrod (usando la función coeficiente:)
coeficientes2 :: (Num a, Eq a) => Polinomio a -> [a]
coeficientes2 p = map (flip coeficiente p) [grado p, grado p - 1..0]

-- Comentario: La definición anterior se puede mejorar reduciendo el
-- cálculo del grado y eliminando flip usando (`coeficiente` p)

-- alvalvdom1
coeficientes3 :: (Num a, Eq a) => Polinomio a -> [a]
coeficientes3 p = [coeficiente k p | k <- [g,g-1..0]]
    where g = grado p

-- ---------------------------------------------------------------------
-- Ejercicio 8. Definir la función
--    potencia :: (Num a, Eq a) => Polinomio a -> Int -> Polinomio a
-- tal que (potencia p n) es la potencia n-ésima del polinomio p. Por
-- ejemplo, 
--    pol2             ==  2*x + 3
--    potencia pol2 2  ==  4*x^2 + 12*x + 9
--    potencia pol2 3  ==  8*x^3 + 36*x^2 + 54*x + 27
-- ---------------------------------------------------------------------

-- abrdelrod manvermor marvilmor isrbelnun josllagam alvalvdom1 lucgamgal
potencia :: (Num a, Eq a) => Polinomio a -> Int -> Polinomio a
potencia p 1 = p
potencia p n = multPol p (potencia p (n-1))

-- ---------------------------------------------------------------------
-- Ejercicio 9. Mejorar la definición de potencia definiendo la función
--    potenciaM :: (Num a, Eq a) => Polinomio a -> Int -> Polinomio a
-- tal que (potenciaM p n) es la potencia n-ésima del polinomio p,
-- utilizando las siguientes propiedades:
--    * Si n es par,   entonces x^n = (x^2)^(n/2)
--    * Si n es impar, entonces x^n = x * (x^2)^((n-1)/2)
-- Por ejemplo, 
--    pol2              ==  2*x + 3
--    potenciaM pol2 2  ==  4*x^2 + 12*x + 9
--    potenciaM pol2 3  ==  8*x^3 + 36*x^2 + 54*x + 27
-- ---------------------------------------------------------------------

-- abrdelrod manvermor marvilmor isrbelnun josllagam alvalvdom1
-- lucgamgal 
potenciaM :: (Num a, Eq a) => Polinomio a -> Int -> Polinomio a
potenciaM p 1 = p
potenciaM p n 
    | even n    = potenciaM (multPol p p) (div n 2)
    | otherwise = multPol p (potenciaM (multPol p p) (div (n-1) 2))

-- ---------------------------------------------------------------------
-- Ejercicio 10. Definir la función
--    integral :: (Fractional a, Eq a) => Polinomio a -> Polinomio a
-- tal que (integral p) es la integral del polinomio p cuyos coefientes
-- son números racionales. Por ejemplo,
--    ghci> pol3
--    2*x^7 + 5*x^4 + 5*x^2
--    ghci> integral pol3
--    0.25*x^8 + x^5 + 1.6666666666666667*x^3
--    ghci> integral pol3 :: Polinomio Rational
--    1 % 4*x^8 + x^5 + 5 % 3*x^3
-- ---------------------------------------------------------------------

-- silgongal jespergue manvermor marvilmor isrbelnun josllagam alvalvdom1
integral :: (Fractional a, Eq a) => Polinomio a -> Polinomio a
integral p 
    | esPolCero p = polCero
    | otherwise   = consPol g 
                            (coefLider p / fromIntegral  g)
                            (integral (restoPol p))
    where g = grado p + 1

-- abrdelrod
integral2 :: (Fractional a, Eq a) => Polinomio a -> Polinomio a
integral2 = 
    creaPolDensa . map (\(x,y) -> (x+1, y/fromIntegral (x+1))) . densa

-- ---------------------------------------------------------------------
-- Ejercicio 11. Definir la función
--    integralDef :: (Fractional t, Eq t) => Polinomio t -> t -> t -> t
-- tal que (integralDef p a b) es la integral definida del polinomio p
-- cuyos coefientes son números racionales. Por ejemplo,
--    ghci> integralDef pol3 0 1
--    2.916666666666667
--    ghci> integralDef pol3 0 1 :: Rational
--    35 % 12
-- ---------------------------------------------------------------------

-- silgongal jespergue marvilmor
integralDef :: (Fractional t, Eq t) => Polinomio t -> t -> t -> t          
integralDef p a b = sustituye i b - sustituye i a
    where i = integral p

sustituye :: (Eq a, Num a) => Polinomio a -> a -> a
sustituye p a = sum [y*a^x | (x,y) <- densa p]

-- abrdelrod manvermor isrbelnun josllagam alvalvdom1
integralDef2 :: (Fractional t, Eq t) => Polinomio t -> t -> t -> t          
integralDef2 p a b = valor p' b - valor p' a
   where p' = integral p

-- ---------------------------------------------------------------------
-- Ejercicio 12. Definir la función
--    multEscalar :: (Num a, Eq a) => a -> Polinomio a -> Polinomio a
-- tal que (multEscalar c p) es el polinomio obtenido multiplicando el
-- número c por el polinomio p. Por ejemplo, 
--    pol2                    ==  2*x + 3
--    multEscalar 4 pol2      ==  8*x + 12
--    multEscalar (1%4) pol2  ==  1 % 2*x + 3 % 4
-- ---------------------------------------------------------------------

-- silgongal jespergue manvermor marvilmor
multEscalar :: (Num a, Eq a) => a -> Polinomio a -> Polinomio a
multEscalar c p = creaPolDensa [(x,y*c) | (x,y) <- densa p]

-- abrdelrod josllagam
multEscalar2 :: (Num a, Eq a) => a -> Polinomio a -> Polinomio a
multEscalar2 c p = multPorTerm (creaTermino 0 c) p

-- Comentario: La definición anterior se puede simplificar eliminando un
-- argumento. 

-- isrbelnun alvalvdom1
multEscalar3 :: (Num a, Eq a) => a -> Polinomio a -> Polinomio a
multEscalar3 c p 
    | esPolCero p = polCero
    | otherwise   = consPol (grado p) 
                            (c*coefLider p) 
                            (multEscalar3 c (restoPol p))

-- ---------------------------------------------------------------------
-- Ejercicio 13. Definir la función
--    cociente:: (Fractional a, Eq a) => 
--               Polinomio a -> Polinomio a -> Polinomio a
-- tal que (cociente p q) es el cociente de la división de p entre
-- q. Por ejemplo, 
--    pol4  ==  3 % 1*x^4 + 5 % 1*x^2 + 3 % 1
--    pol5  ==  6 % 1*x^2 + 2 % 1*x
--    cociente pol4 pol5  ==  1 % 2*x^2 + (-1) % 6*x + 8 % 9
-- ---------------------------------------------------------------------

-- manvermor josllagam abrdelrod
cociente:: (Fractional a, Eq a) => Polinomio a -> Polinomio a -> Polinomio a
cociente p q | n == 0    = multEscalar (1/x) p
             | m < n     = polCero
             | otherwise = consPol s z (cociente r q)
    where x = coefLider p
          m = grado p
          y = coefLider q
          n = grado q
          s = m-n
          z = x/y
          r = restaPol p (multPorTerm (creaTermino s z) q)

-- isrbelnun
miniCociente:: (Fractional a, Eq a) => Polinomio a -> Polinomio a -> Polinomio a
miniCociente p q = 
    consPol (grado p - grado q) (coefLider p/coefLider q) polCero

cociente2 :: (Fractional a, Eq a) => Polinomio a -> Polinomio a -> Polinomio a
cociente2 p q 
    | grado p < grado q = polCero
    | otherwise         = 
        consPol (grado p - grado q) 
                (coefLider p/coefLider q)
                (cociente (restaPol p (multPol (miniCociente p q) q)) q)

-- ---------------------------------------------------------------------
-- Ejercicio 14. Definir la función
--    resto :: (Fractional a, Eq a) => 
--             Polinomio a -> Polinomio a -> Polinomio a
-- tal que (resto p q) es el resto de la división de p entre q. Por
-- ejemplo,  
--    pol4  ==  3 % 1*x^4 + 5 % 1*x^2 + 3 % 1
--    pol5  ==  6 % 1*x^2 + 2 % 1*x
--    resto pol4 pol5  ==  (-16) % 9*x + 3 % 1
-- ---------------------------------------------------------------------

-- manvermor abrdelrod isrbelnun josllagam lucgamgal jespergue
resto :: (Fractional a, Eq a) => Polinomio a -> Polinomio a -> Polinomio a
resto p q = restaPol p (multPol q (cociente p q))

-- ---------------------------------------------------------------------
-- Ejercicio 15. Definir la función
--    divisiblePol :: (Fractional a, Eq a) => 
--                    Polinomio a -> Polinomio a -> Bool
-- tal que (divisiblePol p q) se verifica si el polinomio p es divisible
-- por el polinomio q. Por ejemplo,
--    pol6  ==  8 % 1*x^2 + 14 % 1*x + 3 % 1
--    pol2  ==  2*x + 3
--    pol5  ==  6 % 1*x^2 + 2 % 1*x
--    divisiblePol pol6 pol2  ==  True
--    divisiblePol pol6 pol5  ==  False
-- ---------------------------------------------------------------------

-- abrdelrod manvermor marvilmor isrbelnun josllagam lucgamgal jespergue
divisiblePol :: (Fractional a, Eq a) => Polinomio a -> Polinomio a -> Bool
divisiblePol p q = resto p q == polCero

-- ---------------------------------------------------------------------
-- Ejercicio 16. El método de Horner para calcular el valor de un
-- polinomio se basa en representarlo de una forma forma alternativa. Por
-- ejemplo, para calcular el valor de 
--    a*x^5 + b*x^4 + c*x^3 + d*x^2 + e*x + f
-- se representa como
--   ((((a * x + b) * x + c) * x + d) * x + e) * x + f
-- y se evalúa de dentro hacia afuera. 
-- 
-- Definir la función
--    horner:: (Num a, Eq a) => Polinomio a -> a -> a
-- tal que (horner p x) es el valor del polinomio p al sustituir su
-- variable por el número x. Por ejemplo, 
--    horner pol1 0     ==  0
--    horner pol1 1     ==  10
--    horner pol1 1.5   ==  24.84375
--    horner pol1 (3%2) ==  795 % 32
-- ---------------------------------------------------------------------

-- silgongal jespergue marvilmor isrbelnun
horner:: (Num a, Eq a) => Polinomio a -> a -> a
horner p x = sum [y*x^z | (z,y) <- densa p]

-- abrdelrod
horner2 :: (Num a, Eq a) => Polinomio a -> a -> a
horner2 p x = foldl1 (\a b -> a*x+b) (dispersa p)

-- alvalvdom1
horner4 :: (Num a, Eq a) => Polinomio a -> a -> a
horner4 p x = aux (dispersa p) x
    where aux [] _       = 0
          aux [a] x      = a
          aux (a:b:ys) x = aux ((a*x+b):ys) x

-- Comentario: La definición anterior se puede simplificar eliminando un
-- argumento. 

