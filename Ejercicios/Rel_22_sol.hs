-- I1M 2015-16: Relación 22 (19 de febrero de 2016)
-- Enumeraciones de los números racionales. 
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- El objetivo de esta relación es construir dos enumeraciones de los
-- números racionales. Concretamente, 
-- + una enumeración basada en las representaciones hiperbinarias y
-- + una enumeración basada en los los árboles de Calkin-Wilf.
-- También se incluye la comprobación de la igualdad de las dos
-- sucesiones y una forma alternativa de calcular el número de
-- representaciones hiperbinarias mediante la función fucs.
-- 
-- Esta relación se basa en los siguientes artículos:
-- + Gaussianos "Sorpresa sumando potencias de 2" http://goo.gl/AHdAG
-- + N. Calkin y H.S. Wilf "Recounting the rationals" http://goo.gl/gVZtW
-- + Wikipedia "Calkin-Wilf tree" http://goo.gl/cB3vn

-- ---------------------------------------------------------------------
-- Importación de librerías                                           --
-- ---------------------------------------------------------------------

import Data.List
import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Numeración de los racionales mediante representaciones hiperbinarias
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la constante
--    potenciasDeDos :: [Integer]
-- tal que potenciasDeDos es la lista de las potencias de 2. Por
-- ejemplo, 
--    take 10 potenciasDeDos  ==  [1,2,4,8,16,32,64,128,256,512]
-- ---------------------------------------------------------------------

-- erisancha juanarcon jespergue javoliher manvermor carruirui3
-- josllagam pabmorgar alvalvdom1
potenciasDeDos :: [Integer]
potenciasDeDos = [2^x | x <- [0..]]

-- blaruiher manpende rubvilval
potenciasDeDos2 :: [Integer]
potenciasDeDos2 = iterate (*2) 1

-- fracruzam
potenciasDeDos3 :: [Integer]
potenciasDeDos3 = map (2^) [0..]

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    empiezaConDos :: Eq a => a -> [a] -> Bool
-- tal que (empiezaConDos x ys) se verifica si los dos primeros
-- elementos de ys son iguales a x. Por ejemplo,
--    empiezaConDos 5 [5,5,3,7]  ==  True
--    empiezaConDos 5 [5,3,5,7]  ==  False
--    empiezaConDos 5 [5,5,5,7]  ==  True
-- ---------------------------------------------------------------------

-- erisancha juanarcon jespergue javoliher pabmorgar
empiezaConDos :: Eq a => a -> [a] -> Bool
empiezaConDos x (y:z:ys) = x == y && z == x

-- Comentario: La definición anterior está incompleta.

-- blaruiher
empiezaConDos2 :: Eq a => a -> [a] -> Bool
empiezaConDos2 x ys = and (head ys == x ,  head (tail ys)== x)

-- Comentario: La definición anterior está incompleta y se puede simplificar.

-- manvermor carruiriu3
empiezaConDos3 :: Eq a => a -> [a] -> Bool
empiezaConDos3 x ys = isPrefixOf [x,x] ys

-- manpende josllagam
empiezaConDos4 :: Eq a => a -> [a] -> Bool
empiezaConDos4 x ys = takeWhile (==x) ys == take 2 ys

-- alvalvdom1
empiezaConDos5 :: Eq a => a -> [a] -> Bool
empiezaConDos5 x ys | length ys < 2 = False
                    | otherwise     = all (==x) (take 2 ys)

-- fracruzam
empiezaConDos6 :: Eq a => a -> [a] -> Bool
empiezaConDos6 n (x:y:_) = n == x && n == y
empiezaConDos6 _  xs     = False

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--    representacionesHB :: Integer -> [[Integer]]
-- tal que (representacionesHB n) es la lista de las representaciones
-- hiperbinarias del número n como suma de potencias de 2 donde cada
-- sumando aparece como máximo 2 veces. Por ejemplo
--    representacionesHB 5  ==  [[1,2,2],[1,4]]
--    representacionesHB 6  ==  [[1,1,2,2],[1,1,4],[2,4]]
-- ---------------------------------------------------------------------

-- juanarcon javoliher manpende pabmorgar
representacionesHB :: Integer -> [[Integer]]
representacionesHB n = nub $ suman n

potenciasMenores n = takeWhile (<=n) potenciasDeDos

duplica xs = sort $ xs ++ xs

suman n = [xs | xs <- subsequences (duplica (potenciasMenores n)), 
                sum xs == n]

-- Comentario: Es conveniente escribir el tipo de todas las funciones.

-- erisancha jespergue blaruiher carruirui3 josllagam rubvilval
-- alvalvdom1 
representacionesHB2 :: Integer -> [[Integer]]
representacionesHB2 n = 
    nub [xs | xs <- subsequences (concatMap (replicate 2) (potenciasMenores2 n)),
              sum xs == n]

potenciasMenores2 :: Integer -> [Integer] 
potenciasMenores2 n = takeWhile (<=n) potenciasDeDos

-- manvermor
representacionesHB3 :: Integer -> [[Integer]]
representacionesHB3 0 = [[0]]
representacionesHB3 n = aux n potenciasDeDos
    where aux n (x:xs) 
              | n == 0 = [[]]
              | n == x = [[x]]
              | x < n  = [x:ys | ys <- aux (n-x) (x:xs), 
                                 not (empiezaConDos x ys)] 
                         ++ aux n xs
              | otherwise = []   

-- fracruzam
-- En mi opinión, es mejor quitar las repeticiones antes de comprobar si la suma
-- es correcta
representacionesHB4 :: Integer -> [[Integer]]
representacionesHB4 n = [ns | ns <- ws, sum ns == n]
    where xs = takeWhile (<=n) potenciasDeDos
          ys = concatMap (replicate 2) xs
          ws = nub $ subsequences ys

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función
--    nRepresentacionesHB :: Integer -> Integer
-- tal que (nRepresentacionesHB n) es el número de las representaciones 
-- hiperbinarias del número n como suma de potencias de 2 donde cada
-- sumando aparece como máximo 2 veces. Por ejemplo,
--    ghci> [nRepresentacionesHB n | n <- [0..20]]
--    [1,1,2,1,3,2,3,1,4,3,5,2,5,3,4,1,5,4,7,3,8]
-- ---------------------------------------------------------------------

-- erisancha juanarcon jespergue javoliher blaruiher manvermor manpende
-- carruirui3 pabmorgar rubvilval josllagam alvalvdom1 fracruzam
nRepresentacionesHB :: Integer -> Integer
nRepresentacionesHB =  genericLength . representacionesHB 

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la función
--    termino :: Integer -> (Integer,Integer)
-- tal que (termino n) es el par formado por el número de
-- representaciones hiperbinarias de n y de n+1 (que se interpreta como 
-- su cociente). Por ejemplo, 
--    termino 4  ==  (3,2)
-- ---------------------------------------------------------------------

-- erisancha juanarcon jespergue manvermor manpende carruirui3 pabmorgar
-- josllagam rubvilval alvalvdom1 fracruzam
termino :: Integer -> (Integer,Integer)
termino n = (nRepresentacionesHB n, nRepresentacionesHB (n + 1))

-- javoliher
termino2 :: Integer -> (Integer,Integer)
termino2 n = (genericLength (representacionesHB n) ,
                          genericLength (representacionesHB (n+1)))

-- blaruiher
termino3 :: Integer -> (Integer,Integer)
termino3 n = ( x n, x(n+1))
            where x = nRepresentacionesHB 
-- ---------------------------------------------------------------------
-- Ejercicio 6. Definir la función
--    sucesionHB :: [(Integer,Integer)]
-- sucesionHB es la la sucesión cuyo témino n-ésimo es (termino n); es
-- decir, el par formado por el número de representaciones hiperbinarias
-- de n y de n+1. Por ejemplo, 
--    ghci> take 10 sucesionHB
--    [(1,1),(1,2),(2,1),(1,3),(3,2),(2,3),(3,1),(1,4),(4,3),(3,5)]
-- ---------------------------------------------------------------------

-- erisancha juanarcon jespergue javoliher manvermor pabmorgar josllagam
-- alvalvdom1
sucesionHB :: [(Integer,Integer)]
sucesionHB = [termino x | x <- [0..]]

-- blaruiher manpende carruirui3 rubvilval 
sucesionHB2 :: [(Integer,Integer)]
sucesionHB2 = map (termino)[0..]

-- fracruzam
-- No es necesario el paréntesis
sucesionHB3 :: [(Integer,Integer)]
sucesionHB3 = map termino [0..]

-- ---------------------------------------------------------------------
-- Ejercicio 7. Comprobar con QuickCheck que, para todo n,
-- (nRepresentacionesHB n) y  (nRepresentacionesHB (n+1)) son primos
-- entre sí. 
-- ---------------------------------------------------------------------

-- juanarcon jespergue  javoliher blaruiher manvermor manpende carruirui3
-- pabmorgar josllagam rubvilval alvalvdom1
prop_irreducibles :: Integer -> Property
prop_irreducibles n = 
    n >= 0 ==> 
    gcd (nRepresentacionesHB n) (nRepresentacionesHB (n+1)) == 1

-- La comprobación es
--    *Main> quickCheck prop_irreducibles
--    +++ OK, passed 100 tests.

-- fracruzam
prop_irreducibles2 :: Positive Integer -> Bool
prop_irreducibles2 (Positive n) = 
    gcd (nRepresentacionesHB n) (nRepresentacionesHB (n+1)) == 1

-- ---------------------------------------------------------------------
-- Ejercicio 8. Comprobar con QuickCheck que todos los elementos de la
-- sucesionHB son distintos.
-- ---------------------------------------------------------------------

-- erisancha juanarcon jespergue manpende carruirui3 pabmorgar rubvilval
-- josllagam
prop_distintos :: Positive Integer -> Positive Integer -> Bool
prop_distintos (Positive n) (Positive m) = 
    termino x /= termino y
    where x = n
          y = n + m

-- La comprobación es
--    *Main> quickCheck prop_distintos
--    +++ OK, passed 100 tests.

-- fracruzam
prop_distintos2 :: NonNegative Integer ->  Positive Integer -> Bool
prop_distintos2 (NonNegative n) (Positive m) = termino n /= termino (n+m) 

-- ---------------------------------------------------------------------
-- Ejercicio 9. Definir la función
--    contenido :: Integer -> Integer -> Bool
-- tal que (contenido n) se verifica si las expresiones reducidas de
-- todas las fracciones x/y, con x e y entre 1 y n, pertenecen a la
-- sucesionHB. Por ejemplo,  
--    contenidos 5  ==  True
-- ---------------------------------------------------------------------

-- erisancha blaruiher manvermor juanarcon jespergue manpende pabmorgar
-- rubvilval alvalvdom1
contenido :: Integer -> Bool
contenido n = 
    and [elem (fReducida (x,y)) sucesionHB | x <- [1..n], y <- [1..n]]
    where fReducida (x,y) = (x `div` m, y `div` m)
              where m = gcd x y

-- javoliher
-- Falta un criterio matemático para limitar la búsqueda en sucesionHB.
-- aleatoriamente, he escogido n^3

contenido2 :: Integer -> Bool
contenido2 n = and [ x `elem` ys | x <- fraccionesRed n]
    where ys = take' (n^3) sucesionHB

take' :: Integer -> [a] -> [a]
take' 0 xs     = []
take' n (x:xs) = x:take' (n-1) xs

fraccionesRed :: Integer -> [(Integer,Integer)]
fraccionesRed n = [(a,b) | a <- [1..n] , b <- [1..n], gcd a b == 1]

-- manpende josllagam
contenido3 :: Integer -> Bool
contenido3 n = subconjunto (sucesionMenoresHB n) p
    where p = [(x,y) | x <- [1..n], y <- [1..n], gcd x y == 1]

subconjunto :: Eq a => [a] -> [a] -> Bool
subconjunto xs ys = and [elem x ys | x <- xs]

sucesionMenoresHB :: Integer -> [(Integer,Integer)]
sucesionMenoresHB n = [termino x | x <- [1..n]]   

-- fracruzam
contenido4 :: Integer -> Bool
contenido4 n = 
    all (`elem` sucesionHB) [reduc x y | x <- [1..n], y <- [1..n]]
    where reduc :: Integer -> Integer -> (Integer,Integer)
          reduc  x y | mcd == 1     = (x,y)
                     | otherwise    = reduc (div x mcd) (div y mcd)
              where mcd = gcd x y

-- ---------------------------------------------------------------------
-- Ejercicio 10. Definir la función
--    indice :: (Integer,Integer) -> Integer
-- tal que (indice (a,b)) es el índice del par (a,b) en la sucesión de
-- los racionales. Por ejemplo, 
--    indice (3,2)  ==  4
-- ---------------------------------------------------------------------

-- erisancha manpende rubvilval josllagam
indice :: (Integer,Integer) -> Integer
indice (a,b) = 
    head [i | (i,(x,y)) <- zip [0..] sucesionHB , (a,b) == (x,y)]

-- juanarcon javoliher blaruiher jespergue pabmorgar alvalvdom1
indice2 x = genericLength $ takeWhile (/= x) sucesionHB

-- manvermor
indice3 :: (Integer,Integer) -> Integer
indice3 (a,b) = aux (a,b) sucesionHB
    where aux (a,b) [] = 0
          aux (a,b) (x:xs) | x == (a,b) = 0
                           | otherwise  = 1 + aux (a,b) xs

-- Comentario: La definición anterior se puede simplificar.

-- fracruzam
indice4 :: (Integer,Integer) -> Integer
indice4 p = genericLength $ takeWhile (/=p) sucesionHB

-- ---------------------------------------------------------------------
-- Numeraciones mediante árboles de Calkin-Wilf                       --
-- ---------------------------------------------------------------------

-- El árbol de Calkin-Wilf es el árbol definido por las siguientes
-- reglas:
--    * El nodo raíz es el (1,1)
--    * Los hijos del nodo (x,y) son (x,x+y) y (x+y,y)
-- Por ejemplo, los 4 primeros niveles del árbol de Calkin-Wilf son 
--                         (1,1)
--                           |
--               +-----------+-----------+
--               |                       |
--             (1,2)                   (2,1)
--               |                       |
--         +-----+-----+           +-----+-----+
--         |           |           |           |
--       (1,3)       (3,2)       (2,3)       (3,1)
--         |           |           |           |         
--      +--+--+     +--+--+     +--+--+     +--+--+
--      |     |     |     |     |     |     |     | 
--    (1,4) (4,3) (3,5) (5,2) (2,5) (5,3) (3,4) (4,1)

-- ---------------------------------------------------------------------
-- Ejercicio 11. Definir la función 
--    sucesores :: (Integer,Integer) -> [(Integer,Integer)]
-- tal que (sucesores (x,y)) es la lista de los hijos del par (x,y) en
-- el árbol de Calkin-Wilf. Por ejemplo, 
--    sucesores (3,2)  ==  [(3,5),(5,2)]
-- ---------------------------------------------------------------------

-- juanarcon erisancha javoliher blaruiher manvermor jespergue manpende
-- pabmorgar rubvilval alvalvdom1 fracruzam

sucesores :: (Integer,Integer) -> [(Integer,Integer)]
sucesores (x,y) = [(x,x+y),(x+y,y)]

-- ---------------------------------------------------------------------
-- Ejercicio 12. Definir la función
--    siguiente :: [(Integer,Integer)] -> [(Integer,Integer)]
-- tal que (siguiente xs) es la lista formada por los hijos de los
-- elementos de xs en el árbol de Calkin-Wilf. Por ejemplo, 
--    ghci> siguiente [(1,3),(3,2),(2,3),(3,1)]
--    [(1,4),(4,3),(3,5),(5,2),(2,5),(5,3),(3,4),(4,1)]
-- ---------------------------------------------------------------------

-- juanarcon javoliher blaruiher 
siguiente :: [(Integer,Integer)] -> [(Integer,Integer)]
siguiente xs = concat [sucesores x| x <- xs]

-- erisancha 
siguiente2 :: [(Integer,Integer)] -> [(Integer,Integer)]
siguiente2 = concatMap (sucesores)  

-- manvermor jespergue manpende pabmorgar rubvilval alvalvdom1 fracruzam
-- Es la misma que la de erisancha pero sin paréntesis

siguiente3 :: [(Integer,Integer)] -> [(Integer,Integer)]
siguiente3 = concatMap sucesores 


-- ---------------------------------------------------------------------
-- Ejercicio 13. Definir la constante
--    nivelesCalkinWilf:: [[(Integer,Integer)]]
-- tal que nivelesCalkinWilf es la lista de los niveles del árbol de
-- Calkin-Wilf. Por ejemplo, 
--    ghci> take 4 nivelesCalkinWilf
--    [[(1,1)],
--     [(1,2),(2,1)],
--     [(1,3),(3,2),(2,3),(3,1)],
--     [(1,4),(4,3),(3,5),(5,2),(2,5),(5,3),(3,4),(4,1)]]
-- ---------------------------------------------------------------------

-- juanarcon erisancha blaruiher manvermor jespergue pabmorgar
-- rubvilval fracruzam (no se necesitan los paréntesis)
nivelesCalkinWilf:: [[(Integer,Integer)]]
nivelesCalkinWilf = iterate (siguiente) [(1,1)]

-- javoliher manpende alvalvdom1
nivelesCalkinWilf2 :: [[(Integer,Integer)]]
nivelesCalkinWilf2 = [(1,1)]: aux  [(1,1)]

aux :: [(Integer,Integer)] -> [[(Integer,Integer)]]
aux xs = (siguiente xs): aux (siguiente xs)

-- ---------------------------------------------------------------------
-- Ejercicio 14. Definir la constante 
--    sucesionCalkinWilf :: [(Integer,Integer)]
-- tal que sucesionCalkinWilf es la lista correspondiente al recorrido
-- en anchura del árbol de Calkin-Wilf. Por ejemplo,
--    ghci> take 10 sucesionCalkinWilf
--    [(1,1),(1,2),(2,1),(1,3),(3,2),(2,3),(3,1),(1,4),(4,3),(3,5)]
-- ---------------------------------------------------------------------

-- juanarcon erisancha blaruiher jespergue manpende pabmorgar rubvilval
-- alvalvdom1 fracruzam
sucesionCalkinWilf :: [(Integer,Integer)]
sucesionCalkinWilf = concat nivelesCalkinWilf

--javoliher
sucesionCalkinWilf2 :: [(Integer,Integer)]
sucesionCalkinWilf2 = (1,1) : aux' [(1,1)]

aux' :: [(Integer,Integer)] -> [(Integer,Integer)]
aux' xs = (siguiente xs) ++ aux' (siguiente xs)

-- manvermor
sucesionCalkinWilf3 :: [(Integer,Integer)]
sucesionCalkinWilf3 = aux nivelesCalkinWilf
                       where aux (x:xs) = x ++ aux xs

-- ---------------------------------------------------------------------
-- Ejercicio 15. Definir la función
--    igual_sucesion_HB_CalkinWilf :: Int -> Bool
-- tal que (igual_sucesion_HB_CalkinWilf n) se verifica si los n
-- primeros términos de la sucesión HB son iguales que los de la
-- sucesión de Calkin-Wilf. Por ejemplo,
--    igual_sucesion_HB_CalkinWilf 20  ==  True
-- ---------------------------------------------------------------------

-- juanarcon jespergue
igual_sucesion_HB_CalkinWilf :: Int -> Bool
igual_sucesion_HB_CalkinWilf n = and [x == y | (x,y) <- xs]
    where xs = take n (zip sucesionHB sucesionCalkinWilf)

-- erisancha javoliher blaruiher manvermor manpende pabmorgar rubvilval
-- alvalvdom1 fracruzam (no se necesitan los paréntesis)
igual_sucesion_HB_CalkinWilf2 :: Int -> Bool
igual_sucesion_HB_CalkinWilf2 n = 
    take n (sucesionHB) == take n (sucesionCalkinWilf)

-- Comentario: La definición anterior se puede simplificar.

-- ---------------------------------------------------------------------
-- Número de representaciones hiperbinarias mediante la función fusc
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 16. Definir la función
--    fusc :: Integer -> Integer
-- tal que 
--    fusc(0)    = 1
--    fusc(2n+1) = fusc(n)
--    fusc(2n+2) = fusc(n+1)+fusc(n)
-- Por ejemplo,
--    fusc 4  ==  3
-- ---------------------------------------------------------------------

-- erisancha blaruiher juanarcon  pabmorgar rubvilval alvalvdom1
fusc :: Integer -> Integer
fusc 0 = 1
fusc n | odd n      = fusc ((n-1) `div` 2)
       | otherwise  = fusc ((n-2) `div` 2) + fusc (((n-2) `div` 2) + 1)

-- javoliher manpende
fusc2 :: Integer -> Integer
fusc2 0 = 1
fusc2 x | odd x     = fusc2 y
        | otherwise = (fusc2 y) + fusc2 (y -1)
        where y = x `div` 2

-- manvermor jespergue fracruzam
fusc3 :: Integer -> Integer
fusc3 0 = 1
fusc3 n | odd n     = fusc3 (div (n-1) 2)
        | otherwise = fusc3 (n+1) + fusc3 (div (n-2) 2)

-- ---------------------------------------------------------------------
-- Ejercicio 17. Comprobar con QuickCheck que, para todo n, (fusc n) es
-- el número de las representaciones hiperbinarias del número n como
-- suma de potencias de 2 donde cada sumando aparece como máximo 2
-- veces; es decir, que las funciones fusc y nRepresentacionesHB son
-- equivalentes. 
-- ---------------------------------------------------------------------

-- erisancha blaruiher manvermor juanarcon jespergue manpende pabmorgar
-- rubvilval alvalvdom1 fracruzam
prop_fusc :: Positive Integer -> Bool
prop_fusc (Positive n) = fusc n == nRepresentacionesHB n

-- La comprobación es
-- *Main> quickCheck prop_fusc
-- +++ OK, passed 100 tests.
