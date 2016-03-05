-- I1M 2014-15: Rel_26.hs (4 de marzo de 2016).
-- Combinatoria.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- El objetivo de esta relación es estudiar la generación y el número de
-- las principales operaciones de la combinatoria. En concreto, se
-- estudia 
--    * Permutaciones.
--    * Combinaciones sin repetición.          
--    * Combinaciones con repetición
--    * Variaciones sin repetición.
--    * Variaciones con repetición.

-- ---------------------------------------------------------------------
-- Importación de librerías                                           --
-- ---------------------------------------------------------------------
 
import Test.QuickCheck
import Data.List 

-- ---------------------------------------------------------------------
-- § Subconjuntos
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir, por recursión, la función 
--    subconjunto :: Eq a => [a] -> [a] -> Bool
-- tal que (subconjunto xs ys) se verifica si xs es un subconjunto de
-- ys. Por ejemplo,
--    subconjunto [1,3,2,3] [1,2,3]  ==  True
--    subconjunto [1,3,4,3] [1,2,3]  ==  False
-- ---------------------------------------------------------------------

-- carmengar juamorrom1 josllagam blaruiher juanarcon
subconjunto :: Eq a => [a] -> [a] -> Bool
subconjunto [] _      = True
subconjunto (x:xs) ys = elem x ys && subconjunto xs ys
 
-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir, mediante all, la función 
--    subconjunto' :: Eq a => [a] -> [a] -> Bool
-- tal que (subconjunto' xs ys) se verifica si xs es un subconjunto de
-- ys. Por ejemplo,
--    subconjunto' [1,3,2,3] [1,2,3]  ==  True
--    subconjunto' [1,3,4,3] [1,2,3]  ==  False
-- ---------------------------------------------------------------------
 
-- carmengar juamorrom1 josllagam blaruiher juanarcon
subconjunto' :: Eq a => [a] -> [a] -> Bool
subconjunto' xs ys = all (`elem` ys) xs
 
-- ---------------------------------------------------------------------
-- Ejercicio 3. Comprobar con QuickCheck que las funciones subconjunto
-- y subconjunto' son equivalentes.
-- ---------------------------------------------------------------------
 
-- carmengar juamorrom1 josllagam blaruiher juanarcon
-- La propiedad es
prop_equivalencia :: [Int] -> [Int] -> Bool
prop_equivalencia xs ys = s xs ys == s' xs ys
    where s  = subconjunto
          s' = subconjunto'

-- Comentario: La definición anterior se puede simplificar.
 
-- La comprobación es
-- λ> quickCheck prop_equivalencia
-- +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función 
--    igualConjunto :: Eq a => [a] -> [a] -> Bool
-- tal que (igualConjunto xs ys) se verifica si las listas xs e ys,
-- vistas como conjuntos, son iguales. Por ejemplo,
--    igualConjunto [1..10] [10,9..1]   ==  True
--    igualConjunto [1..10] [11,10..1]  ==  False
-- ---------------------------------------------------------------------
 
-- carmengar juamorrom1 josllagam blaruiher juanarcon
igualConjunto :: Eq a => [a] -> [a] -> Bool
igualConjunto xs ys = subconjunto xs ys && subconjunto ys xs
 
-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la función 
--    subconjuntos :: [a] -> [[a]]
-- tal que (subconjuntos xs) es la lista de las subconjuntos de la lista
-- xs. Por ejemplo, 
--    ghci> subconjuntos [2,3,4]
--    [[2,3,4],[2,3],[2,4],[2],[3,4],[3],[4],[]]
--    ghci> subconjuntos [1,2,3,4]
--    [[1,2,3,4],[1,2,3],[1,2,4],[1,2],[1,3,4],[1,3],[1,4],[1],
--       [2,3,4],  [2,3],  [2,4],  [2],  [3,4],  [3],  [4], []]
-- ---------------------------------------------------------------------

-- carmengar
subconjuntos :: [a] -> [[a]]
subconjuntos []     = [[]]
subconjuntos (x:xs) = (map (x:) s) ++ s
    where s = subconjuntos xs

-- Comentario: La definición anterior se puede simplificar eliminando
-- paréntesis. 

-- juamorrom1 blaruiher
subconjuntos2 :: [a] -> [[a]]
subconjuntos2 = subsequences

-- josllagam juanarcon
subconjuntos3 :: [a] -> [[a]]
subconjuntos3 [] = [[]]
subconjuntos3 (x:xs) = [x : ys | ys <- t] ++ t
    where t = subconjuntos3 xs
 
-- ---------------------------------------------------------------------
-- § Permutaciones
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 6. Definir la función
--    intercala :: a -> [a] -> [[a]]
-- tal que (intercala x ys) es la lista de las listas obtenidas
-- intercalando x entre los elementos de ys. Por ejemplo,
--    intercala 1 [2,3]  ==  [[1,2,3],[2,1,3],[2,3,1]]
-- ---------------------------------------------------------------------

-- carmengar josllagam blaruiher
intercala :: a -> [a] -> [[a]]
intercala x []     = [[x]]
intercala x (y:xs) = (x:y:xs) : (map (y:) (intercala x xs))

-- Comentario: La definición anterior se puede simplificar eliminando
-- paréntesis. 

-- juamorrom1 juanarcon
intercala2 :: a -> [a] -> [[a]]
intercala2 x ys = [ (take n ys) ++ x : (drop n ys) | n <- [0..length ys] ]

-- Comentario: La definición anterior se puede simplificar.

-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir la función 
--    permutaciones :: [a] -> [[a]]  
-- tal que (permutaciones xs) es la lista de las permutaciones de la
-- lista xs. Por ejemplo,
--    permutaciones "bc"   ==  ["bc","cb"]
--    permutaciones "abc"  ==  ["abc","bac","bca","acb","cab","cba"]
-- ---------------------------------------------------------------------

-- carmengar josllagam blaruiher juanarcon
permutaciones [] = [[]]
permutaciones (x:xs) = concatMap (intercala x) (permutaciones xs) 

-- Comentario: La definición anterior se puede simplificar usando foldr.

-- juamorrom1
permutaciones2 :: [a] -> [[a]]
permutaciones2 = permutations

-- ---------------------------------------------------------------------
-- Ejercicio 8. Definir la función
--    permutacionesN :: Integer -> [[Integer]]
-- tal que (permutacionesN n) es la lista de las permutaciones de los n
-- primeros números. Por ejemplo,
--    ghci> permutacionesN 3
--    [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]
-- ---------------------------------------------------------------------  

-- carmengar josllagam juamorrom1 blaruiher juanarcon
permutacionesN :: Integer -> [[Integer]]
permutacionesN n = permutaciones [1..n]

-- ---------------------------------------------------------------------
-- Ejercicio 9. Definir, usando permutacionesN, la función
--    numeroPermutacionesN :: Integer -> Integer
-- tal que (numeroPermutacionesN n) es el número de permutaciones de un
-- conjunto con n elementos. Por ejemplo,
--    numeroPermutacionesN 3  ==  6
--    numeroPermutacionesN 4  ==  24
-- ---------------------------------------------------------------------

-- carmengar josllagam juamorrom1 blaruiher juanarcon
numeroPermutacionesN :: Integer -> Integer
numeroPermutacionesN = genericLength . permutacionesN

-- ---------------------------------------------------------------------
-- Ejercicio 10. Definir la función
--    fact :: Integer -> Integer
-- tal que (fact n) es el factorial de n. Por ejemplo,
--    fact 3  ==  6
-- ---------------------------------------------------------------------

-- carmengar josllagam
fact :: Integer -> Integer
fact = numeroPermutacionesN

-- carmengar 
fact2 :: Integer -> Integer 
fact2 n = foldr (*) 1 [1..n]

-- juamorrom1 blaruiher juanarcon
fact3 :: Integer -> Integer
fact3 n = product [1..n]

-- ---------------------------------------------------------------------
-- Ejercicio 11. Definir, usando fact, la función
--    numeroPermutacionesN' :: Integer -> Integer
-- tal que (numeroPermutacionesN' n) es el número de permutaciones de un
-- conjunto con n elementos. Por ejemplo,
--    numeroPermutacionesN' 3  ==  6
--    numeroPermutacionesN' 4  ==  24
-- ---------------------------------------------------------------------

-- carmengar josllagam juamorrom1 blaruiher juanarcon
numeroPermutacionesN' :: Integer -> Integer
numeroPermutacionesN' = fact

-- ---------------------------------------------------------------------
-- Ejercicio 12. Definir la función
--    prop_numeroPermutacionesN :: Integer -> Bool
-- tal que (prop_numeroPermutacionesN n) se verifica si las funciones
-- numeroPermutacionesN y numeroPermutacionesN' son equivalentes para
-- los n primeros números. Por ejemplo,
--    prop_numeroPermutacionesN 5  ==  True
-- ---------------------------------------------------------------------

-- carmengar josllagam juamorrom1 blaruiher juanarcon
prop_numeroPermutacionesN :: Integer -> Bool
prop_numeroPermutacionesN n = map f [1..n] == map f' [1..n]
    where f  = numeroPermutacionesN
          f' = numeroPermutacionesN'

-- ---------------------------------------------------------------------
-- § Combinaciones          
-- ---------------------------------------------------------------------  

-- ---------------------------------------------------------------------
-- Ejercicio 13. Definir la función 
--    combinaciones :: Integer -> [a] -> [[a]]
-- tal que (combinaciones k xs) es la lista de las combinaciones de
-- orden k de los elementos de la lista xs. Por ejemplo,
--    ghci> combinaciones 2 "bcde"
--    ["bc","bd","be","cd","ce","de"]
--    ghci> combinaciones 3 "bcde"
--    ["bcd","bce","bde","cde"]
--    ghci> combinaciones 3 "abcde"
--    ["abc","abd","abe","acd","ace","ade","bcd","bce","bde","cde"]
-- ---------------------------------------------------------------------
 
-- carmengar blaruiher
combinaciones :: Integer -> [a] -> [[a]]
combinaciones n xs =
    filter (\x -> genericLength x == n) (subconjuntos xs)

-- pedestara josllagam juamorrom1 juanarcon
combinaciones2 :: Integer -> [a] -> [[a]]
combinaciones2 n xs = [ys | ys <- subconjuntos xs, 
                            genericLength ys == n]

-- ---------------------------------------------------------------------
-- Ejercicio 14. Definir la función
--    combinacionesN :: Integer -> Integer -> [[Int]]
-- tal que (combinacionesN n k) es la lista de las combinaciones de
-- orden k de los n primeros números. Por ejemplo,
--    ghci> combinacionesN 4 2
--    [[1,2],[1,3],[1,4],[2,3],[2,4],[3,4]]
--    ghci> combinacionesN 4 3
--    [[1,2,3],[1,2,4],[1,3,4],[2,3,4]]
-- ---------------------------------------------------------------------  

-- carmengar josllagam blaruiher juanarcon
combinacionesN :: Integer -> Integer -> [[Integer]]
combinacionesN n k = combinaciones k [1..n]

-- ---------------------------------------------------------------------
-- Ejercicio 15. Definir, usando combinacionesN, la función
--    numeroCombinaciones :: Integer -> Integer -> Integer
-- tal que (numeroCombinaciones n k) es el número de combinaciones de
-- orden k de un conjunto con n elementos. Por ejemplo,
--    numeroCombinaciones 4 2  ==  6
--    numeroCombinaciones 4 3  ==  4
-- ---------------------------------------------------------------------

-- carmengar josllagam blaruiher juanarcon
numeroCombinaciones :: Integer -> Integer -> Integer
numeroCombinaciones n k = 
   genericLength $ combinacionesN n k

-- ---------------------------------------------------------------------
-- Ejercicio 16. Definir la función
--    comb :: Integer -> Integer -> Integer
-- tal que (comb n k) es el número combinatorio n sobre k; es decir, . 
--    (comb n k) = n! / (k!(n-k)!).
-- Por ejemplo,
--    comb 4 2  ==  6
--    comb 4 3  ==  4
-- ---------------------------------------------------------------------
 
-- carmengar josllagam blaruiher juanarcon
comb :: Integer -> Integer -> Integer
comb n k = fact n `div` (fact k * fact (n-k))
 
-- ---------------------------------------------------------------------
-- Ejercicio 17. Definir, usando comb, la función
--    numeroCombinaciones' :: Integer -> Integer -> Integer
-- tal que (numeroCombinaciones' n k) es el número de combinaciones de
-- orden k de un conjunto con n elementos. Por ejemplo,
--    numeroCombinaciones' 4 2  ==  6
--    numeroCombinaciones' 4 3  ==  4
-- ---------------------------------------------------------------------

-- carmengar josllagam blaruiher juanarcon
numeroCombinaciones' :: Integer -> Integer -> Integer
numeroCombinaciones' = comb

-- ---------------------------------------------------------------------
-- Ejercicio 18. Definir la función
--    prop_numeroCombinaciones :: Integer -> Bool
-- tal que (prop_numeroCombinaciones n) se verifica si las funciones
-- numeroCombinaciones y numeroCombinaciones' son equivalentes para
-- los n primeros números y todo k entre 1 y n. Por ejemplo,
--    prop_numeroCombinaciones 5  ==  True
-- ---------------------------------------------------------------------

-- carmengar josllagam blaruiher juanarcon
prop_numeroCombinaciones :: Integer -> Bool
prop_numeroCombinaciones n =  
   map nc [1..n] == map nc' [1..n] 
    where nc  = numeroCombinaciones n
          nc' = numeroCombinaciones' n

-- ---------------------------------------------------------------------
-- § Combinaciones con repetición
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 19. Definir la función
--    combinacionesR :: Integer -> [a] -> [[a]]
-- tal que (combinacionesR k xs) es la lista de las combinaciones orden
-- k de los elementos de xs con repeticiones. Por ejemplo,
--    ghci> combinacionesR 2 "abc"
--    ["aa","ab","ac","bb","bc","cc"]
--    ghci> combinacionesR 3 "bc"
--    ["bbb","bbc","bcc","ccc"]
--    ghci> combinacionesR 3 "abc"
--    ["aaa","aab","aac","abb","abc","acc","bbb","bbc","bcc","ccc"]
-- ---------------------------------------------------------------------

-- carmengar
combinacionesR :: Integer -> [a] -> [[a]]
combinacionesR 0 _  = [[]]
combinacionesR _ [] = []
combinacionesR k ys@(x:xs) = 
    (map (x:) (combinacionesR (k-1) ys)) ++ (combinacionesR k xs)

-- Comentario: La definición anterior se puede simplificar eliminando
-- paréntesis. 

-- ---------------------------------------------------------------------
-- Ejercicio 20. Definir la función
--    combinacionesRN :: Integer -> Integer -> [[Integer]]    
-- tal que (combinacionesRN n k) es la lista de las combinaciones orden
-- k de los primeros n números naturales. Por ejemplo,
--    ghci> combinacionesRN 3 2
--    [[1,1],[1,2],[1,3],[2,2],[2,3],[3,3]]
--    ghci> combinacionesRN 2 3
--    [[1,1,1],[1,1,2],[1,2,2],[2,2,2]]
-- ---------------------------------------------------------------------

-- carmengar
combinacionesRN :: Integer -> Integer -> [[Integer]]    
combinacionesRN n k = combinacionesR k [1..n]

-- ---------------------------------------------------------------------
-- Ejercicio 21. Definir, usando combinacionesRN, la función
--    numeroCombinacionesR :: Integer -> Integer -> Integer
-- tal que (numeroCombinacionesR n k) es el número de combinaciones con
-- repetición de orden k de un conjunto con n elementos. Por ejemplo,
--    numeroCombinacionesR 3 2  ==  6
--    numeroCombinacionesR 2 3  ==  4
-- ---------------------------------------------------------------------

-- carmengar
numeroCombinacionesR :: Integer -> Integer -> Integer
numeroCombinacionesR n k = genericLength $ combinacionesRN n k

-- ---------------------------------------------------------------------
-- Ejercicio 22. Definir, usando comb, la función
--    numeroCombinacionesR' :: Integer -> Integer -> Integer
-- tal que (numeroCombinacionesR' n k) es el número de combinaciones con
-- repetición de orden k de un conjunto con n elementos. Por ejemplo,
--    numeroCombinacionesR' 3 2  ==  6
--    numeroCombinacionesR' 2 3  ==  4
-- ---------------------------------------------------------------------

-- carmengar
numeroCombinacionesR' :: Integer -> Integer -> Integer
numeroCombinacionesR' n k = comb (n+k-1) k

-- ---------------------------------------------------------------------
-- Ejercicio 23. Definir la función
--    prop_numeroCombinacionesR :: Integer -> Bool
-- tal que (prop_numeroCombinacionesR n) se verifica si las funciones
-- numeroCombinacionesR y numeroCombinacionesR' son equivalentes para
-- los n primeros números y todo k entre 1 y n. Por ejemplo,
--    prop_numeroCombinacionesR 5  ==  True
-- ---------------------------------------------------------------------

-- carmengar
prop_numeroCombinacionesR :: Integer -> Bool
prop_numeroCombinacionesR n = map nc [1..n] == map nc' [1..n]
    where nc  = numeroCombinacionesR n
          nc' = numeroCombinacionesR' n

-- ---------------------------------------------------------------------
-- § Variaciones
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 24. Definir la función 
--    variaciones :: Integer -> [a] -> [[a]]
-- tal que (variaciones n xs) es la lista de las variaciones n-arias
-- de la lista xs. Por ejemplo,
--    variaciones 2 "abc"  ==  ["ab","ba","ac","ca","bc","cb"]
-- ---------------------------------------------------------------------
 
-- carmengar
variaciones :: Integer -> [a] -> [[a]]
variaciones k xs = concatMap permutaciones (combinaciones k xs)

-- ---------------------------------------------------------------------
-- Ejercicio 25. Definir la función
--    variacionesN :: Integer -> Integer -> [[Integer]]
-- tal que (variacionesN n k) es la lista de las variaciones de orden k
-- de los n primeros números. Por ejemplo,
--    variacionesN 3 2  ==  [[1,2],[2,1],[1,3],[3,1],[2,3],[3,2]]
-- ---------------------------------------------------------------------  

-- carmengar
variacionesN :: Integer -> Integer -> [[Integer]]
variacionesN n k = variaciones k [1..n]

-- ---------------------------------------------------------------------
-- Ejercicio 26. Definir, usando variacionesN, la función
--    numeroVariaciones :: Integer -> Integer -> Integer
-- tal que (numeroVariaciones n k) es el número de variaciones de orden
-- k de un conjunto con n elementos. Por ejemplo,
--    numeroVariaciones 4 2  ==  12
--    numeroVariaciones 4 3  ==  24
-- ---------------------------------------------------------------------

-- carmengar
numeroVariaciones :: Integer -> Integer -> Integer
numeroVariaciones n k = genericLength $ variacionesN n k

-- ---------------------------------------------------------------------
-- Ejercicio 27. Definir, usando product, la función
--    numeroVariaciones' :: Integer -> Integer -> Integer
-- tal que (numeroVariaciones' n k) es el número de variaciones de orden
-- k de un conjunto con n elementos. Por ejemplo,
--    numeroVariaciones' 4 2  ==  12
--    numeroVariaciones' 4 3  ==  24
-- ---------------------------------------------------------------------

-- carmengar
numeroVariaciones' :: Integer -> Integer -> Integer
numeroVariaciones' n k = product [1..n] `div` product [1..n-k]

-- Comentario: La definición anterior se puede mejorar.

-- ---------------------------------------------------------------------
-- Ejercicio 28. Definir la función
--    prop_numeroVariaciones :: Integer -> Bool
-- tal que (prop_numeroVariaciones n) se verifica si las funciones
-- numeroVariaciones y numeroVariaciones' son equivalentes para
-- los n primeros números y todo k entre 1 y n. Por ejemplo,
--    prop_numeroVariaciones 5  ==  True
-- ---------------------------------------------------------------------

-- carmengar
prop_numeroVariaciones :: Integer -> Bool
prop_numeroVariaciones n = map nv [1..n] == map nv' [1..n]
    where nv = numeroVariaciones n
          nv' = numeroVariaciones' n

-- ---------------------------------------------------------------------
-- § Variaciones con repetición
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 28. Definir la función
--    variacionesR :: Integer -> [a] -> [[a]]
-- tal que (variacionesR k xs) es la lista de las variaciones de orden
-- k de los elementos de xs con repeticiones. Por ejemplo,
--    ghci> variacionesR 1 "ab"
--    ["a","b"]
--    ghci> variacionesR 2 "ab"
--    ["aa","ab","ba","bb"]
--    ghci> variacionesR 3 "ab"
--    ["aaa","aab","aba","abb","baa","bab","bba","bbb"]
-- ---------------------------------------------------------------------

-- carmengar
variacionesR :: Integer -> [a] -> [[a]]
variacionesR _ [] = []
variacionesR 0 _  = [[]]
variacionesR k xs = [x:v | x <- xs, v <- variacionesR (k-1) xs]

-- ---------------------------------------------------------------------
-- Ejercicio 30. Definir la función
--    variacionesRN :: Integer -> Integer -> [[Integer]]    
-- tal que (variacionesRN n k) es la lista de las variaciones orden
-- k de los primeros n números naturales. Por ejemplo,
--    ghci> variacionesRN 3 2
--    [[1,1],[1,2],[1,3],[2,1],[2,2],[2,3],[3,1],[3,2],[3,3]]
--    ghci> variacionesRN 2 3
--    [[1,1,1],[1,1,2],[1,2,1],[1,2,2],[2,1,1],[2,1,2],[2,2,1],[2,2,2]]
-- ---------------------------------------------------------------------

-- carmengar
variacionesRN :: Integer -> Integer -> [[Integer]]    
variacionesRN n k = variacionesR k [1..n]

-- ---------------------------------------------------------------------
-- Ejercicio 31. Definir, usando variacionesR, la función
--    numeroVariacionesR :: Integer -> Integer -> Integer
-- tal que (numeroVariacionesR n k) es el número de variaciones con
-- repetición de orden k de un conjunto con n elementos. Por ejemplo,
--    numeroVariacionesR 3 2  ==  9
--    numeroVariacionesR 2 3  ==  8
-- ---------------------------------------------------------------------

-- carmengar
numeroVariacionesR :: Integer -> Integer -> Integer
numeroVariacionesR n k =  genericLength $ variacionesRN n k

-- ---------------------------------------------------------------------
-- Ejercicio 32. Definir, usando (^), la función
--    numeroVariacionesR' :: Integer -> Integer -> Integer
-- tal que (numeroVariacionesR' n k) es el número de variaciones con
-- repetición de orden k de un conjunto con n elementos. Por ejemplo,
--    numeroVariacionesR' 3 2  ==  9
--    numeroVariacionesR' 2 3  ==  8
-- ---------------------------------------------------------------------

-- carmengar
numeroVariacionesR' :: Integer -> Integer -> Integer
numeroVariacionesR' n k = k^n

-- ---------------------------------------------------------------------
-- Ejercicio 33. Definir la función
--    prop_numeroVariacionesR :: Integer -> Bool
-- tal que (prop_numeroVariacionesR n) se verifica si las funciones
-- numeroVariacionesR y numeroVariacionesR' son equivalentes para
-- los n primeros números y todo k entre 1 y n. Por ejemplo,
--    prop_numeroVariacionesR 5  ==  True
-- ---------------------------------------------------------------------

-- carmengar
prop_numeroVariacionesR :: Integer -> Bool
prop_numeroVariacionesR n = map nvr [1..n] == map nvr' [1..n]
    where nvr  = numeroVariacionesR n
          nvr' = numeroVariacionesR' n
