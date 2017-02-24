-- I1M 2016-17: Rel_20.hs (17 de febrero de 2017).
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
-- Como referencia se puede usar los apuntes de http://bit.ly/2lTFJw4


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

-- eliguivil albcercid roscargar antmorper3 paumacpar eledejim2 joscasgom1
-- marjimcom pabrabmon fatfervaz cescarde josdeher glovizcas enrnarbej migibagar
-- margarvil14 congomgom manruiber juaorture cargonler artmorfer antdursan
-- beagongon1 ignareeva marmerzaf carmarcar5 natmarmar2 fraferpoy monlagare
-- margarflo5 belbenzam felsuacor josrodgal7 margirmon antlopgom2 natruipin
-- antbeacar marlobrip criortcar
subconjunto :: Eq a => [a] -> [a] -> Bool
subconjunto [] _ = True
subconjunto (x:xs) ys = elem x ys && subconjunto xs ys
 
-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir, mediante all, la función 
--    subconjunto' :: Eq a => [a] -> [a] -> Bool
-- tal que (subconjunto' xs ys) se verifica si xs es un subconjunto de
-- ys. Por ejemplo,
--    subconjunto' [1,3,2,3] [1,2,3]  ==  True
--    subconjunto' [1,3,4,3] [1,2,3]  ==  False
-- ---------------------------------------------------------------------
 
-- eliguivil albcercid roscargar antmorper3 paumacpar eledejim2 joscasgom1
-- marjimcom pabrabmon fatfervaz josdeher glovizcas enrnarbej margarvil14
-- congomgom manruiber artmorfer antdursan beagongon1 ignareeva carmarcar5 
-- fraferpoy monlagare migibagar margarflo5 belbenzam felsuacor josrodgal7
-- antlopgom2 natruipin antbeacar marlobrip criortcar natmarmar2 margirmon 
subconjunto' :: Eq a => [a] -> [a] -> Bool
subconjunto' xs ys = all (`elem`ys) xs

-- cescarde cargonler marmerzaf
subconjunto2' :: Eq a => [a] -> [a] -> Bool
subconjunto2' xs ys = and [elem x ys | x <- xs]

-- Comentario: No usa all.

-- juaorture
subconjunto3' :: Eq a => [a] -> [a] -> Bool
subconjunto3' xs ys = all (==True) [y `elem` xs | y <- ys]

-- Comentario: La definición anterior se puede simplificar.
  
-- ---------------------------------------------------------------------
-- Ejercicio 3. Comprobar con QuickCheck que las funciones subconjunto
-- y subconjunto' son equivalentes.
-- ---------------------------------------------------------------------
 
-- eliguivil albcercid roscargar antmorper3 paumacpar eledejim2 joscasgom1
-- marjimcom pabrabmon fatfervaz cescarde josdeher glovizcas enrnarbej
-- congomgom manruiber juaorture cargonler artmorfer antdursan beagongon1
-- ignareeva marmerzaf carmarcar5 natmarmar2 fraferpoy monlagare migibagar
-- margarflo5 belbenzam felsuacor josrodgal7 margirmon antlopgom2 natruipin
-- antbeacar marlobrip criortcar margarvil14

-- La propiedad es
prop_equivalencia :: [Int] -> [Int] -> Bool
prop_equivalencia xs ys = subconjunto xs ys == subconjunto' xs ys
 
-- La comprobación es
--    λ> quickCheck prop_equivalencia
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función 
--    igualConjunto :: Eq a => [a] -> [a] -> Bool
-- tal que (igualConjunto xs ys) se verifica si las listas xs e ys,
-- vistas como conjuntos, son iguales. Por ejemplo,
--    igualConjunto [1..10] [10,9..1]   ==  True
--    igualConjunto [1..10] [11,10..1]  ==  False
-- ---------------------------------------------------------------------
 
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

-- albcercid antmorper3 eledejim2 joscasgom1 pabrabmon fatfervaz enrnarbej
-- beagongon1 juaorture
subconjuntos [] = [[]]
subconjuntos (x:xs) = [x: ys | ys <- p ] ++ p
  where p = subconjuntos xs
 
-- eliguivil roscargar paumacpar marjimcom josdeher congomgom cargonler
-- marmerzaf carmarcar5 natmarmar2 migibagar belbenzam margirmon
-- antlopgom2 criortcar antdursan 
subconjuntos2 :: [a] -> [[a]]
subconjuntos2 []     = [[]]
subconjuntos2 (x:xs) = [x:ys | ys <- subconjuntos2 xs] ++ subconjuntos2 xs

-- albcercid cescarde glovizcas enrnarbej margarvil14 manruiber artmorfer
-- fraferpoy monlagare margarflo5 felsuacor josrodgal7 natruipin
-- antbeacar marlobrip ignareeva  
subconjuntos3 :: [a] -> [[a]]
subconjuntos3 = subsequences

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

-- eliguivil albcercid antmorper3 paumacpar joscasgom1 marjimcom josdeher 
-- enrnarbej margarvil14 fatfervaz congomgom manruiber cargonler antdursan
-- beagongon1 ignareeva natmarmar2 fraferpoy monlagare margarflo5
-- belbenzam josrodgal7 margirmon artmorfer antbeacar marlobrip 
intercala :: a -> [a] -> [[a]]
intercala x []     = [[x]] 
intercala x (y:ys) = (x:y:ys) : map (y:) (intercala x ys)

-- roscargar 
intercala2 :: a -> [a] -> [[a]]
intercala2 y [] = [[y]]
intercala2 y xs = aux y xs (length xs)
  where  aux y [] _ = [[y]]
         aux y xs 0 = [xs ++ [y]]
         aux y xs n = [take n xs ++ [y] ++ drop n xs] ++ aux y xs (n-1)

-- Comentario: La definición intercala2 es incorrecta. Por ejemplo,
--    λ> intercala2 1 [0]
--    [[0,1],[0,1]]

-- eledejim2 migibagar natruipin
intercala3 :: a -> [a] -> [[a]]
intercala3 x [] = [[x]]
intercala3 x (y:ys) = (x:y:ys) : [y:zs | zs <- intercala3 x ys]

-- pabrabmon glovizcas juaorture marmerzaf carmarcar5 felsuacor antlopgom2
-- criortcar
intercala4 :: a -> [a] -> [[a]]
intercala4 x ys = [ take n ys ++ [x] ++ drop n ys | n <- [0..length ys]]

-- cescarde
intercala5 :: a -> [a] -> [[a]]
intercala5 x xs = [intercala' x a | a <- zip (inits xs) (tails xs)]
  where intercala' x (xs,ys) = xs ++ [x] ++ ys

prop_equiv_intercala :: Int -> [Int] -> Bool
prop_equiv_intercala x xs =
  all (== sort (intercala x xs)) [sort (f x xs) | f <- [ -- intercala2 
                                                         intercala3
                                                       , intercala4
                                                       , intercala5
                                                       ]]
  
-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir la función 
--    permutaciones :: [a] -> [[a]]  
-- tal que (permutaciones xs) es la lista de las permutaciones de la
-- lista xs. Por ejemplo,
--    permutaciones "bc"   ==  ["bc","cb"]
--    permutaciones "abc"  ==  ["abc","bac","bca","acb","cab","cba"]
-- ---------------------------------------------------------------------
 
-- juaorture carmarcar5
permutaciones :: [a] -> [[a]]
permutaciones []     = [[]]
permutaciones (x:xs) = concatMap (intercala x) (permutaciones xs)

-- albcercid eliguivil roscargar antmorper3 paumacpar eledejim2 joscasgom1
-- pabrabmon marjimcom cescarde josdeher enrnarbej fatfervaz congomgom
-- manruiber cargonler antdursan artmorfer beagongon1 natmarmar2
-- migibagar belbenzam margirmon antlopgom2 antbeacar criortcar 
permutaciones2 :: [a] -> [[a]]
permutaciones2 [] = [[]]
permutaciones2 (x:xs) = concat [intercala x ys | ys <- permutaciones2 xs]

-- glovizcas margarvil14 ignareeva marmerzaf fraferpoy monlagare margarflo5
-- felsuacor josrodgal7 natruipin marlobrip
permutaciones3 :: [a] -> [[a]]
permutaciones3 = permutations

-- ---------------------------------------------------------------------
-- Ejercicio 8. Definir la función
--    permutacionesN :: Integer -> [[Integer]]
-- tal que (permutacionesN n) es la lista de las permutaciones de los n
-- primeros números. Por ejemplo,
--    ghci> permutacionesN 3
--    [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]
-- ---------------------------------------------------------------------  

-- eledejim2 josdeher glovizcas congomgom juaorture antdursan artmorfer
-- natmarmar2 carmarcar5 felsuacor margirmon antlopgom2 criortcar 
permutacionesN :: Integer -> [[Integer]]
permutacionesN n = permutaciones [1..n]

-- eliguivil albcercid roscargar antmorper3 paumacpar joscasgom1 pabrabmon
-- marjimcom cescarde enrnarbej margarvil14 fatfervaz manruiber cargonler 
-- beagongon1 ignareeva marmerzaf fraferpoy monlagare migibagar
-- margarflo5 belbenzam josrodgal7 natruipin antbeacar marlobrip 
permutacionesN2 :: Integer -> [[Integer]]
permutacionesN2 n = permutations [1..n]

-- ---------------------------------------------------------------------
-- Ejercicio 9. Definir, usando permutacionesN, la función
--    numeroPermutacionesN :: Integer -> Integer
-- tal que (numeroPermutacionesN n) es el número de permutaciones de un
-- conjunto con n elementos. Por ejemplo,
--    numeroPermutacionesN 3  ==  6
--    numeroPermutacionesN 4  ==  24
-- ---------------------------------------------------------------------

-- albcercid roscargar antmorper3 paumacpar joscasgom1 pabrabmon migibagar
-- marjimcom cescarde josdeher glovizcas enrnarbej fatfervaz congomgom
-- manruiber margarflo5 juaorture cargonler antdursan artmorfer
-- beagongon1 ignareeva marmerzaf natmarmar2 fraferpoy belbenzam
-- felsuacor josrodgal7 margirmon antbeacar criortcar 
numeroPermutacionesN :: Integer -> Integer 
numeroPermutacionesN = genericLength . permutacionesN

-- eliguivil
numeroPermutacionesN2 :: Integer -> Integer
numeroPermutacionesN2 = fromIntegral . length . permutacionesN

-- Comentario de josdeher: (fromIntegral . length) se puede sustituir por 
-- la función ya definida: genericLength. Luego se puede simplificar.

-- eledejim2 margarvil14 carmarcar5 monlagare natruipin antlopgom2 marlobrip
numeroPermutacionesN3 :: Integer -> Integer
numeroPermutacionesN3 n = genericLength (permutacionesN n)

-- ---------------------------------------------------------------------
-- Ejercicio 10. Definir la función
--    fact :: Integer -> Integer
-- tal que (fact n) es el factorial de n. Por ejemplo,
--    fact 3  ==  6
-- ---------------------------------------------------------------------

-- eliguivil antmorper3 eledejim2 pabrabmon marjimcom cescarde josdeher
-- enrnarbej fatfervaz congomgom manruiber juaorture cargonler artmorfer
-- beagongon1 ignareeva natmarmar2 fraferpoy monlagare migibagar
-- margarflo5 felsuacor natruipin antlopgom2 margirmon marlobrip
-- criortcar 
fact :: Integer -> Integer
fact n = product [1..n]

-- albcercid roscargar paumacpar joscasgom1 glovizcas margarvil14
-- antdursan ignareeva marmerzaf carmarcar5 belbenzam josrodgal7
-- antbeacar 
fact1 :: Integer -> Integer
fact1 0 = 1
fact1 n = n * fact1 (n-1)

-- ---------------------------------------------------------------------
-- Ejercicio 11. Definir, usando fact, la función
--    numeroPermutacionesN' :: Integer -> Integer
-- tal que (numeroPermutacionesN' n) es el número de permutaciones de un
-- conjunto con n elementos. Por ejemplo,
--    numeroPermutacionesN' 3  ==  6
--    numeroPermutacionesN' 4  ==  24
-- ---------------------------------------------------------------------

-- eliguivil albcercid roscargar antmorper3 paumacpar eledejim2 joscasgom1 
-- pabrabmon marjimcom cescarde josdeher glovizcas enrnarbej margarvil14
-- fatfervaz congomgom manruiber juaorture cargonler antdursan artmorfer
-- beagongon1 ignareeva marmerzaf natmarmar2 fraferpoy monlagare migibagar
-- carmarcar5 margarflo5 belbenzam felsuacor josrodgal7 margirmon natruipin
-- antbeacar antlopgom2 marlobrip criortcar
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

-- eliguivil albcercid roscargar antmorper3 paumacpar eledejim2 joscasgom1
-- pabrabmon marjimcom cescarde josdeher glovizcas enrnarbej margarvil14
-- fatfervaz congomgom manruiber juaorture cargonler antdursan artmorfer
-- beagongon1 ignareeva marmerzaf natmarmar2 fraferpoy monlagare migibagar
-- carmarcar5 margarflo5 belbenzam felsuacor josrodgal7 margirmon natruipin
-- antbeacar antlopgom2 marlobrip criortcar
prop_numeroPermutacionesN :: Integer -> Bool
prop_numeroPermutacionesN n = 
   and [numeroPermutacionesN k == numeroPermutacionesN' k | k <- [1..n]]

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
 
-- eledejim2 pabrabmon marjimcom monlagare carmarcar5 natruipin antlopgom2
combinaciones3 :: Integer -> [a] -> [[a]]
combinaciones3 0 _  = [[]]
combinaciones _ [] = []
combinaciones k (x:xs) =
  [x:ys | ys <- combinaciones3 (k-1) xs] ++ combinaciones k xs

-- cescarde josdeher glovizcas enrnarbej margarvil14 fatfervaz congomgom
-- manruiber cargonler antdursan artmorfer marmerzaf natmarmar2
-- fraferpoy carmarcar5 margarflo5 felsuacor antbeacar marlobrip
-- criortcar 
combinaciones2 :: Integer -> [a] -> [[a]]
combinaciones2 n xs =
  [ys | ys <- subconjuntos xs, genericLength ys == n]

-- eliguivil
combinaciones3 :: Integer -> [a] -> [[a]]
combinaciones3 0 _  = [[]]
combinaciones3 n xs = [y:ys | y:xs' <- tails xs,
                              ys <- combinaciones3 (n-1) xs']

-- fuente https://wiki.haskell.org/99_questions/Solutions/26

-- albcercid antmorper3 joscasgom1 paumacpar margirmon
combinaciones4 :: Integer -> [a] -> [[a]]
combinaciones4 0 xs = [[]]
combinaciones4 n p@(x:xs) | n == genericLength p = [p]
                          | otherwise = map (x:) q ++ t
  where q = combinaciones4 (n-1) xs
        t = combinaciones4 n xs

-- juaorture
combinaciones5 :: Integer -> [a] -> [[a]]
combinaciones5 _ []     = [[]]
combinaciones5 0 _      = [[]]
combinaciones5 n (x:xs) = [cs | cs <- zs
                              , genericLength cs == n]
  where zs = [ x:ys | ys <- combinaciones5 (n-1) xs ] ++ combinaciones5 n xs

-- ignareeva belbenzam
combinaciones6 :: Integer -> [a] -> [[a]]
combinaciones6 n xs = take x zs
  where zs = [z | z <- ys, genericLength z == n]
        ys = subsequences xs
        x = length ys

prop_equiv_combinaciones :: Bool
prop_equiv_combinaciones =
  all (== combinaciones 3 "abcd")
      [ combinaciones2 3 "abcd"
      , combinaciones3 3 "abcd"
      , combinaciones4 3 "abcd"
      , combinaciones5 3 "abcd"
      , combinaciones6 3 "abcd"
      ]      

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

-- albcercid roscargar antmorper3 eledejim2 pabrabmon marjimcom
-- eliguivil joscasgom1 cescarde josdeher glovizcas enrnarbej margarvil14
-- paumacpar fatfervaz congomgom manruiber juaorture cargonler antdursan
-- artmorfer beagongon1 ignareeva marmerzaf natmarmar2 fraferpoy
-- monlagare carmarcar5 margarflo5 belbenzam felsuacor margirmon
-- natruipin antbeacar antlopgom2 marlobrip criortcar 
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

-- albcercid roscargar antmorper3 eledejim2 joscasgom1 pabrabmon
-- marjimcom cescarde josdeher glovizcas enrnarbej margarvil14
-- paumacpar fatfervaz congomgom manruiber juaorture cargonler antdursan
-- artmorfer beagongon1 ignareeva marmerzaf natmarmar2 fraferpoy
-- monlagare carmarcar5 margarflo5 belbenzam felsuacor natruipin
-- antbeacar antlopgom2 marlobrip criortcar 
numeroCombinaciones :: Integer -> Integer -> Integer
numeroCombinaciones n k = genericLength (combinacionesN n k)

-- eliguivil
numeroCombinaciones2 :: Integer -> Integer -> Integer
numeroCombinaciones2 n k = fromIntegral $ length $ combinacionesN n k

-- Comentario (cescarde): (fromIntegral $ length) se puede sustituir por 
-- la función ya definida: genericLength. Luego se puede simplificar.

-- ---------------------------------------------------------------------
-- Ejercicio 16. Definir la función
--    comb :: Integer -> Integer -> Integer
-- tal que (comb n k) es el número combinatorio n sobre k; es decir, 
--    (comb n k) = n! / (k!(n-k)!).
-- Por ejemplo,
--    comb 4 2  ==  6
--    comb 4 3  ==  4
-- ---------------------------------------------------------------------

-- eliguivil albcercid roscargar antmorper3 eledejim2 joscasgom1 pabrabmon
-- marjimcom cescarde josdeher glovizcas enrnarbej margarvil14 paumacpar
-- fatfervaz congomgom manruiber juaorture cargonler antdursan artmorfer
-- beagongon1 marmerzaf natmarmar2 fraferpoy monlagare carmarcar5
-- margarflo5 belbenzam felsuacor margirmon natruipin antbeacar
-- antlopgom2 marlobrip criortcar 
comb :: Integer -> Integer -> Integer
comb n k = div (fact n) (fact k * fact (n-k))
 
-- ---------------------------------------------------------------------
-- Ejercicio 17. Definir, usando comb, la función
--    numeroCombinaciones' :: Integer -> Integer -> Integer
-- tal que (numeroCombinaciones' n k) es el número de combinaciones de
-- orden k de un conjunto con n elementos. Por ejemplo,
--    numeroCombinaciones' 4 2  ==  6
--    numeroCombinaciones' 4 3  ==  4
-- ---------------------------------------------------------------------

-- albcercid eliguivil roscargar antmorper3 eledejim2 joscasgom1 pabrabmon
-- marjimcom cescarde josdeher glovizcas enrnarbej margarvil14 paumacpar
-- fatfervaz congomgom manruiber juaorture cargonler antdursan artmorfer
-- beagongon1 marmerzaf natmarmar2 fraferpoy monlagare carmarcar5
-- margarflo5 belbenzam felsuacor margirmon natruipin antbeacar
-- antlopgom2 marlobrip criortcar 
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

-- eliguivil albcercid roscargar antmorper3 eledejim2 joscasgom1 pabrabmon
-- marjimcom cescarde josdeher glovizcas enrnarbej margarvil14 paumacpar 
-- fatfervaz congomgom manruiber juaorture cargonler antdursan artmorfer
-- beagongon1 marmerzaf natmarmar2 fraferpoy monlagare carmarcar5
-- margarflo5 belbenzam felsuacor margirmon natruipin antbeacar
-- antlopgom2 marlobrip criortcar 
prop_numeroCombinaciones :: Integer -> Bool
prop_numeroCombinaciones n = 
  and [numeroCombinaciones n k == numeroCombinaciones' n k
      | k <- [1..n]]

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

-- eliguivil

combinacionesR :: Integer -> [a] -> [[a]]
combinacionesR 0  _ = [[]]
combinacionesR _ [] = []
combinacionesR k xxs@(x:xs) = 
               map (x:) (combinacionesR (k-1) xxs) ++ combinacionesR k xs

-- fuente: https://rosettacode.org/wiki/Combinations_with_repetitions#Haskell

-- albcercid antmorper3 joscasgom1 enrnarbej paumacpar antdursan margirmon

combinacionesR2 :: Integer -> [a] -> [[a]]
combinacionesR2 n [x] = [replicate (fromIntegral n) x]
combinacionesR2 0 _ = [[]]
combinacionesR2 n p@(x:xs)  = map (x:) q ++ t
                       where q = combinacionesR2 (n-1) p
                             t = combinacionesR2 n xs

-- eledejim2 pabrabmon marjimcom josdeher glovizcas fatfervaz congomgom manruiber artmorfer
-- beagongon1 marmerzaf fraferpoy monlagare carmarcar5 margarflo5 margarvil14 felsuacor antlopgom2
-- marlobrip
 
combinacionesR3 :: Integer -> [a] -> [[a]]
combinacionesR3 0 _  = [[]]
combinacionesR3 _ [] = []
combinacionesR3 k (x:xs) = [x:ys|ys<- combinacionesR3 (k-1)(x:xs)] ++
                          combinacionesR3 k xs

-- cescarde carmarcar5 natruipin antbeacar

combinacionesR4 :: Ord a => Integer -> [a] -> [[a]]
combinacionesR4 k xs = 
   nub [sort ys | ys <- subconjuntos (repite k xs), genericLength ys == k]
               where repite 1 xs = xs
                     repite n xs = xs ++ (repite (n-1) xs)

-- juaorture
combinacionesR5 :: Integer -> [a] -> [[a]]
combinacionesR5 _ []       = [[]]
combinacionesR5 0 _        = [[]]
combinacionesR5 n a@(x:xs) = [ cs | cs <- ys
                               , genericLength cs == n]
               where ys = [ x:ys | ys <- combinacionesR5 (n-1) a] ++ combinacionesR5 n xs

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

-- albcercid roscargar antmorper3 eledejim2 pabrabmon eliguivil joscasgom1
-- marjimcom cescarde josdeher glovizcas enrnarbej paumacpar fatfervaz
-- congomgom manruiber juaorture cargonler antdursan artmorfer beagongon1
-- marmerzaf fraferpoy monlagare carmarcar5 margarflo5 belbenzam margarvil14
-- felsuacor margirmon natruipin antbeacar antlopgom2 marlobrip
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

-- eliguivil

numeroCombinacionesR :: Integer -> Integer -> Integer
numeroCombinacionesR n k = fromIntegral $ length $ combinacionesRN n k

-- Comentario (cescarde): Se puede simplificar por la misma razón que en
-- los ejercicios 9 y 15.

-- albcercid roscargar antmorper3 eledejim2 joscasgom1 pabrabmon marjimcom
-- cescarde josdeher glovizcas enrnarbej paumacpar fatfervaz congomgom manruiber
-- juaorture cargonler antdursan artmorfer beagongon1 marmerzaf fraferpoy monlagare
-- carmarcar5 margarflo5 belbenzam margarvil14 felsuacor margirmon natruipin
-- antbeacar antlopgom2 marlobrip

numeroCombinacionesR2 :: Integer -> Integer -> Integer
numeroCombinacionesR2 n k = genericLength (combinacionesRN n k)

-- ---------------------------------------------------------------------
-- Ejercicio 22. Definir, usando comb, la función
--    numeroCombinacionesR' :: Integer -> Integer -> Integer
-- tal que (numeroCombinacionesR' n k) es el número de combinaciones con
-- repetición de orden k de un conjunto con n elementos. Por ejemplo,
--    numeroCombinacionesR' 3 2  ==  6
--    numeroCombinacionesR' 2 3  ==  4
-- ---------------------------------------------------------------------

-- eliguivil albcercid roscargar antmorper3 eledejim2 joscasgom1 pabrabmon
-- marjimcom cescarde josdeher glovizcas enrnarbej paumacpar fatfervaz
-- congomgom manruiber juaorture cargonler antdursan artmorfer beagongon1
-- marmerzaf fraferpoy monlagare margarflo5 belbenzam margarvil14
-- felsuacor margirmon natruipin antbeacar antlopgom2 marlobrip
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

-- eliguivil albcercid roscargar antmorper3 eledejim2 joscasgom1 pabrabmon
-- marjimcom cescarde josdeher glovizcas enrnarbej paumacpar fatfervaz
-- congomgom manruiber juaorture cargonler antdursan artmorfer beagongon1
-- marmerzaf fraferpoy monlagare carmarcar5 margarflo5 belbenzam margarvil14
-- felsuacor margirmon natruipin antbeacar antlopgom2 marlobrip
prop_numeroCombinacionesR :: Integer -> Bool
prop_numeroCombinacionesR n = 
   and [(numeroCombinacionesR n k) == (numeroCombinacionesR' n k) |
    k <- [1..n]]

-- Comentario: los paréntesis se pueden eliminar

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
 
-- albcercid roscargar antmorper3 eledejim2 joscasgom1 pabrabmon josdeher
-- marjimcom glovizcas enrnarbej paumacpar fatfervaz congomgom manruiber cargonler antdursan artmorfer
-- beagongon1 marmerzaf fraferpoy monlagare carmarcar5 margarflo5 belbenzam
-- felsuacor natruipin antbeacar antlopgom2 marlobrip
variaciones :: Integer -> [a] -> [[a]]
variaciones k xs = concat [permutaciones ys | ys <- combinaciones k xs]

-- cescarde migibagar

variaciones1 :: Eq a => Integer -> [a] -> [[a]]
variaciones1 k xs = nub $ map (take $ fromInteger k) (permutations xs)

-- Comentario (migibagar): la función     take (fromIntegral k) xs     está definida en 
-- Data.List como     genericTake xs.

--margarvil14 margirmon
variaciones2 :: Integer -> [a] -> [[a]]
variaciones2 k xs = concat (map permutaciones (combinaciones k xs))
-- ---------------------------------------------------------------------
-- Ejercicio 25. Definir la función
--    variacionesN :: Integer -> Integer -> [[Integer]]
-- tal que (variacionesN n k) es la lista de las variaciones de orden k
-- de los n primeros números. Por ejemplo,
--    variacionesN 3 2  ==  [[1,2],[2,1],[1,3],[3,1],[2,3],[3,2]]
-- ---------------------------------------------------------------------  

-- albcercid roscargar antmorper3 eledejim2 pabrabmon eliguivil joscasgom1 
-- cescarde josdeher marjimcom glovizcas enrnarbej paumacpar fatfervaz
-- congomgom manruiber juaorture cargonler antdursan artmorfer beagongon1
-- marmerzaf fraferpoy monlagare carmarcar5 margarflo5 belbenzam margarvil14
-- felsuacor margirmon natruipin antbeacar antlopgom2 marlobrip
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

-- eliguivil

numeroVariaciones :: Integer -> Integer -> Integer
numeroVariaciones n k = fromIntegral $ length $ variacionesN n k

-- Comentario (cescarde): Lo mismo que en los ejercicios 9, 15 y 21

-- albcercid roscargar antmorper3 eledejim2 joscasgom1 pabrabmon 
-- cescarde josdeher marjimcom glovizcas enrnarbej paumacpar fatfervaz
-- congomgom manruiber juaorture cargonler antdursan artmorfer beagongon1
-- marmerzaf fraferpoy monlagare carmarcar5 margarflo5 belbenzam margarvil14
-- felsuacor margirmon natruipin antbeacar antlopgom2 marlobrip
numeroVariaciones2 :: Integer -> Integer -> Integer
numeroVariaciones2 n k = genericLength (variacionesN n k)

-- ---------------------------------------------------------------------
-- Ejercicio 27. Definir, usando product, la función
--    numeroVariaciones' :: Integer -> Integer -> Integer
-- tal que (numeroVariaciones' n k) es el número de variaciones de orden
-- k de un conjunto con n elementos. Por ejemplo,
--    numeroVariaciones' 4 2  ==  12
--    numeroVariaciones' 4 3  ==  24
-- ---------------------------------------------------------------------

-- eliguivil albcercid roscargar antmorper3 eledejim2 joscasgom1 glovizcas
-- paumacpar fatfervaz congomgom juaorture cargonler artmorfer marmerzaf
-- fraferpoy margirmon antlopgom2 marlobrip

numeroVariaciones' :: Integer -> Integer -> Integer
numeroVariaciones' n k = (fact n) `div` fact (n-k)

-- pabrabmon cescarde josdeher marjimcom enrnarbej manruiber antdursan beagongon1
-- monlagare margarflo5 belbenzam margarvil14 felsuacor natruipin antbeacar

numeroVariaciones2' :: Integer -> Integer -> Integer
numeroVariaciones2' n k = product [(n-k+1)..n]

-- ---------------------------------------------------------------------
-- Ejercicio 28. Definir la función
--    prop_numeroVariaciones :: Integer -> Bool
-- tal que (prop_numeroVariaciones n) se verifica si las funciones
-- numeroVariaciones y numeroVariaciones' son equivalentes para
-- los n primeros números y todo k entre 1 y n. Por ejemplo,
--    prop_numeroVariaciones 5  ==  True
-- ---------------------------------------------------------------------

-- eliguivil albcercid roscargar antmorper3 eledejim2 joscasgom1 pabrabmon
-- cescarde josdeher marjimcom glovizcas enrnarbej paumacpar fatfervaz
-- congomgom manruiber juaorture cargonler antdursan artmorfer beagongon1
-- marmerzaf fraferpoy monlagare carmarcar5 margarflo5 belbenzam margarvil14 margirmon
--natruipin antbeacar antlopgom2 marlobrip

prop_numeroVariaciones :: Integer -> Bool
prop_numeroVariaciones n = 
   and [(numeroVariaciones n k) == (numeroVariaciones' n k) | k <- [1..n]]

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

-- albcercid antmorper3 joscasgom1 pabrabmon josdeher marjimcom
-- glovizcas enrnarbej paumacpar fatfervaz congomgom manruiber
-- juaorture cargonler antdursan artmorfer beagongon1 marmerzaf
-- carmarcar5 margarflo5 belbenzam margirmon natruipin antbeacar
-- antlopgom2 marlobrip

variacionesR :: Integer -> [a] -> [[a]]
variacionesR 0 _ = [[]]
variacionesR n xs = [x:ys | x <- xs, ys <- variacionesR (n-1) xs]

--eledejim2 monlagare margarvil14

variacionesR2 :: Integer -> [a] -> [[a]]
variacionesR2 _ [] = []
variacionesR2 0 _ = [[]]
variacionesR2 k xs = [y:ys|y<-xs, ys<-variacionesR2 (k-1) xs]

-- cescarde

variacionesR3 :: Eq a => Integer -> [a] -> [[a]]
variacionesR3 k xs = variaciones1 k (repite k xs)
                   where repite 1 xs = xs
                         repite n xs = xs ++ (repite (n-1) xs)

-- albcercid

variacionesR4 n xs = sequenceA (replicate n xs)

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

-- eliguivil albcercid roscargar antmorper3 eledejim2 joscasgom1 pabrabmon
-- cescarde josdeher marjimcom glovizcas enrnarbej paumacpar fatfervaz 
-- congomgom manruiber juaorture cargonler antdursan artmorfer beagongon1
-- marmerzaf monlagare carmarcar5 margarflo5 belbenzam margarvil14 margirmon
--natruipin antbeacar antlopgom2 marlobrip
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

-- eliguivil 

numeroVariacionesR :: Integer -> Integer -> Integer
numeroVariacionesR n k = fromIntegral $ length $ variacionesRN n k

-- Comentario de josdeher: Se puede simplificar por la misma razón que 
-- los ejercicios 9, 15, 21 y 26.

-- albcercid roscargar antmorper3 eledejim2 joscasgom1 pabrabmon
-- cescarde josdeher marjimcom glovizcas enrnarbej paumacpar fatfervaz
-- congomgom manruiber juaorture cargonler antdursan artmorfer beagongon1
-- marmerzaf monlagare carmarcar5 margarflo5 belbenzam margarvil14 margirmon
--natruipin antbeacar antlopgom2 marlobrip

numeroVariacionesR2 :: Integer -> Integer -> Integer
numeroVariacionesR2 n k = genericLength (variacionesRN n k)

-- ---------------------------------------------------------------------
-- Ejercicio 32. Definir, usando (^), la función
--    numeroVariacionesR' :: Integer -> Integer -> Integer
-- tal que (numeroVariacionesR' n k) es el número de variaciones con
-- repetición de orden k de un conjunto con n elementos. Por ejemplo,
--    numeroVariacionesR' 3 2  ==  9
--    numeroVariacionesR' 2 3  ==  8
-- ---------------------------------------------------------------------

-- eliguivil roscargar albcercid antmorper3 eledejim2 joscasgom1 pabrabmon
-- cescarde josdeher marjimcom glovizcas enrnarbej paumacpar fatfervaz
-- congomgom manruiber juaorture cargonler antdursan artmorfer beagongon1
-- marmerzaf monlagare carmarcar5 margarflo5 belbenzam margarvil14 margirmon
--natruipin antbeacar antlopgom2 marlobrip

numeroVariacionesR' :: Integer -> Integer -> Integer
numeroVariacionesR' n k = n^k

-- ---------------------------------------------------------------------
-- Ejercicio 33. Definir la función
--    prop_numeroVariacionesR :: Integer -> Bool
-- tal que (prop_numeroVariacionesR n) se verifica si las funciones
-- numeroVariacionesR y numeroVariacionesR' son equivalentes para
-- los n primeros números y todo k entre 1 y n. Por ejemplo,
--    prop_numeroVariacionesR 5  ==  True
-- ---------------------------------------------------------------------

-- eliguivil roscargar albcercid antmorper3 eledejim2 joscasgom1 pabrabmon
-- cescarde josdeher marjimcom glovizcas enrnarbej paumacpar fatfervaz
-- congomgom manruiber juaorture cargonler antdursan artmorfer beagongon1
-- marmerzaf monlagare carmarcar5 margarflo5 belbenzam margarvil14 margirmon
--natruipin antbeacar antlopgom2 marlobrip
 
prop_numeroVariacionesR :: Integer -> Bool
prop_numeroVariacionesR n = 
  and [(numeroVariacionesR n k) == (numeroVariacionesR' n k) | k <- [1..n]]
