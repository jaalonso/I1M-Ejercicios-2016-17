-- I1M 2015-16: Rel_4.hs (7 de octubre de 2015)
-- Definiciones por comprensión con cadenas: El cifrado César.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla (Revisión del 21 de octubre)
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- En el tema 5, cuyas transparencias se encuentran en 
--    http://www.cs.us.es/~jalonso/cursos/i1m-15/temas/tema-5.html
-- se estudió, como aplicación de las definiciones por comprennsión, el
-- cifrado César. El objetivo de esta relación es modificar el programa
-- de cifrado César para que pueda utilizar también letras
-- mayúsculas. Por ejemplo,  
--    ghci> descifra "Ytit Ufwf Sfif"
--    "Todo Para Nada"
-- Para ello, se propone la modificación de las funciones correspondientes
-- del tema 5.

-- ---------------------------------------------------------------------
-- Importación de librerías auxiliares                                --
-- ---------------------------------------------------------------------

import Data.Char
import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    minuscula2int :: Char -> Int
-- tal que (minuscula2int c) es el entero correspondiente a la letra
-- minúscula c. Por ejemplo, 
--    minuscula2int 'a'  ==  0
--    minuscula2int 'd'  ==  3
--    minuscula2int 'z'  ==  25
-- ---------------------------------------------------------------------

-- guache carruirui3 juanarcon manpende manvermor alvalvdom1 manvazbar1
-- erisancha fracruzam josllagam juamorrom1 marvilmor lucgamgal
-- silgongal carmengar isrbelnun paocabper carboncar irecasmat fatvilpiz
minuscula2int :: Char -> Int
minuscula2int c = ord c - ord 'a'

-- blaruiher crimalrui abrdelrod rubvilval pabmorgar alebergon ivaruicam
-- migandben javperlag fatvilpiz
minuscula3int :: Char -> Int
minuscula3int c = ord c - 97 

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    mayuscula2int :: Char -> Int
-- tal que (mayuscula2int c) es el entero correspondiente a la letra
-- mayúscula c. Por ejemplo, 
--    mayuscula2int 'A'  ==  0
--    mayuscula2int 'D'  ==  3
--    mayuscula2int 'Z'  ==  25
-- ---------------------------------------------------------------------

-- guache carruirui3 blatuiher crimalrui paocabper juanarcon manpende
-- manvermor alvalvdom1 manvazbar1 erisancha fracruzam josllagam
-- juamorrom1 lucgamgal marvilmor silgongal carmengar alebergon
-- isrbelnun carboncar irecasmat
mayuscula2int :: Char -> Int
mayuscula2int c = ord c - ord 'A'

-- abrdelrod rubvilval pabmorgar ivaruiccam migandben javperlag
mayuscula2int2 :: Char -> Int
mayuscula2int2 c = ord c - 65

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--    int2minuscula :: Int -> Char
-- tal que (int2minuscula n) es la letra minúscula correspondiente al
-- entero n. Por ejemplo, 
--    int2minuscula 0   ==  'a'
--    int2minuscula 3   ==  'd'
--    int2minuscula 25  ==  'z'
-- ---------------------------------------------------------------------

-- guache abrdelrod blaruiher crimalrui rubvilval juamorrom1 pabmorgar
-- alebergon ivaruicam migandben javperlag
int2minuscula :: Int -> Char
int2minuscula n = chr(n+97)
 
-- carruirui3 juanarcon manpende manvermor alvalvdom1 manvazbar1 erisancha
-- fracruzam josllagam marvilmor lucgamgal silgongal carmengar isrbelnun
-- paocabper carboncar irecasmat fatvilpiz
int2minuscula2 :: Int -> Char
int2minuscula2 n = chr(n + ord 'a')

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función
--    int2mayuscula :: Int -> Char
-- tal que (int2mayuscula n) es la letra minúscula correspondiente al
-- entero n. Por ejemplo, 
--    int2mayuscula 0   ==  'A'
--    int2mayuscula 3   ==  'D'
--    int2mayuscula 25  ==  'Z'
-- ---------------------------------------------------------------------

-- guache abrdelrod rubvilval juamorrom1 pabmorgar ivaruicam migandben 
-- javperlag
int2mayuscula :: Int -> Char
int2mayuscula n = chr(n+65)

-- carruirui3 blaruiher crimalrui juanarcon manpende manvermor alvalvdom1 
-- manvazbar1 erisancha fracruzam josllagam marvilmor lucgamgal silgongal
-- carmengar alebergon isrbelnun paocabper carboncar irecasmat fatvilpiz
int2mayuscula2 :: Int -> Char
int2mayuscula2 n = chr(n + ord 'A')

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la función
--    desplaza :: Int -> Char -> Char
-- tal que (desplaza n c) es el carácter obtenido desplazando n
-- caracteres el carácter c. Por ejemplo, 
--    desplaza   3  'a'  ==  'd'
--    desplaza   3  'y'  ==  'b'
--    desplaza (-3) 'd'  ==  'a'
--    desplaza (-3) 'b'  ==  'y'
--    desplaza   3  'A'  ==  'D'
--    desplaza   3  'Y'  ==  'B'
--    desplaza (-3) 'D'  ==  'A'
--    desplaza (-3) 'B'  ==  'Y'
-- ---------------------------------------------------------------------

-- guache juanarcon manpende manvermor alvalvdom1 erisancha manvazbar1
-- alebergon fracruzam rubvilval josllagam juamorrom1 marvilmorm
-- lucgamgal silgongal isrbelnun paocabper ivaruicam migandben javperlag
-- irecasmat 
desplaza :: Int -> Char -> Char
desplaza n c 
    |elem c ['a'..'z'] = int2minuscula (mod (minuscula2int c + n) 26)
    |elem c ['A'..'Z'] = int2mayuscula (mod (mayuscula2int c + n) 26)
    |otherwise         = c

-- carruirui3 blaruiher crimalrui pabmorgar carmengar carboncar
desplaza2 :: Int -> Char -> Char
desplaza2 n c 
    |c `elem` ['a'..'z'] = int2minuscula (n + minuscula2int c `mod` 26)
    |c `elem` ['A'..'Z'] = int2mayuscula (n + mayuscula2int c `mod` 26)
    |otherwise           = c

-- abrdelrod
desplaza3 :: Int -> Char -> Char
desplaza3 n c | c `elem` ['a'..'z'] = chr ((ord c-97+n) `mod` 26+97) 
              | c `elem` ['A'..'Z'] = chr ((ord c-65+n) `mod` 26+65)
              | otherwise = c

-- ---------------------------------------------------------------------
-- Ejercicio 6.1. Definir la función
--    codifica :: Int -> String -> String
-- tal que (codifica n xs) es el resultado de codificar el texto xs con
-- un desplazamiento n. Por ejemplo, 
--    ghci> codifica   3  "En Todo La Medida" 
--    "Hq Wrgr Od Phglgd"
--    ghci> codifica (-3) "Hq Wrgr Od Phglgd"
--    "En Todo La Medida"
-- ---------------------------------------------------------------------

-- guache carruirui3 juanarcon manpende manvermor alvalvdom1 abrdelrod
-- erisancha manvazbar1 rubvilval juamorrom1 pabmorgar marvilmor lucgamgal
-- blaruiher silgongal carmengar alebergon isrbelnun carboncar paocabper
-- ivaruicam migandben javperlag irecasmat fatvilpiz
codifica :: Int -> String -> String
codifica n xs = [desplaza n x | x <- xs]

-- fracruzam josllagam
codifica2 :: Int -> String -> String
codifica2 n xs = map (desplaza n) xs

-- ---------------------------------------------------------------------
-- Ejercicio 6.2. Comprobar con QuickCheck que para cualquier entero n y
-- cualquier cadena cs se tiene que (codifica (-n) (codifica n cs)) es
-- igual a cs.
-- ---------------------------------------------------------------------
 
-- guache carruirui3 juanarcon manpende manvermor alvalvdom1 abrdelrod
-- erisancha manvazbar1 fracruzam rubvilval josllagam juamorrom1
-- lucgamgal pabmorgar blaruiher silgongal carmengar alebergon isrbelnun
-- carboncar paocabper ivaruicam migandben javperlag irecasmat

-- La propiedad es
prop_codifica :: Int -> String -> Bool
prop_codifica n cs = codifica (-n) (codifica n cs) == cs

-- La comprobación es
--   *Main> quickCheck prop_codifica
--   +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir la función
--    tabla :: [Float]
-- tal que tabla es la lista de la frecuencias de las letras en
-- castellano, Por ejemplo, la frecuencia de la 'a' es del 12.53%, la de
-- la 'b' es 1.42%. 
-- ---------------------------------------------------------------------

-- carruirui3 juanarcon manpende manvermor alvalvdom1 abrdelrod
-- erisancha manvazbar1 fracruzam rubvilval juamorrom1 pabmorgar
-- lucgamgal josllagam blaruiher silgongal carmengar alebergon isrbelnun
-- paocabper carboncar javperlag fatvilpiz
tabla :: [Float]
tabla = [12.53, 1.42, 4.68, 5.86, 13.68, 0.69, 1.01, 
          0.70, 6.25, 0.44, 0.01,  4.97, 3.15, 6.71, 
          8.68, 2.51, 0.88, 6.87,  7.98, 4.63, 3.93, 
          0.90, 0.02, 0.22, 0.90,  0.52]

-- ---------------------------------------------------------------------
-- Ejercicio 8. Definir la función
--    porcentaje :: Int -> Int -> Float
-- tal que (porcentaje n m) es el porcentaje de n sobre m. Por ejemplo,
--    porcentaje 2 5  ==  40.0  
-- ---------------------------------------------------------------------

-- carruirui3 manvermor alvalvdom1 juamorrom1 josllagam marvilmor
-- lucgamgal blaruiher silgongal alebergon carboncar paocabper migandben
-- irecasmat 
porcentaje :: Int -> Int -> Float
porcentaje n m = 100 * (fromIntegral n) / (fromIntegral m)

-- juanarcon manpende abrdelrod erisancha fracruzam rubvilval pabmorgar
-- carmengar isrbelnun ivaruicam javperlag
porcentaje2 :: Int -> Int -> Float
porcentaje2 n m = fromIntegral n / fromIntegral m * 100

-- ---------------------------------------------------------------------
-- Ejercicio 9. Definir la función
--    letras :: String -> String
-- tal que (letras xs) es la cadena formada por las letras de la cadena
-- xs. Por ejemplo,  
--    letras "Esto Es Una Prueba"        ==  "EstoEsUnaPrueba"
--    letras "son las 8:45 de la noche"  ==  "sonlasdelanoche"
-- ---------------------------------------------------------------------

-- carruirui3 manvermor pabmorgar silgongal carmengar juamorrom1
-- alebergon carboncar paocabper ivaruicammigandben javperlag fatvilpiz
letras :: String -> String
letras xs = [x | x <- xs, x `elem` ['A'..'Z'] || x `elem` ['a'..'z']]

-- guache erisancha marvilmor
letras2 :: String -> String
letras2 xs = [x | x <- xs, elem x (ys++zs)]
    where ys = ['a'..'z']
          zs = ['A'..'Z']

-- manpende manvazbar1 rubvilval juanarcon josllagam blaruiher isrbelnun
-- irecasmat 
letras3 :: String -> String
letras3 xs = [x | x <- xs, elem x (['a'..'z']++['A'..'Z'])]

-- abrdelrod lucgamgal
letras4 :: String -> String
letras4 xs = [x | x <- xs, x /= desplaza 1 x]

-- alvalvdom1
letras5 :: String -> String
letras5 xs = [x | x <- xs, isAlpha x]

-- ---------------------------------------------------------------------
-- Ejercicio 10.1. Definir la función
--    ocurrencias :: Eq a => a -> [a] -> Int
-- tal que (ocurrencias x xs) es el número de veces que ocurre el
-- elemento x en la lista xs. Por ejemplo, 
--    ocurrencias 'a' "Salamanca"  ==  4  
-- ---------------------------------------------------------------------

-- carruirui3 guache alvalvdom1 pabmorgar silgongal alebergon
ocurrencias :: Eq a => a -> [a] -> Int
ocurrencias x xs = sum [1 | a <- xs, x == a]

-- guache manpende manvermor abrdelrod erisancha manvazbar1 fracruzam
-- juanarcon rubvilval josllagam marvilmor lucgamgal blaruiher carmengar 
-- juamorrom1 paocabper carbonar ivaruicam migandben isrbelnun javperlag
-- irecasmat 
ocurrencias2 :: Eq a => a -> [a] -> Int
ocurrencias2 x xs = length [y | y <- xs, x==y]

-- ---------------------------------------------------------------------
-- Ejercicio 10.2. Comprobar con QuickCheck si el número de ocurrencias
-- de un elemento x en una lista xs es igual que en su inversa.
-- ---------------------------------------------------------------------

-- guache manpende manvermor alvalvdom1 abrdelrod erisancha manvazbar1
-- fracruzam rubvilval pabmorgar juanarcon lucgamgal silgongal blaruiher
-- carmengar juamorrom1 josllagam alebergon paocabper carboncar ivaruicam
-- migandben isrbelnun javperlag irecasmat

-- La propiedad es 
prop_ocurrencia_inv :: Int -> [Int] -> Bool
prop_ocurrencia_inv x xs = ocurrencias x xs == ocurrencias x (reverse xs)

-- La comprobación es
--    *Main> quickCheck prop_ocurrencia_inv
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 10.3. Comprobar con QuickCheck si el número de ocurrencias
-- de un elemento x en la concatenación de las listas xs e ys es igual a
-- la suma del número de ocurrencias de x en xs y en ys.
-- ---------------------------------------------------------------------
 
-- guache manpende manvermor alvalvdom1 abrdelrod erisancha manvazbar1
-- fracruzam rubvilval pabmorgar juanarcon lucgamgal silgongal blaruiher
-- carmengar juamorrom1 josllagam alebergon paocabper carboncar ivaruicam
 -- migandben isrbelnun javperlag irecasmat

-- La propiedad es
prop_ocurrencia_conc :: Int -> [Int] -> [Int] -> Bool
prop_ocurrencia_conc x xs ys = 
    ocurrencias x (xs ++ ys) == ocurrencias x xs + ocurrencias x ys

-- La comprobación es
--    *Main> quickCheck prop_ocurrencia_conc
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 12. Definir la función
--    frecuencias :: String -> [Float]
-- tal que (frecuencias xs) es la frecuencia de cada una de las letras
-- de la cadena xs. Por ejemplo, 
--    ghci> frecuencias "En Todo La Medida"
--    [14.3,0,0,21.4,14.3,0,0,0,7.1,0,0,7.1,
--     7.1,7.1,14.3,0,0,0,0,7.1,0,0,0,0,0,0]
-- Nota: Se puede usar la función toLower (ver http://bit.ly/1vSxhhd )
-- ---------------------------------------------------------------------

-- guache carruirui3 manvermor alvalvdom1 abrdelrod erisancha fracruzam
-- rubvilval pabmorgar juanarcon lucgamgal silgongal blaruiher carmengar
-- juamorrom1 josllagam alebergon carboncar ivaruicam paocabper irecasmat

frecuencias :: String -> [Float]
frecuencias xs = [porcentaje (ocurrencias x ys) n | x <- ['a'..'z']]
    where ys = [toLower x | x <- xs]
          n  = length(letras xs)

-- javperlag 
frecuencias2 :: String -> [Float]
frecuencias2 xs =
    [porcentaje (ocurrencias c xs + ocurrencias (toLower c) xs)
                (length (letras xs))
     | c <- ['A'..'Z']]

-- Comentario: La definición anterior se puede mejorar.

-- ------------------------------------------------------
-- Ejercicio 13.1. Definir la función
--    chiCuad :: [Float] -> [Float] -> Float
-- tal que (chiCuad os es) es la medida chi cuadrado de las
-- distribuciones os y es. Por ejemplo, 
--    chiCuad [3,5,6] [3,5,6]  ==  0.0
--    chiCuad [3,5,6] [5,6,3]  ==  3.9666667
-- ---------------------------------------------------------------------

-- carruirui3 manvermor alvalvdom1 manpende erisancha manvazbar1
-- fracruzam rubvilval pabmorgar juanarcon marvilmor lucgamgal silgongal
-- blaruiher carmengar juamorrom1 josllagam alebergon carboncar
-- paocabper migandben isrbelnun javperlag
chiCuad :: [Float] -> [Float] -> Float
chiCuad os es = sum [((o-e)^2)/e | (o,e) <- zip os es]

-- ---------------------------------------------------------------------
-- Ejercicio 13.2, Comprobar con QuickCheck que para cualquier par de
-- listas xs e ys se verifica que (chiCuad xs ys) es 0 syss xs e ys son
-- iguales. 
-- ---------------------------------------------------------------------

-- carruirui3 guache manvermor abrdelrod alvalvdom1 manpende erisancha
-- manvazbar1 rubvilval fracruzam pabmorgar juanarcon lucgamgal silgongal
-- blaruiher carmengar juamorrom1 josllagam alebergon ivaruicam
-- migandben paocabper isrbelnun javperlag

-- La propiedad es
prop_chiCuad_1 :: [Float] -> [Float] -> Bool
prop_chiCuad_1 xs ys = (chiCuad xs ys == 0) == (xs == ys)

-- La comprobación es
--    *Main> quickCheck prop_chiCuad_1
--    *** Failed! Falsifiable (after 2 tests and 2 shrinks): 
--    [0.0]
--    []

-- ---------------------------------------------------------------------
-- Ejercicio 13.3. A la vista de los contraejemplos del apartado
-- anterior, qué condición hay que añadir para que se verifique la
-- propiedad.
-- ---------------------------------------------------------------------

-- guache manvermor alvalvdom1 manpende erisancha manvazbar1 fracruzam 
-- rubvilval pabmorgar juanarcon lucgamgal silgongal blaruiher carmengar
-- alebergon migandben paocabper isrbelnun javperlag irecasmat

-- La propiedad es
prop_chiCuad_2 :: [Float] -> [Float] -> Property
prop_chiCuad_2 xs ys = 
    xs /= [] && ys /= [] ==> (chiCuad xs ys == 0) == (xs == ys)

-- La comprobación es
--    *Main> quickCheck prop_chiCuad_2
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 13.3. A la vista del apartado anterior, el número de tests
-- que ha pasado puede ser menor que 100. Reescribir la propiedad de
-- forma que se verifique en los 100 tests.
-- ---------------------------------------------------------------------

-- carruirui3 guache manvermor manpende erisancha fracruzam rubvilval
-- pabmorgar juanarcon  paocabper lucgamgal silgongal blaruiher
-- carmengar juamorrom1 alebergon isrbelnun javperlag

-- La propiedad es
prop_chiCuad :: [Float] -> [Float] -> Bool
prop_chiCuad xs ys = 
    (chiCuad xs ys == 0) == (xs == ys) || xs == [] || ys == []

-- La comprobación es
--    ghci> quickCheck prop_chiCuad
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 14.1. Definir la función
--    rota :: Int -> [a] -> [a]
-- tal que (rota n xs) es la lista obtenida rotando n posiciones los
-- elementos de la lista xs. Por ejemplo, 
--    rota  2 "manolo"              ==  "noloma"  
--    rota 10 "manolo"              ==  "lomano"
--    [rota n "abc" | n <- [0..5]]  ==  ["abc","bca","cab","abc","bca","cab"]
-- ---------------------------------------------------------------------

-- guache alvalvdom1 carruirui3 manpende erisancha manvazbar1 fracruzam
-- rubvilval pabmorgar juanarcon marvilmor silgongal blaruiher carmengar
-- juamorrom1 josllagam alebergon ivaruicam javperlag irecasmat
rota :: Int -> [a] -> [a]
rota n [] = []
rota n xs = drop m xs ++ take m xs
    where m = mod n (length xs)

-- isrbelnun
rota2 :: Int -> [a] -> [a]
rota2 n xs = drop (mod n (length xs)) xs ++ take (mod n (length xs)) xs

-- Comentario: La definición anterior se puede mejorar.

-- ---------------------------------------------------------------------
-- Ejercicio 14.2. Comprobar con QuickCkeck si para cualquier lista xs
-- si se rota n veces y el resultado se rota m veces se obtiene lo mismo
-- que rotando xs (n+m) veces, donde n y m son números no nulos.
-- ---------------------------------------------------------------------

-- guache manvermor abrdelrod alvalvdom1 carruirui3 manpende erisancha
-- manvazbar1 rubvilval fracruzam pabmogar juanarcon lucgamgal silgongal
-- blaruiher juamorrom1 alebergon ivaruicam paocabper isrbelnun
-- javperlag irecasmat 

-- La propiedad es
prop_rota :: Int -> Int -> [Int] -> Property
prop_rota n m xs =  
    n /= 0 && m /= 0 ==> rota m (rota n xs) == rota (n+m) xs

-- La comprobación es
--    *Main> quickCheck prop_rota
--    *** Failed! Falsifiable (after 79 tests and 52 shrinks): 
--    220674372
--    1926809277
--    [0,0,1]

-- Comentario: El error se debe a que el número de rotaciones del ejemplo 
-- (220674372 + 1926809277) es un entero demasiado grande. Acotando los
-- números de las rotaciones se puede verificar la propiedad.

-- isrbelnun

-- La propiedad es
prop_rota2 :: Int -> Int -> [Int] -> Property
prop_rota2 n m xs = 
    n /= 0 && m /= 0 && xs /= [] ==> rota m (rota n xs) == rota (n+m) xs

-- Comentario: La definición anterior se puede mejorar.

-- La comprobacion es
-- +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 15.1. Definir la función
--    descifra :: String -> String
-- tal que (descifra xs) es la cadena obtenida descodificando la cadena
-- xs por el anti-desplazamiento que produce una distribución de letras
-- con la menor deviación chi cuadrado respecto de la tabla de
-- distribución de las letras en castellano. Por ejemplo, 
--    ghci> codifica 5 "Todo Para Nada"
--    "Ytit Ufwf Sfif"
--    ghci> descifra "Ytit Ufwf Sfif"
--    "Todo Para Nada"
-- ---------------------------------------------------------------------

-- manvermor alvalvdom1 fracruzam rubvilval silgongal juamorrom1 paocabper
descifra :: String -> String
descifra xs =  codifica (-factor) xs
    where factor = head (posiciones (minimum tabChi) tabChi)
          tabChi = [chiCuad (rota n tabla') tabla | n <- [0..25]]
          tabla' = frecuencias xs
  
posiciones x xs = [i | (x',i) <- zip xs [0..n], x == x']
    where n = length xs - 1

-- Comentario: La definición anterior se puede simplificar.

-- carruirui3 erisancha pabmorgar juanarcon marvilmor lucgamgal
-- carmengar alebergon
descifra2 xs =  codifica (-factor) xs
    where factor = head (posiciones (minimum tabChi) tabChi)
          tabChi = [chiCuad (rota n (frecuencias xs)) tabla | n <- [0..25]]
