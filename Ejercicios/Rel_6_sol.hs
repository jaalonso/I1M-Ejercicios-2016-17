-- I1M 2015-16: Rel_6.hs (16 de octubre de 2015)
-- Funciones sobre cadenas.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Importación de librerías auxiliares                                --
-- ---------------------------------------------------------------------

import Data.Char
import Data.List
import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Definir, por comprensión, la función
--    sumaDigitosC :: String -> Int
-- tal que (sumaDigitosC xs) es la suma de los dígitos de la cadena
-- xs. Por ejemplo, 
--    sumaDigitosC "SE 2431 X"  ==  10
-- Nota: Usar las funciones (isDigit c) que se verifica si el carácter c
-- es un dígito y (digitToInt d) que es el entero correspondiente al
-- dígito d.
-- ---------------------------------------------------------------------

-- carruirui3 guache blaruiher alvalvdom1 manvermor  erisancha carmengar
-- enrvalmor silgongal pabmorgar fracruzam manpende migandben rubvilval
-- josllagam anaagusil juanarcon abrdelrod alebergon marvilmor paocabper
-- javperlag isrbelnun lucgamgal fatvilpiz
sumaDigitosC :: String -> Int
sumaDigitosC xs = sum [digitToInt x | x <- xs, isDigit x]

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Definir, por recursión, la función
--    sumaDigitosR :: String -> Int
-- tal que (sumaDigitosR xs) es la suma de los dígitos de la cadena
-- xs. Por ejemplo, 
--    sumaDigitosR "SE 2431 X"  ==  10
-- Nota: Usar las funciones isDigit y digitToInt.
-- ---------------------------------------------------------------------

-- carruirui3 guache blaruiher alvalvdom1 erisancha carmengar enrvalmor
-- silgongal pabmorgar fracruzam manpende migandben rubvilval josllagam
-- anaagusil juanarcon abrdelrod alebergon marvilmor paocabper javperlag
-- isrbelnun lucgamgal

sumaDigitosR :: String -> Int
sumaDigitosR "" = 0
sumaDigitosR (x:xs) 
    | isDigit x = sumaDigitosR xs + digitToInt x
    | otherwise = sumaDigitosR xs 

-- ---------------------------------------------------------------------
-- Ejercicio 1.3. Comprobar con QuickCheck que ambas definiciones son
-- equivalentes. 
-- ---------------------------------------------------------------------

-- carruirui3 guache blaruiher alvalvdom1 manvermor erisancha carmengar
-- enrvalmor silgongal pabmorgar fracruzam manpende migandben rubvilval
-- josllagam anaagusil juanarcon abrdelrod alebergon marvilmor paocabper
-- javperlag isrbelnun lucgamgal fatvilpiz

-- La propiedad es
prop_sumaDigitosC :: String -> Bool
prop_sumaDigitosC xs = sumaDigitosC xs == sumaDigitosR xs

-- La comprobación es
--    *Main> quickCheck prop_sumaDigitosC
--    +++ OK, passed 100 tests

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Definir, por comprensión, la función
--    mayusculaInicial :: String -> String
-- tal que (mayusculaInicial xs) es la palabra xs con la letra inicial
-- en mayúscula y las restantes en minúsculas. Por ejemplo, 
--    mayusculaInicial "sEviLLa"  ==  "Sevilla"
--    mayusculaInicial ""         ==  ""
-- Nota: Usar las funciones (toLower c) que es el carácter c en
-- minúscula y (toUpper c) que es el carácter c en mayúscula.
-- ---------------------------------------------------------------------

-- guache alvalvdom1 blaruiher manvermor erisancha enrvalmor silgongal
-- pabmorgar fracruzam migandben rubvilval carmengar josllagam juanarcon
-- abrdelrod paocabper carruirui3 javperlag isrbelnun lucgamgal

mayusculaInicial :: String -> String
mayusculaInicial []     = []
mayusculaInicial (x:xs) = toUpper x : [toLower x | x <- xs]

-- fracruzam anaagusil marvilmor
mayusculaInicial2 :: String -> String
mayusculaInicial2 []     = []
mayusculaInicial2 (x:xs) = toUpper x : map toLower xs

-- alebergon manpende fatvilpiz
mayusculaInicial3 :: String -> String
mayusculaInicial3 "" = ""
mayusculaInicial3 xs = toUpper (head xs): [toLower x | x <- tail xs]

-- Comentario: La definición anterior se puede simplificar.

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Definir, por recursión, la función
--    mayusculaInicialRec :: String -> String
-- tal que (mayusculaInicialRec xs) es la palabra xs con la letra
-- inicial en mayúscula y las restantes en minúsculas. Por ejemplo,
--    mayusculaInicialRec "sEviLLa"  ==  "Sevilla"
-- ---------------------------------------------------------------------

-- guache blaruiher erisancha silgongal migandben alvalvdom1 carmengar
-- anaagusil juanarcon paocabper abrdelrod javperlag isrbelnun lucgamgal 
mayusculaInicialRec :: String -> String
mayusculaInicialRec []     = []
mayusculaInicialRec (x:xs) = toUpper x :minuscula xs

minuscula []     = []
minuscula (x:xs) = toLower x : minuscula xs

-- manvermor enrvalmor pabmorgar rubvilval josllagam manpende marvilmor 
-- alebergon
mayusculaInicialRec2 :: String -> String
mayusculaInicialRec2 ""     = ""
mayusculaInicialRec2 (x:xs) = 
    toUpper x : (map toLower (mayusculaInicialRec2 xs))

-- ---------------------------------------------------------------------
-- Ejercicio 2.3. Comprobar con QuickCheck que ambas definiciones son
-- equivalentes. 
-- ---------------------------------------------------------------------

-- guache blaruiher manvermor erisancha enrlvamor silgongal pabmorgar
-- migandben rubvilval alvalvdom1 carmengar josllagam manpende anaagusil
-- juanarcon abrdelrod marvilmor paocabper alebergon javperlag isrbelnun
-- lucgamgal fatvilpiz

-- La propiedad es
prop_mayusculaInicial :: String -> Bool
prop_mayusculaInicial xs = 
    mayusculaInicialRec xs == mayusculaInicial xs

-- La comprobación es
--    *Main> quickCheck prop_mayusculaInicial
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Se consideran las siguientes reglas de mayúsculas
-- iniciales para los títulos: 
--    * la primera palabra comienza en mayúscula y
--    * todas las palabras que tienen 4 letras como mínimo empiezan
--      con mayúsculas
-- Definir, por comprensión, la función
--    titulo :: [String] -> [String]
-- tal que (titulo ps) es la lista de las palabras de ps con
-- las reglas de mayúsculas iniciales de los títulos. Por ejemplo,
--    ghci> titulo ["eL","arTE","DE","La","proGraMacion"]
--    ["El","Arte","de","la","Programacion"]
-- ---------------------------------------------------------------------

-- guache erisancha enrvalmor silgongal pabmorgar fracruzam migandben 
-- rubvilval alvalvdom1 carmengar anaagusil josllagam juanarcon
-- abrdelrod manpende marvilmor alebergon carruirui3 javperlag paocabper
-- blaruiher lucgamgal isrbelnun
titulo :: [String] -> [String]
titulo [] = []
titulo (x:xs) = mayusculaInicial x : [change x| x <- xs]

change :: String -> String
change xs | length xs < 4 = minuscula xs
          | otherwise     = mayusculaInicial xs

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Definir, por recursión, la función
--    tituloRec :: [String] -> [String]
-- tal que (tituloRec ps) es la lista de las palabras de ps con
-- las reglas de mayúsculas iniciales de los títulos. Por ejemplo,
--    ghci> tituloRec ["eL","arTE","DE","La","proGraMacion"]
--    ["El","Arte","de","la","Programacion"]
-- ---------------------------------------------------------------------

-- guache erisancha enrvalmor silgongal pabmorgar rubvilval carmengar
-- anaagusil josllagam juanarcon abrdelrod marvilmor javperlag paocabper
-- blaruiher lucgamgal
tituloRec :: [String] -> [String]
tituloRec [] = []
tituloRec (x:xs) = mayusculaInicial x : aux xs
    where aux []     = []
          aux (x:xs) = change x : aux xs

-- manpende alebergon
tituloRec2 :: [String] -> [String]
tituloRec2 [] = []
tituloRec2 (p:ps)  = mayusculaInicial p : titulo' ps
    where titulo' [] = []
          titulo' (p:ps) | length p >= 4 = mayusculaInicial p : titulo' ps
                         | otherwise = map toLower p : titulo' ps 

-- Comentario: La definición anterior se puede simplificar.

-- isrbelnun
tituloRec3 :: [String] -> [String]
tituloRec3 []       = []
tituloRec3 (xs:xss) = mayusculaInicialRec xs : tituloRec xss

-- ---------------------------------------------------------------------
-- Ejercicio 3.3. Comprobar con QuickCheck que ambas definiciones son
-- equivalentes. 
-- ---------------------------------------------------------------------

-- guache erisancha enrvalmor silgongal pabmorgar rubvilval carmengar
-- anaagusil josllagam juanarcon abrdelrod marvilmor alebergon manpende
-- javperlag paocabper blaruiher isrbelnun lucgamgal fatvilpiz

-- La propiedad es
prop_titulo :: [String] -> Bool
prop_titulo xs = tituloRec xs == titulo xs

-- La comprobación es
--    *Main> quickCheck prop_titulo
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. Definir, por comprensión, la función
--    buscaCrucigrama :: Char -> Int -> Int -> [String] -> [String]
-- tal que (buscaCrucigrama l pos lon ps) es la lista de las palabras de
-- la lista de palabras ps que tienen longitud lon y poseen la letra l en
-- la posición pos (comenzando en 0). Por ejemplo,
--    ghci> buscaCrucigrama 'c' 1 7 ["ocaso", "casa", "ocupado"]
--    ["ocupado"]
--    ghci> buscaCrucigrama 'o' 4 5 ["ocaso", "casa", "ocupado"]
--    ["ocaso"]
--    ghci> buscaCrucigrama 'c' (-1) 7 ["ocaso", "casa", "ocupado"]
--    []
-- ---------------------------------------------------------------------

-- erisancha enrvalmor pabmorgar fracruzam silgongal alvalvdom1
-- carmengar anaagusil juanarcon marvilmor abrdelrod carruirui3
-- javperlag paocabper blaruiher isrbelnun lucgamgal fatvilpiz
buscaCrucigrama :: Char -> Int -> Int -> [String] -> [String]
buscaCrucigrama y pos lon ps = 
    [x | x <- ps, 
         length x == lon, 
         pos > 0, pos < length x, 
         x !! pos == y] 

-- rubvilval
buscaCrucigrama2 :: Char -> Int -> Int -> [String] -> [String]
buscaCrucigrama2 a pos lon ps = 
    [x | x <- ps, length x == lon && (elem (a,pos) (zip x [0..lon])) ]

-- alebergon manpende
buscaCrucigrama3 :: Char -> Int -> Int -> [String] -> [String]
buscaCrucigrama3 l pos lon ps 
    | pos >= 0  = [x | x <- ps, length x == lon, x!!pos==l] 
    | otherwise = []

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Definir, por recursión, la función
--    buscaCrucigramaR :: Char -> Int -> Int -> [String] -> [String]
-- tal que (buscaCrucigramaR l pos lon ps) es la lista de las palabras
-- de la lista de palabras ps que tienen longitud lon y posen la letra l
-- en la posición pos (comenzando en 0). Por ejemplo,
--    ghci> buscaCrucigramaR 'c' 1 7 ["ocaso", "acabado", "ocupado"]
--    ["acabado","ocupado"]
--    ghci> buscaCrucigramaR 'o' 4 5 ["ocaso", "casa", "ocupado"]
--    ["ocaso"]
--    ghci> buscaCrucigramaR 'c' (-1) 7 ["ocaso", "casa", "ocupado"]
--    []
-- ---------------------------------------------------------------------

-- erisancha enrvalmor pabmorgar fracruzam silgongal alvalvdom1
-- carmengar anaagusil juanarcon abrdelrod marvilmor abrdelrod carruirui3
-- manpende javperlag paocabper blaruiher isrbelnun lucgamgal
buscaCrucigramaR :: Char -> Int -> Int -> [String] -> [String]
buscaCrucigramaR y pos lon [] = []
buscaCrucigramaR y pos lon (x:xs) 
    | length x == lon && pos > 0 && pos < length x && x !! pos == y 
        = x : buscaCrucigramaR y pos lon xs
    | otherwise = buscaCrucigramaR y pos lon xs

-- rubvilval
buscaCrucigramaR2 :: Char -> Int -> Int -> [String] -> [String]
buscaCrucigramaR2 a pos lon [] = []
buscaCrucigramaR2 a pos lon (x:xs)
    | length x == lon && (elem (a,pos) (zip x [0..lon])) = 
        x : buscaCrucigramaR a pos lon xs
    | otherwise = buscaCrucigramaR a pos lon xs

-- ---------------------------------------------------------------------
-- Ejercicio 4.3. Comprobar con QuickCheck que ambas definiciones son
-- equivalentes. 
-- ---------------------------------------------------------------------

-- erisancha enrvalmor pabmorgar fracruzam silgongal rubvilval alvalvdom1
-- carmengar anaagusil juanarcon abrdelrod marvilmor carruirui3 manpende
-- paocabper blaruiher isrbelnun lucgamgal fatvilpiz

-- La propiedad es
prop_buscaCrucigrama :: Char -> Int -> Int -> [String] -> Bool
prop_buscaCrucigrama y pos lon ps = 
    buscaCrucigramaR y pos lon ps ==  buscaCrucigrama y pos lon ps   

-- La comprobación es
--    *Main> quickCheck prop_buscaCrucigrama
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 5.1. Definir, por comprensión, la función
--    posiciones :: String -> Char -> [Int]
-- tal que (posiciones xs y) es la lista de la posiciones del carácter y
-- en la cadena xs. Por ejemplo,
--    posiciones "Salamamca" 'a'  ==  [1,3,5,8]
-- ---------------------------------------------------------------------

-- guache blaruiher manvermor erisancha enrvalmor pabmorgar fracruzam 
-- silgongal rubvilval alvalvdom1 carmengar anagusil juanarcon abrdelrod
-- marvilmor josllagam carruirui3 javperlag paocabper isrbelnun
-- lucgamgal fatvilpiz 
posiciones :: String -> Char -> [Int]
posiciones xs y = [v | (u,v) <- zip xs [0..], y == u]

-- manpende alebergon
posiciones2 :: String -> Char -> [Int]
posiciones2 xs y = [c | c <- [0..(length xs - 1)], xs!!c==y]

-- Comentario: La definición anterior se puede mejorar.

-- ---------------------------------------------------------------------
-- Ejercicio 5.2. Definir, por recursión, la función
--    posicionesR :: String -> Char -> [Int]
-- tal que (posicionesR xs y) es la lista de la posiciones del
-- carácter y en la cadena xs. Por ejemplo,
--    posicionesR "Salamamca" 'a'  ==  [1,3,5,8]
-- ---------------------------------------------------------------------

-- erisancha enrvalmor pabmorgar silgongal rubvilval carmengar anaagusil
-- juanarcon abrdelrod marvilmor carruirui3 manpende paocabper blaruiher
-- isrbelnun lucgamgal

posicionesR :: String -> Char -> [Int]
posicionesR xs y = pR xs y 0
    where pR [] y z = []
          pR (x:xs) y z | x == y    = z : pR xs y (z+1)
                        | otherwise = pR xs y (z+1)

-- ---------------------------------------------------------------------
-- Ejercicio 5.3. Comprobar con QuickCheck que ambas definiciones son
-- equivalentes. 
-- ---------------------------------------------------------------------

-- guache erisancha enrvalmor pabmorgar fracruzam silgongal rubvilval
-- carmengar anaagusil juanarcon abrdelrod marvilmor carruirui3
-- paocabper blaruiher isrbelnun lucgamgal fatvilpiz

-- La propiedad es
prop_posiciones :: String -> Char -> Bool
prop_posiciones xs y = posicionesR xs y == posiciones xs y 

-- La comprobación es
--    *Main> quickCheck prop_posiciones
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 6.1. Definir, por recursión, la función
--    contieneR :: String -> String -> Bool
-- tal que (contieneR xs ys) se verifica si ys es una subcadena de
-- xs. Por ejemplo, 
--    contieneR "escasamente" "casa"   ==  True
--    contieneR "escasamente" "cante"  ==  False
--    contieneR "" ""                  ==  True
-- Nota: Se puede usar la predefinida (isPrefixOf ys xs) que se verifica
-- si ys es un prefijo de xs.
-- ---------------------------------------------------------------------

-- blaruiher josllagam isrbelnun
contieneR :: String -> String -> Bool
contieneR []     [] = True
contieneR (x:xs) [] = True
contieneR []     ys = False
contieneR (x:xs) ys
    | isPrefixOf ys (x:xs) = True
    | otherwise            = contieneR xs ys

-- Comentario: La definición anterior se puede simplificar.

-- erisancha enrvalmor pabmorgar rubvilval anaagusil paocabper
contieneR2 :: String -> String -> Bool
contieneR2 _ []  = True       
contieneR2 [] ys = False
contieneR2 xs ys | isPrefixOf ys xs = True
                 | otherwise = contieneR2 (tail xs) ys

-- Comentario: La definición anterior se puede simplificar.

-- carmengar silgongal manpende juanarcon 
contieneR3 :: String -> String -> Bool
contieneR3 _ ""  = True 
contieneR3 "" ys = False
contieneR3 (x:xs) ys 
     | isPrefixOf ys (x:xs) = True
     | otherwise            = contieneR3 xs ys

-- Comentario: La definición anterior se puede simplificar.

-- alvalvdom1
contieneR4 :: String -> String -> Bool
contieneR4 _ []  = True
contieneR4 [] ys = False
contieneR4 xs ys = isPrefixOf ys xs || contieneR4 (tail xs) ys

-- abrdelrod
contieneR5 :: String -> String -> Bool
contieneR5 [] ys = False
contieneR5 xs ys | isPrefixOf ys xs = True
                 | otherwise        = contieneR5 (tail xs) ys

-- Comentario: La definición anterior se puede simplificar.

-- carruirui3 erisancha lucgamgal fatvilpiz
contieneR6 :: String -> String -> Bool
contieneR6 _  [] = True
contieneR6 [] _  = False
contieneR6 xs ys = isPrefixOf ys xs || contieneR6 (tail xs) ys

-- ---------------------------------------------------------------------
-- Ejercicio 6.2. Definir, por comprensión, la función
--    contiene :: String -> String -> Bool
-- tal que (contiene xs ys) se verifica si ys es una subcadena de
-- xs. Por ejemplo, 
--    contiene "escasamente" "casa"      ==  True
--    contiene "escasamente" "cante"     ==  False
--    contiene "casado y casada" "casa"  ==  True
--    contiene "" ""                     ==  True
-- Nota: Se puede usar la predefinida (isPrefixOf ys xs) que se verifica
-- si ys es un prefijo de xs.
-- ---------------------------------------------------------------------

-- fracruzam rubvilval anaagusil 
contiene :: String -> String -> Bool
contiene xs [] = True
contiene xs ys = or (map (isPrefixOf ys) (contieneAuxiliar xs))

contieneAuxiliar [] = []
contieneAuxiliar xs = [xs] ++ contieneAuxiliar (tail xs)

-- Comentario: La definición anterior se puede simplificar.

-- erisancha carmengar silgongal juanarcon abrdelrod marvilmor
-- carruirui3 paocabper blaruiher isrbelnun lucgamgal
contiene2 :: String -> String -> Bool
contiene2 xs ys = or [isPrefixOf ys x | x <- elimina xs]
    where elimina xs = [drop v xs | v <- [0..length xs]]

-- javperlag
contiene3 :: String -> String -> Bool
contiene3 xs ys = 
    elem True [isPrefixOf ys (drop n xs) | n <- [0..length xs]]

-- Comentario: La definición anterior se puede simplificar.

-- ---------------------------------------------------------------------
-- Ejercicio 6.3. Comprobar con QuickCheck que ambas definiciones son
-- equivalentes. 
-- ---------------------------------------------------------------------

-- rubvilval erisancha carmengar paocabper silgongal anagusil juanarcon
-- abrdelrod marvilmor carruirui3 blaruiher isrbelnun lucgamgal
-- fatvilpiz 

-- La propiedad es
prop_contiene :: String -> String -> Bool
prop_contiene xs ys = contiene xs ys == contieneR xs ys

-- La comprobación es
--    *Main> quickCheck prop_contiene
--    +++ OK, passed 100 tests
