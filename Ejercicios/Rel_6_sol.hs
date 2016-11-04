-- I1M 2016-17: Rel_6.hs (28 de octubre de 2016)
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

-- paumacpar josrodgal7 marjimcom josdeher albcercid antmorper3 eliguivil
-- marmerzaf glovizcas eledejim2 cargonler fraferpoy fatfervaz pabrabmon
-- roscargar congomgom beagongon1 joscasgom1 albagucen manruiber migibagar
-- enrnarbej margarflo5 josjimgon2 javcancif juaorture carmarcar5
-- margirmon cescarde juacasnie
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

-- paumacpar josrodgal7 marjimcom josdeher albcercid antmorper3 roscargar
-- eliguivil glovizcas marmerzaf eledejim2 cargonler fraferpoy fatfervaz
-- pabrabmon congomgom beagongon1 joscasgom1 albagucen manruiber migibagar
-- enrnarbej margarflo5 josjimgon2 javcancif juaorture carmarcar5
-- margirmon cescarde juacasnie
sumaDigitosR :: String -> Int
sumaDigitosR [] = 0
sumaDigitosR (x:xs) | isDigit x = digitToInt x + sumaDigitosR xs
                    | otherwise = sumaDigitosR xs


-- ---------------------------------------------------------------------
-- Ejercicio 1.3. Comprobar con QuickCheck que ambas definiciones son
-- equivalentes. 
-- ---------------------------------------------------------------------

-- paumacpar josrodgal7 marjimcom josdeher albcercid antmorper3 roscargar
-- eliguivil glovizcas marmerzaf eledejim2 cargonler fatfervaz pabrabmon
-- beagongon1 congomgom joscasgom1 albagucen manruiber enrnarbej migibagar
-- margarflo5 josjimgon2 javcancif juaorture carmarcar5 margirmon cescarde
-- juacasnie
-- La propiedad es
prop_sumaDigitosC :: String -> Bool
prop_sumaDigitosC xs = sumaDigitosR xs == sumaDigitosC xs

-- La comprobación es
--    λ> quickCheck prop_sumaDigitosC
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Definir, por comprensión, la función
--    mayusculaInicial :: String -> String
-- tal que (mayusculaInicial xs) es la palabra xs con la letra inicial
-- en mayúscula y las restantes en minúsculas. Por ejemplo, 
--    mayusculaInicial "sEviLLa"  ==  "Sevilla"
--    mayusculaInicial "coco"     ==  "Coco"
--    mayusculaInicial ""         ==  ""
-- Nota: Usar las funciones (toLower c) que es el carácter c en
-- minúscula y (toUpper c) que es el carácter c en mayúscula.
-- ---------------------------------------------------------------------

-- josrodgal7 albcercid josdeher eliguivil antmorper3 glovizcas
-- marmerzaf cargonler paumacpar fatfervaz pabrabmon roscargar
-- beagongon1 congomgom marjimcom joscasgom1 albagucen manruiber
-- enrnarbej margarflo5 josjimgon2 javcancif juaorture carmarcar5 migibagar
-- margirmon cescarde juacasnie
mayusculaInicial :: String -> String
mayusculaInicial []     = []
mayusculaInicial (x:xs) = toUpper x : [ toLower a | a <- xs ]

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Definir, por recursión, la función
--    mayusculaInicialRec :: String -> String
-- tal que (mayusculaInicialRec xs) es la palabra xs con la letra
-- inicial en mayúscula y las restantes en minúsculas. Por ejemplo,
--    mayusculaInicialRec "sEviLLa"  ==  "Sevilla"
-- ---------------------------------------------------------------------

-- josdeher albcercid antmorper3 glovizcas marmerzaf cargonler paumacpar 
-- josrodgal7 fatfervaz pabrabmon roscargar beagongon1 marjimcom congomgom
-- joscasgom1 albagucen manruiber enrnarbej margarflo5 josjimgon2 migibagar
-- juaorture carmarcar5 margirmon juacasnie
mayusculaInicialRec :: String -> String
mayusculaInicialRec []     = []
mayusculaInicialRec (x:xs) = toUpper x : minusculaResto xs

minusculaResto :: String -> String
minusculaResto []     = []
minusculaResto (x:xs) = toLower x : minusculaResto xs

-- albcercid eliguivil
mayusculaInicialRec3 :: String -> String
mayusculaInicialRec3 []     = ""
mayusculaInicialRec3 (x:xs) =
  toUpper x : (map (toLower) (mayusculaInicialRec3 xs))

-- Comentario: La definición anterior se puede mejorar.

-- cescarde
mayusculaInicialRec4 :: String -> String
mayusculaInicialRec4 [] = []
mayusculaInicialRec4 (x:xs) = toUpper x : minusculas xs
  where minusculas (x:xs) = toLower x : minusculas xs
        minusculas []     = []

-- ---------------------------------------------------------------------
-- Ejercicio 2.3. Comprobar con QuickCheck que ambas definiciones son
-- equivalentes. 
-- ---------------------------------------------------------------------

-- josdeher albcercid eliguivil antmorper3 glovizcas marmerzaf cargonler
-- paumacpar josrodgal7 fatfervaz pabrabmon roscargar beagongon1 marjimcom
-- congomgom joscasgom1 albagucen manruiber enrnarbej margarflo5 josjimgon2
-- juaorture carmarcar5 margirmon migibagar cescarde juacasnie
-- La propiedad es
prop_mayusculaInicial :: String -> Bool
prop_mayusculaInicial xs = mayusculaInicial xs == mayusculaInicialRec xs

-- La comprobación es
-- λ> quickCheck prop_mayusculaInicial
-- +++ OK, passed 100 tests.

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

-- josdeher
titulo :: [String] -> [String]
titulo [] = []
titulo (xs:yss) =
  mayusculaInicial xs :
  [if length ys >= 4
   then mayusculaInicial ys
   else todoMinusculas ys | ys <- yss]

todoMinusculas :: String -> String
todoMinusculas ys = [toLower y | y <- ys ]

-- Comentario: La definición titulo se puede mejorar.

-- albcercid eliguivil
titulo2 :: [String] -> [String]
titulo2 []     = []
titulo2 (x:xs) = mayusculaInicial x : [otraRegla a | a <- xs]

otraRegla xs | 4 <= length xs = mayusculaInicial xs
             | otherwise      = map (toLower) xs

-- Comentario: La definición anterior se puede simplificar.

-- antmorper3 paumacpar  marmerzaf cargonler pabrabmon roscargar 
-- beagongon1 marjimcom congomgom albagucen fatfervaz manruiber
-- enrnarbej  margarflo5 josjimgon2 carmarcar5 margirmon cescarde
-- juacasnie
titulo3 :: [String] -> [String]
titulo3 []     = []
titulo3 (p:ps) = mayusculaInicial p : [resto k | k <- ps]

resto :: [Char] -> [Char]
resto xs | length xs >= 4 = mayusculaInicial xs
         | otherwise      = [toLower k | k <- xs]

-- josrodgal7 
titulo4 :: [String] -> [String]
titulo4 ps = [cadaPalabra ps l | l <- ps]

cadaPalabra :: [String] -> String -> String
cadaPalabra cad l  | l == []       = []
                   | l == head cad = mayusculaInicial l
                   | length l < 4  = minusculaResto l
                   | otherwise     = mayusculaInicial l

-- juaorture
titulo5 :: [String] -> [String]
titulo5 []       = []
titulo5 (xs:xss) = mayusculaInicial xs : [mayusculas a | a <- xss]

minuscula :: String -> String
minuscula "" = ""
minuscula xs = [toLower a | a <- xs]

mayusculas :: String -> String
mayusculas xs | length xs >= 4 = mayusculaInicial xs
              | otherwise      = minuscula xs

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Definir, por recursión, la función
--    tituloRec :: [String] -> [String]
-- tal que (tituloRec ps) es la lista de las palabras de ps con
-- las reglas de mayúsculas iniciales de los títulos. Por ejemplo,
--    ghci> tituloRec ["eL","arTE","DE","La","proGraMacion"]
--    ["El","Arte","de","la","Programacion"]
-- ---------------------------------------------------------------------

-- albcercid josdeher antmorper3 congomgom joscasgom1 manruiber jucasnie
tituloRec :: [String] -> [String]
tituloRec []     = []
tituloRec (x:xs) = mayusculaInicial x : segundaRegla xs

segundaRegla :: [String] -> [String]
segundaRegla [] = []
segundaRegla (xs:xss)
  | length xs >= 4 = mayusculaInicial xs:segundaRegla xss
  | otherwise      = map toLower xs : segundaRegla xss

-- eliguivil
tituloRec2 :: [String] -> [String]
tituloRec2 [] = []
tituloRec2 (p:ps) = mayusculaInicial p : (map regla (tituloRec2 ps))

-- Comentario: La definición tituloRec2 se puede mejorar.

regla :: String -> String
regla xs | length xs >= 4 = mayusculaInicial xs
         | otherwise      = map toLower xs

-- paumacpar cargonler pabrabmon roscargar beagongon1 marjimcom
-- fatfervaz enrnarbej margarflo5 josjimgon2 carmarcar5 margirmon cescarde
tituloRec3 :: [String] -> [String]
tituloRec3 (x:xs) = mayusculaInicial x : restoRec xs 
tituloRec3 []     = []

restoRec :: [String] -> [String]
restoRec []     = []
restoRec (x:xs) = resto x : restoRec xs 

-- juaorture
tituloRec1a :: String -> String
tituloRec1a [] = []
tituloRec1a xs | length xs >= 4 = mayusculaInicial xs
               | otherwise      = [toLower a | a <- xs] 

tituloRec2a :: [String] -> [String]
tituloRec2a []       = []
tituloRec2a (xs:xss) = tituloRec1a xs : tituloRec2a xss

tituloRec4 :: [String] -> [String]
tituloRec4 []       = []
tituloRec4 (xs:xss) = mayusculaInicial xs : tituloRec2a xss

-- ---------------------------------------------------------------------
-- Ejercicio 3.3. Comprobar con QuickCheck que ambas definiciones son
-- equivalentes. 
-- ---------------------------------------------------------------------

-- albcercid josdeher eliguivil antmorper3 paumacpar cargonler pabrabmon
-- roscargar beagongon1 marjimcom congomgom joscasgom1 albagucen
-- fatfervaz manruiber enrnarbej margarflo5 josjimgon2 juaorture
-- carmarcar5 margirmon cescarde juacasnie
-- La propiedad es
prop_titulo :: [String] -> Bool
prop_titulo xs = titulo xs == tituloRec xs

-- La comprobación es
-- λ> quickCheck prop_titulo
-- +++ OK, passed 100 tests.

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
--    ghci> buscaCrucigrama 'o' 7 5 ["ocaso", "casa", "ocupado"]
--    *** Exception: Prelude.!!: index too large
-- ---------------------------------------------------------------------
 
-- albcercid josdeher antmorper3 josrodgal7 paumacpar pabrabmon
-- roscargar beagongon1 marjimcom joscasgom1 congomgom albagucen
-- manruiber enrnarbej fatfervaz cargonler margarflo5 josjimgon2
-- carmarcar5 margirmon cescarde juacasnie
buscaCrucigrama :: Char -> Int -> Int -> [String] -> [String]
buscaCrucigrama l  pos lon ps
  | pos >= 0 && lon > pos = [a | a <- ps
                               , length a == lon,
                                 l == a!!pos]
  | otherwise             = []

-- eliguivil
buscaCrucigrama2 :: Char -> Int -> Int -> [String] -> [String]
buscaCrucigrama2 _ _   _   [""]   = []
buscaCrucigrama2 _ _   _   []   = []
buscaCrucigrama2 l pos lon ps
  | pos < 0 || lon < 0 || lon <= pos = []
  | otherwise = [p | p <- ps, length p == lon, p !! pos == l]

-- juaorture
buscaCrucigrama3 :: Char -> Int -> Int -> [String] -> [String]
buscaCrucigrama3 l pos lon ps
  | lon < 0 || pos > lon || pos < 0  = []
  | otherwise                        = [ a | a <- ps
                                           , length a == lon
                                           , a!!pos == l ]

-- migibagar
buscaCrucigrama4 :: Char -> Int -> Int -> [String] -> [String]
buscaCrucigrama4 l pos lon ps
  | pos >= 0   =  [xs | xs <- ps, length xs == lon, xs !! pos == l]
  | otherwise  =  []

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Definir, por recursión, la función
--    buscaCrucigramaR :: Char -> Int -> Int -> [String] -> [String]
-- tal que (buscaCrucigramaR l pos lon ps) es la lista de las palabras
-- de la lista de palabras ps que tienn longitud lon y posen la letra l
-- en la posición pos (comenzando en 0). Por ejemplo,
--    ghci> buscaCrucigramaR 'c' 1 7 ["ocaso", "acabado", "ocupado"]
--    ["acabado","ocupado"]
-- ---------------------------------------------------------------------

-- albcercid josdeher antmorper3 paumacpar pabrabmon roscargar beagongon1
-- marjimcom joscasgom1 congomgom manruiber enrnarbej fatfervaz
-- cargonler margarflo5 josjimgon2 carmarcar5 margirmon cescarde juacasnie
buscaCrucigramaR :: Char -> Int -> Int -> [String] -> [String]
buscaCrucigramaR l  pos lon [] = []
buscaCrucigramaR l  pos lon (p:ps)
  | pos >= 0 && lon > pos && length p == lon && l == p!!pos =
    p:buscaCrucigramaR l pos lon ps
  | otherwise = buscaCrucigramaR l pos lon ps

-- eliguivil
buscaCrucigramaR2 :: Char -> Int -> Int -> [String] -> [String]
buscaCrucigramaR2 _ _   _   [""]   = []
buscaCrucigramaR2 _ _   _   []     = []
buscaCrucigramaR2 l pos lon (p:ps)
  | pos < 0 || lon < 0 || lon <= pos = []
  | length p == lon &&
    p !! pos == l      = p:(buscaCrucigramaR l pos lon ps)
  | otherwise          = buscaCrucigramaR l pos lon ps

-- juaorture
buscaCrucigramaR3 :: Char -> Int -> Int -> [String] -> [String]
buscaCrucigramaR3 _ 0   _   _  = []
buscaCrucigramaR3 _ _   _   [] = []
buscaCrucigramaR3 l pos lon (p:ps)
  | lon < pos || pos < 0 || lon < 0 = []
  | length p == lon && p!!pos == l  = p : buscaCrucigramaR3 l pos lon ps
  | otherwise                       = buscaCrucigramaR3 l pos lon ps
                       
-- ---------------------------------------------------------------------
-- Ejercicio 4.3. Comprobar con QuickCheck que ambas definiciones son
-- equivalentes. 
-- ---------------------------------------------------------------------

-- albcercid josdeher antmorper3 paumacpar pabrabmon roscargar beagongon1
-- marjimcom joscasgom1 congomgom eliguivil manruiber enrnarbej
-- fatfervaz cargonler margarflo5 josjimgon2 juaorture carmarcar5
-- margirmon cescarde juacasnie
-- La propiedad es
prop_buscaCrucigrama :: Char -> Int -> Int -> [String] -> Bool
prop_buscaCrucigrama l  pos lon ps =
  buscaCrucigrama l pos lon ps == buscaCrucigramaR l pos lon ps 

-- La comprobación es
-- λ> quickCheck prop_buscaCrucigrama
-- +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 5.1. Definir, por comprensión, la función
--    posiciones :: String -> Char -> [Int]
-- tal que (posiciones xs y) es la lista de la posiciones del carácter y
-- en la cadena xs. Por ejemplo,
--    posiciones "Salamamca" 'a'  ==  [1,3,5,8]
-- ---------------------------------------------------------------------

-- albcercid josdeher eliguivil antmorper3 glovizcas josrodgal7
-- paumacpar cargonler roscargar beagongon1 marjimcom migibagar
-- pabrabmon joscasgom1 congomgom manruiber enrnarbej fatfervaz
-- margarflo5 josjimgon2 juaorture carmarcar5 margirmon cescarde juacasnie
posiciones :: String -> Char -> [Int]
posiciones xs y = [b | (a,b) <- zip xs [0..]
                     , a == y]

-- ---------------------------------------------------------------------
-- Ejercicio 5.2. Definir, por recursión, la función
--    posicionesR :: String -> Char -> [Int]
-- tal que (posicionesR xs y) es la lista de la posiciones del
-- carácter y en la cadena xs. Por ejemplo,
--    posicionesR "Salamamca" 'a'  ==  [1,3,5,8]
-- ---------------------------------------------------------------------

-- albcercid cargonler pabrabmon joscasgom1 manruiber enrnarbej
-- fatfervaz margarflo5 roscargar cescarde juacasnie
posicionesR :: String -> Char -> [Int]
posicionesR [] _ = []
posicionesR xs y
  | last xs == y = posicionesR (init xs) y ++ [length xs - 1]
  | otherwise    = posicionesR (init xs) y

-- Comentario: La definición posicionesR se puede mejorar. Por ejemplo,
--    λ> posicionesR (replicate 20000 'a') 'b'
--    []
--    (2.61 secs, 11,193,362,944 bytes)
--    λ> posicionesR' (replicate 20000 'a') 'b'
--    []
--    (0.01 secs, 0 bytes)

-- eliguivil antmorper3 josdeher glovizcas beagongon1 marjimcom
-- congomgom josjimgon2 margirmon
posicionesR2 :: String -> Char -> [Int]
posicionesR2 []     _ = []
posicionesR2 (x:xs) y
  | x == y    = map (+1) ((-1) : posicionesR2 xs y)
  | otherwise = map (+1) (posicionesR2 xs y)

-- paumacpar 
posicionesR3 :: String -> Char -> [Int]
posicionesR3 (x:xs) y = reverse (posicionesRR (reverse (x:xs)) y )
posicionesR3 [] _     = []

posicionesRR :: String -> Char -> [Int]
posicionesRR (x:xs) y 
  | y == x    =  length (x:xs) -1 : posicionesRR xs y
  | otherwise = posicionesRR xs y
posicionesRR [] _ = []

-- juaorture
posicionesR4 :: String -> Char -> [Int]
posicionesR4 [] _ = []
posicionesR4 xs n
  | last xs == n = sort (length xs - 1 : posicionesR4 (init xs) n)
  | otherwise    = posicionesR4 (init xs) n

-- Comentario: La definición posicionesR se puede mejorar. Por ejemplo,
--    λ> posicionesR4 (replicate 20000 'a') 'b'
--    []
--    (7.06 secs, 11,212,895,232 bytes)
--    λ> posicionesR' (replicate 20000 'a') 'b'
--    []
--    (0.01 secs, 0 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 5.3. Comprobar con QuickCheck que ambas definiciones son
-- equivalentes. 
-- ---------------------------------------------------------------------
 
-- albcercid eliguivil antmorper3 josdeher glovizcas paumacpar cargonler
-- pabrabmon beagongon1 marjimcom joscasgom1 congomgom manruiber
-- enrnarbej fatfervaz margarflo5 josjimgon2 juaorture roscargar
-- margirmon cescarde juacasnie
-- La propiedad es
prop_posiciones :: String -> Char -> Bool
prop_posiciones xs y = posiciones xs y == posicionesR xs y

-- La comprobación es
-- λ> quickCheck prop_posiciones
-- +++ OK, passed 100 tests.

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

-- albcercid josdeher glovizcas paumacpar pabrabmon beagongon1 marjimcom
-- joscasgom1 congomgom manruiber enrnarbej fatfervaz cargonler roscargar
-- margarflo5 josjimgon2 antmorper3 margirmon migibagar cescarde juacasnie
contieneR :: String -> String -> Bool
contieneR _ ""      = True
contieneR "" _      = False
contieneR (x:xs) ys = isPrefixOf ys (x:xs) || contieneR xs ys

-- eliguivil carmarcar5
contieneR2 :: String -> String -> Bool
contieneR2 _      "" = True
contieneR2 ""     _  = False
contieneR2 (x:xs) ys | isPrefixOf ys (x:xs) = True 
                     | otherwise            = contieneR2 xs ys 

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

-- albcercid josdeher paumacpar pabrabmon beagongon1 marjimcom joscasgom1
-- congomgom manruiber enrnarbej fatfervaz cargonler margarflo5 josjimgon2
-- antmorper3 roscargar margirmon cescarde juacasnie
contiene :: String -> String -> Bool
contiene xs ys = any (isPrefixOf ys) (tails xs)

-- eliguivil
contiene2 :: String -> String -> Bool
contiene2 xs ys = or [isPrefixOf ys xs' | xs' <- recortes xs]

recortes :: String -> [String]
recortes []     = []
recortes (x:xs) = [x:xs] ++ recortes xs

-- ---------------------------------------------------------------------
-- Ejercicio 6.3. Comprobar con QuickCheck que ambas definiciones son
-- equivalentes. 
-- ---------------------------------------------------------------------

-- albcercid eliguivil josdeher paumacpar pabrabmon beagongon1 marjimcom
-- joscasgom1 congomgom manruiber enrnarbej fatfervaz cargonler juacasnie
-- margarflo5 josjimgon2 antmorper3 juaorture roscargar margirmon cescarde
-- La propiedad es
prop_contiene :: String -> String -> Bool
prop_contiene xs ys = contieneR xs ys == contiene xs ys

-- La comprobación es
-- λ> quickCheck prop_contiene
-- +++ OK, passed 100 tests.
