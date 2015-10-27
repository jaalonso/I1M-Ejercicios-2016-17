-- I1M 2015-16: Rel_8.hs (23 de octubre de 2015)
-- El algoritmo de Luhn
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- § Introducción                                                     --
-- ---------------------------------------------------------------------

-- El objetivo de esta relación es estudiar un algoritmo para validar
-- algunos identificadores numéricos como los números de algunas tarjetas
-- de crédito; por ejemplo, las de tipo Visa o Master Card.  
--
-- El algoritmo que vamos a estudiar es el algoritmo de Luhn consistente
-- en aplicar los siguientes pasos a los dígitos del número de la
-- tarjeta.    
--    1. Se invierten los dígitos del número; por ejemplo, [9,4,5,5] se
--       transforma en [5,5,4,9].
--    2. Se duplican los dígitos que se encuentra en posiciones impares
--       (empezando a contar en 0); por ejemplo, [5,5,4,9] se transforma
--       en [5,10,4,18].
--    3. Se suman los dígitos de cada número; por ejemplo, [5,10,4,18]
--       se transforma en 5 + (1 + 0) + 4 + (1 + 8) = 19.
--    4. Si el último dígito de la suma es 0, el número es válido; y no
--       lo es, en caso contrario. 
--
-- A los números válidos, los llamaremos números de Luhn. 

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    digitosInv :: Integer -> [Integer]
-- tal que (digitosInv n) es la lista de los dígitos del número n. en
-- orden inverso. Por ejemplo, 
--    digitosR 320274  ==  [4,7,2,0,2,3]
-- ---------------------------------------------------------------------

-- silgongal blaruiher erisancha paocabper enrvalmor
digitosInv :: Integer -> [Integer]
digitosInv n = reverse [read [c] | c <- show n]

-- guache juanarcon
digitosInv2 :: Integer -> [Integer]
digitosInv2 n = [read [x] | x <- reverse (show n)]

-- guache pabmorgar alvalvdom1
digitosInv3 :: Integer -> [Integer]
digitosInv3 n = reverse (digit n)

digit :: Integer -> [Integer]
digit n = [read [x] | x <- show n]

-- manvermor fracruzam carruirui3
digitosInv4 :: Integer -> [Integer]
digitosInv4 n | n < 10    = [n]
              | otherwise = (n `rem` 10) : digitosInv4 (n `div` 10)

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    doblePosImpar :: [Integer] -> [Integer]
-- tal que (doblePosImpar ns) es la lista obtenida doblando los
-- elementos en las posiciones impares (empezando a contar en cero y
-- dejando igual a los que están en posiciones pares. Por ejemplo,
--    doblePosImpar [4,9,5,5]    ==  [4,18,5,10] 
--    doblePosImpar [4,9,5,5,7]  ==  [4,18,5,10,7]
-- ---------------------------------------------------------------------

-- guache silgongal manvermor 
doblePosImpar :: [Integer] -> [Integer]
doblePosImpar []       = []
doblePosImpar [a]      = [a]
doblePosImpar (x:y:xs) = [x,2*y] ++ doblePosImpar2 xs

-- Comentario: La definición anterior se puede mejorar.

-- guache blaruiher erisancha juanarcon pabmorgar alvalvdom1 paocabper
-- enrvalmor 
doblePosImpar1 :: [Integer] -> [Integer]
doblePosImpar1 []       = []
doblePosImpar1 [a]      = [a]
doblePosImpar1 (x:y:xs) = x : 2*y : doblePosImpar1 xs

-- guache carruirui3 fracruzam
doblePosImpar2 :: [Integer] -> [Integer]
doblePosImpar2 (x:y:xs) = x : 2*y : doblePosImpar xs
doblePosImpar2 xs       = xs

-- guache
doblePosImpar3  :: [Integer] -> [Integer]
doblePosImpar3 []     = []
doblePosImpar3 (x:xs) = x : concatMap (\(a,b) -> [a*b]) (zip xs ys)
    where ys = take (length xs) (concat [digit n | n <- repeat 21])

-- guache
doblePosImpar4  :: [Integer] -> [Integer]
doblePosImpar4 []     = []     
doblePosImpar4 (x:xs) = x : concat [[a*b] | (a,b) <- (zip xs ys)]    
    where ys = take (length xs) (concat [digit n | n <- repeat 21])          

{- los 3 primeros lo hice por recursión y los dos finales por comprensión
   la idea es la misma,doblePosImpar2 es la mejor solucion por su simplicidad,
   pero como sepan que las soluciones casi nunca son únicas -}

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--    sumaDigitos :: [Integer] -> Integer
-- tal que (sumaDigitos ns) es la suma de los dígitos de ns. Por
-- ejemplo, 
--    sumaDigitos [10,5,18,4] = 1 + 0 + 5 + 1 + 8 + 4 =
--                            = 19
-- ---------------------------------------------------------------------
 
-- silgongal erisancha  paocabper alvalvdom1 enrvalmor
sumaDigitos :: [Integer] -> Integer
sumaDigitos ns = sum (concat [digitos n | n <- ns])

digitos n = [read [c]| c <- show n]

-- guache pabmorgar
sumaDigitos1 :: [Integer] -> Integer
sumaDigitos1 ns = sum (concat [digitosInv2  k | k <- ns])

-- guache
sumaDigitos2 :: [Integer] -> Integer
sumaDigitos2 ns = sum (digit (numero ns))

numero :: [Integer] -> Integer
numero xs = sum [y*10^n | (y,n) <-zip (reverse xs) [0.. ]]

-- manvermor 
sumaDigitos3 :: [Integer] -> Integer
sumaDigitos3 []     = 0
sumaDigitos3 (n:ns) = sum (digitos n) + sumaDigitos ns
    where digitos n = reverse (digitosInv n)

-- Comentario: La definición anterior se puede mejorar.

-- fracruzam carruirui3 juanarcon
sumaDigitos4 :: [Integer] -> Integer
sumaDigitos4 ns = sum (concat (map digitosInv4 ns))

-- guache
sumaDigitos5 :: [Integer] -> Integer
sumaDigitos5 ns = sum (concat (map digit ns))

-- blaruiher 
sumaDigitos6 :: [Integer] -> Integer
sumaDigitos6 ns = sum [sum (digitosInv x) | x <- ns]

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función  
--    ultimoDigito :: Integer -> Integer
-- tal que (ultimoDigito n) es el último dígito de n. Por ejemplo,
--    ultimoDigito 123 == 3
--    ultimoDigito   0 == 0
-- ---------------------------------------------------------------------

-- silgongal manvermor pabmorgar blaruiher alvalvdom1 
ultimoDigito :: Integer -> Integer
ultimoDigito n = last (digitos n)
 
-- guache enrvalmor
ultimoDigito1 :: Integer -> Integer
ultimoDigito1 n = head (digitosInv2 n)

-- guache carruirui3 erisancha juanarcon
ultimoDigito2 :: Integer -> Integer
ultimoDigito2 n = rem n 10

-- guache fracruzam
ultimoDigito3 :: Integer -> Integer
ultimoDigito3 n = mod n 10

-- paocabper
ultimoDigito4 :: Integer -> Integer
ultimoDigito4 n = last [read [c] | c <- show n]

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la función 
--    luhn :: Integer -> Bool
-- tal que (luhn n) se verifica si n es un número de Luhn. Por ejemplo,
--    luhn 5594589764218858  ==  True
--    luhn 1234567898765432  ==  False
-- ---------------------------------------------------------------------

-- silgongal guache manvermor fracruzam carruirui3 erisancha  juanarcon
-- pabmorgar blaruiher alvalvdom1 paocabper enrvalmor
luhn :: Integer -> Bool
luhn n = ultimoDigito (sumaDigitos (doblePosImpar (digitosInv n))) == 0
