-- I1M 2015-16: Rel_8.hs (23 de octubre de 2015)
-- El algoritmo de Luhn
-- Departamento de Ciencias de la Computaci�n e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- � Introducci�n                                                     --
-- ---------------------------------------------------------------------

-- El objetivo de esta relaci�n es estudiar un algoritmo para validar
-- algunos identificadores num�ricos como los n�meros de algunas tarjetas
-- de cr�dito; por ejemplo, las de tipo Visa o Master Card.  
--
-- El algoritmo que vamos a estudiar es el algoritmo de Luhn consistente
-- en aplicar los siguientes pasos a los d�gitos del n�mero de la
-- tarjeta.    
--    1. Se invierten los d�gitos del n�mero; por ejemplo, [9,4,5,5] se
--       transforma en [5,5,4,9].
--    2. Se duplican los d�gitos que se encuentra en posiciones impares
--       (empezando a contar en 0); por ejemplo, [5,5,4,9] se transforma
--       en [5,10,4,18].
--    3. Se suman los d�gitos de cada n�mero; por ejemplo, [5,10,4,18]
--       se transforma en 5 + (1 + 0) + 4 + (1 + 8) = 19.
--    4. Si el �ltimo d�gito de la suma es 0, el n�mero es v�lido; y no
--       lo es, en caso contrario. 
--
-- A los n�meros v�lidos, los llamaremos n�meros de Luhn. 

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la funci�n
--    digitosInv :: Integer -> [Integer]
-- tal que (digitosInv n) es la lista de los d�gitos del n�mero n. en
-- orden inverso. Por ejemplo, 
--    digitosR 320274  ==  [4,7,2,0,2,3]
-- ---------------------------------------------------------------------

-- silgongal
digitosInv :: Integer -> [Integer]
digitosInv n = reverse [read [c] | c <- show n]

-- guache
digitosInv2 :: Integer -> [Integer]
digitosInv2 n = [read [x] | x <- reverse (show n)]

-- guache
digitosInv3 :: Integer -> [Integer]
digitosInv3 n = reverse (digit n)

digit :: Integer -> [Integer]
digit n = [read [x] | x <- show n]

-- manvermor
digitosInv4 :: Integer -> [Integer]
digitosInv4 n | n < 10    = [n]
              | otherwise = (n `rem` 10) : digitosInv4 (n `div` 10)

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la funci�n
--    doblePosImpar :: [Integer] -> [Integer]
-- tal que (doblePosImpar ns) es la lista obtenida doblando los
-- elementos en las posiciones impares (empezando a contar en cero y
-- dejando igual a los que est�n en posiciones pares. Por ejemplo,
--    doblePosImpar [4,9,5,5]    ==  [4,18,5,10] 
--    doblePosImpar [4,9,5,5,7]  ==  [4,18,5,10,7]
-- ---------------------------------------------------------------------

-- guache silgongal manvermor
doblePosImpar :: [Integer] -> [Integer]
doblePosImpar []       = []
doblePosImpar [a]      = [a]
doblePosImpar (x:y:xs) = [x,2*y] ++ doblePosImpar2 xs

-- Comentario: La definici�n anterior se puede mejorar.

-- guache 
doblePosImpar1 :: [Integer] -> [Integer]
doblePosImpar1 []       = []
doblePosImpar1 [a]      = [a]
doblePosImpar1 (x:y:xs) = x : 2*y : doblePosImpar1 xs

-- guache 
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

{- los 3 primeros lo hice por recursi�n y los dos finales por comprensi�n
   la idea es la misma,doblePosImpar2 es la mejor solucion por su simplicidad,
   pero como sepan que las soluciones casi nunca son �nicas -}

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la funci�n
--    sumaDigitos :: [Integer] -> Integer
-- tal que (sumaDigitos ns) es la suma de los d�gitos de ns. Por
-- ejemplo, 
--    sumaDigitos [10,5,18,4] = 1 + 0 + 5 + 1 + 8 + 4 =
--                            = 19
-- ---------------------------------------------------------------------

-- silgongal
sumaDigitos :: [Integer] -> Integer
sumaDigitos ns = sum (concat [digitos n | n <- ns])

-- guache
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

-- Comentario: La definici�n anterior se puede mejorar.

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la funci�n  
--    ultimoDigito :: Integer -> Integer
-- tal que (ultimoDigito n) es el �ltimo d�gito de n. Por ejemplo,
--    ultimoDigito 123 == 3
--    ultimoDigito   0 == 0
-- ---------------------------------------------------------------------

-- silgongal manvermor
ultimoDigito :: Integer -> Integer
ultimoDigito n = last (digitos n)
    where digitos n = [read [c]| c <- show n]

-- guache
ultimoDigito1 :: Integer -> Integer
ultimoDigito1 n = head (digitosInv2 n)

-- guache
ultimoDigito2 :: Integer -> Integer
ultimoDigito2 n = rem n 10

-- guache
ultimoDigito3 :: Integer -> Integer
ultimoDigito3 n = mod n 10

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la funci�n 
--    luhn :: Integer -> Bool
-- tal que (luhn n) se verifica si n es un n�mero de Luhn. Por ejemplo,
--    luhn 5594589764218858  ==  True
--    luhn 1234567898765432  ==  False
-- ---------------------------------------------------------------------

-- silgongal guache manvermor
luhn :: Integer -> Bool
luhn n = ultimoDigito (sumaDigitos (doblePosImpar (digitosInv n))) == 0
