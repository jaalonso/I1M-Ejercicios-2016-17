-- I1M 2016-17: Rel_2.hs (28 de septiembre de 2016)
-- Definiciones con condicionales, guardas o patrones.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- En esta relación se presentan ejercicios con definiciones elementales
-- (no recursivas) de funciones que usan condicionales, guardas o
-- patrones. 
-- 
-- Estos ejercicios se corresponden con el tema 4 cuyas transparencias
-- se encuentran en  
--    http://www.cs.us.es/~jalonso/cursos/i1m-16/temas/tema-4.html

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Test.QuickCheck
import Data.List
import Data.Tuple

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función 
--    divisionSegura :: Double -> Double -> Double
-- tal que (divisionSegura x y) es x/y si y no es cero y 9999 en caso
-- contrario. Por ejemplo,
--    divisionSegura 7 2  ==  3.5
--    divisionSegura 7 0  ==  9999.0
-- ---------------------------------------------------------------------

-- paumacpar roscargar
divisionSegura :: Double -> Double -> Double
divisionSegura x y = if y /= 0 then x/y else 999

-- enrnarbej
divisionSegura2 _ 0 = 9999
divisionSegura2 x y = x/y

-- enrnarbej fatfervaz
divisionSegura3 x y
  | y == 0    = 9999
  | otherwise = x/y

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. La disyunción excluyente xor de dos fórmulas se
-- verifica si una es verdadera y la otra es falsa. Su tabla de verdad
-- es
--    x     | y     | xor x y
--    ------+-------+---------
--    True  | True  | False 
--    True  | False | True
--    False | True  | True
--    False | False | False
--    
-- Definir la función 
--    xor1 :: Bool -> Bool -> Bool
-- tal que (xor1 x y) es la disyunción excluyente de x e y, calculada a
-- partir de la tabla de verdad. Usar 4 ecuaciones, una por cada línea
-- de la tabla. 
-- ---------------------------------------------------------------------

-- antdursan roscargar
xor1 :: Bool -> Bool -> Bool
xor1 x y | x == True  && y == True  = False
         | x == True  && y == False = True
         | x == False && y == True  = True
         | x == False && y == False = False

-- enrnarbej
xor1b :: Bool -> Bool -> Bool
xor1b True  True  = False
xor1b True  False = True
xor1b False True  = True
xor1b False False = False

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Definir la función 
--    xor2 :: Bool -> Bool -> Bool
-- tal que (xor2 x y) es la disyunción excluyente de x e y, calculada a
-- partir de la tabla de verdad y patrones. Usar 2 ecuaciones, una por
-- cada valor del primer argumento. 
-- ---------------------------------------------------------------------

-- enrnarbej roscargar
xor2 :: Bool -> Bool -> Bool
xor2 True x  = not x
xor2 False x = x

-- ---------------------------------------------------------------------
-- Ejercicio 2.3. Definir la función 
--    xor3 :: Bool -> Bool -> Bool
-- tal que (xor3 x y) es la disyunción excluyente de x e y, calculada 
-- a partir de la disyunción (||), conjunción (&&) y negación (not). 
-- Usar 1 ecuación. 
-- ---------------------------------------------------------------------

-- enrnarbej
xor3 :: Bool -> Bool -> Bool
xor3 x y = ((not (x && y)) && (x || y))

-- Comentario: La definición anterior se puede simplificar.

-- ---------------------------------------------------------------------
-- Ejercicio 2.4. Definir la función 
--    xor4 :: Bool -> Bool -> Bool
-- tal que (xor4 x y) es la disyunción excluyente de x e y, calculada
-- a partir de desigualdad (/=). Usar 1 ecuación.
-- ---------------------------------------------------------------------

-- enrnarbej roscargar
xor4 :: Bool -> Bool -> Bool
xor4 x y = x /= y


-- ---------------------------------------------------------------------
-- Ejercicio 2.5. Comprobar con QuickCheck que las cuatros definiciones
-- de xor son equivalentes.
-- ---------------------------------------------------------------------

-- enrnarbej
-- La propiedad es
prop_xor_equivalentes :: Bool -> Bool -> Bool
prop_xor_equivalentes x y =
  (xor1 x y == xor2 x y) &&
  (xor2 x y == xor3 x y) &&
  (xor3 x y == xor4 x y)

-- La comprobación es 
--    >>> quickCheck prop_xor_equivalentes
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 3. Las dimensiones de los rectángulos puede representarse 
-- por pares; por ejemplo, (5,3) representa a un rectángulo de base 5 y 
-- altura 3. 
-- 
-- Definir la función 
--    mayorRectangulo :: (Num a, Ord a) => (a,a) -> (a,a) -> (a,a)
-- tal que (mayorRectangulo r1 r2) es el rectángulo de mayor área entre
-- r1 y r2. Por ejemplo,  
--    mayorRectangulo (4,6) (3,7)  ==  (4,6)
--    mayorRectangulo (4,6) (3,8)  ==  (4,6)
--    mayorRectangulo (4,6) (3,9)  ==  (3,9)
-- ---------------------------------------------------------------------

-- enrnarbej
mayorRectangulo :: (Num a, Ord a) => (a,a) -> (a,a) -> (a,a)
mayorRectangulo (x,y) (a, b) | x*y >= a*b = (x,y)
                             | otherwise  = (a,b)

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. Definir la función 
--    intercambia :: (a,b) -> (b,a)
-- tal que (intercambia p) es el punto obtenido intercambiando las
-- coordenadas del punto p. Por ejemplo, 
--    intercambia (2,5)  ==  (5,2)
--    intercambia (5,2)  ==  (2,5)
-- ---------------------------------------------------------------------

-- enrnarbej, paumacpar
intercambia :: (a,b) -> (b,a)
intercambia (a,b)=(b,a) 

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Comprobar con QuickCheck que la función intercambia es
-- idempotente; es decir, si se aplica dos veces es lo mismo que no
-- aplicarla ninguna.
-- ---------------------------------------------------------------------

-- enrnarbej
-- La propiedad es intercambia (intercambia p)
prop_intercambia :: (Int,Int) -> Bool
prop_intercambia p = p == intercambia (intercambia p)

-- La comprobación es
--    >>> quickCheck prop_intercambia
--    +++ OK, passed 100 tests.

-- paumacpar
prop_intercambia2 :: (Int,Int) -> Bool
prop_intercambia2 (a,b) = intercambia (intercambia (a,b)) == (a,b)

-- Comentario: La definición anterior se puede simplificar.

-- ---------------------------------------------------------------------
-- Ejercicio 5.1. Definir la función 
--    distancia :: (Double,Double) -> (Double,Double) -> Double
-- tal que (distancia p1 p2) es la distancia entre los puntos p1 y
-- p2. Por ejemplo, 
--    distancia (1,2) (4,6)  ==  5.0
-- ---------------------------------------------------------------------

-- enrnarbej, paumacpar   
distancia :: (Double,Double) -> (Double,Double) -> Double
distancia (x1, y1) (x2,y2)= sqrt ((x2-x1)^2 + (y2-y1)^2) 

-- ---------------------------------------------------------------------
-- Ejercicio 5.2. Comprobar con QuickCheck que se verifica la propiedad
-- triangular de la distancia; es decir, dados tres puntos p1, p2 y p3,
-- la distancia de p1 a p3 es menor o igual que la suma de la distancia
-- de p1 a p2 y la de p2 a p3.
-- ---------------------------------------------------------------------

-- enrnarbej
-- La propiedad es distancia p1 p3 <= distancia p1 p2 + distancia p2 p3
prop_triangular :: (Double,Double) -> (Double,Double) -> (Double,Double) -> Bool
prop_triangular p1 p2 p3 =
  distancia p1 p3 <= distancia p1 p2 + distancia p2 p3

-- La comprobación es
--    >>> quickCheck prop_triangular
--    +++ OK, passed 100 tests.

-- paumacpar 
prop_triangular2 :: (Double,Double) -> (Double,Double) -> (Double,Double) -> Bool
prop_triangular2 (x1,y1) (x2,y2) (x3,y3) =
  distancia (x1,y1) (x3,y3) <= distancia (x2,y2) (x3,y3) + distancia (x1,y1) (x2,y2) 

-- Comentario: La definición anterior se puede simplificar.

-- ---------------------------------------------------------------------
-- Ejercicio 6.1. Definir una función 
--    ciclo :: [a] -> [a]
-- tal que (ciclo xs) es la lista obtenida permutando cíclicamente los
-- elementos de la lista xs, pasando el último elemento al principio de
-- la lista. Por ejemplo, 
--    ciclo [2,5,7,9]  == [9,2,5,7]
--    ciclo []         == []
--    ciclo [2]        == [2]
-- ---------------------------------------------------------------------

--enrnarbej
ciclo :: [a] -> [a]
ciclo [] = []
ciclo xs = last xs : take (length xs - 1) xs 

-- Comentario: La definición anterior se puede simplificar.

-- ---------------------------------------------------------------------
-- Ejercicio 6.2. Comprobar que la longitud es un invariante de la
-- función ciclo; es decir, la longitud de (ciclo xs) es la misma que la
-- de xs.
-- ---------------------------------------------------------------------

-- enrnarbej
-- La propiedad es length xs == length (ciclo xs)
prop_ciclo :: [a] -> Bool 
prop_ciclo xs = length xs == length (ciclo xs)

-- La comprobación es
--    >>> quickCheck prop_ciclo
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir la función 
--    numeroMayor :: (Num a, Ord a) => a -> a -> a
-- tal que (numeroMayor x y) es el mayor número de dos cifras que puede
-- construirse con los dígitos x e y. Por ejemplo,  
--    numeroMayor 2 5 ==  52
--    numeroMayor 5 2 ==  52
-- ---------------------------------------------------------------------

-- enrnarbej
numeroMayor :: (Num a, Ord a) => a -> a -> a
numeroMayor x y | n >= m    = n
                | otherwise = m
  where
    n = x*10 + y
    m = y*10 + x

-- ---------------------------------------------------------------------
-- Ejercicio 8. Definir la función 
--    numeroDeRaices :: (Num t, Ord t) => t -> t -> t -> Int
-- tal que (numeroDeRaices a b c) es el número de raíces reales de la
-- ecuación a*x^2 + b*x + c = 0. Por ejemplo,
--    numeroDeRaices 2 0 3    ==  0
--    numeroDeRaices 4 4 1    ==  1
--    numeroDeRaices 5 23 12  ==  2
-- ---------------------------------------------------------------------

-- enrnarbej
numeroDeRaices :: (Num t, Ord t) => t -> t -> t -> Int
numeroDeRaices 0 _ _ = 1
numeroDeRaices a b c
  | discriminante > 0 = 2
  | discriminante < 0 = 0
  | otherwise         = 1
  where
    discriminante = b^2 - 4*a*c

-- ---------------------------------------------------------------------
-- Ejercicio 9.1. Definir la función 
--    raices :: Double -> Double -> Double -> [Double]
-- tal que (raices a b c) es la lista de las raíces reales de la
-- ecuación ax^2 + bx + c = 0. Por ejemplo, 
--    raices 1 3 2    ==  [-1.0,-2.0]
--    raices 1 (-2) 1 ==  [1.0,1.0]
--    raices 1 0 1    ==  []
-- ---------------------------------------------------------------------


-- enrnarbej
raices :: Double -> Double -> Double -> [Double]
raices 0 b c = [-c/b, -c/b]
raices a b c
  | (numeroDeRaices a b c /= 0) = [s1, s2]
  | otherwise                   = []
  where
    s1 = (-b + sqrt (b^2 - 4*a*c))/(2*a)
    s2 = (-b - sqrt (b^2 - 4*a*c))/(2*a)

-- ---------------------------------------------------------------------
-- Ejercicio 9.2. Definir el operador
--    (~=) :: (Fractional a, Ord a) => a -> a -> Bool
-- tal que (x ~= y) se verifica si x e y son casi iguales; es decir si
-- el valor absoluto de su diferencia es menor que una milésima. Por
-- ejemplo, 
--    12.3457 ~= 12.3459  ==  True
--    12.3457 ~= 12.3479  ==  False
-- ---------------------------------------------------------------------

-- enrnarbej paumacpar
(~=) :: (Fractional a, Ord a) => a -> a -> Bool
x ~= y = abs ( x - y ) < 0.001

-- --------------------------------------------------------------------- 
-- Ejercicio 9.3. Comprobar con QuickCheck que la suma de las raíces
-- de la ecuación ax^2 + bx + c = 0 (con a no nulo) es -b/a y su
-- producto es c/a.
--
-- Nota. En la comparación usar ~= en lugar de ==
-- ---------------------------------------------------------------------

-- enrnarbej
-- La propiedad es
prop_raices :: Double -> Double -> Double -> Property
prop_raices a b c =
  a /= 0 && numeroDeRaices a b c /= 0 ==>
  (sum (raices a b c) ~= (-b/a)) && (product (raices a b c) ~= (c/a))

-- La comprobación es
--    >>> quickCheck prop_raices
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 10. En geometría, la fórmula de Herón, descubierta por
-- Herón de Alejandría, dice que el área de un triángulo cuyo lados
-- miden a, b y c es la raíz cuadrada de s(s-a)(s-b)(s-c) donde s es el
-- semiperímetro 
--    s = (a+b+c)/2
-- 
-- Definir la función 
--    area :: Double -> Double -> Double -> Double 
-- tal que (area a b c) es el área del triángulo de lados a, b y c. Por
-- ejemplo, 
--    area 3 4 5  ==  6.0
-- ---------------------------------------------------------------------


-- enrnarbej paumacpar
area :: Double -> Double -> Double -> Double 
area a b c = sqrt (s*(s-a)*(s-b)*(s-c))
  where s = (a+b+c)/2

-- ---------------------------------------------------------------------
-- Ejercicio 11.1. Los intervalos cerrados se pueden representar mediante
-- una lista de dos números (el primero es el extremo inferior del
-- intervalo y el segundo el superior). 
-- 
-- Definir la función 
--    interseccion :: Ord a => [a] -> [a] -> [a]
-- tal que (interseccion i1 i2) es la intersección de los intervalos i1 e
-- i2. Por ejemplo,
--    interseccion [] [3,5]     ==  []
--    interseccion [3,5] []     ==  []
--    interseccion [2,4] [6,9]  ==  []
--    interseccion [2,6] [6,9]  ==  [6,6]
--    interseccion [2,6] [0,9]  ==  [2,6]
--    interseccion [2,6] [0,4]  ==  [2,4]
--    interseccion [4,6] [0,4]  ==  [4,4]
--    interseccion [5,6] [0,4]  ==  []
-- ---------------------------------------------------------------------

-- enrnarbej
interseccion ::(Enum a, Ord a) => [a] -> [a] -> [a]
interseccion [a,b] [c,d] = take 1 xs ++ drop (length xs-1) xs 
  where xs = (intersect [a..b] [c..d])

-- Comentario: La definición anterior se puede mejorar. Por ejemplo,
--    >>> :set +s
--    >>> interseccion [1,10^5] [1,10^4]
--    [1,10000]
--    (15.93 secs, 16,614,456 bytes)
-- y con otra definición
--    >>> interseccion2 [1,10^5] [1,10^4]
--    [1,10000]
--    (0.01 secs, 0 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 11.2. Comprobar con QuickCheck que la intersección de
-- intervalos es conmutativa.
-- ---------------------------------------------------------------------

-- enrnarbej
-- La propiedad es
prop_interseccion :: Int -> Int -> Int -> Int -> Property
prop_interseccion a1 b1 a2 b2 =
  a1 <= b1 && a2 <= b2 ==>
  interseccion [a1,b1] [a2,b2] == interseccion [a2,b2] [a1,b1]

-- La comprobación es 
--    >>> quickCheck prop_interseccion
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 12.1. Los números racionales pueden representarse mediante
-- pares de números enteros. Por ejemplo, el número 2/5 puede
-- representarse mediante el par (2,5). 
-- 
-- Definir la función 
--    formaReducida :: (Int,Int) -> (Int,Int) 
-- tal que (formaReducida x) es la forma reducida del número racional
-- x. Por ejemplo, 
--    formaReducida (4,10)  ==  (2,5)
--    formaReducida (0,5)   ==  (0,1)
--    formaReducida (0,-5)  ==  (0,1)
-- ---------------------------------------------------------------------

-- enrnarbej
formaReducida :: (Int,Int) -> (Int,Int)
formaReducida (_,0) = (0,0)
formaReducida (a,b)
  | a < 0 && b < 0 = formaReducida (abs a, abs b)
  | a < 0 && b > 0 = (div (abs a)  (gcd (abs a) b), (div b (gcd (abs a) b)*(-1)))
  | a > 0 && b < 0 = formaReducida (-1*a, abs b)
  | otherwise      = (div a  (gcd a b), div b (gcd a b))

-- Comentario: La definición anterior falla en el 3º ejemplo.

-- paumacpar
formaReducida2 :: (Int,Int) -> (Int, Int)
formaReducida2 (a,b) = (div a x, div b x)
  where x = gcd a b

-- Comentario: La definición anterior falla en el 3º ejemplo.

-- ---------------------------------------------------------------------
-- Ejercicio 12.2. Definir la función 
--    sumaRacional :: (Int,Int) -> (Int,Int) -> (Int,Int)
-- tal que (sumaRacional x y) es la suma de los números racionales x e
-- y, expresada en forma reducida. Por ejemplo, 
--    sumaRacional (2,3) (5,6)  ==  (3,2)
--    sumaRacional (3,5) (-3,5) ==  (0,1)
-- ---------------------------------------------------------------------

-- enrnarbej
sumaRacional :: (Int,Int) -> (Int,Int) -> (Int,Int)
sumaRacional (a,b) (c,d) = formaReducida (a*d + c*b,b*d) 

-- paumacpar 
sumaRacional2 :: (Int,Int) -> (Int,Int) -> (Int,Int)
sumaRacional2 (a,b) (c,d) =
  formaReducida((div (lcm b d) b)*a + (div (lcm b d) d)*c, lcm b d)

-- Comentario: La definición anterior se puede simplificar.
  
-- ---------------------------------------------------------------------
-- Ejercicio 12.3. Definir la función 
--    productoRacional :: (Int,Int) -> (Int,Int) -> (Int,Int)
-- tal que (productoRacional x y) es el producto de los números
-- racionales x e y, expresado en forma reducida. Por ejemplo, 
--    productoRacional (2,3) (5,6)  ==  (5,9)
-- ---------------------------------------------------------------------

-- enrnarbej paumacpar
productoRacional :: (Int,Int) -> (Int,Int) -> (Int,Int)
productoRacional (a,b) (c,d) = formaReducida (a*c,b*d)

-- ---------------------------------------------------------------------
-- Ejercicio 12.4. Definir la función 
--    igualdadRacional :: (Int,Int) -> (Int,Int) -> Bool
-- tal que (igualdadRacional x y) se verifica si los números racionales
-- x e y son iguales. Por ejemplo, 
--    igualdadRacional (6,9) (10,15)  ==  True
--    igualdadRacional (6,9) (11,15)  ==  False
--    igualdadRacional (0,2) (0,-5)   ==  True
-- ---------------------------------------------------------------------

-- enrnarbej
igualdadRacional :: (Int,Int) -> (Int,Int) -> Bool
igualdadRacional (0,_) (0,_) = True
igualdadRacional (a,b) (c,d) = formaReducida (a,b) == formaReducida (c,d)

-- paumacpar
igualdadRacional2 :: (Int,Int) -> (Int,Int) -> Bool
igualdadRacional2 (a,b) (c,d) =  formaReducida2 (a,b) == formaReducida2 (c,d)

-- ---------------------------------------------------------------------
-- Ejercicio 12.5. Comprobar con QuickCheck la propiedad distributiva
-- del producto racional respecto de la suma.
-- ---------------------------------------------------------------------

-- enrnarbej
-- La propiedad es
prop_distributiva :: (Int,Int) -> (Int,Int) -> (Int,Int) -> Property
prop_distributiva x y z =
  snd x /= 0 && snd y /= 0 && snd z /= 0 ==>
  igualdadRacional (productoRacional z (sumaRacional x y) )
                   (sumaRacional (productoRacional z x)
                                 (productoRacional z y))


-- La comprobación es
--    >>> quickCheck prop_distributiva
--    +++ OK, passed 100 tests.
