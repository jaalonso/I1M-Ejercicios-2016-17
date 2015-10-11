-- I1M 2015-16: Rel_2.hs (30 de septiembre de 2015)
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
--    http://www.cs.us.es/~jalonso/cursos/i1m-14/temas/tema-4t.pdf

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función 
--    divisionSegura :: Double -> Double -> Double
-- tal que (divisionSegura x y) es x/y si y no es cero y 9999 en caso
-- contrario. Por ejemplo,
--    divisionSegura 7 2  ==  3.5
--    divisionSegura 7 0  ==  9999.0
-- ---------------------------------------------------------------------

-- guache paocabper lucgamgal irecasmat fatvilpiz manpende pedestara
-- marcamde3 
divisionSegura :: Double -> Double -> Double
divisionSegura x y = if y /= 0 then x/y else 9999

-- Comentario: La definición anterior se puede simplificar.


-- carruirui3 carmengar alelobcan blaruiher manvermor marvilmor
-- fracruzam enrvalmor juanarcon manvazbar1 juamorrom1 javoliher
-- jespergue carboncar 
divisionSegura2 x y | y == 0    = 9999
                    | otherwise = x/y

-- alvalvdom1 pabmorgar abrdelrod rubvilval
divisionSegura3 x y | y /= 0 = x/y
                    | otherwise = 9999

-- erisancha, alebergon silgongal isrbelnun enrvalmor josllagam
-- ivaruicam 
divisionSegura4 x y | y /= 0 = x/y
                    | y == 0 = 9999

-- Comentario: La definición anterior se puede mejorar.

-- guache
divisionSegura5 x 0 = 9999
divisionSegura5 x y = x/y

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

-- carruirui3 paocabper carmengar alvalvdom1 alelobcan blaruiher
-- lucgamgal silgongal pabmorgar manvermor irecasmat fatvilpiz marvilmor
-- enrvalmor pedestara juanarcon abrdelrod enrvalmor manvazbar1
-- juamorrom1 jespergue carboncar alebergon rubvilval 
xor1 :: Bool -> Bool -> Bool
xor1 True  True  = False
xor1 True  False = True
xor1 False True  = True
xor1 False False = False

-- fracruzam manpende josllagam  isrbelnun ivaruicam javoliher marcamde3
xor1b x y | x == True  && y == True  = False
          | x == False && y == True  = True
          | x == True  && y == False = True
          | x == False && y == False = False
 
-- Comentario: La definición anterior se puede simplificar.

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Definir la función 
--    xor2 :: Bool -> Bool -> Bool
-- tal que (xor2 x y) es la disyunción excluyente de x e y, calculada a
-- partir de la tabla de verdad y patrones. Usar 2 ecuaciones, una por
-- cada valor del primer argumento. 
-- ---------------------------------------------------------------------

-- carruirui3 lucgamgal silgongal irecasmat marvilmor juanarcon manvazbar1
-- enrvalmor fracruzam javoliher jespergue carboncar rubvilval
xor2 :: Bool -> Bool -> Bool
xor2 True  y = not y
xor2 False y = y

-- paocabper alelobcan pabmorgar carmengar alvalvdom1 erisancha
-- enrvalmor blaruiher
xor2b :: Bool -> Bool -> Bool
xor2b False y = y
xor2b True  y = not y

-- ---------------------------------------------------------------------
-- Ejercicio 2.3. Definir la función 
--    xor3 :: Bool -> Bool -> Bool
-- tal que (xor3 x y) es la disyunción excluyente de x e y, calculada 
-- a partir de la disyunción (||), conjunción (&&) y negación (not). 
-- Usar 1 ecuación. 
-- ---------------------------------------------------------------------

-- carruirui3 carmengar alvalvdom1 paocabper alelobcan lucgamgal
-- marvilmor ivaruicam blaruiher pabmorgar manvermor manpende enrvalmor
-- josllagam isrbelnun juanarcon enrvalmor irecasmat manvazbar1
-- fracruzam silgongal jespergue juamorrom1 javoliher alebergon
-- rubvilval marcamde3 
xor3 :: Bool -> Bool -> Bool
xor3 x y = (x || y) && not (x && y)

-- guache
xor3b :: Bool -> Bool -> Bool
xor3b x y = not ((x&&y)|| not (x||y))

-- Comentario: La definición anterior se puede mejorar.

-- erisancha
xor3c :: Bool -> Bool -> Bool
xor3c x y = not (x&&y) && (x||y)

-- ---------------------------------------------------------------------
-- Ejercicio 2.4. Definir la función 
--    xor4 :: Bool -> Bool -> Bool
-- tal que (xor4 x y) es la disyunción excluyente de x e y, calculada
-- a partir de desigualdad (/=). Usar 1 ecuación.
-- ---------------------------------------------------------------------

-- carruirui3 lucgamgal alelobcan rubvilval
xor4 :: Bool -> Bool -> Bool
xor4 = (/=)

-- guache blaruiher marvilmor manpende pedestara manvazbar1
xor4b :: Bool -> Bool -> Bool
xor4b x y = if x/=y then True else False

-- Comentario: La definición anterior se puede simplificar.

-- paocabper 
xor4c :: Bool -> Bool -> Bool
xor4c x y = (x || y) /= (x && y) 

-- Comentario: La definición anterior se puede mejorar.

-- carmengar pabmorgar alvalvdom1 josllagam juanarcon ivaruicam
-- irecasmat enrvalmor silgongal juamorrom1 javoliher alebergon
xor4d :: Bool -> Bool -> Bool
xor4d x y = x /= y

-- manvermor erisancha
xor4e :: Bool -> Bool -> Bool
xor4e x y | x /= y    = True
          | otherwise = False

-- Comentario: La definición anterior se puede simplificar.

-- ---------------------------------------------------------------------
-- Ejercicio 2.5. Comprobar con QuickCheck que las cuatros definiciones
-- de xor son equivalentes.
-- ---------------------------------------------------------------------

-- pabmorgar alvalvdom1 carruirui3 lucgamgal manvermor manpende juanarcon
-- enrvalmor erisancha isrbelnun paocabper irecasmat enrvalmor
-- manvazbar1 silgongal juamorrom1 javoliher alebergon blaruiher

-- La propiedad es
prop_xor_equivalentes :: Bool -> Bool -> Bool
prop_xor_equivalentes x y = 
    xor1 x y == xor2 x y &&
    xor2 x y == xor3 x y && 
    xor3 x y == xor4 x y 

-- La comprobación es
--    ghci> quickCheck prop_xor_equivalentes
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

-- guache alvalvdom1 paocabper lucgamgal blaruiher irecasmat marvilmor
-- fatvilpiz manpende carboncar marcamde3
mayorRectangulo :: (Num a, Ord a) => (a,a) -> (a,a) -> (a,a)
mayorRectangulo (x,y) (u,v) = if x*y >=u*v then (x,y) else (u,v)

-- Comentario: La definición anterior se puede simplificar.

-- guache pabmorgar carruirui3 manvermor juanarcon abrdelrod ivaruicam
-- alebergon irecasmat enrvalmor manvazbar1 fracruzam silgongal
-- jespergue juamorrom1 javoliher 
mayorRectangulo2 :: (Num a, Ord a) => (a,a) -> (a,a) -> (a,a)
mayorRectangulo2 (x,y) (u,v) | x*y>=u*v  = (x,y)
                             | otherwise = (u,v)

-- alelobcan josllagam isrbelnun erisancha
mayorRectangulo3 :: (Num a, Ord a) => (a,a) -> (a,a) -> (a,a)
mayorRectangulo3 r1 r2 | fst r1*snd r1 >= fst r2*snd r2 = r1
                       | otherwise                      = r2

-- Comentario: La definición anterior se puede simplificar.

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. Definir la función 
--    intercambia :: (a,b) -> (b,a)
-- tal que (intercambia p)  es el punto obtenido intercambiando las
-- coordenadas del punto p. Por ejemplo, 
--    intercambia (2,5)  ==  (5,2)
--    intercambia (5,2)  ==  (2,5)
-- ---------------------------------------------------------------------

-- guache pabmorgar fracruzam marvilmor irecasmat carruirui3 manvermor
-- manpende juanarcon ivaruicam irecasmat enrvalmor manvazbar1 silgongal
-- javoliher juamorrom1 alebergon jespergue carboncar marcamde3
intercambia :: (a,b) -> (b,a)
intercambia (x,y) = (y,x)

-- guache 
intercambia2 :: (a,b) -> (b,a)
intercambia2 (a,b) = (snd (a,b),fst (a,b))

-- Comentario: La definición anterior se puede mejorar.

-- paocabper lucgamgal fatvilpiz
intercambia3 :: (a,b) -> (b,a)
intercambia3 (x,y) = (snd (x,y), fst (x,y))

-- Comentario: La definición anterior se puede mejorar.

-- alelobcan blaruiher alvalvdom1 josllagam isrbelnun erisancha
intercambia5 :: (a,b) -> (b,a)
intercambia5 p = (snd p,fst p)

-- Comentario: La definición anterior se puede mejorar.

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Comprobar con QuickCheck que la función intercambia es
-- idempotente; es decir, si se aplica dos veces es lo mismo que no
-- aplicarla ninguna.
-- ---------------------------------------------------------------------

-- blaruiher pabmorgar alvalvdom1 carruirui3 paocabper lucgamgal
-- fracruzam ivaruicam manpende isrbelnun erisancha josllagam fatvilpiz
-- manvermor juanarcon abrdelrod enrvalmor irecasmat manvazbar1 silgongal
-- juamorrom1 javoliher jespergue alebergon

-- La propiedad es
prop_intercambia :: (Int,Int) -> Bool
prop_intercambia p = intercambia (intercambia p) == p

-- La comprobación es 
--    *Main> quickCheck prop_intercambia
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 5.1. Definir la función 
--    distancia :: (Double,Double) -> (Double,Double) -> Double
-- tal que (distancia p1 p2) es la distancia entre los puntos p1 y
-- p2. Por ejemplo, 
--    distancia (1,2) (4,6)  ==  5.0
-- ---------------------------------------------------------------------
 
-- guache paocabper pabmorgar lucgamgal fracruzam manvermor manpende
-- juanarcon abrdelrod ivaruicam enrvalmor irecasmat manvazbar1 juamorrom1 
-- javoliher alebergon carboncar marcamde3 
distancia :: (Double,Double) -> (Double,Double) -> Double
distancia (x,y) (u,v) = sqrt((x-u)^2+(y-v)^2)

-- guache 
distancia2 :: (Double,Double) -> (Double,Double) -> Double
distancia2 (x,y) (u,v) = sqrt(x^2+y^2+u^2+v^2-2*(x*u+y*v))

-- alelobcan isrbelnun blaruiher alvalvdom1 marvilmor erisancha
-- josllagam 
distancia3 :: (Double,Double) -> (Double,Double) -> Double
distancia3 p1 p2 = sqrt((fst p1-fst p2)^2 + (snd p1 - snd p2)^2)

-- Comentario: La definición anterior se puede mejorar.

-- ---------------------------------------------------------------------
-- Ejercicio 5.2. Comprobar con QuickCheck que se verifica la propiedad
-- triangular de la distancia; es decir, dados tres puntos p1, p2 y p3,
-- la distancia de p1 a p3 es menor o igual que la suma de la distancia
-- de p1 a p2 y la de p2 a p3.
-- ---------------------------------------------------------------------

-- pabmorgar fracruzam juanarcon manvazbar1 javoliher
prop_triangular :: (Double,Double) -> (Double,Double) -> (Double,Double)
                -> Bool
prop_triangular (a,b) (c,d) (e,f) = 
    distancia (a,b) (e,f) <= 
      distancia (a,b) (c,d) + distancia (c,d) (e,f)

-- Comentario: La definición anterior se puede simplificar.

-- La comprobación es 
--    *Main> quickCheck prop_triangular
--    +++ OK, passed 100 tests.

-- alvalvdom1 carruirui3 marvilmor manvermor manpende josllagam alebergon
-- isrbelnun lucgamgal paocabper abrdelrod enrvalmor irecasmat juamorrom1
--blaruiher 
prop_triangular2 :: (Double,Double) -> (Double,Double) -> (Double,Double)
                 -> Bool
prop_triangular2 p1 p2 p3 = 
    distancia p1 p3 <= (distancia p1 p2 + distancia p2 p3)

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

-- guache manpende erisancha isrbelnun josllagam juanarcon abrdelrod
-- manvazbar1 carboncar
ciclo :: [a] -> [a]
ciclo [] = []
ciclo xs = [last xs] ++ init xs

-- Comentario: La definición anterior se puede mejorar.

-- guache 
ciclo2 [] = []
ciclo2 xs = [last xs] ++ take (length xs -1) xs

-- Comentario: La definición anterior se puede mejorar.

-- guache carruirui3 paocabper enrvalmor irecasmat fracruzam alebergon
ciclo3 [] = []
ciclo3 xs = last xs : init xs

-- guache 
ciclo4 [] = []
ciclo4 xs = last xs: take (length xs -1) xs 

-- Comentario: La definición anterior se puede mejorar.

-- guache pabmorgar marvilmor
ciclo5 :: [a] -> [a]
ciclo5 [] = []
ciclo5 xs = drop (length xs -1) xs ++ init xs

-- Comentario: La definición anterior se puede mejorar.

-- alelobcan guache lucgamgal
ciclo6 :: [a] -> [a]
ciclo6 xs = drop (length xs -1) xs ++ take (length xs -1) xs

-- Comentario: La definición anterior se puede mejorar.

-- alvalvdom1
ciclo8 :: [a] -> [a]
ciclo8 xs | null xs = []
          | otherwise = [last xs] ++ init xs

-- Comentario: La definición anterior se puede mejorar.

-- manvermor
ciclo10 :: [a] -> [a]
ciclo10 xs = [head (reverse xs)] ++ init xs

-- Comentario: La definición anterior se puede mejorar.

-- juamorrom1
ciclo11 :: [a] -> [a]
ciclo11 xs = init (last xs:xs)

-- javoliher
ciclo12 :: [a] -> [a]
ciclo12 xs | null xs   = []
           | otherwise = tail xs ++ [head xs]

-- Comentario: La definición anterior se puede simplificar.

-- marcamde3
ciclo13 :: [a] -> [a]
ciclo13 xs = drop 1 xs ++ [head xs]

-- Comentario: La definición anterior se puede simplificar.

-- ---------------------------------------------------------------------
-- Ejercicio 6.2. Comprobar que la longitud es un invariante de la
-- función ciclo; es decir, la longitud de (ciclo xs) es la misma que la
-- de xs.
-- ---------------------------------------------------------------------

-- pabmorgar carruirui3 alvalvdom1 fracruzam manpende manvermor juanarcon
-- isrbelnun josllagam lucgamgal juanarcon paocabper abrdelrod enrvalmor
-- irecasmat manvazbar1 juamorrom1 jespergue javoliher alebergon blaruiher

-- La propiedad es
prop_ciclo :: [Int] -> Bool 
prop_ciclo xs = length xs == length (ciclo xs)

-- La comprobación es
--    *Main> quickCheck prop_ciclo
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir la función 
--    numeroMayor :: (Num a, Ord a) => a -> a -> a
-- tal que (numeroMayor x y) es el mayor número de dos cifras que puede
-- construirse con los dígitos x e y. Por ejemplo,  
--    numeroMayor 2 5 ==  52
--    numeroMayor 5 2 ==  52
-- ---------------------------------------------------------------------

-- guache paocabper alelobcan marcamde3
numeroMayor :: (Num a, Ord a) => a -> a -> a
numeroMayor x y = if x>=y then 10*x + y else 10*y +x

-- Comentario: La definición anterior se puede simplificar.

-- guache pabmorgar blaruiher manvermor erisancha josllagam juanarcon
-- enrvalmor irecasmat manvazbar1 javoliher
numeroMayor2 :: (Num a, Ord a) => a -> a -> a
numeroMayor2 x y | x >= y    = 10*x + y
                 | otherwise = 10*y + x

-- guache
numeroMayor3 :: (Num a, Ord a) => a -> a -> a
numeroMayor3 x y = v + 10*u
    where v = min x y
          u = max x y

-- lucgamgal
numeroMayor4 :: (Num a, Ord a) => a -> a -> a
numeroMayor4 x y = if x<=y then 10*y + x else 10*x + y

-- Comentario: La definición anterior se puede simplificar.

-- carruirui3 fracruzam
numeroMayor5 :: (Num a, Ord a) => a -> a -> a
numeroMayor5 x y = max (10*x + y) (10*y + x)

-- alvalvdom1 juamorrom1 alebergon
numeroMayor6 :: (Num a, Ord a) => a -> a -> a
numeroMayor6 x y = max x y * 10 + min x y

-- manpende
numeroMayor7 :: (Num a, Ord a) => a -> a -> a
numeroMayor7 x y = if 10*x+y > 10*y+x then 10*x+y else 10*y+x

-- isrbelnun abrdelrod
numeroMayor8 :: (Num a, Ord a) => a -> a -> a
numeroMayor8 x y = 10*(max x y) + min x y

-- ---------------------------------------------------------------------
-- Ejercicio 8. Definir la función 
--    numeroDeRaices :: (Num t, Ord t) => t -> t -> t -> Int
-- tal que (numeroDeRaices a b c) es el número de raíces reales de la
-- ecuación a*x^2 + b*x + c = 0. Por ejemplo,
--    numeroDeRaices 2 0 3    ==  0
--    numeroDeRaices 4 4 1    ==  1
--    numeroDeRaices 5 23 12  ==  2
-- ---------------------------------------------------------------------

-- guache pabmorgar alvalvdom1 carruirui3 marvilmor juanarcon enrvalmor
-- irecasmat ivaruicam manvazbar1
numeroDeRaices :: (Num t, Ord t) => t -> t -> t -> Int
numeroDeRaices a b c | d > 0     = 2
                     | d == 0    = 1
                     | otherwise = 0
                     where d = b^2-4*a*c

-- fracruzam manvermor isrbelnun erisancha paocabper josllagam lucgamgal
-- abrdelrod juamorrom1 javoliher alebergon
numeroDeRaices3 :: (Num t, Ord t) => t -> t -> t -> Int
numeroDeRaices3 a b c | b^2-4*a*c > 0 = 2
                      | b^2-4*a*c == 0 = 1
                      | b^2-4*a*c < 0 = 0

-- Comentario: La definición anterior se puede mejorar.

-- guache manpende
numeroDeRaices4 :: (Num t, Ord t) => t -> t -> t -> Int
numeroDeRaices4 a b c = 
    if b^2- 4*a*c > 0 
    then 2 
    else if b^2- 4*a*c == 0 
         then 1 
         else 0

-- Comentario: La definición anterior se puede simplificar.

-- ---------------------------------------------------------------------
-- Ejercicio 9.1. Definir la función 
--    raices :: Double -> Double -> Double -> [Double]
-- tal que (raices a b c) es la lista de las raíces reales de la
-- ecuación ax^2 + bx + c = 0. Por ejemplo, 
--    raices 1 3 2    ==  [-1.0,-2.0]
--    raices 1 (-2) 1 ==  [1.0,1.0]
--    raices 1 0 1    ==  []
-- ---------------------------------------------------------------------

-- alvalvdom1 josllagam abrdelrod javoliher
raices :: Double -> Double -> Double -> [Double]
raices a b c 
    | b^2-4*a*c < 0 = []
    | otherwise = [(-b + sqrt (b^2-4*a*c))/(2*a)] ++ 
                  [(-b - sqrt (b^2-4*a*c))/(2*a)]

-- Comentario: La definición anterior se puede mejorar.

-- carruirui3 lucgamgal juanarcon paocabper enrvalmor manpende fracruzam 
-- juamorrom1 irecasmat alebergon
raices2 :: Double -> Double -> Double -> [Double]
raices2 a b c | d <  0 = []
              | otherwise = [(-b + sqrt d)/(2*a), (-b - sqrt d)/(2*a)]
              where d = b^2 - 4*a*c

-- ---------------------------------------------------------------------
-- Ejercicio 9.2. Definir el operador
--    (~=) :: (Fractional a, Ord a) => a -> a -> Bool
-- tal que (x ~= y) se verifica si x e y son casi iguales; es decir si
-- el valor absoluto de su diferencia es menor que una milésima. Por
-- ejemplo, 
--    12.3457 ~= 12.3459  ==  True
--    12.3457 ~= 12.3479  ==  False
-- ---------------------------------------------------------------------

-- guache paocabper blaruiher marvilmor
(~==) :: (Fractional a, Ord a) => a -> a -> Bool
x ~== y = if abs (x - y) < 0.001 then True else False

-- Comentario: La definición anterior se puede simplificar.

-- guache pabmorgar manvermor erisancha enrvalmor isrbelnun ivaruicam
-- irecasmat javoliher
(~===) :: (Fractional a, Ord a) => a -> a -> Bool
x ~=== y | abs (x - y) < 0.001   = True
         | otherwise             = False  

-- Comentario: La definición anterior se puede simplificar.

-- alvalvdom1 fracruzam manpende carruirui3 josllagam lucgamgal
-- abrdelrod juanarcon manvazbar1 juamorrom1 alebergon
x ~= y = abs(x-y) < 0.001

-- --------------------------------------------------------------------- 
-- Ejercicio 9.3. Comprobar con QuickCheck que la suma de las raíces
-- de la ecuación ax^2 + bx + c = 0 (con a no nulo) es -b/a y su
-- producto es c/a.
--
-- Nota. En la comparación usar ~= en lugar de ==
-- ---------------------------------------------------------------------

-- pabmorgar carruirui3 paocabper juanarcon manpende isrbelnun fracruzam
-- lucgamgal javoliher alebergon

-- La propiedad es
prop_raices :: Double -> Double -> Double -> Property
prop_raices a b c = 
    a /= 0 && not (null xs) ==> 
      sum xs  ~= (-b/a) && product xs ~= (c/a) 
    where xs = raices a b c 

-- La comprobación es
--    *Main> quickCheck prop_raices
--    +++ OK, passed 100 tests


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

-- guache pabmorgar alvalvdom1 fracruzam manpende manvermor carruirui3
-- erisancha josllagam lucgamgal paocabper abrdelrod juanarcon enrvalmor
-- isrbelnun manvazbar1 jespergue juamorrom1 irecasmat javoliher
-- alebergon 
area :: Double -> Double -> Double -> Double 
area a b c = sqrt(s*(s-a)*(s-b)*(s-c))
    where s = (a+b+c)/2

-- blaruiher ivaruicam
area2 :: Double -> Double -> Double -> Double 
area2 a b c = sqrt ((a+b+c)/2*((b+c-a)/2)*((a-b+c)/2)*((a+b-c)/2))

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

-- guache pabmorgar marvilmor manvermor erisancha juanarcon josllagam
-- isrbelnun irecasmat alebergon
interseccion :: Ord a => [a] -> [a] -> [a]
interseccion [] _  = []
interseccion _ []  = []
interseccion [x,y] [u,v] | max x u <= min y v = [max x u, min y v]
                         | otherwise          = []

-- manpende carruirui3 lucgamgal paocabper
interseccion1 [a,b] [c,d] = 
    if max a c <= min b d 
    then [max a c, min b d] 
    else []

-- Comentario: La definición anterior se puede simplificar.

-- ---------------------------------------------------------------------
-- Ejercicio 11.2. Comprobar con QuickCheck que la intersección de
-- intervalos es conmutativa.
-- ---------------------------------------------------------------------

-- pabmorgar erisancha carruirui3  paocabper juanarcon manpende
-- isrbelnun  lucgamgal irecasmat alebergon

-- La propiedad es
prop_interseccion :: Int -> Int -> Int -> Int -> Property
prop_interseccion a1 b1 a2 b2 =  
    a1 <= a2 && b1 <=b2  ==> 
       interseccion [a1,a2] [b1,b2] == interseccion [b1,b2] [a1,a2]

-- La comprobación es
--    ghci> quickCheck prop_interseccion
--    +++ OK, passed 100 tests

-- ---------------------------------------------------------------------
-- Ejercicio 12.1. Los números racionales pueden representarse mediante
-- pares de números enteros. Por ejemplo, el número 2/5 puede
-- representarse mediante el par (2,5). 
-- 
-- Definir la función 
--    formaReducida :: (Integer,Integer) -> (Integer,Integer) 
-- tal que (formaReducida x) es la forma reducida del número racional
-- x. Por ejemplo, 
--    formaReducida (4,10)  ==  (2,5)
--    formaReducida (0,5)   ==  (0,1)
-- ---------------------------------------------------------------------

-- guache paocabper manpende
formaReducida :: (Integer,Integer) -> (Integer,Integer)
formaReducida (a,b) = 
    if a==0 then (0,1) else (div a (gcd a b),div b (gcd a b))

-- Comentario: La definición anterior se puede mejorar.

-- guache isrbelnun ivaruicam 
formaReducida2 :: (Integer,Integer) -> (Integer,Integer)
formaReducida2 (a,b) | a == 0    = (0,1)
                     | otherwise = (div a (gcd a b),div b (gcd a b))

-- Comentario: La definición anterior se puede mejorar.

-- guache juanarcon josllagam manvazbar1 irecasmat blaruiher
formaReducida3 :: (Integer,Integer) -> (Integer,Integer)
formaReducida3 (a,b) | a == 0    = (0,1)
                     | otherwise = (div a c , div b c)
                     where c = gcd a b

-- pabmorgar blaruiher
formaReducida4 :: (Integer,Integer) -> (Integer,Integer)
formaReducida4 (a,b) | gcd a b == 1 = (a,b)
                     | otherwise = (h,k)
                     where h = a `div` gcd a b
                           k = b `div` gcd a b 

-- Comentario: La definición anterior se puede mejorar.

-- alvalvdom1 marvilmor erisancha
formaReducida5 :: (Integer,Integer) -> (Integer,Integer)
formaReducida5 (a,b) | a == 0    = (0,1)
                     | otherwise = (a `div` gcd a b, b `div` gcd a b)

-- Comentario: La definición anterior se puede mejorar.

-- manvermor carruirui3 lucgamgal abrdelrod enrvalmor juamorrom1 alebergon
formaReducida6 :: (Integer,Integer) -> (Integer,Integer) 
formaReducida6 (a,b) = (a `div` d, b `div` d)
    where d = gcd a b

-- ---------------------------------------------------------------------
-- Ejercicio 12.2. Definir la función 
--    sumaRacional :: (Integer,Integer) -> (Integer,Integer) -> 
--                    (Integer,Integer)
-- tal que (sumaRacional x y) es la suma de los números racionales x e
-- y, expresada en forma reducida. Por ejemplo, 
--    sumaRacional (2,3) (5,6)  ==  (3,2)
--    sumaRacional (3,5) (-3,5) ==  (0,1)
-- ---------------------------------------------------------------------

-- guache
sumaRacional :: (Integer,Integer) -> (Integer,Integer) ->
                (Integer,Integer)
sumaRacional (a,b) (c,d) = 
    if a*d+b*c == 0 
    then (0,1) 
    else (div u (gcd u v),div v (gcd u v))
    where u = a*d+b*c
          v = b*d

-- Comentario: La definición anterior se puede mejorar.

-- guache pabmorgar alvalvdom1 marvilmor erisancha carruirui3 lucgamgal
-- paocabper abrdelrod josllagam jespergue juanarcon manpende manvazbar1
-- isrbelnun ivaruicam uamorrom1 irecasmat blaruiher alebergon
sumaRacional2 :: (Integer,Integer) -> (Integer,Integer) -> (Integer,Integer)
sumaRacional2 (a,b)(c,d) = formaReducida (a*d + b*c, b*d)

-- manvermor
sumaRacional4 :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
sumaRacional4 (a,b)(c,d) = 
    formaReducida (((e `div` b) * a) + ((e `div `d) * c), lcm b d)
    where e = lcm b d

-- Comentario: La definición anterior se puede mejorar.

-- ---------------------------------------------------------------------
-- Ejercicio 12.3. Definir la función 
--    productoRacional :: (Integer,Integer) -> (Integer,Integer) -> 
--                        (Integer,Integer)
-- tal que (productoRacional x y) es el producto de los números
-- racionales x e y, expresado en forma reducida. Por ejemplo, 
--    productoRacional (2,3) (5,6)  ==  (5,9)
-- ---------------------------------------------------------------------

-- guache,pabmorgar alvalvdom1 manvermor marvilmor erisancha carruirui3
-- lucgamgal paocabper abrdelrod josllagam enrvalmor juanarcon manpende
--manvazbar1 isrbelnun ivaruicam jesperguejuamorrom1 irecasmat alebergon
productoRacional :: (Integer,Integer) -> (Integer,Integer) ->
                    (Integer,Integer)
productoRacional (a,b) (c,d) = formaReducida (a*c, b*d)

-- ---------------------------------------------------------------------
-- Ejercicio 12.4. Definir la función 
--    igualdadRacional :: (Integer,Integer) -> (Integer,Integer) -> Bool
-- tal que (igualdadRacional x y) se verifica si los números racionales
-- x e y son iguales. Por ejemplo, 
--    igualdadRacional (6,9) (10,15)  ==  True
--    igualdadRacional (6,9) (11,15)  ==  False
--    igualdadRacional (0,2) (0,-5)   ==  True
-- ---------------------------------------------------------------------´

-- paocabper pabmorgar fracruzam marvilmor carruirui3 lucgamgal
-- juanarcon enrvalmor isrbelnun irecasmat blaruiher alebergon
igualdadRacional :: (Integer,Integer) -> (Integer,Integer) -> Bool
igualdadRacional (a,b) (c,d) = a*d == b*c 

-- alvalvdom1 erisancha abrdelrod manpende manvazbar1 ivaruicam
-- juamorrom1
igualdadRacional7 :: (Integer,Integer) -> (Integer,Integer) -> Bool
igualdadRacional7 (a,b) (c,d) = 
    formaReducida (a,b) == formaReducida (c,d)

-- manvermor
igualdadRacional8 :: (Integer,Integer) -> (Integer,Integer) -> Bool
igualdadRacional8 (a,b) (c,d) | a*d == b*c = True
                              | otherwise  = False

-- Comentario: La definición anterior se puede simplificar.

-- ---------------------------------------------------------------------
-- Ejercicio 12.5. Comprobar con QuickCheck la propiedad distributiva
-- del producto racional respecto de la suma.
-- ---------------------------------------------------------------------

-- pabmorgar carruirui3 paocabper juanarcon enrvalmor manpende
-- manvazbar1 isrbelnun lucgamgal blaruiher alebergon

-- La propiedad es
prop_distributiva :: (Integer,Integer) -> (Integer,Integer) ->
                     (Integer,Integer) -> Property
prop_distributiva x y z = 
    snd x /= 0 && snd y /= 0 && snd z/= 0 ==>
        igualdadRacional (productoRacional x (sumaRacional y z))
                         (sumaRacional (productoRacional x y)
                                       (productoRacional x z))

-- La comprobación es
--    *Main> quickCheck prop_distributiva 
--    +++ OK, passed 100 tests
