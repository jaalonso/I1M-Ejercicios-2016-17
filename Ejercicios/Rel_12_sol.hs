-- I1M 2015-16: Relación 12 (21 de noviembre de 2015)
-- Tipos de datos algebraicos.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- En esta relación se presenta ejercicios sobre distintos tipos de
-- datos algebraicos. Concretamente,
--    * Árboles binarios:
--      + Árboles binarios con valores en los nodos.
--      + Árboles binarios con valores en las hojas.
--      + Árboles binarios con valores en las hojas y en los nodos.
--      + Árboles booleanos.  
--    * Árboles generales
--    * Expresiones aritméticas
--      + Expresiones aritméticas básicas.
--      + Expresiones aritméticas con una variable.
--      + Expresiones aritméticas con varias variables.
--      + Expresiones aritméticas generales. 
--      + Expresiones aritméticas con tipo de operaciones.
--    * Expresiones vectoriales
-- 
-- Los ejercicios corresponden al tema 9 que se encuentran en 
--    http://www.cs.us.es/~jalonso/cursos/i1m-15/temas/tema-9.html

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Los árboles binarios con valores en los nodos se
-- pueden definir por
--    data Arbol1 a = H1 
--                  | N1 a (Arbol1 a) (Arbol1 a)
--                  deriving (Show, Eq)
-- Por ejemplo, el árbol
--         9                
--        / \    
--       /   \   
--      8     6  
--     / \   / \ 
--    3   2 4   5
-- se puede representar por
--    N1 9 (N1 8 (N1 3 H1 H1) (N1 2 H1 H1)) (N1 6 (N1 4 H1 H1) (N1 5 H1 H1))
--
-- Definir por recursión la función 
--    sumaArbol :: Num a => Arbol1 a -> a
-- tal (sumaArbol x) es la suma de los valores que hay en el árbol
-- x. Por ejemplo,
--    ghci> sumaArbol (N1 2 (N1 5 (N1 3 H1 H1) (N1 7 H1 H1)) (N1 4 H1 H1))  
--    21
-- ---------------------------------------------------------------------

data Arbol1 a = H1 
             | N1 a (Arbol1 a) (Arbol1 a)
             deriving (Show, Eq)

-- fracruzam alvalvdom1 manvermor blaruiher manpende marvilmor silgongal
-- josllagam alebergon javperlag juanarcon ivaruicam abrdelrod migandben
-- erisancha
sumaArbol :: Num a => Arbol1 a -> a
sumaArbol H1           = 0
sumaArbol (N1 a ai ad) = a + sumaArbol ai + sumaArbol ad

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Definir la función 
--    mapArbol :: (a -> b) -> Arbol1 a -> Arbol1 b
-- tal que (mapArbol f x) es el árbol que resulta de sustituir cada nodo
-- n del árbol x por (f n). Por ejemplo,
--    ghci> mapArbol (+1) (N1 2 (N1 5 (N1 3 H1 H1) (N1 7 H1 H1)) (N1 4 H1 H1))
--    N1 3 (N1 6 (N1 4 H1 H1) (N1 8 H1 H1)) (N1 5 H1 H1)
-- ---------------------------------------------------------------------

-- fracruzam alvalvdom1 manvermor blaruiher manpende marvilmor silgongal
-- josllagam alebergon javperlag juanarcon ivaruicam abrdelrod migandben
-- erisancha
mapArbol :: (a -> b) -> Arbol1 a -> Arbol1 b
mapArbol _ H1           = H1
mapArbol f (N1 a ai ad) = N1 (f a) (mapArbol f ai) (mapArbol f ad)

-- ---------------------------------------------------------------------
-- Ejercicio 1.3. Definir la función
--    ramaIzquierda :: Arbol1 a -> [a]
-- tal que (ramaIzquierda a) es la lista de los valores de los nodos de
-- la rama izquierda del árbol a. Por ejemplo,
--    ghci> ramaIzquierda (N1 2 (N1 5 (N1 3 H1 H1) (N1 7 H1 H1)) (N1 4 H1 H1))
--    [2,5,3]
-- ---------------------------------------------------------------------

-- fracruzam alvalvdom1 manvermor blaruiher manpende marvilmor silgongal
-- josllagam alebergon javperlag juanarcon ivaruicam abrdelrod migandben
-- erisancha
ramaIzquierda :: Arbol1 a -> [a]
ramaIzquierda H1           = []
ramaIzquierda (N1 a ai ad) = a : ramaIzquierda ai

-- ---------------------------------------------------------------------
-- Ejercicio 1.4. Diremos que un árbol está balanceado si para cada nodo
-- v la diferencia entre el número de nodos (con valor) de sus subárboles
-- izquierdo y derecho es menor o igual que uno.  
--
-- Definir la función 
--    balanceado :: Arbol1 a -> Bool
-- tal que (balanceado a) se verifica si el árbol a está balanceado. Por 
-- ejemplo,
--    balanceado (N1 5 H1 (N1 3 H1 H1))           == True
--    balanceado (N1 5 H1 (N1 3 (N1 4 H1 H1) H1)) == False
--    balanceado (N1 5 H1 (N1 3 H1 (N1 4 H1 H1))) == False
-- ---------------------------------------------------------------------

-- fracruzam manvermor blaruiher alvalvdom1 manpende marvilmor silgongal
-- alebergon javperlag juanarcon ivaruicam abrdelrod migandben erisancha

balanceado :: Arbol1 a -> Bool
balanceado H1 = True
balanceado (N1 a ai ad) = 
   (abs (nNodos ai - nNodos ad) <= 1) && balanceado ai && balanceado ad
   where nNodos :: Arbol1 a -> Int
         nNodos H1           = 0
         nNodos (N1 a ai ad) = 1 + nNodos ai + nNodos ad


-- ---------------------------------------------------------------------
-- Ejercicio 2. Los árboles binarios con valores en las hojas se pueden
-- definir por
--    data Arbol2 a = H2 a
--                  | N2 (Arbol2 a) (Arbol2 a) 
--                  deriving Show
-- Por ejemplo, los árboles 
--    árbol1          árbol2       árbol3     árbol4 
--       o              o           o           o    
--      / \            / \         / \         / \   
--     1   o          o   3       o   3       o   1  
--        / \        / \         / \         / \     
--       2   3      1   2       1   4       2   3    
-- se representan por
--    arbol1, arbol2, arbol3, arbol4 :: Arbol2 Int
--    arbol1 = N2 (H2 1) (N2 (H2 2) (H2 3))
--    arbol2 = N2 (N2 (H2 1) (H2 2)) (H2 3)
--    arbol3 = N2 (N2 (H2 1) (H2 4)) (H2 3)
--    arbol4 = N2 (N2 (H2 2) (H2 3)) (H2 1)
-- 
-- Definir la función
--    igualBorde :: Eq a => Arbol2 a -> Arbol2 a -> Bool
-- tal que (igualBorde t1 t2) se verifica si los bordes de los árboles
-- t1 y t2 son iguales. Por ejemplo,
--    igualBorde arbol1 arbol2  ==  True
--    igualBorde arbol1 arbol3  ==  False
--    igualBorde arbol1 arbol4  ==  False
-- ---------------------------------------------------------------------

data Arbol2 a = N2 (Arbol2 a) (Arbol2 a) 
              | H2 a
              deriving Show

arbol1, arbol2, arbol3, arbol4 :: Arbol2 Int
arbol1 = N2 (H2 1) (N2 (H2 2) (H2 3))
arbol2 = N2 (N2 (H2 1) (H2 2)) (H2 3)
arbol3 = N2 (N2 (H2 1) (H2 4)) (H2 3)
arbol4 = N2 (N2 (H2 2) (H2 3)) (H2 1)

-- fracruzam alvalvdom1 blaruiher manpende marvilmor silgongal josllagam
-- alebergon javperlag juanarcon ivaruicam abrdelrod migandben erisancha
igualBorde :: Eq a => Arbol2 a -> Arbol2 a -> Bool
igualBorde t1 t2 = borde t1 == borde t2
    where borde :: Arbol2 a -> [a]
          borde (H2 a)     = [a]
          borde (N2 ai ad) = borde ai ++ borde ad

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Los árboles binarios con valores en las hojas y en los
-- nodos se definen por
--    data Arbol3 a = H3 a
--                 | N3 a (Arbol3 a) (Arbol3 a) 
--                 deriving Show
-- Por ejemplo, los árboles
--         5              8             5           5
--        / \            / \           / \         / \
--       /   \          /   \         /   \       /   \
--      9     7        9     3       9     2     4     7
--     / \   / \      / \   / \     / \               / \
--    1   4 6   8    1   4 6   2   1   4             6   2
-- se pueden representar por
--    ej3arbol1, ej3arbol2, ej3arbol3, ej3arbol4 :: Arbol3 Int
--    ej3arbol1 = N3 5 (N3 9 (H3 1) (H3 4)) (N3 7 (H3 6) (H3 8))
--    ej3arbol2 = N3 8 (N3 9 (H3 1) (H3 4)) (N3 3 (H3 6) (H3 2))
--    ej3arbol3 = N3 5 (N3 9 (H3 1) (H3 4)) (H3 2)
--    ej3arbol4 = N3 5 (H3 4) (N3 7 (H3 6) (H3 2))
--
-- Definir la función
--    igualEstructura :: Arbol3 -> Arbol3 -> Bool
-- tal que (igualEstructura a1 a1) se verifica si los árboles a1 y a2 
-- tienen la misma estructura. Por ejemplo,
--    igualEstructura ej3arbol1 ej3arbol2 == True
--    igualEstructura ej3arbol1 ej3arbol3 == False
--    igualEstructura ej3arbol1 ej3arbol4 == False
-- ---------------------------------------------------------------------

data Arbol3 a = H3 a
              | N3 a (Arbol3 a) (Arbol3 a) 
              deriving (Show, Eq)

ej3arbol1, ej3arbol2, ej3arbol3, ej3arbol4 :: Arbol3 Int
ej3arbol1 = N3 5 (N3 9 (H3 1) (H3 4)) (N3 7 (H3 6) (H3 8))
ej3arbol2 = N3 8 (N3 9 (H3 1) (H3 4)) (N3 3 (H3 6) (H3 2))
ej3arbol3 = N3 5 (N3 9 (H3 1) (H3 4)) (H3 2)
ej3arbol4 = N3 5 (H3 4) (N3 7 (H3 6) (H3 2))

-- fracruzam
igualEstructura :: Arbol3 a -> Arbol3 a -> Bool
igualEstructura arb1 arb2 = estructura arb1 == estructura arb2
    where estructura :: Arbol3 a -> [Char]
          estructura (H3 a)       = ['H']
          estructura (N3 a ai ad) = 'N' : estructura ai ++ estructura ad

-- Comentario: La definición anterior se puede simplificar (sin
-- necesidad de construir listas).

-- fracruzam alvalvdom1 manpende marvilmor josllagam silgongal alebergon
-- javperlag juanarcon ivaruicam abrdelrod migandben erisancha

-- Misma idea que la de arriba, pero sin usar listas.
igualEstructura2 :: Arbol3 a -> Arbol3 a -> Bool
igualEstructura2 (H3 _) (H3 _)     = True
igualEstructura2 (N3 _ a b) (N3 _ c d) =
    igualEstructura2 a c && igualEstructura2 b d
igualEstructura2 _ _               = False

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Definir la función
--    algunoArbol :: Arbol3 t -> (t -> Bool) -> Bool
-- tal que (algunoArbol a p) se verifica si algún elemento del árbol a
-- cumple la propiedad p. Por ejemplo,
--    algunoArbol3 (N3 5 (N3 3 (H3 1) (H3 4)) (H3 2)) (>4)  ==  True
--    algunoArbol3 (N3 5 (N3 3 (H3 1) (H3 4)) (H3 2)) (>7)  ==  False
-- ---------------------------------------------------------------------

-- fracruzam blaruiher manpende migandben
algunoArbol :: Arbol3 a -> (a -> Bool) -> Bool
algunoArbol arb3 p = any p (arb2lista arb3)
    where arb2lista :: Arbol3 a -> [a]
          arb2lista (H3 a)       = [a]
          arb2lista (N3 a ai ad) = a : arb2lista ai ++ arb2lista ad

-- Comentario: La definición anterior se puede simplificar (sin
-- necesidad de construir listas).

-- fracruzam alvalvdom1 marvilmor silgongal josllagam alebergon javperlag
-- juanarcon ivaruicam abrdelrod erisancha

-- Misma idea que la de arriba, pero sin usar listas
algunoArbol2 :: Arbol3 a -> (a -> Bool) -> Bool
algunoArbol2 (H3 a) p       = p a
algunoArbol2 (N3 a ai ad) p = p a || algunoArbol2 ai p || algunoArbol2 ad p

-- ---------------------------------------------------------------------
-- Ejercicio 3.3. Un elemento de un árbol se dirá de nivel k si aparece
-- en el árbol a distancia k  de la raíz.  
-- 
-- Definir la función
--    nivel :: Int -> Arbol3 a -> [a]
-- tal que (nivel k a) es la lista de los elementos de nivel k del árbol
-- a. Por ejemplo,
--    nivel 0 (N3 7 (N3 2 (H3 5) (H3 4)) (H3 9))  ==  [7]
--    nivel 1 (N3 7 (N3 2 (H3 5) (H3 4)) (H3 9))  ==  [2,9]
--    nivel 2 (N3 7 (N3 2 (H3 5) (H3 4)) (H3 9))  ==  [5,4]
--    nivel 3 (N3 7 (N3 2 (H3 5) (H3 4)) (H3 9))  ==  []
-- ---------------------------------------------------------------------

-- fracruzam blaruiher alvalvdom1 manpende marvilmor silgongal josllagam
-- alebergon javperlag juanarcon ivaruicam migandben abrdelrod
nivel :: Int -> Arbol3 a -> [a]
nivel 0 (H3 a)       = [a]
nivel 0 (N3 a ai ad) = [a]
nivel n (H3 a)       = []
nivel n (N3 a ai ad) = nivel (n-1) ai ++ nivel (n-1) ad

-- erisancha
nivel2 :: Int -> Arbol3 a -> [a]
nivel2 0 (H3 a)     = [a]
nivel2 0 (N3 a _ _) = [a]
nivel2 n (H3 a)     = []
nivel2 n (N3 a b c) = nivel2 (n-1) b ++ nivel2 (n-1) c

-- ---------------------------------------------------------------------
-- Ejercicio 3.4.  Los divisores medios de un número son los que ocupan
-- la posición media entre los divisores de n, ordenados de menor a
-- mayor. Por ejemplo, los divisores de 60 son 
-- [1,2,3,4,5,6,10,12,15,20,30,60] y sus divisores medios son 6 y 10.
-- 
-- El árbol de factorización de un número compuesto n se construye de la
-- siguiente manera: 
--    * la raíz es el número n, 
--    * la rama izquierda es el árbol de factorización de su divisor
--      medio menor y
--    * la rama derecha es el árbol de factorización de su divisor
--      medio mayor
-- Si el número es primo, su árbol de factorización sólo tiene una hoja
-- con dicho número. Por ejemplo, el árbol de factorización de 60 es
--        60
--       /  \
--      6    10
--     / \   / \
--    2   3 2   5
--
-- Definir la función
--    arbolFactorizacion :: Int -> Arbol3
-- tal que (arbolFactorizacion n) es el árbol de factorización de n. Por
-- ejemplo, 
--    arbolFactorizacion 60 == N3 60 (N3 6 (H3 2) (H3 3)) (N3 10 (H3 2) (H3 5))
--    arbolFactorizacion 45 == N3 45 (H3 5) (N3 9 (H3 3) (H3 3))
--    arbolFactorizacion 7  == H3 7
--    arbolFactorizacion 14 == N3 14 (H3 2) (H3 7)
--    arbolFactorizacion 28 == N3 28 (N3 4 (H3 2) (H3 2)) (H3 7)
--    arbolFactorizacion 84 == N3 84 (H3 7) (N3 12 (H3 3) (N3 4 (H3 2) (H3 2)))
-- ---------------------------------------------------------------------

-- fracruzam silgongal 
arbolFactorizacion :: Int -> Arbol3 Int
arbolFactorizacion n | primo n    = H3 n
                     | elem 1 divsMediosn = N3 n (H3 lastdivsMediosn) 
                                                 (H3 lastdivsMediosn)
                     | otherwise  = N3 n 
                                   (arbolFactorizacion $ head divsMediosn) 
                                   (arbolFactorizacion lastdivsMediosn)
  where divsMediosn = divsMedios n
        lastdivsMediosn = last divsMediosn

primo :: Int -> Bool
primo n = divs n == [1,n]

divsMedios :: Int -> [Int]
divsMedios n = take 2 . drop (div (length $ divs n) 2 - 1) $ divs n

divs :: Int -> [Int]  
divs n = filter (\x -> mod n x == 0) [1..n]


-- ivaruicam 
arbolFactorizacion2 :: Int -> Arbol3 Int
arbolFactorizacion2 n 
    | esPrimo n = (H3 n)
    | otherwise = (N3 n 
                      (arbolFactorizacion2 (head (medianos n))) 
                      (arbolFactorizacion2 (last (medianos n))))

-- Comentario: La definición anterior se puede simplificar.

medianos :: Integral a => a -> [a]
medianos n 
    | even (b n) = 
        factores n !! ((div (b n) 2)-1) : [factores n !! (div (b n) 2)]
    | otherwise = [factores n !! (div (b n) 2)]
    where b n = length (factores n) 

-- Comentario: La definición anterior se puede simplificar.

factores :: Integral t => t -> [t]
factores n = [x | x <- [1..n] , mod n x == 0] 

esPrimo :: Int -> Bool
esPrimo n = factores n == [1,n]

-- abrdelro erisancha juanarcon 
divisoresMedios n | even (length xs) =  [xs !! (m-1), xs !! m]
                  | otherwise        = [xs !! m, xs !! m]
    where xs = [x | x <- [1..n], rem n x == 0]
          m  = div (length xs) 2

arbolFactorizacion3 :: Int -> Arbol3 Int
arbolFactorizacion3 n 
    | ds /= [1,n] = N3 n (arbolFactorizacion3 (head ds))
                         (arbolFactorizacion3 (last ds))
    | otherwise   = H3 n
    where ds = divisoresMedios n

-- ---------------------------------------------------------------------
-- Ejercicio 4. Se consideran los árboles con operaciones booleanas
-- definidos por   
--    data ArbolB = HB Bool 
--                | Conj ArbolB ArbolB
--                | Disy ArbolB ArbolB
--                | Neg ArbolB
-- 
-- Por ejemplo, los árboles
--                Conj                            Conj          
--               /   \                           /   \          
--              /     \                         /     \         
--           Disy      Conj                  Disy      Conj     
--          /   \       /  \                /   \      /   \    
--       Conj    Neg   Neg True          Conj    Neg   Neg  True 
--       /  \    |     |                 /  \    |     |        
--    True False False False          True False True  False     
--
-- se definen por
--    ej1, ej2:: ArbolB
--    ej1 = Conj (Disy (Conj (HB True) (HB False))
--                     (Neg (HB False)))
--               (Conj (Neg (HB False))
--                     (HB True))
--    
--    ej2 = Conj (Disy (Conj (HB True) (HB False))
--                     (Neg (HB True)))
--               (Conj (Neg (HB False))
--                     (HB True))
-- 
-- Definir la función 
--    valorB :: ArbolB -> Bool
-- tal que (valorB ar) es el resultado de procesar el árbol realizando
-- las operaciones booleanas especificadas en los nodos. Por ejemplo,
--    valorB ej1 == True
--    valorB ej2 == False
-- ---------------------------------------------------------------------

data ArbolB = HB Bool 
            | Conj ArbolB ArbolB
            | Disy ArbolB ArbolB
            | Neg ArbolB

ej1, ej2:: ArbolB
ej1 = Conj (Disy (Conj (HB True) (HB False))
                 (Neg (HB False)))
           (Conj (Neg (HB False))
                 (HB True))

ej2 = Conj (Disy (Conj (HB True) (HB False))
                 (Neg (HB True)))
           (Conj (Neg (HB False))
                 (HB True))

-- fracruzam alvalvdom1 josllagam alebergon manvermor blaruiher javperlag
-- juanarcon ivaruicam abrdelrod migandben erisancha
valorB:: ArbolB -> Bool
valorB (HB   b)     = b
valorB (Conj bi bd) = valorB bi && valorB bd
valorB (Disy bi bd) = valorB bi || valorB bd
valorB (Neg  b)     = not $ valorB b

-- ---------------------------------------------------------------------
-- Ejercicio 5. Los árboles generales se pueden representar mediante el
-- siguiente tipo de dato  
--    data ArbolG a = N a [ArbolG a]
--                  deriving (Eq, Show)
-- Por ejemplo, los árboles
--      1               3               3
--     / \             /|\            / | \
--    2   3           / | \          /  |  \
--        |          5  4  7        5   4   7
--        4          |     /\       |   |  / \
--                   6    2  1      6   1 2   1
--                                     / \
--                                    2   3
--                                        |
--                                        4
-- se representan por
--    ejG1, ejG2, ejG3 :: ArbolG Int
--    ejG1 = N 1 [N 2 [],N 3 [N 4 []]]
--    ejG2 = N 3 [N 5 [N 6 []], 
--               N 4 [], 
--               N 7 [N 2 [], N 1 []]]
--    ejG3 = N 3 [N 5 [N 6 []], 
--               N 4 [N 1 [N 2 [],N 3 [N 4 []]]], 
--               N 7 [N 2 [], N 1 []]]
-- 
-- Definir la función
--     ramifica :: ArbolG a -> ArbolG a -> (a -> Bool) -> ArbolG a
-- tal que (ramifica a1 a2 p) el árbol que resulta de añadir una copia
-- del árbol a2 a los nodos de a1 que cumplen un predicado p. Por
-- ejemplo, 
--    mifica ejG1 (N 8 []) (>4)
--    N 1 [N 2 [],N 3 [N 4 []]]
--    ghci> ramifica ejG1 (N 8 []) (>3)
--    N 1 [N 2 [],N 3 [N 4 [N 8 []]]]
--    ghci> ramifica ejG1 (N 8 []) (>2)
--    N 1 [N 2 [],N 3 [N 4 [N 8 []],N 8 []]]
--    ghci> ramifica ejG1 (N 8 []) (>1)
--    N 1 [N 2 [N 8 []],N 3 [N 4 [N 8 []],N 8 []]]
--    ghci> ramifica ejG1 (N 8 []) (>0)
--    N 1 [N 2 [N 8 []],N 3 [N 4 [N 8 []],N 8 []],N 8 []]
-- ---------------------------------------------------------------------

data ArbolG a = N a [ArbolG a]
              deriving (Eq, Show)

ejG1, ejG2, ejG3 :: ArbolG Int
ejG1 = N 1 [N 2 [],N 3 [N 4 []]]
ejG2 = N 3 [N 5 [N 6 []], 
           N 4 [], 
           N 7 [N 2 [], N 1 []]]
ejG3 = N 3 [N 5 [N 6 []], 
           N 4 [N 1 [N 2 [],N 3 [N 4 []]]], 
           N 7 [N 2 [], N 1 []]]

-- fracruzam javperlag ivaruicam erisancha juanarcon 
ramifica :: ArbolG a -> ArbolG a -> (a -> Bool) -> ArbolG a
ramifica (N a b) arbB p 
    | p a       = N a $ [ramifica x arbB p | x <- b]++[arbB]
    | otherwise = N a   [ramifica x arbB p | x <- b]

-- ---------------------------------------------------------------------
-- Ejercicio 6.1. Las expresiones aritméticas básicas pueden
-- representarse usando el siguiente tipo de datos  
--    data Expr1 = C1 Int 
--               | S1 Expr1 Expr1 
--               | P1 Expr1 Expr1  
--               deriving Show
-- Por ejemplo, la expresión 2*(3+7) se representa por
--    P1 (C1 2) (S1 (C1 3) (C1 7))
-- 
-- Definir la función
--    valor :: Expr1 -> Int                   
-- tal que (valor e) es el valor de la expresión aritmética e. Por
-- ejemplo, 
--    valor (P1 (C1 2) (S1 (C1 3) (C1 7)))  ==  20
-- ---------------------------------------------------------------------

data Expr1 = C1 Int 
           | S1 Expr1 Expr1 
           | P1 Expr1 Expr1  
           deriving Show
                   
-- fracruzam alvalvdom1 manvermor josllagam blaruiher javperlag
-- ivaruicam abrdelrod migandben erisancha juanarcon 
valor :: Expr1 -> Int                    
valor (C1 n)   = n
valor (S1 n m) = valor n + valor m
valor (P1 n m) = valor n * valor m

-- ---------------------------------------------------------------------
-- Ejercicio 6.2. Definir la función  
--    aplica :: (Int -> Int) -> Expr1 -> Expr1
-- tal que (aplica f e) es la expresión obtenida aplicando la función f
-- a cada uno de los números de la expresión e. Por ejemplo, 
--    ghci> aplica (+2) (S1 (P1 (C1 3) (C1 5)) (P1 (C1 6) (C1 7)))
--    S1 (P1 (C1 5) (C1 7)) (P1 (C1 8) (C1 9))
--    ghci> aplica (*2) (S1 (P1 (C1 3) (C1 5)) (P1 (C1 6) (C1 7)))
--    S1 (P1 (C1 6) (C1 10)) (P1 (C1 12) (C1 14))
-- ---------------------------------------------------------------------

-- fracruzam alvalvdom1 manvermor josllagam blaruiher javperlag
-- ivaruicam abrdelrod migandben erisancha juanarcon 
 
aplica :: (Int -> Int) -> Expr1 -> Expr1
aplica p (C1 n)   = C1 (p n)
aplica p (S1 n m) = S1 (aplica p n) (aplica p m)
aplica p (P1 n m) = P1 (aplica p n) (aplica p m)

-- ---------------------------------------------------------------------
-- Ejercicio 7.1. Las expresiones aritméticas construidas con una
-- variable (denotada por X), los números enteros y las operaciones de
-- sumar y multiplicar se pueden representar mediante el tipo de datos
-- Expr2 definido por     
--    data Expr2 = X
--               | C2 Int
--               | S2 Expr2 Expr2
--               | P2 Expr2 Expr2
-- Por ejemplo, la expresión "X*(13+X)" se representa por
-- "P2 X (S2 (C2 13) X)".
-- 
-- Definir la función 
--    valorE :: Expr2 -> Int -> Int
-- tal que (valorE e n) es el valor de la expresión e cuando se
-- sustituye su variable por n. Por ejemplo,
--    valorE (P2 X (S2 (C2 13) X)) 2  ==  30
-- ---------------------------------------------------------------------
 
data Expr2 = X
           | C2 Int
           | S2 Expr2 Expr2
           | P2 Expr2 Expr2

-- fracruzam manvermor alvalvdom1 josllagam blaruiher javperlag
-- ivaruicam abrdelrod erisancha juanarcon 

valorE :: Expr2 -> Int -> Int
valorE X n        = n
valorE (C2 n) _   = n
valorE (S2 x m) n = valorE x n + valorE m n
valorE (P2 x m) n = valorE x n * valorE m n

-- ---------------------------------------------------------------------
-- Ejercicio 7.2. Definir la función
--    numVars :: Expr2 -> Int
-- tal que (numVars e) es el número de variables en la expresión e. Por
-- ejemplo, 
--    numVars (C2 3)                 ==  0
--    numVars X                      ==  1
--    numVars (P2 X (S2 (C2 13) X))  ==  2
-- ---------------------------------------------------------------------
 
-- fracruzam manvermor alvalvdom1 josllagam blaruiher javperlag
-- ivaruicam abrdelrod erisancha juanarcon 
numVars :: Expr2 -> Int
numVars  X       = 1
numVars (C2 _)   = 0
numVars (S2 m n) = numVars m + numVars n
numVars (P2 m n) = numVars m + numVars n

-- ---------------------------------------------------------------------
-- Ejercicio 8.1. Las expresiones aritméticas con variables pueden
-- representarse usando el siguiente tipo de datos  
--    data Expr3 = C3 Int 
--               | V3 Char 
--               | S3 Expr3 Expr3 
--               | P3 Expr3 Expr3  
--               deriving Show
-- Por ejemplo, la expresión 2*(a+5) se representa por
--    P3 (C3 2) (S3 (V3 'a') (C3 5))
-- 
-- Definir la función
--    valor3 :: Expr3 -> [(Char,Int)] -> Int                   
-- tal que (valor3 x e) es el valor3 de la expresión x en el entorno e (es
-- decir, el valor3 de la expresión donde las variables de x se sustituyen
-- por los valores según se indican en el entorno e). Por ejemplo,
--    ghci> valor3 (P3 (C3 2) (S3 (V3 'a') (V3 'b'))) [('a',2),('b',5)]
--    14
-- ---------------------------------------------------------------------

data Expr3 = C3 Int 
           | V3 Char 
           | S3 Expr3 Expr3 
           | P3 Expr3 Expr3  
           deriving Show
                   
-- fracruzam
valor3 :: Expr3 -> [(Char,Int)] -> Int                   
valor3 (C3 n) val   = n
valor3 (V3 c) val   = snd $ head $ filter (\(v,_) -> v==c) val
valor3 (S3 m n) val = valor3 m val + valor3 n val
valor3 (P3 m n) val = valor3 m val * valor3 n val

-- alvalvdom1 ivaruicam abrdelrod erisancha juanarcon 
valor32 :: Expr3 -> [(Char,Int)] -> Int                   
valor32 (V3 x) e   = head [v | (c,v) <- e, c == x]
valor32 (C3 a) e   = a
valor32 (S3 a b) e = valor3 a e + valor3 b e
valor32 (P3 a b) e = valor3 a e * valor3 b e

-- ---------------------------------------------------------------------
-- Ejercicio 8.2. Definir la función
--    sumas :: Expr3 -> Int
-- tal que (sumas e) es el número de sumas en la expresión e. Por 
-- ejemplo, 
--    sumas (P3 (V3 'z') (S3 (C3 3) (V3 'x')))  ==  1
--    sumas (S3 (V3 'z') (S3 (C3 3) (V3 'x')))  ==  2
--    sumas (P3 (V3 'z') (P3 (C3 3) (V3 'x')))  ==  0
-- ---------------------------------------------------------------------
                
-- fracruzam manvermor ivaruicam abrdelrod
sumas :: Expr3 -> Int
sumas (C3 _)   = 0
sumas (V3 _)   = 0
sumas (S3 m n) = 1 + sumas m + sumas n
sumas (P3 m n) = sumas m + sumas n

-- alvalvdom1 erisancha juanarcon 
sumas2 :: Expr3 -> Int
sumas2 (S3 a b) = 1 + sumas a + sumas b
sumas2 (P3 a b) = sumas a + sumas b
sumas2 _        = 0

-- ---------------------------------------------------------------------
-- Ejercicio 8.3. Definir la función
--    sustitucion :: Expr3 -> [(Char, Int)] -> Expr3
-- tal que (sustitucion e s) es la expresión obtenida sustituyendo las
-- variables de la expresión e según se indica en la sustitución s. Por
-- ejemplo, 
--    ghci> sustitucion (P3 (V3 'z') (S3 (C3 3) (V3 'x'))) [('x',7),('z',9)]
--    P3 (C3 9) (S3 (C3 3) (C3 7))
--    ghci> sustitucion (P3 (V3 'z') (S3 (C3 3) (V3 'y'))) [('x',7),('z',9)]
--    P3 (C3 9) (S3 (C3 3) (V3 'y'))
-- ---------------------------------------------------------------------
       
-- fracruzam            
sustitucion :: Expr3 -> [(Char, Int)] -> Expr3
sustitucion (V3 c) val = aux c val
  where aux :: Char -> [(Char,Int)] -> Expr3
        aux c val | null $ valorDeC = V3 c
                  | otherwise = C3 (snd $ head $ valorDeC)
        valorDeC = filter (\(v,_) -> v == c) val
sustitucion (C3 n)  _    = C3 n
sustitucion (S3 n m) val = S3 (sustitucion n val) (sustitucion m val)
sustitucion (P3 n m) val = P3 (sustitucion n val) (sustitucion m val)

-- Comentario: La definición anterior se puede simplificar.

-- alvalvdom1 ivaruicam abrdelrod erisancha
sustitucion2 :: Expr3 -> [(Char, Int)] -> Expr3
sustitucion2 (V3 x) e 
    | null [1 | (x',_) <- e, x' == x] = V3 x
    | otherwise                       = C3 (head [v | (c,v) <- e, c == x])
sustitucion2 (C3 a) e   = C3 a
sustitucion2 (S3 a b) e = S3 (sustitucion a e) (sustitucion b e)
sustitucion2 (P3 a b) e = P3 (sustitucion a e) (sustitucion b e)

-- Comentario: La definición anterior se puede simplificar.

-- juanarcon
sustitucion3 :: Expr3 -> [(Char, Int)] -> Expr3
sustitucion3 (V3 x) e | null xs   = V3 x
                      | otherwise = C3 (head xs)
                      where xs = [v | (c,v) <- e, c == x]
sustitucion3 (C3 a) _   = C3 a
sustitucion3 (S3 a b) e = S3 (sustitucion3 a e) (sustitucion3 b e)
sustitucion3 (P3 a b) e = P3 (sustitucion3 a e) (sustitucion3 b e)

-- ---------------------------------------------------------------------
-- Ejercicio 8.4. Definir la función
--    reducible :: Expr3 -> Bool
-- tal que (reducible a) se verifica si a es una expresión reducible; es
-- decir, contiene una operación en la que los dos operandos son números. 
-- Por ejemplo,
--    reducible (S3 (C3 3) (C3 4))               == True
--    reducible (S3 (C3 3) (V3 'x'))             == False
--    reducible (S3 (C3 3) (P3 (C3 4) (C3 5)))   == True
--    reducible (S3 (V3 'x') (P3 (C3 4) (C3 5))) == True
--    reducible (S3 (C3 3) (P3 (V3 'x') (C3 5))) == False
--    reducible (C3 3)                           == False
--    reducible (V3 'x')                         == False
-- ---------------------------------------------------------------------

-- fracruzam alvalvdom1 ivaruicam abrdelrod erisancha juanarcon
reducible :: Expr3 -> Bool
reducible (S3 (C3 _) (C3 _)) = True
reducible (P3 (C3 _) (C3 _)) = True
reducible (S3 n m)           = reducible n || reducible m
reducible (P3 n m)           = reducible n || reducible m
reducible _                  = False

-- ---------------------------------------------------------------------
-- Ejercicio 9. Las expresiones aritméticas generales se pueden definir
-- usando el siguiente tipo de datos 
--    data Expr4 = C4 Int 
--               | Y 
--               | S4 Expr4 Expr4 
--               | R4 Expr4 Expr4 
--               | P4 Expr4 Expr4 
--               | E4 Expr4 Int
--               deriving (Eq, Show)
-- Por ejemplo, la expresión 
--    3*x - (x+2)^7
-- se puede definir por
--    R4 (P4 (C4 3) Y) (E4 (S4 Y (C4 2)) 7)
-- 
-- Definir la función  
--    maximo :: Expr4 -> [Int] -> (Int,[Int])
-- tal que (maximo e xs) es el par formado por el máximo valor de la
-- expresión e para los puntos de xs y en qué puntos alcanza el
-- máximo. Por ejemplo, 
--    ghci> maximo (E4 (S4 (C4 10) (P4 (R4 (C4 1) Y) Y)) 2) [-3..3]
--    (100,[0,1])
-- ---------------------------------------------------------------------

data Expr4 = C4 Int 
          | Y 
          | S4 Expr4 Expr4 
          | R4 Expr4 Expr4 
          | P4 Expr4 Expr4 
          | E4 Expr4 Int
          deriving (Eq, Show)

-- fracruzam
maximo :: Expr4 -> [Int] -> (Int,[Int])
maximo e ns = (maxi, filter (\n -> valor4 e [n] == maxi) ns)
    where maxi = maximum $ valores e ns

valor4 :: Expr4 -> [Int] -> Int
valor4 e (n:ns) = op (sust e (n:ns)) n

valores :: Expr4 -> [Int] -> [Int]
valores _ []     = []
valores e (n:ns) = valor4 e (n:ns) : valores e ns

op :: Expr4 -> Int -> Int
op Y v        = v
op (C4 n) _   = n
op (S4 n m) v = op n v + op m v
op (R4 n m) v = op n v - op m v
op (P4 n m) v = op n v * op m v
op (E4 n m) v = op n v ^ m

sust :: Expr4 -> [Int] -> Expr4
sust Y val        = C4 (head val)
sust (C4 n)  _    = C4 n
sust (S4 n m) val = S4 (sust n val) (sust m val)
sust (R4 n m) val = R4 (sust n val) (sust m val)
sust (P4 n m) val = P4 (sust n val) (sust m val)
sust (E4 n m) val = E4 (sust n val) m

-- ivaruicam erisancha juanarcon
maximo2 :: Expr4 -> [Int] -> (Int,[Int])
maximo2 e ns = (a ns,[n | n <- ns, opera(sustituye e n) == a ns])
    where a ns = maximum [opera (sustituye e n)| n <- ns]

opera :: Expr4 -> Int
opera (C4 x)   = x
opera (S4 i d) = opera i + opera d
opera (R4 i d) = opera i - opera d
opera (P4 i d) = opera i * opera d
opera (E4 i d) = (opera i)^d

sustituye :: Expr4 -> Int -> Expr4
sustituye Y x        = C4 x
sustituye (C4 x) _   = C4 x
sustituye (S4 i d) x = S4 (sustituye i x) (sustituye d x)
sustituye (R4 i d) x = R4 (sustituye i x) (sustituye d x)
sustituye (P4 i d) x = P4 (sustituye i x) (sustituye d x)
sustituye (E4 i d) x = E4 (sustituye i x) d

-- ---------------------------------------------------------------------
-- Ejercicio 10. Las operaciones de suma, resta y  multiplicación se
-- pueden representar mediante el siguiente tipo de datos 
--    data Op = Su | Re | Mu
-- La expresiones aritméticas con dichas operaciones se pueden
-- representar mediante el siguiente tipo de dato algebraico
--    data Expr5 = C5 Int 
--               | A Op Expr5 Expr
-- Por ejemplo, la expresión
--    (7-3)+(2*5)
-- se representa por
--    A Su (A Re (C5 7) (C5 3)) (A Mu (C5 2) (C5 5))
--
-- Definir la función
--    valorEG :: Expr5 -> Int
-- tal que (valorEG e) es el valorEG de la expresión e. Por ejemplo,
--    valorEG (A Su (A Re (C5 7) (C5 3)) (A Mu (C5 2) (C5 5)))  ==  14
--    valorEG (A Mu (A Re (C5 7) (C5 3)) (A Su (C5 2) (C5 5)))  ==  28
-- ---------------------------------------------------------------------

data Op = Su | Re | Mu

data Expr5 = C5 Int | A Op Expr5 Expr5

-- fracruzam alvalvdom1 ivaruicam abrdelrod erisancha juanarcon
valorEG :: Expr5 -> Int
valorEG (C5 n)     = n
valorEG (A Su n m) = valorEG n + valorEG m
valorEG (A Re n m) = valorEG n - valorEG m
valorEG (A Mu n m) = valorEG n * valorEG m

-- ---------------------------------------------------------------------
-- Ejercicio 11. Se consideran las expresiones vectoriales formadas por
-- un vector, la suma de dos expresiones vectoriales o el producto de un
-- entero por una expresión vectorial. El siguiente tipo de dato define
-- las expresiones vectoriales  
--    data ExpV = Vec Int Int
--              | Sum ExpV ExpV
--              | Mul Int ExpV
--              deriving Show
-- 
-- Definir la función 
--    valorEV :: ExpV -> (Int,Int)
-- tal que (valorEV e) es el valorEV de la expresión vectorial c. Por
-- ejemplo, 
--    valorEV (Vec 1 2)                                  ==  (1,2)
--    valorEV (Sum (Vec 1 2 ) (Vec 3 4))                 ==  (4,6)
--    valorEV (Mul 2 (Vec 3 4))                          ==  (6,8)
--    valorEV (Mul 2 (Sum (Vec 1 2 ) (Vec 3 4)))         ==  (8,12)
--    valorEV (Sum (Mul 2 (Vec 1 2)) (Mul 2 (Vec 3 4)))  ==  (8,12)
-- ---------------------------------------------------------------------

data ExpV = Vec Int Int
          | Sum ExpV ExpV
          | Mul Int ExpV
          deriving Show

-- fracruzam alvalvdom1 ivaruicam
valorEV :: ExpV -> (Int,Int)
valorEV (Vec n m)   = (n,m)
valorEV (Sum v1 v2) = mas (valorEV v1) (valorEV v2)
valorEV (Mul n v)   = por n (valorEV v)

mas :: (Int,Int) -> (Int,Int) -> (Int,Int)
mas (a,b) (c,d) = (a+c,b+d)

por :: Int -> (Int,Int) -> (Int,Int)
por n (a,b) = (n*a,n*b)

-- abrdelrod erisancha juanarcon
valorEV2 :: ExpV -> (Int,Int)
valorEV2 (Vec a b) = (a,b)
valorEV2 (Sum a b) = (fst (valorEV2 a) + fst (valorEV2 b), 
                      snd (valorEV2 a) + snd (valorEV2 b))
valorEV2 (Mul n b) = (n * fst (valorEV2 b), n * snd (valorEV2 b))
