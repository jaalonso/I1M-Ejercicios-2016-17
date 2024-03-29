-- I1M 2016-17: Relaci�n 10 (22 de noviembre de 2016)
-- Tipos de datos algebraicos.
-- Departamento de Ciencias de la Computaci�n e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducci�n                                                       --
-- ---------------------------------------------------------------------

-- En esta relaci�n se presenta ejercicios sobre distintos tipos de
-- datos algebraicos. Concretamente,
--    * �rboles binarios:
--      + �rboles binarios con valores en los nodos.
--      + �rboles binarios con valores en las hojas.
--      + �rboles binarios con valores en las hojas y en los nodos.
--      + �rboles booleanos.  
--    * �rboles generales
--    * Expresiones aritm�ticas
--      + Expresiones aritm�ticas b�sicas.
--      + Expresiones aritm�ticas con una variable.
--      + Expresiones aritm�ticas con varias variables.
--      + Expresiones aritm�ticas generales. 
--      + Expresiones aritm�ticas con tipo de operaciones.
--    * Expresiones vectoriales
-- 
-- Los ejercicios corresponden al tema 9 que se encuentran en 
--    http://www.cs.us.es/~jalonso/cursos/i1m-16/temas/tema-9.html

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Los �rboles binarios con valores en los nodos se
-- pueden definir por
--    data Arbol1 a = H1 
--                  | N1 a (Arbol1 a) (Arbol1 a)
--                  deriving (Show, Eq)
-- Por ejemplo, el �rbol
--         9                
--        / \    
--       /   \   
--      8     6  
--     / \   / \ 
--    3   2 4   5
-- se puede representar por
--    N1 9 (N1 8 (N1 3 H1 H1) (N1 2 H1 H1)) (N1 6 (N1 4 H1 H1) (N1 5 H1 H1))
--
-- Definir por recursi�n la funci�n 
--    sumaArbol :: Num a => Arbol1 a -> a
-- tal (sumaArbol x) es la suma de los valores que hay en el �rbol
-- x. Por ejemplo,
--    ghci> sumaArbol (N1 2 (N1 5 (N1 3 H1 H1) (N1 7 H1 H1)) (N1 4 H1 H1))  
--    21
-- ---------------------------------------------------------------------

data Arbol1 a = H1 
             | N1 a (Arbol1 a) (Arbol1 a)
             deriving (Show, Eq)

sumaArbol :: Num a => Arbol1 a -> a
sumaArbol = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Definir la funci�n 
--    mapArbol :: (a -> b) -> Arbol1 a -> Arbol1 b
-- tal que (mapArbol f x) es el �rbol que resulta de sustituir cada nodo
-- n del �rbol x por (f n). Por ejemplo,
--    ghci> mapArbol (+1) (N1 2 (N1 5 (N1 3 H1 H1) (N1 7 H1 H1)) (N1 4 H1 H1))
--    N1 3 (N1 6 (N1 4 H1 H1) (N1 8 H1 H1)) (N1 5 H1 H1)
-- ---------------------------------------------------------------------

mapArbol :: (a -> b) -> Arbol1 a -> Arbol1 b
mapArbol = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 1.3. Definir la funci�n
--    ramaIzquierda :: Arbol1 a -> [a]
-- tal que (ramaIzquierda a) es la lista de los valores de los nodos de
-- la rama izquierda del �rbol a. Por ejemplo,
--    ghci> ramaIzquierda (N1 2 (N1 5 (N1 3 H1 H1) (N1 7 H1 H1)) (N1 4 H1 H1))
--    [2,5,3]
-- ---------------------------------------------------------------------

ramaIzquierda :: Arbol1 a -> [a]
ramaIzquierda = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 1.4. Diremos que un �rbol est� balanceado si para cada nodo
-- v la diferencia entre el n�mero de nodos (con valor) de sus sub�rboles
-- izquierdo y derecho es menor o igual que uno.  
--
-- Definir la funci�n 
--    balanceado :: Arbol1 a -> Bool
-- tal que (balanceado a) se verifica si el �rbol a est� balanceado. Por 
-- ejemplo,
--    balanceado (N1 5 H1 (N1 3 H1 H1))           == True
--    balanceado (N1 5 H1 (N1 3 (N1 4 H1 H1) H1)) == False
-- ---------------------------------------------------------------------

balanceado :: Arbol1 a -> Bool
balanceado = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 2. Los �rboles binarios con valores en las hojas se pueden
-- definir por
--    data Arbol2 a = H2 a
--                  | N2 (Arbol2 a) (Arbol2 a) 
--                  deriving Show
-- Por ejemplo, los �rboles 
--    �rbol1          �rbol2       �rbol3     �rbol4 
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
-- Definir la funci�n
--    igualBorde :: Eq a => Arbol2 a -> Arbol2 a -> Bool
-- tal que (igualBorde t1 t2) se verifica si los bordes de los �rboles
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

igualBorde :: Eq a => Arbol2 a -> Arbol2 a -> Bool
igualBorde t1 t2 = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Los �rboles binarios con valores en las hojas y en los
-- nodos se definen por
--    data Arbol3 a = H3 a
--                 | N3 a (Arbol3 a) (Arbol3 a) 
--                 deriving Show
-- Por ejemplo, los �rboles
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
-- Definir la funci�n
--    igualEstructura :: Arbol3 -> Arbol3 -> Bool
-- tal que (igualEstructura a1 a1) se verifica si los �rboles a1 y a2 
-- tienen la misma estructura. Por ejemplo,
--    igualEstructura ej3arbol1 ej3arbol2 == True
--    igualEstructura ej3arbol1 ej3arbol3 == False
--    igualEstructura ej3arbol1 ej3arbol4 == False
-- ---------------------------------------------------------------------

data Arbol3 a = H3 a
              | N3 a (Arbol3 a) (Arbol3 a) 
              deriving Show

ej3arbol1, ej3arbol2, ej3arbol3, ej3arbol4 :: Arbol3 Int
ej3arbol1 = N3 5 (N3 9 (H3 1) (H3 4)) (N3 7 (H3 6) (H3 8))
ej3arbol2 = N3 8 (N3 9 (H3 1) (H3 4)) (N3 3 (H3 6) (H3 2))
ej3arbol3 = N3 5 (N3 9 (H3 1) (H3 4)) (H3 2)
ej3arbol4 = N3 5 (H3 4) (N3 7 (H3 6) (H3 2))

igualEstructura :: Arbol3 a -> Arbol3 a -> Bool
igualEstructura = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Definir la funci�n
--    algunoArbol :: Arbol3 t -> (t -> Bool) -> Bool
-- tal que (algunoArbol a p) se verifica si alg�n elemento del �rbol a
-- cumple la propiedad p. Por ejemplo,
--    algunoArbol3 (N3 5 (N3 3 (H3 1) (H3 4)) (H3 2)) (>4)  ==  True
--    algunoArbol3 (N3 5 (N3 3 (H3 1) (H3 4)) (H3 2)) (>7)  ==  False
-- ---------------------------------------------------------------------

algunoArbol :: Arbol3 a -> (a -> Bool) -> Bool
algunoArbol = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 3.3. Un elemento de un �rbol se dir� de nivel k si aparece
-- en el �rbol a distancia k  de la ra�z.  
-- 
-- Definir la funci�n
--    nivel :: Int -> Arbol3 a -> [a]
-- tal que (nivel k a) es la lista de los elementos de nivel k del �rbol
-- a. Por ejemplo,
--    nivel 0 (N3 7 (N3 2 (H3 5) (H3 4)) (H3 9))  ==  [7]
--    nivel 1 (N3 7 (N3 2 (H3 5) (H3 4)) (H3 9))  ==  [2,9]
--    nivel 2 (N3 7 (N3 2 (H3 5) (H3 4)) (H3 9))  ==  [5,4]
--    nivel 3 (N3 7 (N3 2 (H3 5) (H3 4)) (H3 9))  ==  []
-- ---------------------------------------------------------------------

nivel :: Int -> Arbol3 a -> [a]
nivel = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 3.4.  Los divisores medios de un n�mero son los que ocupan
-- la posici�n media entre los divisores de n, ordenados de menor a
-- mayor. Por ejemplo, los divisores de 60 son 
-- [1,2,3,4,5,6,10,12,15,20,30,60] y sus divisores medios son 6 y 10.
-- 
-- El �rbol de factorizaci�n de un n�mero compuesto n se construye de la
-- siguiente manera: 
--    * la ra�z es el n�mero n, 
--    * la rama izquierda es el �rbol de factorizaci�n de su divisor
--      medio menor y
--    * la rama derecha es el �rbol de factorizaci�n de su divisor
--      medio mayor
-- Si el n�mero es primo, su �rbol de factorizaci�n s�lo tiene una hoja
-- con dicho n�mero. Por ejemplo, el �rbol de factorizaci�n de 60 es
--        60
--       /  \
--      6    10
--     / \   / \
--    2   3 2   5
--
-- Definir la funci�n
--    arbolFactorizacion :: Int -> Arbol3
-- tal que (arbolFactorizacion n) es el �rbol de factorizaci�n de n. Por
-- ejemplo, 
--    arbolFactorizacion 60 == N3 60 (N3 6 (H3 2) (H3 3)) (N3 10 (H3 2) (H3 5))
--    arbolFactorizacion 45 == N3 45 (H3 5) (N3 9 (H3 3) (H3 3))
--    arbolFactorizacion 7  == H3 7
--    arbolFactorizacion 14 == N3 14 (H3 2) (H3 7)
--    arbolFactorizacion 28 == N3 28 (N3 4 (H3 2) (H3 2)) (H3 7)
--    arbolFactorizacion 84 == N3 84 (H3 7) (N3 12 (H3 3) (N3 4 (H3 2) (H3 2)))
-- ---------------------------------------------------------------------

arbolFactorizacion :: Int -> Arbol3 Int
arbolFactorizacion n = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 4. Se consideran los �rboles con operaciones booleanas
-- definidos por   
--    data ArbolB = HB Bool 
--                | Conj ArbolB ArbolB
--                | Disy ArbolB ArbolB
--                | Neg ArbolB
-- 
-- Por ejemplo, los �rboles
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
-- Definir la funci�n 
--    valorB :: ArbolB -> Bool
-- tal que (valorB ar) es el resultado de procesar el �rbol realizando
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

valorB:: ArbolB -> Bool
valorB = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 5. Los �rboles generales se pueden representar mediante el
-- siguiente tipo de dato  
--    data ArbolG a = N a [ArbolG a]
--                  deriving (Eq, Show)
-- Por ejemplo, los �rboles
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
-- Definir la funci�n
--     ramifica :: ArbolG a -> ArbolG a -> (a -> Bool) -> ArbolG a
-- tal que (ramifica a1 a2 p) el �rbol que resulta de a�adir una copia
-- del �rbol a2 a los nodos de a1 que cumplen un predicado p. Por
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

ramifica :: ArbolG a -> ArbolG a -> (a -> Bool) -> ArbolG a
ramifica = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 6.1. Las expresiones aritm�ticas b�sicas pueden
-- representarse usando el siguiente tipo de datos  
--    data Expr1 = C1 Int 
--               | S1 Expr1 Expr1 
--               | P1 Expr1 Expr1  
--               deriving Show
-- Por ejemplo, la expresi�n 2*(3+7) se representa por
--    P1 (C1 2) (S1 (C1 3) (C1 7))
-- 
-- Definir la funci�n
--    valor :: Expr1 -> Int                   
-- tal que (valor e) es el valor de la expresi�n aritm�tica e. Por
-- ejemplo, 
--    valor (P1 (C1 2) (S1 (C1 3) (C1 7)))  ==  20
-- ---------------------------------------------------------------------

data Expr1 = C1 Int 
           | S1 Expr1 Expr1 
           | P1 Expr1 Expr1  
           deriving Show
                   
valor :: Expr1 -> Int                   
valor = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 6.2. Definir la funci�n  
--    aplica :: (Int -> Int) -> Expr1 -> Expr1
-- tal que (aplica f e) es la expresi�n obtenida aplicando la funci�n f
-- a cada uno de los n�meros de la expresi�n e. Por ejemplo, 
--    ghci> aplica (+2) (S1 (P1 (C1 3) (C1 5)) (P1 (C1 6) (C1 7)))
--    S1 (P1 (C1 5) (C1 7)) (P1 (C1 8) (C1 9))
--    ghci> aplica (*2) (S1 (P1 (C1 3) (C1 5)) (P1 (C1 6) (C1 7)))
--    S1 (P1 (C1 6) (C1 10)) (P1 (C1 12) (C1 14))
-- ---------------------------------------------------------------------

aplica :: (Int -> Int) -> Expr1 -> Expr1
aplica = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 7.1. Las expresiones aritm�ticas construidas con una
-- variable (denotada por X), los n�meros enteros y las operaciones de
-- sumar y multiplicar se pueden representar mediante el tipo de datos
-- Expr2 definido por     
--    data Expr2 = X
--               | C2 Int
--               | S2 Expr2 Expr2
--               | P2 Expr2 Expr2
-- Por ejemplo, la expresi�n "X*(13+X)" se representa por
-- "P2 X (S2 (C2 13) X)".
-- 
-- Definir la funci�n 
--    valorE :: Expr2 -> Int -> Int
-- tal que (valorE e n) es el valor de la expresi�n e cuando se
-- sustituye su variable por n. Por ejemplo,
--    valorE (P2 X (S2 (C2 13) X)) 2  ==  30
-- ---------------------------------------------------------------------
 
data Expr2 = X
           | C2 Int
           | S2 Expr2 Expr2
           | P2 Expr2 Expr2

valorE :: Expr2 -> Int -> Int
valorE = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 7.2. Definir la funci�n
--    numVars :: Expr2 -> Int
-- tal que (numVars e) es el n�mero de variables en la expresi�n e. Por
-- ejemplo, 
--    numVars (C2 3)                 ==  0
--    numVars X                      ==  1
--    numVars (P2 X (S2 (C2 13) X))  ==  2
-- ---------------------------------------------------------------------

numVars :: Expr2 -> Int
numVars = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 8.1. Las expresiones aritm�ticas con variables pueden
-- representarse usando el siguiente tipo de datos  
--    data Expr3 = C3 Int 
--               | V3 Char 
--               | S3 Expr3 Expr3 
--               | P3 Expr3 Expr3  
--               deriving Show
-- Por ejemplo, la expresi�n 2*(a+5) se representa por
--    P3 (C3 2) (S3 (V3 'a') (C3 5))
-- 
-- Definir la funci�n
--    valor3 :: Expr3 -> [(Char,Int)] -> Int                   
-- tal que (valor3 x e) es el valor3 de la expresi�n x en el entorno e (es
-- decir, el valor3 de la expresi�n donde las variables de x se sustituyen
-- por los valores seg�n se indican en el entorno e). Por ejemplo,
--    ghci> valor3 (P3 (C3 2) (S3 (V3 'a') (V3 'b'))) [('a',2),('b',5)]
--    14
-- ---------------------------------------------------------------------

data Expr3 = C3 Int 
           | V3 Char 
           | S3 Expr3 Expr3 
           | P3 Expr3 Expr3  
           deriving Show
                   
valor3 :: Expr3 -> [(Char,Int)] -> Int                   
valor3 = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 8.2. Definir la funci�n
--    sumas :: Expr3 -> Int
-- tal que (sumas e) es el n�mero de sumas en la expresi�n e. Por 
-- ejemplo, 
--    sumas (P3 (V3 'z') (S3 (C3 3) (V3 'x')))  ==  1
--    sumas (S3 (V3 'z') (S3 (C3 3) (V3 'x')))  ==  2
--    sumas (P3 (V3 'z') (P3 (C3 3) (V3 'x')))  ==  0
-- ---------------------------------------------------------------------
                   
sumas :: Expr3 -> Int
sumas = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 8.3. Definir la funci�n
--    sustitucion :: Expr3 -> [(Char, Int)] -> Expr3
-- tal que (sustitucion e s) es la expresi�n obtenida sustituyendo las
-- variables de la expresi�n e seg�n se indica en la sustituci�n s. Por
-- ejemplo, 
--    ghci> sustitucion (P3 (V3 'z') (S3 (C3 3) (V3 'x'))) [('x',7),('z',9)]
--    P3 (C3 9) (S3 (C3 3) (C3 7))
--    ghci> sustitucion (P3 (V3 'z') (S3 (C3 3) (V3 'y'))) [('x',7),('z',9)]
--    P3 (C3 9) (S3 (C3 3) (V3 'y'))
-- ---------------------------------------------------------------------
                   
sustitucion :: Expr3 -> [(Char, Int)] -> Expr3
sustitucion = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 8.4. Definir la funci�n
--    reducible :: Expr3 -> Bool
-- tal que (reducible a) se verifica si a es una expresi�n reducible; es
-- decir, contiene una operaci�n en la que los dos operandos son n�meros. 
-- Por ejemplo,
--    reducible (S3 (C3 3) (C3 4))               == True
--    reducible (S3 (C3 3) (V3 'x'))             == False
--    reducible (S3 (C3 3) (P3 (C3 4) (C3 5)))   == True
--    reducible (S3 (V3 'x') (P3 (C3 4) (C3 5))) == True
--    reducible (S3 (C3 3) (P3 (V3 'x') (C3 5))) == False
--    reducible (C3 3)                           == False
--    reducible (V3 'x')                         == False
-- ---------------------------------------------------------------------

reducible :: Expr3 -> Bool
reducible = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 9. Las expresiones aritm�ticas generales se pueden definir
-- usando el siguiente tipo de datos 
--    data Expr4 = C4 Int 
--               | Y 
--               | S4 Expr4 Expr4 
--               | R4 Expr4 Expr4 
--               | P4 Expr4 Expr4 
--               | E4 Expr4 Int
--               deriving (Eq, Show)
-- Por ejemplo, la expresi�n 
--    3*x - (x+2)^7
-- se puede definir por
--    R4 (P4 (C4 3) Y) (E4 (S4 Y (C4 2)) 7)
-- 
-- Definir la funci�n  
--    maximo :: Expr4 -> [Int] -> (Int,[Int])
-- tal que (maximo e xs) es el par formado por el m�ximo valor de la
-- expresi�n e para los puntos de xs y en qu� puntos alcanza el
-- m�ximo. Por ejemplo, 
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

maximo :: Expr4 -> [Int] -> (Int,[Int])
maximo e ns = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 10. Las operaciones de suma, resta y  multiplicaci�n se
-- pueden representar mediante el siguiente tipo de datos 
--    data Op = Su | Re | Mu
-- La expresiones aritm�ticas con dichas operaciones se pueden
-- representar mediante el siguiente tipo de dato algebraico
--    data Expr5 = C5 Int 
--               | A Op Expr5 Expr
-- Por ejemplo, la expresi�n
--    (7-3)+(2*5)
-- se representa por
--    A Su (A Re (C5 7) (C5 3)) (A Mu (C5 2) (C5 5))
--
-- Definir la funci�n
--    valorEG :: Expr5 -> Int
-- tal que (valorEG e) es el valorEG de la expresi�n e. Por ejemplo,
--    valorEG (A Su (A Re (C5 7) (C5 3)) (A Mu (C5 2) (C5 5)))  ==  14
--    valorEG (A Mu (A Re (C5 7) (C5 3)) (A Su (C5 2) (C5 5)))  ==  28
-- ---------------------------------------------------------------------

data Op = Su | Re | Mu

data Expr5 = C5 Int | A Op Expr5 Expr5

valorEG :: Expr5 -> Int
valorEG = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 11. Se consideran las expresiones vectoriales formadas por
-- un vector, la suma de dos expresiones vectoriales o el producto de un
-- entero por una expresi�n vectorial. El siguiente tipo de dato define
-- las expresiones vectoriales  
--    data ExpV = Vec Int Int
--              | Sum ExpV ExpV
--              | Mul Int ExpV
--              deriving Show
-- 
-- Definir la funci�n 
--    valorEV :: ExpV -> (Int,Int)
-- tal que (valorEV e) es el valorEV de la expresi�n vectorial c. Por
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

valorEV :: ExpV -> (Int,Int)
valorEV = undefined
