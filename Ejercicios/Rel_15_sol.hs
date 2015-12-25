-- I1M 2015-16: Relación 15 (21 de diciembre de 2015)
-- El juego del nim y las funciones de entrada/salida. 
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

------------------------------------------------------------------------
-- § Introducción                                                     --
------------------------------------------------------------------------

-- En el juego del nim el tablero tiene 5 filas numeradas de estrellas,
-- cuyo contenido inicial es el siguiente 
--    1:      
--    2:     
--    3:    
--    4:   
--    5:  
-- Dos jugadores retiran por turno una o más estrellas de una fila. El
-- ganador es el jugador que retire la última estrella. En este
-- ejercicio se va implementar el juego del Nim para practicar con las
-- funciones de entrada y salida estudiadas en el tema 13 cuyas
-- transparencias se encuentran en
--    http://www.cs.us.es/~jalonso/cursos/i1m-15/temas/tema-13.html
--
-- Nota: El juego debe de ejecutarse en una consola, no en la shell de
-- emacs. 

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.Char

-- ---------------------------------------------------------------------
-- § Representación                                                   --
-- ---------------------------------------------------------------------
 
-- El tablero se representará como una lista de números indicando el
-- número de estrellas de cada fila. Con esta representación, el tablero
-- inicial es [5,4,3,2,1]. 

-- Representación del tablero.
type Tablero = [Int]

-- inicial es el tablero al principio del juego.
inicial ::  Tablero
inicial =  [5,4,3,2,1]

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    finalizado :: Tablero -> Bool
-- tal que (finalizado t) se verifica si t es el tablero de un juego
-- finalizado; es decir, sin estrellas. Por ejemplo,
--    finalizado [0,0,0,0,0]  ==  True
--    finalizado [1,3,0,0,1]  ==  False
-- ---------------------------------------------------------------------

-- erisancha
finalizado :: Tablero -> Bool
finalizado xs = if xs == [0,0,0,0,0] then True else False

-- Comentario: La definición anterior se puede simplificar.

-- fracruzam alvalvdom1
finalizado2 :: Tablero -> Bool
finalizado2 = all (0==)

-- ---------------------------------------------------------------------
-- Ejecicio 2.2. Definir la función
--    valida :: Tablero -> Int -> Int -> Bool
-- tal que (valida t f n) se verifica si se puede coger n estrellas en
-- la fila f del tablero t y n es mayor o igual que 1. Por ejemplo,
--    valida [4,3,2,1,0] 2 3  ==  True
--    valida [4,3,2,1,0] 2 4  ==  False
--    valida [4,3,2,1,0] 2 2  ==  True
--    valida [4,3,2,1,0] 2 0  ==  False
-- ---------------------------------------------------------------------

-- erisancha
valida :: Tablero -> Int -> Int -> Bool
valida t f n = if n >= 1 && head (drop (f-1) t) >= n then True else False

-- Comentario: La definición anterior se puede simplificar.

-- fracruzam
valida2 :: Tablero -> Int -> Int -> Bool
valida2 _ _ 0 = False
valida2 t f n = t !! (f-1) >= n

-- alvalvdom1
valida3 :: Tablero -> Int -> Int -> Bool
valida3 t f n = n > 0 && t !! (f-1) >= n

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--    jugada :: Tablero -> Int -> Int -> Tablero
-- tal que (jugada t f n) es el tablero obtenido a partir de t
-- eliminando n estrellas de la fila f. Por ejemplo,
--    jugada [4,3,2,1,0] 2 1  ==  [4,2,2,1,0]
-- ---------------------------------------------------------------------

-- erisancha
jugada :: Tablero -> Int -> Int -> Tablero
jugada t _ 0 = t
jugada t f n = if f > length t then t else init p ++ [(last p)-n] ++ drop f t
    where p = take f t

-- fracruzam alvalvdom1
jugada2 :: Tablero -> Int -> Int -> Tablero
jugada2 t f n = take (f-1) t ++ (t !! (f-1) - n) : drop f t

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la acción
--    nuevaLinea :: IO ()
-- que consiste en escribir una nueva línea. Por ejemplo,
--    ghci> nuevaLinea
--    
--    ghci> 
-- ---------------------------------------------------------------------

-- erisancha alvalvdom1
nuevaLinea :: IO ()
nuevaLinea = do putChar '\n'

-- fracruzam
nuevaLinea2 :: IO ()
nuevaLinea2 = do putStrLn ""

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la función
--    estrellas :: Int -> String
-- tal que (estrellas n) es la cadena formada con n estrellas. Por
-- ejemplo, 
--    ghci> estrellas 3
--    "* * * "
-- ---------------------------------------------------------------------

-- erisancha alvalvdom1
estrellas :: Int -> String
estrellas n = concat (replicate n "* ") 

-- fracruzam
estrellas2 :: Int -> String
estrellas2 0 = ""
estrellas2 n = "* " ++ estrellas (n-1)
                              
-- ---------------------------------------------------------------------
-- Ejercicio 6. Definir la acción
--    escribeFila :: Int -> Int -> IO ()
-- tal que (escribeFila f n) escribe en la fila f n estrellas. Por
-- ejemplo, 
--    ghci> escribeFila 2 3
--    2: * * *
-- ---------------------------------------------------------------------

-- erisancha
escribeFila :: Int -> Int -> IO ()
escribeFila f n =  do putStr (show f ++ ": ") 
                      putStrLn (estrellas n)

-- fracruzam alvalvdom1
escribeFila2 :: Int -> Int -> IO ()
escribeFila2 f n = do putStrLn ((show f) ++ ":" ++ estrellas n)

-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir la acción
--    escribeTablero :: Tablero -> IO ()
-- tal que (escribeTablero t) escribe el tablero t. Por
-- ejemplo,
--    ghci> escribeTablero [3,4,1,0,1]
--    1: * * * 
--    2: * * * * 
--    3: * 
--    4: 
--    5: * 
-- ---------------------------------------------------------------------

-- erisancha fracruzam alvalvdom1

escribeTablero :: Tablero -> IO ()
escribeTablero [a,b,c,d,e] = do escribeFila 1 a
                                escribeFila 2 b
                                escribeFila 3 c
                                escribeFila 4 d
                                escribeFila 5 e  

-- ---------------------------------------------------------------------
-- Ejercicio 8. Definir la acción
--    leeDigito :: String -> IO Int
-- tal que (leeDigito c) escribe una nueva línea con l cadena "prueba",
-- lee un carácter y comprueba que es un dígito. Además, si el carácter
-- leido es un dígito entonces devuelve el entero correspondiente y si
-- no lo es entonces escribe el mensaje "Entrada incorrecta" y vuelve a
-- leer otro carácter. Por ejemplo,  
--    ghci> leeDigito "prueba "
--    prueba 3
--    3
--    ghci> leeDigito "prueba "
--    prueba c
--    ERROR: Entrada incorrecta
--    prueba 3
--    3
-- ---------------------------------------------------------------------

-- carruirui3
-- ¿Se podría utilizar un Maybe Int para validar n?
leeDigito :: String -> IO Int
leeDigito c = do putStr c
                 n <- getChar
                 putChar '\n'
                 if isDigit n
                    then return $ digitToInt n
                    else do putStrLn "ERROR: Entrada incorrecta"
                            leeDigito c

-- fracruzam alvalvdom1
leeDigito2 :: String -> IO Int
leeDigito2 c = do putStr c
                  a <- getLine
                  if all isDigit a 
                     then return (read a) 
                     else do putStrLn "ERROR: Entrada incorrecta"  
                             leeDigito2 c
 
-- ---------------------------------------------------------------------
-- Ejercicio 9. Los jugadores se representan por los números 1 y 2.
-- Definir la función 
--    siguiente :: Int -> Int
-- tal que (siguiente j) es el jugador siguiente de j. 
-- ---------------------------------------------------------------------

-- erisancha alvalvdom1
siguiente :: Int -> Int
siguiente 1 = 2
siguiente 2 = 1

-- fracruzam
siguiente2 :: Int -> Int
siguiente2 n = n `mod` 2 + 1

-- ---------------------------------------------------------------------
-- Ejercicio 10. Definir la acción
--    juego :: Tablero -> Int -> IO ()
-- tal que (juego t j) es el juego a partir del tablero t y el turno del
-- jugador j. Por ejemplo,
--    ghci> juego [0,1,0,1,0] 2
--    
--    1: 
--    2: * 
--    3: 
--    4: * 
--    5: 
--    
--    J 2
--    Elige una fila: 2
--    Elige cuantas estrellas retiras: 1
--    
--    1: 
--    2: 
--    3: 
--    4: * 
--    5: 
--    
--    J 1
--    Elige una fila: 4
--    Elige cuantas estrellas retiras: 1
--    
--    1: 
--    2: 
--    3: 
--    4: 
--    5: 
--    
--    J 1 He ganado
-- ---------------------------------------------------------------------

-- fracruzam
juego :: Tablero -> Int -> IO ()
juego t j = do nuevaLinea
               escribeTablero t
               nuevaLinea
               putStrLn ("J " ++ show j)
               putStr "Elige una fila: " 
               f <- getLine
               putStr "Elige cuantas estrellas retiras: " 
               n <- getLine
               (juegoAux t (read f) (read n) j)

juegoAux :: Tablero -> Int -> Int -> Int -> IO ()
juegoAux t f n j 
    | finalizado sig = putStrLn ("J " ++ show j ++ " ha ganado")
    | valida t f n   = juego sig (siguiente j)
    | otherwise      = juego t j   
  where sig = jugada t f n                       

-- carruirui3 alvalvdom1
juego2 :: Tablero -> Int -> IO ()
juego2 t j = do nuevaLinea
                escribeTablero t
                nuevaLinea
                if finalizado t
                   then putStr ("J " ++ show (anterior j) ++ " He ganado")
                   else do putStr ("J " ++ show j)
                           nuevaLinea
                           f <- leeDigito "Elige una fila: "
                           n <- leeDigito "Elige cuántas estrellas retiras: "
                           if (valida t f n)
                              then juego2 (jugada t f n) (siguiente j)
                              else do putStrLn "ERROR: Jugada no válida"
                                      juego2 t j

anterior :: Int -> Int
anterior = siguiente

-- Versión para r jugadores:
-- anterior j = (j-2) `mod` r + 1

-- ---------------------------------------------------------------------
-- Ejercicio 11. Definir la acción
--    nim :: IO ()
-- consistente en una partida del nim. Por ejemplo, se puede desarrollar
-- en una consola (no en la shell de emacs) como sigue
--    ghci> nim
--    
--    1: * * * * * 
--    2: * * * * 
--    3: * * * 
--    4: * * 
--    5: * 
--    
--    J 1
--    Elige una fila: 1
--    Elige cuantas estrellas retiras: 4
--    
--    1: * 
--    2: * * * * 
--    3: * * * 
--    4: * * 
--    5: * 
--    
--    J 2
--    Elige una fila: 3
--    Elige cuantas estrellas retiras: 3
--    
--    1: * 
--    2: * * * * 
--    3: 
--    4: * * 
--    5: * 
--    
--    J 1
--    Elige una fila: 2
--    Elige cuantas estrellas retiras: 4
--    
--    1: * 
--    2: 
--    3: 
--    4: * * 
--    5: * 
--    
--    J 2
--    Elige una fila: 4
--    Elige cuantas estrellas retiras: 1
--    
--    1: * 
--    2: 
--    3: 
--    4: * 
--    5: * 
--    
--    J 1
--    Elige una fila: 1
--    Elige cuantas estrellas retiras: 1
--    
--    1: 
--    2: 
--    3: 
--    4: * 
--    5: * 
--    
--    J 2
--    Elige una fila: 4
--    Elige cuantas estrellas retiras: 1
--    
--    1: 
--    2: 
--    3: 
--    4: 
--    5: * 
--    
--    J 1
--    Elige una fila: 5
--    Elige cuantas estrellas retiras: 1
--    
--    1: 
--    2: 
--    3: 
--    4: 
--    5: 
--    
--    J 1 He ganado
-- ---------------------------------------------------------------------

-- fracruzam
nim :: IO ()
nim = juego [1..5] 1

-- rubvilval alvalvdom1
nim2 :: IO ()
nim2 = juego inicial 1
