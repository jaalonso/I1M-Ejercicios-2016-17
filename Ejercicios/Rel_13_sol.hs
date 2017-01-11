-- I1M 2016-17: Relación 13 (22 de diciembre de 2016)
-- El juego del nim y las funciones de entrada/salida. 
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

------------------------------------------------------------------------
-- § Introducción                                                     --
------------------------------------------------------------------------

-- En el juego del nim el tablero tiene 5 filas numeradas de estrellas,
-- cuyo contenido inicial es el siguiente 
--    1: * * * * * 
--    2: * * * * 
--    3: * * * 
--    4: * * 
--    5: * 
-- Dos jugadores retiran por turno una o más estrellas de una fila. El
-- ganador es el jugador que retire la última estrella. En este
-- ejercicio se va implementar el juego del Nim para practicar con las
-- funciones de entrada y salida estudiadas en el tema 13 cuyas
-- transparencias se encuentran en
--    http://www.cs.us.es/~jalonso/cursos/i1m-16/temas/tema-13.html
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

-- albcercid eliguivil paumacpar marjimcom roscargar joscasgom1 ignareeva
-- antdursan glovizcas antmorper3 josdeher eledejim2 pabrabmon natmarmar2 
-- belbenzam fatfervaz cescarde criortcar marmerzaf fraferpoy artmorfer
-- alvfercen margarvil14 natruipin congomgom carmarcar5 josrodgal7
-- beagongon1 margirmon manruiber antbeacar
finalizado :: Tablero -> Bool
finalizado t = all (== 0) t

-- juaorture
finalizado2 :: Tablero -> Bool
finalizado2 t = t == replicate 5 0

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

-- albcercid paumacpar marjimcom roscargar joscasgom1 antdursan glovizcas
-- antmorper3 josdeher pabrabmon belbenzam criortcar fraferpoy alvfercen
-- congomgom carmarcar5 beagongon1 margirmon manruiber antbeacar
valida :: Tablero -> Int -> Int -> Bool
valida t f n = t!!(f-1) >= n && n >= 1

-- eliguivil ignareeva eledejim2 marmerzaf artmorfer josrodgal7
valida2 :: Tablero -> Int -> Int -> Bool
valida2 t f n | n > 0     = t !! (f-1) >= n
              | otherwise = False

-- natmarmar2
valida3 :: Tablero -> Int -> Int -> Bool
valida3 t f 0 = False
valida3 t f n | n > length t - f = False
              | otherwise        = True

-- Comentario: La definición valida3 es incorrecta. Por ejemplo,
--    λ> valida3 [5,4,3,2,1] 1 5
--    False
--    λ> valida [5,4,3,2,1] 1 5
--    True

-- cescarde
valida4 :: Tablero -> Int -> Int -> Bool
valida4 t f n | n < 1     = False
              | otherwise = validax t f n
  where validax t f n | (!!(f-1)) t < n = False
                      | otherwise       = True

-- margarvil14
valida5 :: Tablero -> Int -> Int -> Bool
valida5 t f 0 = False 
valida5 t f n | n <= f    = True
              | otherwise = False

-- Comentario: La definición valida5 es incorrecta. Por ejemplo,
--    λ> valida5 [5,4,3,2,1] 5 2
--    True
--    λ> valida [5,4,3,2,1] 5 2
--    False
--    

-- natruipin
valida6 :: Tablero -> Int -> Int -> Bool
valida6 t f n | n > 0     = last (take f t) >= n
              | otherwise = False

-- juaorture
valida7 :: Tablero -> Int -> Int -> Bool
valida7 t f n = n >= 1 && and [a >= 0 | a <- jugada t f n]

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--    jugada :: Tablero -> Int -> Int -> Tablero
-- tal que (jugada t f n) es el tablero obtenido a partir de t
-- eliminando n estrellas de la fila f. Por ejemplo,
--    jugada [4,3,2,1,0] 2 1  ==  [4,2,2,1,0]
-- ---------------------------------------------------------------------

-- paumacpar roscargar antdursan antmorper3 belbenzam artmorfer
-- alvfercen natmarmar2 manruiber 
jugada3 :: Tablero -> Int -> Int -> Tablero
jugada3 t f n = take (f-1) t ++ (((t!!(f-1))-n) : drop f t) 

-- albcercid
jugada :: Tablero -> Int -> Int -> Tablero
jugada t f n = aux t (f-1) n
  where aux []     f n = []
        aux (x:xs) 0 n = (x-n) : aux xs (-1) n
        aux (x:xs) f n | f < 0     = (x:xs)
                       | otherwise = x:aux xs (f-1) n

-- eliguivil josdeher fraferpoy natruipin congomgom margirmon antbeacar
jugada2 :: Tablero -> Int -> Int -> Tablero
jugada2 (x:t) 1 n = (x-n) : t
jugada2 (x:t) f n = x : jugada2 t (f-1) n

-- marjimcom joscasgom1 criortcar carmarcar5 beagongon1
jugada4 :: Tablero -> Int -> Int -> Tablero
jugada4 t f n = [coger (x,y) | (x,y) <- zip t [1..]]
   where
     coger (a,b) | b /= f    = a
                 | otherwise = a-n

-- glovizcas eledejim2 pabrabmon marmerzaf margarvil14 
jugada5 :: Tablero -> Int -> Int -> Tablero
jugada5 t f n = concat [take (f-1) t, [t!!(f-1) - n], drop f t ]

-- cescarde josrodgal7
jugada6 :: Tablero -> Int -> Int -> Tablero
jugada6 t f n | valida t f n = take (f-1) t ++ ((!!(f-1)) t)-n : drop f t
              | otherwise    = t

-- juaorture
jugada7 :: Tablero -> Int -> Int -> Tablero
jugada7 t f n = [ t!!a | a <- [0..f-2]] ++ [t!!(f-1) - n] ++
                [t!!b | b <- [f.. length t - 1]]

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la acción
--    nuevaLinea :: IO ()
-- que consiste en escribir una nueva línea. Por ejemplo,
--    ghci> nuevaLinea
--    
--    ghci> 
-- ---------------------------------------------------------------------

-- juaorture congomgom
nuevaLinea :: IO ()
nuevaLinea = putChar '\n'

-- eliguivil paumacpar marjimcom antmorper3 pabrabmon cescarde
-- josrodgal7 margirmon 
nuevaLinea2 :: IO ()
nuevaLinea2 = do putStr "\n"

-- glovizcas josdeher eledejim2 belbenzam criortcar fatfervaz marmerzaf
nuevaLinea3 :: IO ()
nuevaLinea3 = putStrLn []

-- albcercid roscargar joscasgom1 ignareeva antdursan fraferpoy artmorfer
-- alvfercen margarvil14 natruipin carmarcar5 beagongon1 natmarmar2
-- manruiber 
nuevaLinea4 :: IO ()
nuevaLinea4 = do putStrLn []

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la función
--    estrellas :: Int -> String
-- tal que (estrellas n) es la cadena formada con n estrellas. Por
-- ejemplo, 
--    ghci> estrellas 3
--    "* * * "
-- ---------------------------------------------------------------------

-- juaorture
estrellas :: Int -> String
estrellas n = concat $ replicate n "* "
                              
-- glovizcas antmorper3 fatfervaz margirmon
estrellas3 :: Int -> String
estrellas3 n = concat[ "* " | _ <- [1..n]]

-- eliguivil paumacpar marjimcom antdursan ignareeva marmerzaf fraferpoy
-- congomgom
estrellas2 :: Int -> String
estrellas2 0 = ""
estrellas2 n = "* " ++ estrellas (n-1)

-- pabrabmon eledejim2 josrodgal7
estrellas4 :: Int -> String
estrellas4 n | n <= 0 = []
             | otherwise = "* " ++ estrellas4 (n-1)

-- cescarde
estrellas5 :: Int -> String
estrellas5 0 = []
estrellas5 n = '*' : ' ' : estrellas (n-1)

-- albcercid roscargar joscasgom1 josdeher criortcar alvfercen artmorfer 
-- margarvil14 natruipin carmarcar5 beagongon1 natmarmar2 manruiber
estrellas6 :: Int -> String
estrellas6 0 = []
estrellas6 n = '*' : ' ' : estrellas6 (n-1)

-- ---------------------------------------------------------------------
-- Ejercicio 6. Definir la acción
--    escribeFila :: Int -> Int -> IO ()
-- tal que (escribeFila f n) escribe en la fila f n estrellas. Por
-- ejemplo, 
--    ghci> escribeFila 2 3
--    2: * * *
-- ---------------------------------------------------------------------
 
-- albcercid eliguivil paumacpar marjimcom roscargar joscasgom1 antdursan
-- ignareeva glovizcas antmorper3 josdeher eledejim2 pabrabmon fatfervaz
-- marmerzaf alvfercen artmorfer fraferpoy margarvil14 natruipin juaorture
-- criortcar congomgom carmarcar5 beagongon1 margirmon natmarmar2 manruiber
escribeFila :: Int -> Int -> IO ()
escribeFila f n = do putStrLn ((show f) ++ ": " ++ estrellas n)  

-- Comentario: La definición escribeFila se puede simplificar.

-- cescarde
escribeFila2 :: Int -> Int -> IO ()
escribeFila2 f n =  do putStr (show f ++ ": " ++ estrellas n ++ "\n")

-- Comentario: La definición escribeFila2 se puede simplificar.

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

-- albcercid eliguivil paumacpar marjimcom roscargar joscasgom1 antdursan
-- ignareeva glovizcas antmorper3 josdeher eledejim2 pabrabmon fatfervaz
-- cescarde marmerzaf alvfercen fraferpoy artmorfer margarvil14 natruipin
-- juaorture criortcar congomgom carmarcar5 josrodgal7 beagongon1
-- margirmon natmarmar2 manruiber 
escribeTablero :: Tablero -> IO ()
escribeTablero [a,b,c,d,e] = do escribeFila 1 a
                                escribeFila 2 b
                                escribeFila 3 c
                                escribeFila 4 d
                                escribeFila 5 e

-- Comentario: La definición escribeTablero se puede simplificar.

escribeTablero2 :: Tablero -> IO ()
escribeTablero2 t = 
    sequence_ [escribeFila n (t!!(n-1)) | n <- [1..length t]]

-- ---------------------------------------------------------------------
-- Ejercicio 8. Definir la acción
--    leeDigito :: String -> IO Int
-- tal que (leeDigito c) escribe una nueva línea con la cadena c,
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

-- albcercid roscargar joscasgom1 glovizcas pabrabmon artmorfer beagongon1 
-- natmarmar2 manruiber josrodgal7 
leeDigito :: String -> IO Int
leeDigito xs = do
  putStr xs
  c <- getLine
  let x = c!!0
  if isDigit x
    then return (read c)
    else do putStrLn "ERROR: Entrada incorrecta"
            leeDigito xs

-- eliguivil josdeher
leeDigito2 :: String -> IO Int
leeDigito2 c = do
  putStr "prueba "
  s <- getLine
  if isDigit (head s)
    then do return (read s)
    else do putStrLn "ERROR: Entrada incorrecta"
            leeDigito2 c

-- paumacpar fatfervaz alvfercen criortcar margirmon
leeDigito3 :: String -> IO Int
leeDigito3 c = do
  putStr c
  s <- getLine
  let x = head s 
  case (isDigit x) of
    True  -> do return (read s)
    False -> do putStrLn "ERROR: Entrada incorrecta"
                leeDigito3 c

-- antdursan marjimcom ignareeva eledejim2 fraferpoy congomgom carmarcar5
leeDigito4 :: String -> IO Int
leeDigito4 c = do
  putStr c
  x <- getLine
  if isDigit (head x)
    then do return (read x)
    else do putStrLn "ERROR: Entrada incorrecta"
            leeDigito4 c

-- cescarde 
leeDigito5 :: String -> IO Int
leeDigito5 c = do
  putStr "Prueba "
  x <- getLine
  if isDigit (head x)
    then do return (read x)
    else do putStr "Entrada incorrecta"
            putStr "\n"
            leeDigito5 c

-- ---------------------------------------------------------------------
-- Ejercicio 9. Los jugadores se representan por los números 1 y 2.
-- Definir la función 
--    siguiente :: Int -> Int
-- tal que (siguiente j) es el jugador siguiente de j. 
-- ---------------------------------------------------------------------

-- albcercid eliguivil roscargar joscasgom1 antdursan marjimcom paumacpar
-- ignareeva glovizcas josdeher pabrabmon fatfervaz eledejim2 cescarde
-- alvfercen fraferpoy artmorfer juaorture criortcar congomgom carmarcar5
-- beagongon1 margirmon natmarmar2 manruiber josrodgal7 
siguiente :: Int -> Int
siguiente 1 = 2
siguiente 2 = 1

-- eliguivil antmorper3 natruipin
siguiente2 :: Int -> Int
siguiente2 1 = 2
siguiente2 2 = 1
siguiente2 _ = error "es un juego por parejas, solo hay 2 jugadores"

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

-- albcercid roscargar paumacpar josdeher pabrabmon eledejim2 alvfercen
-- criortcar congomgom juaorture carmarcar5 beagongon1 margirmon natmarmar2 
-- manruiber josrodgal7 
juego :: Tablero -> Int -> IO ()
juego t j =
  if elem j [1,2]
  then do nuevaLinea
          if finalizado t
            then do escribeTablero t
                    nuevaLinea
                    putStrLn ("J " ++ show (siguiente j) ++ " He ganado")
            else do nuevaLinea
                    escribeTablero t
                    nuevaLinea
                    putStrLn ("J " ++ show j)
                    putStr "Elige una fila: "
                    x <- getLine
                    putStr "Elige cuantas estrellas retiras: "
                    y <- getLine
                    let c = x!!0
                    let d = y!!0
                    nuevaLinea
                    if (isDigit c)&&(isDigit d)&&valida t (read x) (read y)
                      then juego (jugada t (read x) (read y)) (siguiente j)
                      else do putStr "Error: jugada no valida"
                              juego t j
    else putStrLn "Error, jugador no valido" 

-- eliguivil marjimcom
juego2 :: Tablero -> Int -> IO ()
juego2 t j =
  if finalizado t
  then do escribeTablero t
          putStr "\n"
          putStr ("J "++ show j ++ " He ganado")
  else do
  putStr "\n"
  escribeTablero t
  putStr "\n"
  putStrLn ("J " ++ show j)
  fase1 t
   where fase1 t = do                   
         putStr "Elige una fila: "
         f <- getLine
         if (head f) `elem` ['1'..'5']
         then do fase2 f t
         else do putStrLn "El numero debe ser entre 1 y 5"
                 fase1 t
                  where
         fase2 f t = do
         putStr "Elige cuantas estrellas retiras: "
         e <- getLine
         if valida t (read f) (read e)
         then do juego2 (jugada t (read f) (read e)) (siguiente j)
         else do putStrLn "La jugada no puede ejecutarse"
                 fase2 f t

-- cescarde
juego3 :: Tablero -> Int -> IO ()
juego3 t j
  | finalizado t = do
      escribeTablero t
      nuevaLinea
      putStr ("Enhorabuena, el J " ++ show j ++ " " ++ "HA GANADO" ++ "\n")
  | otherwise    = do
      nuevaLinea
      escribeTablero t
      nuevaLinea
      putStr ("Turno de: J " ++ show j)
      nuevaLinea
      putStr "Elija una fila, por favor: "
      x <- getLine
      putStr "Elija cuantas estrellas va a retirar: "
      y <- getLine
      nuevaLinea
      if isDigit (head x) && isDigit (head y) && valida t (read x) (read y)
        then juego3 (jugada t (read x) (read y)) (siguiente j)
        else do putStr "Jugada no valida, sorry"
                nuevaLinea
                juego3 t j

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

-- eliguivil albcercid roscargar paumacpar pabrabmon marjimcom criortcar
nim :: IO ()
nim = juego [5,4,3,2,1] 1

-- antdursan ignareeva glovizcas antmorper3 josdeher eledejim2
-- cescarde marmerzaf alvfercen congomgom juaorture carmarcar5
-- beagongon1 margirmon natmarmar2 manruiber
nim2 :: IO ()
nim2 = juego inicial 1
