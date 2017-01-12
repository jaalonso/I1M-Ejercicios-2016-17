-- I1M 2016-17: Relación 14 (10 de enero de 2017)
-- Cálculo del número pi mediante el método de Montecarlo.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

------------------------------------------------------------------------
-- § Introducción                                                     --
------------------------------------------------------------------------

-- El objetivo de esta relación de ejercicios es el uso de los números
-- aleatorios para calcular el número pi mediante el método de
-- Montecarlo. Un ejemplo del método se puede leer en el artículo de
-- Pablo Rodríguez "Calculando Pi con gotas de lluvia" que se encuentra
-- en http://bit.ly/1cNfSR0 

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import System.Random
import System.IO.Unsafe
import Graphics.Gnuplot.Simple

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    aleatorio :: Random t => t -> t -> t
-- tal que (aleatorio a b) es un número aleatorio entre a y b. Por
-- ejemplo, 
--    ghci> aleatorio 0 1000
--    681
--    ghci> aleatorio 0 1000
--    66
-- ---------------------------------------------------------------------

aleatorio :: Random t => t -> t -> t
aleatorio a b = unsafePerformIO $ 
                getStdRandom (randomR (a,b))

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    aleatorios :: Random t => t -> t -> [t]
-- (aleatorios m n) es una lista infinita de números aleatorios entre m y
-- n. Por ejemplo, 
--    ghci> take 20 (aleatorios 2 9)
--    [6,5,3,9,6,3,6,6,2,7,9,6,8,6,2,4,2,6,9,4]
--    ghci> take 20 (aleatorios 2 9)
--    [3,7,7,5,7,7,5,8,6,4,7,2,8,8,2,8,7,6,5,5]
-- ---------------------------------------------------------------------

-- enrnarbej paumacpar congomgom juaorture antmorper3 marjimcom
-- josrodgal7 
aleatorios :: Random t => t -> t -> [t]
aleatorios m n = aleatorio m n : aleatorios m n

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--    puntosDelCuadrado :: [(Double,Double)]
-- tal que puntosDelCuadrado es una lista infinita de puntos del
-- cuadrado de vértices opuestos (-1,-1) y (1,1). Por ejemplo,
--    ghci> take 3 puntosDelCuadrado
--    [(0.5389481918223398,0.9385662370820778),
--     (-0.419123718392838,0.9982440984579455),
--     (0.5610432040657063,-0.7648360614536891)]
-- ---------------------------------------------------------------------

-- enrnarbej paumacpar congomgom antmorper3 marjimcom josrodgal7
puntosDelCuadrado :: [(Double,Double)]
puntosDelCuadrado = zip (aleatorios (-1.0) 1.0) (aleatorios (-1.0) 1.0)  

-- juaorture
puntosDelCuadrado2 :: [(Double,Double)]
puntosDelCuadrado2 = [(x,y) | x <- aleatorios (-1) 1, y <- aleatorios (-1) 1] 

-- Comentario: La primera coordenada es constante. Por ejemplo,
--    λ> mapM_ print (take 5 puntosDelCuadrado2)
--    (-0.9671986959078924,0.5884139668410833)
--    (-0.9671986959078924,-0.9803961883723866)
--    (-0.9671986959078924,5.565818394240751e-2)
--    (-0.9671986959078924,0.280043199439866)
--    (-0.9671986959078924,-0.4042062171348333)

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función
--    puntosEnElCirculo :: [(Double,Double)] -> Int
-- tal que (puntosEnElCirculo xs) es el número de puntos de la lista xs
-- que están en el círculo de centro (0,0) y radio 1.
--    ghci> puntosEnElCirculo [(1,0), (0.5,0.9), (0.2,-0.3)]
--    2
-- ---------------------------------------------------------------------

-- enrnarbej paumacpar congomgom antmorper3 marjimcom josrodgal7
puntosEnElCirculo :: [(Double,Double)] -> Int
puntosEnElCirculo = length . filter (\(x,y) -> x^2+y^2 <= 1)

-- juaorture
puntosEnElCirculo1 :: [(Double,Double)] -> Int
puntosEnElCirculo1 xs = length [ (x,y) | (x,y) <- xs
                                        , x^2 + y^2 <= 1]

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la función
--    calculoDePi :: Int -> Double
-- tal que (calculoDePi n) es el cálculo del número pi usando n puntos
-- aleatorios (la probabilidad de que estén en el círculo es pi/4). Por
-- ejemplo, 
--    ghci> calculoDePi 1000
--    3.076
--    ghci> calculoDePi 10000
--    3.11
--    ghci> calculoDePi 100000
--    3.13484
-- ---------------------------------------------------------------------

-- enrnarbej paumacpar congomgom juaorture antmorper3 marjimcom josrodgal7
calculoDePi :: Int -> Double
calculoDePi n =
  4
  * fromIntegral (puntosEnElCirculo (take n puntosDelCuadrado))
  / fromIntegral n


grafica = 
    plotLists [Key Nothing]
              [[(n,calculoDePi n) | n <- [0,10..4000]]
              ,[(n,pi) | n <- [0,10..4000]]]
