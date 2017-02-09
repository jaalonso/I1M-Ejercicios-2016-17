-- Problema 15 del proyecto Euler
-- Caminos en una retícula
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Comenzando en la esquina superior derecha de una rejilla de 2x2, hay
-- 6 caminos hasta la esquina inferior derecha (sin vuelta atrás).
--
-- Calcular cuántos caminos hay en una rejilla de 20x20.
-- ---------------------------------------------------------------------

-- 1ª solución (albcercid)

data Arbol a = Hoja (a,a) | Nodo (a,a) (Arbol a) (Arbol a)
        deriving Show

creaArbol (20,_) = 1
creaArbol (_,20) = 1
creaArbol (a,b) = creaArbol (a+1,b) + creaArbol (a,b+1)

euler15a =
  creaArbol (10,10)^2 + 2*sum [(creaArbol (a,20-a))^2 | a <- [0..9]]

-- Cálculo
--    λ> euler15a
--    137846528820
--    

-- 2ª solución (albcercid)

triangulo =
  [1] : zipWith (zipWith (+)) (map (++[0]) triangulo) (map (0:) triangulo)

euler15b = maximum (triangulo!!40)

-- Cálculo
--    λ> euler15b
--    137846528820


