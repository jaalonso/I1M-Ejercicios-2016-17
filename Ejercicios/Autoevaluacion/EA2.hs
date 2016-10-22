-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    todosDistintos :: Eq a => [a] -> Bool
-- tal que (todosDistintos xs) se verifica si todos los elementos de xs
-- son distintos. Por ejemplo,
--    todosDistintos [2,3,5,7,9]    ==  True
--    todosDistintos [2,3,5,7,9,3]  ==  False
--    todosDistintos "Betis"        ==  True
--    todosDistintos "Sevilla"      ==  False
-- ---------------------------------------------------------------------
 
--- carmarcar5
todosDistintos :: Eq a => [a] -> Bool
todosDistintos [] = True
todosDistintos (x:xs) | x `elem` xs = False
                      | otherwise   = todosDistintos xs
 
-- Comentario: La definición todosDistintos se puede simplificar.
