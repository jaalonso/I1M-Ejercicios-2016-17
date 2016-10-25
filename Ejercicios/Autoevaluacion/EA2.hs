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

import Data.List

--- carmarcar5 eledejim2 margarflo5
todosDistintos :: Eq a => [a] -> Bool
todosDistintos [] = True
todosDistintos (x:xs) | x `elem` xs = False
                      | otherwise   = todosDistintos xs

-- Comentario: La definición todosDistintos se puede simplificar.

-- luimotmar
todosDistintos2 :: Eq a => [a] -> Bool
todosDistintos2 (x:[]) = True
todosDistintos2 (x:xs) | elem x xs == True = False
                       | otherwise = todosDistintos xs

-- Comentario: La definición anterior se puede simplificar.

-- juaorture
todosDistintos3 :: Eq a => [a] -> Bool 
todosDistintos3 xs =
  [a | a <- xs
     , b <- xs \\ [a]
     , a == b]
  == []

-- glovizcas marmerzaf natmarmar2 josdeher
todosDistintos4 []     = True
todosDistintos4 (x:xs) = x `notElem` xs && todosDistintos xs

-- antmorper3
todosDistintos5 :: Eq a => [a] -> Bool
todosDistintos5 xs = and [x /= n | x <- xs , n <- (delete x xs)]

-- albcercid
todosDistintos6 :: Eq a => [a] -> Bool
todosDistintos6 [] = True
todosDistintos6 (x:xs)
  | and [x /= a | a <- xs] = todosDistintos xs
  | otherwise              = False

-- eliguivil
todosDistintos7 :: Eq a => [a] -> Bool
todosDistintos7 []     = True
todosDistintos7 (x:xs) = (not (elem x xs)) && todosDistintos xs
