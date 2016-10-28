-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    todosIguales :: Eq a => [a] -> Bool
-- tal que (todosIguales xs) se verifica si todos los elementos de xs
-- son iguales. Por ejemplo,
--    todosIguales [2,2,2,2]  ==  True
--    todosIguales "oso"      ==  False
--    todosIguales []         ==  True
-- ---------------------------------------------------------------------

-- enrnarbej antmorper3 pabrabmon joscasgom1 manruiber roscargar
-- artmorfer fraferpoy albagucen cargonler natmarmar2 eliguivil fatfervaz
todosIguales :: Eq a => [a] -> Bool
todosIguales []     = True
todosIguales (x:xs) = and [x == y | y <- xs]

-- albcercid glovizcas beagongon1 cargonler eliguivil josdeher juacasnie
-- natruipin marjimcom margirmon carmarcar5 josjimgon2 antdursan roscargar
todosIguales2 :: Eq a => [a] -> Bool
todosIguales2 xs = and [x == y | (x,y) <- zip xs (tail xs)]

-- paumacpar mardelrui eledejim2 migibagar margarflo5 marmerzaf
todosIguales3 :: Eq a => [a] -> Bool
todosIguales3 xs = and [x == y | (x,y) <- adyacentes3 xs]

adyacentes3 :: [a] -> [(a,a)]
adyacentes3 xs= zip xs (tail xs) 

-- juaorture
todosIguales4 :: (Eq a)=> [a] -> Bool
todosIguales4 xs =
  (length xs)^2 == length [(a,b) | a <- xs
                                 , b <- xs
                                 , a == b ]

-- josrodgal7
todosIguales5a :: Eq a => [a] -> Bool
todosIguales5a xs = and [a == b | a <- xs, b <- xs ]

todosIguales5b :: Eq a => [a] -> Bool
todosIguales5b []     = True
todosIguales5b (x:xs) = and [x == a | a <- xs] && todosIguales5b xs   


-- luimotmar
todosIguales6 :: Eq a => [a] -> Bool
todosIguales6 (x:[]) = True
todosIguales6 (x:xs)  | [x] == take 1 xs  = todosIguales6 xs
                      | [x] /= take 1 xs  = False

-- Comentario: La definición anterior se puede simplificar. Además, le
-- falta el caso en que el argumento es la lista vacía.
