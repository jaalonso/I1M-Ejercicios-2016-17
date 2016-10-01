-- I1M 2016-17: Rel_1.hs (21 de septiembre de 2016)
-- Definiciones por composición sobre números, listas y booleanos. 
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- En esta relación se plantean ejercicios con definiciones de funciones 
-- por composición sobre números, listas y booleanos.
-- 
-- Para solucionar los ejercicios puede ser útil el manual de
-- funciones de Haskell que se encuentra en http://bit.ly/1uJZiqi y su
-- resumen en http://bit.ly/ZwSMHO

import Data.List

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función media3 tal que (media3 x y z) es
-- la media aritmética de los números x, y y z. Por ejemplo, 
--    media3 1 3 8     ==  4.0
--    media3 (-1) 0 7  ==  2.0
--    media3 (-3) 0 3  ==  0.0
-- ---------------------------------------------------------------------

-- enrnarbej ignareeva marjimcom cargonler manruiber roscargar felsuacor
-- congomgom fatfervaz antmorper3 belbenzam luimotmar beagongon1
-- eliguivil eledejim2 monlagare glovizcas joscasgom1 pabrabmon josdeher 
-- javleilor alvfercen fraferpoy marmerzaf josrodgal7 carmarcar5
-- margarflo5 migibagar natmarmar2 artmorfer josjimgon2 albcercid
-- margarvil14 paumacpar natruipin
media3_1 x y z = (x+y+z)/3

-- juaorture
media3_2 x y z = x/3 + y/3 + z/3

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función sumaMonedas tal que 
-- (sumaMonedas a b c d e) es la suma de los euros correspondientes a 
-- a monedas de 1 euro, b de 2 euros, c de 5 euros, d 10 euros y
-- e de 20 euros. Por ejemplo,
--    sumaMonedas 0 0 0 0 1  ==  20
--    sumaMonedas 0 0 8 0 3  == 100
--    sumaMonedas 1 1 1 1 1  ==  38
-- ---------------------------------------------------------------------

-- enrnarbej juaorture ignareeva marjimcom paumacpar manruiber roscargar
-- felsuacor congomgom fatfervaz antmorper3 belbenzam luimotmar
-- beagongon1 eliguivil eledejim2 monlagare glovizcas joscasgom1 
-- pabrabmon josdeher cargonler javleilor alvfercen fraferpoy marmerzaf
-- josrodgal7 carmarcar5 margarflo5 migibagar natmarmar2 artmorfer
-- josjimgon2 albcercid margarvil14 natruipin
sumaMonedas a b c d e = a+2*b+5*c+10*d+20*e

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función volumenEsfera tal que 
-- (volumenEsfera r) es el volumen de la esfera de radio r. Por ejemplo,
--    volumenEsfera 10  ==  4188.790204786391
-- Indicación: Usar la constante pi.
-- ---------------------------------------------------------------------

-- enrnarbej juaorture ignareeva marjimcom paumacpar manruiber roscargar
-- felsuacor congomgom fatfervaz antmorper3 belbenzam beagongon1
-- eliguivil eledejim2 monlagare glovizcas joscasgom1 pabrabmon cargonler
-- javleilor alvfercen marmerzaf josrodgal7 carmarcar5 migibagar
-- artmorfer albcercid margarvil14 natruipin
volumenEsfera1 r = 4/3*pi*r^3

-- luimotmar
volumenEsfera2 r = 4/3 * (pi * r^3)

-- josdeher
volumenEsfera3 r = 4/3*pi*r**3

-- fraferpoy margarflo5
volumenEsfera4 r =(4/3)*pi*r^3

-- josjimgon2
volumenEsfera5 r = (4*pi*r^3)/3

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función areaDeCoronaCircular tal que 
-- (areaDeCoronaCircular r1 r2) es el área de una corona circular de
-- radio interior r1 y radio exterior r2. Por ejemplo,
--    areaDeCoronaCircular 1 2 == 9.42477796076938
--    areaDeCoronaCircular 2 5 == 65.97344572538566
--    areaDeCoronaCircular 3 5 == 50.26548245743669
-- ---------------------------------------------------------------------

-- enrnarbej
areaDeCoronaCircular1 r1 r2 = areaCirculo r2 - areaCirculo r1

areaCirculo r = pi*r^2

-- juaorture marjimcom paumacpar manruiber roscargar felsuacor eledejim2
-- congomgom fatfervaz antmorper3 belbenzam luimotmar beagongon1
-- eliguivil ignareeva monlagare glovizcas joscasgom1 pabrabmon josdeher
-- cargonler javleilor alvfercen marmerzaf margarflo5 migibagar
-- natmarmar2 artmorfer josjimgon2 margarvil14 natruipin
areaDeCoronaCircular2 r1 r2 = pi*(r2^2 - r1^2)

-- fraferpoy josrodgal7 carmarcar5 albcercid
areaDeCoronaCircular3 r1 r2 =(pi*r2^2)-(pi*r1^2)

-- Comentario: La definición de areaDeCoronaCircular2 se puede
-- simplificar. 

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la función ultimaCifra tal que (ultimaCifra x)
-- es la última cifra del número x. Por ejemplo,
--    ultimaCifra 325  ==  5
-- Indicación: Usar la función rem
-- ---------------------------------------------------------------------

-- enrnarbej manruiber roscargar , marjimcom felsuacor congomgom
-- antmorper3 belbenzam beagongon1 eliguivil ignareeva paumacpar
-- glovizcas joscasgom1 pabrabmon eledejim2 cargonler alvfercen
-- javleilor marmerzaf josrodgal7 natmarmar2 artmorfer albcercid
ultimaCifra1 x = x `rem` 10

-- juaorture
ultimaCifra2 x = x `rem` (10 * (length "x"))

-- Comentario: La definición de ultimaCifra2 se puede simplificar.

-- josdeher fraferpoy margarflo5 migibagar josjimgon2 natruipin
ultimaCifra3 x = rem x 10

-- ---------------------------------------------------------------------
-- Ejercicio 6. Definir la función maxTres tal que (maxTres x y z) es
-- el máximo de x, y y z. Por ejemplo,
--    maxTres 6 2 4  ==  6
--    maxTres 6 7 4  ==  7
--    maxTres 6 7 9  ==  9
-- Indicación: Usar la función max.
-- ---------------------------------------------------------------------

-- enrnarbej juaorture paumacpar manruiber roscargar marjimcom felsuacor
-- congomgom antmorper3 beagongon1 eliguivil ignareeva glovizcas
-- joscasgom1 pabrabmon fatfervaz cargonler fraferpoy alvfercen marmerzaf
-- josrodgal7 margarflo5 natmarmar2 artmorfer natruipin
maxTres1 x y z = max x (max z y)

-- belbenzam monlagare javleilor josjimgon2 carmarcar5
maxTres2 x y z = maximum [x,y,z]

-- Comentario: La definición maxTres2 no cumple la restricción del
-- enunciado.

-- eledejim2 josdeher migibagar margarvil14
maxTres3 x y z = max (max x y) z

-- albcercid
maxTres4 x y z = max x (max y (max z (z-1)))

-- Comentario: La definición maxTres4 se puede mejorar.

-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir la función rota1 tal que (rota1 xs) es la lista
-- obtenida poniendo el primer elemento de xs al final de la lista. Por
-- ejemplo, 
--    rota1 [3,2,5,7]  ==  [2,5,7,3]
-- ---------------------------------------------------------------------

-- paumacpar enrnarbej juaorture manruiber roscargar marjimcom felsuacor
-- antmorper3 beagongon1 belbenzam eliguivil fatfervaz ignareeva
-- monlagare glovizcas joscasgom1 eledejim2 josdeher cargonler fraferpoy
-- javleilor alvfercen congomgom marmerzaf margarflo5 artmorfer
-- josjimgon2 carmarcar5
rota1 xs = tail xs ++ [head xs]

-- pabrabmon josrodgal7 migibagar natruipin
rota12 xs = drop 1 xs ++ [head xs]

-- albcercid
rota13 xs = tail xs ++ take 1 xs

-- ---------------------------------------------------------------------
-- Ejercicio 8. Definir la función rota tal que (rota n xs) es la lista
-- obtenida poniendo los n primeros elementos de xs al final de la
-- lista. Por ejemplo, 
--    rota 1 [3,2,5,7]  ==  [2,5,7,3]
--    rota 2 [3,2,5,7]  ==  [5,7,3,2]
--    rota 3 [3,2,5,7]  ==  [7,3,2,5]
-- ---------------------------------------------------------------------

-- enrnarbej
rota_1 0 xs = xs
rota_1 n xs = rota_1 (n-1) (rota1 xs)

-- juaorture paumacpar manruiber roscargar marjimcom felsuacor antmorper3
-- beagongon1 eliguivil ignareeva glovizcas joscasgom1 pabrabmon
-- eledejim2 josdeher cargonler javleilor alvfercen fraferpoy congomgom
-- marmerzaf josrodgal7 margarflo5 migibagar natmarmar2 artmorfer
-- josjimgon2 albcercid natruipin
rota_2 n xs = drop n xs ++ take n xs

-- ---------------------------------------------------------------------
-- Ejercicio 9. Definir la función rango tal que (rango xs) es la
-- lista formada por el menor y mayor elemento de xs.
--    rango [3,2,7,5]  ==  [2,7]
-- Indicación: Se pueden usar minimum y maximum.
-- ---------------------------------------------------------------------

-- enrnarbej juaorture roscargar marjimcom beagongon1 belbenzam
-- fatfervaz ignareeva joscasgom1 eledejim2 javleilor alvfercen
-- josrodgal7 margarflo5 josjimgon2 
rango1 xs = [minimum xs, maximum xs]

-- paumacpar manruiber antmorper3 eliguivil glovizcas pabrabmon josdeher
-- cargonler fraferpoy congomgom marmerzaf migibagar artmorfer albcercid
-- natruipin 
rango2 xs = [minimum xs] ++ [maximum xs]

-- ---------------------------------------------------------------------
-- Ejercicio 10. Definir la función palindromo tal que (palindromo xs) se
-- verifica si xs es un palíndromo; es decir, es lo mismo leer xs de
-- izquierda a derecha que de derecha a izquierda. Por ejemplo,
--    palindromo [3,2,5,2,3]    ==  True
--    palindromo [3,2,5,6,2,3]  ==  False
-- ---------------------------------------------------------------------

-- enrnarbej juaorture paumacpar manruiber roscargar marjimcom felsuacor
-- antmorper3 beagongon1 belbenzam eliguivil fatfervaz ignareeva
-- glovizcas joscasgom1 pabrabmon eledejim2 josdeher cargonler javleilor
-- alvfercen fraferpoy congomgom marmerzaf josrodgal7 margarflo5
-- migibagar natmarmar2 artmorfer josjimgon2 natruipin
palindromo xs = xs == reverse xs

-- albcercid
palindromo2 xs = reverse xs == xs

-- ---------------------------------------------------------------------
-- Ejercicio 11. Definir la función interior tal que (interior xs) es la
-- lista obtenida eliminando los extremos de la lista xs. Por ejemplo,
--    interior [2,5,3,7,3]  ==  [5,3,7]
--    interior [2..7]       ==  [3,4,5,6]
-- ---------------------------------------------------------------------

-- paumacpar manruiber roscargar joscasgom1 pabrabmon javleilor
interior1 xs = tail (reverse (tail (reverse xs)))

-- Comentario: La definición anterior se puede simplificar usando la
-- función init (ver http://bit.ly/2d5QVAJ )

-- enrnarbej
interior2 xs = take (length xs - 2) (tail xs)

-- juaorture felsuacor antmorper3 alvfercen congomgom margarflo5
-- migibagar artmorfer josjimgon2 
interior3 xs = init (tail xs)

-- marjimcom beagongon1 glovizcas pabrabmon cargonler marmerzaf
-- natruipin 
interior4 xs = drop 1 (init xs)

-- eliguivil
interior5 xs = tail (take ((length xs)-1) xs)

-- fatfervaz eledejim2 fraferpoy 
interior6 xs = tail (init xs)

-- josdeher
interior7 xs = init (drop 1 xs)

--ignareeva josrodgal7
interior8 xs = reverse (tail (reverse (tail xs)))

-- albcercid
interior9 xs = tail (take (length xs - 1) xs)

-- margarvil14
interior10 xs = tail (reverse (tail xs))

-- ---------------------------------------------------------------------
-- Ejercicio 12. Definir la función finales tal que (finales n xs) es la
-- lista formada por los n finales elementos de xs. Por ejemplo,
--    finales 3 [2,5,4,7,9,6]  ==  [7,9,6]
-- ---------------------------------------------------------------------
 
-- enrnarbej juaorture paumacpar manruiber beagongon1 belbenzam
-- glovizcas joscasgom1 pabrabmon eledejim2 javleilor marmerzaf
-- congomgom josrodgal7 margarflo5 migibagar natmarmar2 artmorfer
-- josjimgon2 albcercid natruipin
finales1 n xs = drop (length xs - n) xs

-- felsuacor antmorper3 eliguivil roscargar josdeher ignareeva fraferpoy
-- margarvil14 
finales2 n xs = reverse (take n ( reverse xs))

-- Se usará la 1ª
finales = finales1
  
-- ---------------------------------------------------------------------
-- Ejercicio 13. Definir la función segmento tal que (segmento m n xs) es
-- la lista de los elementos de xs comprendidos entre las posiciones m y
-- n. Por ejemplo,
--    segmento 3 4 [3,4,1,2,7,9,0]  ==  [1,2]
--    segmento 3 5 [3,4,1,2,7,9,0]  ==  [1,2,7]
--    segmento 5 3 [3,4,1,2,7,9,0]  ==  []
-- ---------------------------------------------------------------------
 
-- enrnarbej natruipin
segmento1 m n xs =
  reverse (drop (length xs - n) (reverse (drop (m-1) xs)))

-- enrnarbej juaorture manruiber beagongon1 joscasgom1 pabrabmon
-- josdeher cargonler glovizcas javleilor marmerzaf congomgom margarflo5
-- artmorfer josjimgon2 albcercid 
segmento2 m n xs = drop (m-1) (take n xs)

-- antmorper3 roscargar josrodgal paumacpar
segmento3 m n xs = take (n-(m-1)) (drop (m-1)xs)

-- eliguivil
segmento4 m n xs =
  reverse (drop ((length xs)-n) (reverse (drop (m-1) xs)))

-- migibagar
segmento5 m n xs = take n xs \\ take (m - 1) xs
  
-- ---------------------------------------------------------------------
-- Ejercicio 14. Definir la función extremos tal que (extremos n xs) es
-- la lista formada por los n primeros elementos de xs y los n finales
-- elementos de xs. Por ejemplo, 
--    extremos 3 [2,6,7,1,2,4,5,8,9,2,3]  ==  [2,6,7,9,2,3]
-- ---------------------------------------------------------------------

-- enrnarbej juaorture paumacpar manruiber antmorper3 beagongon1
-- pabrabmon cargonler glovizcas javleilor marmerzaf fraferpoy artmorfer
extremos1 n xs = take n xs ++ finales n xs

--felsuacor eliguivil roscargar joscasgom1 josdeher josrodgal7 congomgom
--margarflo5 josjimgon2 natruipin
extremos2 n xs = take n xs ++ reverse (take n (reverse xs))

-- migibagar
extremos3 n xs = take n xs ++ drop (length xs - n) xs

-- albcercid
extremos4 n xs = take n xs ++ drop (length xs-n) xs

-- ---------------------------------------------------------------------
-- Ejercicio 15. Definir la función mediano tal que (mediano x y z) es el
-- número mediano de los tres números x, y y z. Por ejemplo,
--    mediano 3 2 5  ==  3
--    mediano 2 4 5  ==  4
--    mediano 2 6 5  ==  5
--    mediano 2 6 6  ==  6
--    mediano 3 0 0  ==  0
--    mediano 0 0 0  ==  0
-- Indicación: Usar maximum y minimum.
-- ---------------------------------------------------------------------

-- juaorture
mediano1 x y z = (sort [x,y,z]) !! 1

-- enrnarbej marjimcom
mediano2 x y z =
  minimum [minimum [maximum [x,y], maximum [x,z]], maximum [y,z]]

-- manruiber joscasgom1 pabrabmon cargonler marmerzaf josrodgal7
-- migibagar josjimgon2 natruipin
mediano3 x y z =
  minimum [maximum [x,y], maximum [y,z], maximum [x,z]]

-- beagongon1 pabrabmon josdeher cargonler, paumacpar
mediano4 x y z =
  maximum [minimum [x,y], minimum [x,z], minimum [y,z]]

-- eliguivil antmorper3 margarflo5 fraferpoy
mediano5 x y z =
  sum [x,y,z] - maximum [x,y,z] - minimum [x,y,z]

-- albcercid
mediano6 x y z =
  maximum [x,y] + maximum [z,x] + maximum [y,z] - 2*maximum [x,y,z]

-- ---------------------------------------------------------------------
-- Ejercicio 16. Definir la función tresIguales tal que 
-- (tresIguales x y z) se verifica si los elementos x, y y z son
-- iguales. Por ejemplo, 
--    tresIguales 4 4 4        ==  True
--    tresIguales 4 3 4        ==  False
--    tresIguales 'a' 'a' 'a'  ==  True
-- ---------------------------------------------------------------------

-- enrnarbej manruiber antmorper3 beagongon1 eliguivil roscargar
-- joscasgom1 pabrabmon eledejim2 josdeher cargonler fatfervaz glovizcas
-- marmerzaf congomgom margarflo5 monlagare artmorfer josjimgon2
-- paumacpar natruipin fraferpoy
tresIguales x y z = (x==y) && (y==z)

-- juaorture 
--    tresIguales2 x y z = x - y == 0 && y - z == 0

-- Comentario: La definición tresIguales2 falla en el 3º ejemplo.

-- migibagar albcercid
tresIguales3 x y z = (x == y) && (y == z) && (x == z)

-- Comentario: La definición tresIguales3 se puede simplificar.

-- ---------------------------------------------------------------------
-- Ejercicio 17. Definir la función tresDiferentes tal que 
-- (tresDiferentes x y z) se verifica si los elementos x, y y z son
-- distintos. Por ejemplo, 
--    tresDiferentes 3 5 2        ==  True
--    tresDiferentes 3 5 3        ==  False
--    tresDiferentes 'a' 'b' 'c'  ==  True
-- ---------------------------------------------------------------------

-- enrnarbej manruiber antmorper3 beagongon1 eliguivil roscargar
-- joscasgom1 pabrabmon eledejim2 josdeher cargonler fatfervaz glovizcas 
-- marmerzaf margarflo5 congomgom monlagare migibagar artmorfer
-- josjimgon2 albcercid paumacpar natruipin fraferpoy
tresDiferentes x y z = (x /= y) && (y /= z) && (x /= z) 

-- juaorture
--    tresDiferentes2 x y z = x - y /= 0 && x - z /= 0 && y - z /= 0

-- Comentario: La definición tresDiferentes2 falla en el 3º ejemplo.

-- ---------------------------------------------------------------------
-- Ejercicio 18. Definir la función cuatroIguales tal que 
-- (cuatroIguales x y z u) se verifica si los elementos x, y, z y u son
-- iguales. Por ejemplo, 
--    cuatroIguales 5 5 5 5          ==  True
--    cuatroIguales 5 5 4 5          ==  False
--    cuatroIguales 'a' 'a' 'a' 'a'  ==  True
-- Indicación: Usar la función tresIguales.
-- ---------------------------------------------------------------------

-- enrnarbej manruiber marjimcom antmorper3 beagongon1 roscargar
-- joscasgom1 pabrabmon eledejim2 josdeher cargonler fatfervaz glovizcas 
-- marmerzaf margarflo5 congomgom artmorfer josjimgon2 paumacpar
-- natruipin fraferpoy
cuatroIguales x y z u = tresIguales x y z && u == x 

-- juaorture
--    cuatroIguales2 x y z u = x - y == 0 && y - z == 0 && z - u == 0

-- Comentario: La definición cuatroIguales2 falla en el 3º ejemplo.

-- eliguivil
cuatroIguales3 x y z u = tresIguales x y z && tresIguales x y u

-- migibagar
cuatroIguales4 x y z u = tresIguales x y z && x == z && y == u

-- Comentario: La definición cuatroIguales4 se puede simplificar.

-- albcercid
cuatroIguales5 x y z u = (tresIguales x y z)&&(x==u)

-- Comentario: La definición cuatroIguales5 se puede simplificar.
