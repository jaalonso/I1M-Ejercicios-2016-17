import Rel_1_sol
import Test.Hspec

corrigeTodos :: IO ()
corrigeTodos = sequence_ [corrige n | n <- [1..18]]

corrige :: Int -> IO ()
corrige n = hspec (especificacion n)

especificacion :: Int -> Spec

especificacion 1 = 
  describe "Ejercicio 1: media3" $ do
    it "e1_1" $
       media3_1 1 3 8 `shouldBe` 4.0
    it "e2_1" $
       media3_1 (-1) 0 7 `shouldBe` 2.0
    it "e3_1" $
       media3_1 (-3) 0 3 `shouldBe` 0.0
    it "e1_2" $
       media3_2 1 3 8 `shouldBe` 4.0
    it "e2_2" $
       media3_2 (-1) 0 7 `shouldBe` 2.0
    it "e3_2" $
       media3_2 (-3) 0 3 `shouldBe` 0.0

especificacion 2 = 
  describe "Ejercicio 2: sumaMoedas" $ do
    it "e1" $
      sumaMonedas 0 0 0 0 1  `shouldBe`  20
    it "e1" $
      sumaMonedas 0 0 8 0 3  `shouldBe` 100
    it "e1" $
      sumaMonedas 1 1 1 1 1  `shouldBe`  38

especificacion 3 = 
  describe "Ejercicio 3: volumenEsfera" $ do
    it "e1" $
      volumenEsfera 10 `shouldBe` 4188.790204786391

especificacion 4 = 
  describe "Ejercicio 4: areaDeCoronaCircular" $ do
    it "e1_1" $
      areaDeCoronaCircular1 1 2 `shouldBe` 9.42477796076938
    it "e2_1" $
      areaDeCoronaCircular1 2 5 `shouldBe` 65.97344572538566
    it "e3_1" $
      areaDeCoronaCircular1 3 5 `shouldBe` 50.26548245743669
    it "e1_2" $
      areaDeCoronaCircular2 1 2 `shouldBe` 9.42477796076938
    it "e2_2" $
      areaDeCoronaCircular2 2 5 `shouldBe` 65.97344572538566
    it "e3_2" $
      areaDeCoronaCircular2 3 5 `shouldBe` 50.26548245743669

especificacion 5 = 
  describe "Ejercicio 5: ultimaCifra" $ do
    it "e1_1" $
      ultimaCifra1 325 `shouldBe` 5
    it "e1_2" $
      ultimaCifra2 325 `shouldBe` 5

especificacion 6 = 
  describe "Ejercicio 6: maxTres" $ do
    it "e1" $
      maxTres 6 2 4  `shouldBe`  6
    it "e2" $
      maxTres 6 7 4  `shouldBe`  7
    it "e3" $
      maxTres 6 7 9  `shouldBe`  9

especificacion 7 = 
  describe "Ejercicio 7: rota1" $ do
    it "e1" $
      rota1 [3,2,5,7] `shouldBe` [2,5,7,3]

especificacion 8 = 
  describe "Ejercicio 8: rota" $ do
    it "e1_1" $
      rota_1 1 [3,2,5,7]  `shouldBe`  [2,5,7,3]
    it "e2_1" $
      rota_1 2 [3,2,5,7]  `shouldBe`  [5,7,3,2]
    it "e3_1" $
      rota_1 3 [3,2,5,7]  `shouldBe`  [7,3,2,5]
    it "e1_2" $
      rota_2 1 [3,2,5,7]  `shouldBe`  [2,5,7,3]
    it "e2_2" $
      rota_2 2 [3,2,5,7]  `shouldBe`  [5,7,3,2]
    it "e3_2" $
      rota_2 3 [3,2,5,7]  `shouldBe`  [7,3,2,5]

especificacion 9 = 
  describe "Ejercicio 9: rango" $ do
    it "e1_1" $
      rango1 [3,2,7,5]  `shouldBe`  [2,7]
    it "e1_2" $
      rango2 [3,2,7,5]  `shouldBe`  [2,7]

especificacion 10 = 
  describe "Ejercicio 10: palindromo" $ do
    it "e1" $
      palindromo [3,2,5,2,3]    `shouldBe`  True
    it "e2" $
      palindromo [3,2,5,6,2,3]  `shouldBe`  False

especificacion 11 = 
  describe "Ejercicio 11: interior" $ do
    it "e1_1" $
      interior1 [2,5,3,7,3]  `shouldBe`  [5,3,7]
    it "e2_1" $
      interior1 [2..7]       `shouldBe`  [3,4,5,6]
    it "e1_2" $
      interior2 [2,5,3,7,3]  `shouldBe`  [5,3,7]
    it "e2_2" $
      interior2 [2..7]       `shouldBe`  [3,4,5,6]
    it "e1_3" $
      interior3 [2,5,3,7,3]  `shouldBe`  [5,3,7]
    it "e2_3" $
      interior3 [2..7]       `shouldBe`  [3,4,5,6]

especificacion 12 = 
  describe "Ejercicio 12: finales" $ do
    it "e1" $
      finales 3 [2,5,4,7,9,6]  `shouldBe`  [7,9,6]

especificacion 13 = 
  describe "Ejercicio 13: segmento" $ do
    it "e1_1" $
      segmento1 3 4 [3,4,1,2,7,9,0]  `shouldBe`  [1,2]
    it "e2_1" $
      segmento1 3 5 [3,4,1,2,7,9,0]  `shouldBe`  [1,2,7]
    it "e3_1" $
      segmento1 5 3 [3,4,1,2,7,9,0]  `shouldBe`  []
    it "e1_2" $
      segmento2 3 4 [3,4,1,2,7,9,0]  `shouldBe`  [1,2]
    it "e2_2" $
      segmento2 3 5 [3,4,1,2,7,9,0]  `shouldBe`  [1,2,7]
    it "e3_2" $
      segmento2 5 3 [3,4,1,2,7,9,0]  `shouldBe`  []

especificacion 14 = 
  describe "Ejercicio 14: segmento" $ do
    it "e1" $
      extremos 3 [2,6,7,1,2,4,5,8,9,2,3]  `shouldBe`  [2,6,7,9,2,3]

especificacion 15 = 
  describe "Ejercicio 15: mediano" $ do
    it "e1_1" $
      mediano1 3 2 5  `shouldBe`  3
    it "e2_1" $
      mediano1 2 4 5  `shouldBe`  4
    it "e3_1" $
      mediano1 2 6 5  `shouldBe`  5
    it "e4_1" $
      mediano1 2 6 6  `shouldBe`  6
    it "e1_2" $
      mediano2 3 2 5  `shouldBe`  3
    it "e2_2" $
      mediano2 2 4 5  `shouldBe`  4
    it "e3_2" $
      mediano2 2 6 5  `shouldBe`  5
    it "e4_2" $
      mediano2 2 6 6  `shouldBe`  6

especificacion 16 = 
  describe "Ejercicio 16: tresIguales" $ do
    it "e1" $
      tresIguales 4 4 4        `shouldBe`  True
    it "e2" $
      tresIguales 4 3 4        `shouldBe`  False
    it "e3" $
      tresIguales 'a' 'a' 'a'  `shouldBe`  True

especificacion 17 = 
  describe "Ejercicio 17: tresDiferentes" $ do
    it "e1" $
      tresDiferentes 3 5 2        `shouldBe`  True
    it "e2" $
      tresDiferentes 3 5 3        `shouldBe`  False
    it "e3" $
      tresDiferentes 'a' 'b' 'c'  `shouldBe`  True

especificacion 18 = 
  describe "Ejercicio 18: cuatroIguales" $ do
    it "e1" $
      cuatroIguales 5 5 5 5          `shouldBe`  True
    it "e2" $
      cuatroIguales 5 5 4 5          `shouldBe`  False
    it "e3" $
      cuatroIguales 'a' 'a' 'a' 'a'  `shouldBe`  True
