--Nome:

bmi :: Float -> Float -> String
bmi peso altura =
    let altquad = altura^2
        bmi = peso/altquad
        abaixo = 18.5
        acima = 30
     in if bmi <= abaixo
        then "Abaixo"
        else if bmi >= acima
             then "Acima"
             else "Normal"

bmi' :: Float -> Float -> String
bmi' peso altura  
    | bmi <= abaixo = abaixoS  
    | bmi > abaixo && bmi < acima = normalS
    | bmi >= acima = acimaS  
    where bmi = peso / altquad
          abaixo = 18.5  
          acima = 30.0
          altquad = altura^2
          abaixoS = "Abaixo"
          acimaS = "Acima"
          normalS = "Normal"


cpfValid :: [Int] -> Bool
cpfValid cpf = dv1 == cpf !! 9 && dv2 == cpf !! 10
  where dv1 = cpfDV digits [10,9..]
        dv2 = cpfDV (digits ++ [dv1]) [11,10..]
        digits = take 9 cpf 

cpfDV :: [Int] -> [Int] -> Int
cpfDV digits mults = 
  let expr = (sum $ zipWith (*) digits mults) `mod` 11
   in if expr < 2 then 0 else 11-expr

  
andTable :: [(Bool, Bool, Bool)]
andTable = zipWith (\x y -> (x,y,x && y)) p q
    where p = [x | x <- [True,True,False,False]]
          q = [x | x <- take qtd $ cycle[True,False]]
          qtd = 4