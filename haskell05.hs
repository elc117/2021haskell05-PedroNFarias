--Nome:

bmi :: Float -> Float -> String
bmi :: Float -> Float -> String
bmi peso altura =
    let altquad = altura^2
        bmi = peso/altquad
        in bmi
            | bmi <= 18.5 = "Abaixo" 
            | bmi > 18.5 && bmi < 30 = "Normal"
            | bmi >= 30 = "Acima"

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
