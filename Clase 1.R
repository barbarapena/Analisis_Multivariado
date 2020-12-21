### Clase 1 Analisis multivariado

library(tidyverse)
library(MuMIn)
library(caret)
library(broom)

data("mtcars")

plot(x=mtcars$hp, y=mtcars$mpg)
# Comparacion poder explicativo vs Poder predictivo
# Utilizacion de datos de entrenamiento (para ajustar el modelo y ver poder explicativo) y datos de prueba (para ver el poder predictivo)
f 
set.seed(2020)
index <- sample(1:nrow(mtcars), size=round(nrow(mtcars)/2))
Train <- mtcars[index, ]
Test <- mtcars[-index,]

mpg=Bhp + B2hp^2 + c 
B= estimadores
c=intercepto


#Ajusemos un mpodelo lineal


modelo <- lm (mpg ~ hp + I(hp^2), data=Train)
summary(modelo)

## broom entrega la misma info, pero con una tabla más amigable 

broom::tidy(modelo)
broom::glance(modelo)

## Agrego en mi base de datos TEst la preduccion de mpg, de acuerdo al modelo que obtuve con la base de datos Train

Test$Pred <- predict(modelo, Test)

Test <- Test %>%  mutate (resid=mpg-Pred) %>%  select(hp, mpg, Pred, resid)
Test

hist(Test$resid)


## Calculo poder predictivo 

postResample(pred=Test$Pred, obs=Test$mpg)



### Sobre ajuste. 
## cuando ponemos más parametros, podemos hacer un sobre ajuste

# al coomplejizar el modelo, el poder explicativo, y el r cuadrado por lo tanto, aumenta.
# No hay errores de explicacion 
# solo es valido para los datos de enrnamiento Train
Esto funciona cuando voy agregando parametros, pero llega un punto donde elpoder predictivo baja mucho mucho mucho
Caundo llegamos al momento en que baja el R2 del predictivo, lleamos al punto de sobreauste


### Caundo me puede interesar maximizar el poder predictivo?
No siempre voy a desear mazimizar el poder prectivico, pero por ejemplo, para la prediccion de casos de Covid, para el texto predictivo del celu
auto-corrector, efecto cambio climtico, deteccion de caras en redes sociaes, eficiencia de medidas ante epideemias, 
maximizar metrica n base de prueba (test) 

¡Cuado maximizar el poder predictivo ?
  PAra pureba de hptesis causales, como por ejemplo, que causa el cambio climatico ?
  En general para trabajr bajo esa paroximacion debemos seguir los sigueintes pasos!:
  
  - Generacion de hipotesis (plural)
- Geracion de modelos para cada hipoesis
- Interpretacion de resultados en base a modeles e hiptesis


¡ Explicacion o Prediccion ?
  En general queremos ambos, en equilibrio!
  
  
  Existen criterios de informacion para tomar decisiones, y hoy vamos a ver el de Akaike
AIC= 2k- ln(L)

k= numero de parametros
L=likelihood


#AIC corregido, lo corrige de anucerdo al numeor de observaciones que tneemos


AICc=AIC + (2k[2 + 2k)/(n-k-1)])
n= numeor de obsersvacines


## Poder predictivo vs Pder explicativo .




AIC(modelo)
AICc(modelo)

El valor mas pequeño al comparar dos AICc, es el que tiene la combinacion m{as adedicada entre explicacion y prediccion
}

EEl blaance, es el valor mas bajo de AIcc que toma en el parametro K, y el inicio de la asintota al graficar r cuadrado vs k 






