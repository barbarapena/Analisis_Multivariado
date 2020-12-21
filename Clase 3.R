##Clase 3 



library(MuMIn)
library(tidyverse)
library(broom)
data("ChickWeight")

#Cambio de intercepto solamente, la misma pendiente. 
fit1 <- lm(weight ~ Time + Diet, data = ChickWeight)
tidy(fit1)

#cmabio de pendiente, pero no intercepto (poner dieta como interracion)
fit2 <- lm(weight ~ Time + Time:Diet, data = ChickWeight)
tidy(fit2)
##Cambio pendiente e inercepto (esto no es lo que esperamos, pq esperamos que los pollos partan con el mmismo peso)
fit3 <- lm(weight ~ Time * Diet, data = ChickWeight)
tidy(fit3)

## Cual es el mejor modelo ?
Select <- MuMIn::model.sel(list(fit1, fit2, fit3))
Select
## Deberia primar el sentido comun, en este caso, el modelo 2 tiene sentido, pero es importante visualizar los datos para tomar la desicion 


## moelo linal simple/Diagnosticos con brrom 

Test <- augment(fit2)
hist(Test$.resid)

# pero vemos que en la medida que aumenta el peso, los residuales aumentan.... es decir, nuestro predictor no es taaan bueno
ggplot(Test, aes(x = .fitted, y = .resid)) + geom_point() + theme_classic() + geom_hline(yintercept = 0, 
                                                                                         lty = 2, color = "red")
## No tienen una varianza constante
## no esta cumpliedo los supuestos de un modelo lineal simple (si cumpiera los uspuesto, esperariamos que los residuales estan alrededor de al liena roja en todo el rango de peso que estamos prediciendo)



######GLM
## Permite trabajar con otros datos, que no tendran una ormalizad de varianzas 



## Hay que especificar la familia
Estructura de error
family =
  gaussian (variable dependiente continua)
binomial (variable dependiente 0 o 1)
poisson (variable dependiente cuentas 1, 2 ,3 ,4 ,5)
gamma (variable dependiente continua solo positiva)

#Gamma
#respuesta (link = inverso)
#1/y=β1X1+C0
# Modelo Gamma
fit2g <- glm(weight ~ Time + Time:Diet, family=Gamma, data = ChickWeight)

DF<- data.frame(Time=3, Diet =as.factor(1))
predict(fit2g, newdata=DF, type="response")
predict(fit2, newdata=DF, type="response")
tidy(fit2g)

## Poisson
#respuesta (link = log)
#logy=β1X1+C0

fit2p <- glm(weight ~ Time + Time:Diet, poisson, data = ChickWeight)
predict(fit2p, newdata=DF, type="response")


##Binomial
### Tittanic 
train <- read_csv("https://raw.githubusercontent.com/derek-corcoran-barrios/derek-corcoran-barrios.github.io/master/CursoMultiPres/Capitulo_3/train.csv") %>% 
  filter(Embarked == "S")
train


respuesta (link = logit)
log(p/1−p)=β1X1+C0
## Esto es li neal... modelando la sobreviviencia de acuerdo al ticket y su sexo
FitBin <- glm(Survived ~ Fare + Sex, data = train)
FitBin2 <- glm(Survived ~ Fare + Sex, data = train, family = binomial)
tidy(FitBin)
tidy(FitBin2)
FitBin3 <- glm(Survived ~ Fare * Sex, data = train, family = binomial)
tidy(FitBin3)

DF2<- data.frame(Fare= c(100,100),Sex=c("male", "female"))
predict(FitBin2, newdata=DF2, type = "response")

#ATTENCION! NO PUEDO COMÁPRAR AICc DE MODELOS ECHOS CON DIFERENTES FAMILIAS!

Data <- ChickWeight

Data$PredL <- predict(fit2, newdata = Data)
Data$PredP <- predict(fit2p, newdata = Data, type = "response")
Data$PredG <- predict(fit2g, newdata = Data, type = "response")

Data$ResidL <- Data$PredL - Data$weight
Data$ResidP <- Data$PredP - Data$weight
Data$ResidG <- Data$PredG - Data$weight

sqrt(sum(Data$ResidP^2))
sqrt(sum(Data$ResidG^2))

## eL MAS ADECUADO ES EL QUE TIENE LE MENOR VALOR, EN ESTE CASO p





### Resumen funcion link. 
Actua sobre Y
family Gaussian, link = identidad
family Gamma, link = inverso
family poisson, link = log
family binomial, link = logit


