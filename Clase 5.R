## Clase 5



library(glmnet)
library(tidyverse)
library(caret)
library(broom)

data("mtcars")
Fit <- lm(mpg~wt, data=mtcars)
tidy(Fit)


## Como se obetiene ls pendiente. 
Usando los minimos cuadrados. 

Preds<- augment(Fit)
ggplot(Preds, aes(x=wt, y=mpg))+ 
  geom_point() + 
  geom_path(aes(y= .fitted)) + 
  geom_linerange(aes(ymin= .fitted, ymax=mpg)) + 
  theme_bw()

Los lasrgo de las lineas del grafico, son los minmos cuadrados...  
Summa de errosres al cuadreado 
SSE.. 
SSE<-Preds %>% mutate(SE= .resid^2) %>% summarise(SEE=sum(SE))


En general funciona bien, pero cuando nuestra muestra es muy disitnta a la poblacion, tenemos problemas
Aqui conviene hacer una regresion penalizada.
Se agrega una penalizacion segun el B cuadrado