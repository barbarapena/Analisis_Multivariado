## Clase  2


## Cpmp decdimos que modelo eleginmos para 



library (tidyverse)
library (MuMIn)
library (broom)
library(caret)

data("mtcars")
#odelos 
fit1 <- lm(mpg ~ carb + cyl, data = mtcars)
fit2 <- lm(mpg ~ cyl + wt, data = mtcars)
fit3 <- lm(mpg ~ am + qsec + wt, data = mtcars)
fit4 <- lm(mpg ~ carb + cyl + wt, data = mtcars)
fit5 <- lm(mpg ~ am + carb + cyl + qsec + wt, data = mtcars)
fit6 <- lm(mpg ~ am + carb + cyl + hp + qsec, data = mtcars)
## losp moenos en lista
models <- list(fit1, fit2, fit3, fit4, fit5, fit6)
# generar tabla de selecion de modelos 
Select<- model.sel(models)


¡ que modelo seleccionar ? Pesos Akaike? 
  suman uno, a mayor peso, mejor modelo, el peso, consindera el numero modelos que estoy comparndo. 

## por ocnvencion, se dice que los mejores modelos tienen ul delta igual o menor a dos, por eso seleccionaremos solo los modelos que cumplaneste criterio 

Selected <- subset(Select, delta <= 2)

## como seleccionar el mejor moelo ?

BestModel <- get.models(Select, 1)[[1]]
broom::glance(BestModel)


broom::tidy(BestModel)
 

#p del intercepto no significativo ??
- No importa, pqcestamos busano el mas parcimonioso. 


##Promediar modelos

Promedio de los mejore s modelos, que tienen un delta menor o igual a 2
# pimero vremos numero de cilindros


S<- as.data.frame(Selected)
S <- as.data.frame(Selected) %>% select(cyl, weight)


#Hay dos formas de primemdiar modelos, el full 

S_full <- S
S_full <- S_full %>%  mutate(cyl=ifelse(is.na(cyl), 0, cyl)) %>%  mutate(Theta_i=cyl*weight) %>%
  summarise(Theta=sum(Theta_i))

# Metodo subset
S_subset <- S
S_subset <- S_subset %>% filter(!is.na(cyl)) %>%  mutate(Theta_i=cyl*weight) %>%
  summarise(Theta= sum(Theta_i)/sum(weight))

S_full
S_subset



## colculo promedio oedlos con MuIN

Modelo_promedio <- model.avg(Select, subset=delta <=2, fit=T)
Modelo_promedio



### Multicolinearidad

library(readr)
bloodpress <- read_delim("https://online.stat.psu.edu/onlinecourses/sites/stat501/files/data/bloodpress.txt", 
                         "\t", escape_double = FALSE, col_types = cols(Pt = col_skip()), trim_ws = TRUE)


Correlacion <- cor(bloodpress[,-1 ])
Correlacion
## El pnto es, no podner dos variables juntas cuando tienen una correlaciono mayor de 0.70 pearson




ggplot(mtcars, aes (x=qsec, y=mpg)) + 
  geom_smooth(method = "lm")+
  geom_point()+ theme_bw()

