## Instalación de paquetes

#install.packages("psych")

## Cargando librería

library(psych)

## Carga de datos

data("galton")

#describe(galton)

## Gráfica

plot(galton)

## Estimación del modelo lineal
## Se espefica child = f(parent)
fit <- lm(data = galton, child ~ parent)

## Se grafica la regresión

abline(fit, col = "red", lwd = 3)


## Estimación manual del modelo

## Los valores de los coeficientes

fit$coefficients

## Se reconstruyen los valores con la fórmula 
## de ecuación lineal de la recta
## y = b + mx

recta <-as.numeric(lapply(galton$parent, 
                          function(x) fit$coefficients[1] + fit$coefficients[2] * x))

## Se extran los valores del modelo de regresión

original <- fitted.values(fit)

## Graficamos los valores que se predicen y los valores estimados por el modelo
par(mfrow=c(1,2))

plot(galton, main = "Modelo de regresión lineal")
lines(galton$parent, original, col = "red", lwd = 3)

plot(galton, main = "Ecuación de la recta")
lines(galton$parent,recta, col = "blue", lwd = 3)


