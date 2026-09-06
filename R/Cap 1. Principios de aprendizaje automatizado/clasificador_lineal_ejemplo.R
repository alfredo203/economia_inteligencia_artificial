## Ejemplo de clasificación de regresión lineal

## Se crean los datos negros (1)
set.seed(2)
a <- data.frame(
  matrix(rexp(200, rate=100), ncol=2) * 100
)
a$clase <- 1

## Se crean los datos rojos (2)
set.seed(2)
b <- data.frame(
  matrix(rexp(200, rate=100), ncol=2) *(100) + 1.3 ## 0.15
)
b$clase <- 2

## Se juntan ambos datos
df <-rbind(a, b)

## Gráfica 1
par(mfrow=c(2,2))
plot(df$X1, df$X2, col = df$clase, xlab = "", ylab = "")


## Gráfica de toda la información
plot(df$X1, df$X2, col = df$clase, xlab = "", ylab = "")

## Modelo de regresión
a <-lm(data = df, X1 ~ X2)
abline(a, col = "black", lwd = 2)

#abline(1.3,0, col = "red", lwd = 3)

## Número de clasificaciones erroneas
# 
df$clase[df$clase == 1 & df$X2> 1.3 & df$X1 < 3] <- 3

# 
# porcentaje_errores <-(sum(df$clase == 5) + sum(df$clase == 3) + 6) / nrow(df)
# 
plot(df$X1, df$X2, col = df$clase, xlab = "", ylab = "")
abline(a, col = "black", lwd = 2)
# 

df$clase[df$clase == 2 & df$X2 < 1.7 | df$X1 > 3.7] <- 5
plot(df$X1, df$X2, col = df$clase, xlab = "", ylab = "")
abline(a, col = "black", lwd = 2)