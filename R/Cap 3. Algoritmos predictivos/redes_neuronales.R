## Redes neuronales

set.seed(500)
library(MASS)
data <- Boston

index <- sample(1:nrow(data),round(0.75*nrow(data)))
train <- data[index,]
test <- data[-index,]
lm.fit <- glm(medv~., data=train)
summary(lm.fit)
pr.lm <- predict(lm.fit,test)
MSE.lm <- sum((pr.lm - test$medv)^2)/nrow(test)

maxs <- apply(data, 2, max) 
mins <- apply(data, 2, min)

scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))

train_ <- scaled[index,]
test_ <- scaled[-index,]

library(neuralnet)
n <- names(train_)
f <- as.formula(paste("medv ~", paste(n[!n %in% "medv"], collapse = " + ")))
nn <- neuralnet(f,data=train_,hidden=c(5,3),linear.output=T)
neuralnet(f,data=train_,hidden=c(5,3),linear.output=T)

pr.nn <- compute(nn,test_[,1:13])

pr.nn_ <- pr.nn$net.result*(max(data$medv)-min(data$medv))+min(data$medv)
test.r <- (test_$medv)*(max(data$medv)-min(data$medv))+min(data$medv)

MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_)

print(paste(MSE.lm,MSE.nn))

par(mfrow=c(1,2))

plot(test$medv,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN',pch=18,col='red', bty='n')

plot(test$medv,pr.lm,col='blue',main='Real vs predicted lm',pch=18, cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='LM',pch=18,col='blue', bty='n', cex=.95)

plot(test$medv,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
points(test$medv,pr.lm,col='blue',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend=c('NN','LM'),pch=18,col=c('red','blue'))


## Canada case

df<-read.csv("Cap 3. Algoritmos predictivos/data/Canada_vars.csv")

summary(df)


## Descargando la información

library(quantmod)
getSymbols("GOOG",src="google")

## Gráfica quantmod
barChart(GOOG)

## Extrayendo la información a un data frame
df <- as.data.frame(GOOG)

## Se selecciona el número de días a predecir
num_test_days <- 10

## Se divide el set de entrenamiento
## Se eligen 191 observaciones para entrenar el modelo
df.train <- df[(nrow(GOOG)-200):(nrow(GOOG)-num_test_days), ]
df.train$days <- as.Date(row.names(df.train))

## Se selecciona la sección que corresponde al set de pruebas
## Se eligen 11 observaciones (las más recientes) para probar
## el desempeño del modelo
df.test <- df[(nrow(GOOG)-num_test_days):nrow(GOOG), ]
df.test.values <- df.test$GOOG.Close[1:num_test_days]
df.test$days <- as.Date(row.names(df.test))

## Se grafican los precios de cierre
plot(df.train$days, 
     df.train$GOOG.Close, 
     type = "l", 
     main = "Precio diario al cierre de la emisora Alphabet Inc (Google)")

# plot(df.test$days, df.test$GOOG.Close, type = "l", main = "Precio diario de la emisora Alphabet Inc (Google) 2007-2018")


library(forecast)

##Ajuste del modelo
par(mfrow=c(2,1))
fit.nn <- nnetar(df.train$GOOG.Close, size = 2, repeats = 5000, scale.inputs = TRUE) 

## predicción del modelo de redes neuronales
fcast.nn <- forecast(fit.nn, h = num_test_days)

## Gráfica de las predicciones
plot(fcast.nn)
lines(fitted(fcast.nn), 
      col = "red") ## se grafica el ajuste de entrenamiento en rojo

## Tabla resumen de desempeño del modelo
accuracy.nn <-as.data.frame(accuracy(fcast.nn$mean, x = df.test.values ))

##
fit.arima <-auto.arima(df.train$GOOG.Close)
fcast.arima <- forecast(fit.arima, h = num_test_days)
plot(fcast.arima)
lines(fitted(fit.arima), col = "green")

accuracy.arima <- as.data.frame(accuracy(fcast.arima$mean, x = df.test.values))

accuracy.table<-rbind(accuracy.nn, accuracy.arima)
row.names(accuracy.table) <- c("Red Neuronal", "ARIMA")
accuracy.table

plot(df$GOOG.Close, type = "l")
