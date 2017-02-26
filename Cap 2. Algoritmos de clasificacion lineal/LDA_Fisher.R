## LDA para datos de Fisher 1936

library(dplyr)

## Se carga la informacion
df <- iris

## Se describen las primeras líneas de los datos
head(df)

## Se grafican las 5 dimensiones de la información
## se colorea según la especie a la que pertenece cada flor
plot(df , col = df$Species)
plot(df %>% dplyr::select(-Species), col = df$Species)

## Se grafican 2 dimensiones y se colocan líneas arbitrarias
## como ejemplo de segmentación manual

plot(df$Petal.Length, df$Sepal.Length, col = df$Species)
abline(v = 2.5, h = 6, lty = 3)
abline(v = 4.8, col ="blue")



## Fase de entrenamiento
## Selección del 70% de la información

porcentaje <- 70/100

num_muestras <- porcentaje * nrow(df)

## Muestreo aleatorio
set.seed(20)
train_ind <- sample(seq_len(nrow(df)), size = num_muestras)

## Seleccionamos las muestras que pertenecen
## al set de entrenamiento
train_set <- df[train_ind, ]

## Seleccionamos el resto de muestras
## que pertenecen al set de pruebas
test_set <- df[-train_ind, ]

## Ejecución del algoritmo de LDA

library(MASS)
source(file = 'Cap 2. Algoritmos de clasificacion lineal/diferencias_iris.R')

modelo <- lda(data =train_set, Species ~.)

## Proabilidades a priori del modelo
modelo$prior

## Número de observaciones por clase
modelo$counts

## Se predice el modelo sobre el set
## de entrenamiento, por default 
## no se especifica un dataset nuevo
prediccion <- predict(modelo)



## Gráfica de clasificación original
par(mfrow=c(2,1))
plot(prediccion$x, 
     col =train_set$Species,
     main = "Original")

## Gráfica de clasificación según el modelo estimado
plot(prediccion$x, 
     col =prediccion$class,
     main = "Predicción")

## Matriz de confusión
## Se muestran las clasificaciones correctas
## en la diagonal para el set de entrenamiento
table(prediccion$class, train_set$Species)


## Gráfica de barras por número de observaciones
barplot(summary(train_set$Species), main = "Conjunto de pruebas")

barplot(summary(test_set$Species), main = "Conjunto de entrenamiento")

## Validación del modelo LDA
## Se ejecutan las predicciones sobre
## el set de pruebas con el modelo original "modelo"
prediccion <- predict(modelo, newdata = test_set)

## Matriz de confusión
## Se muestran las clasificaciones correctas
## en la diagonal para el set de pruebas
table(prediccion$class, test_set$Species)

diferentes <- diferencias_iris(prediccion, test_set)
