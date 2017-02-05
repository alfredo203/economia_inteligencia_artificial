## LDA para datos de Fisher 1936

## Se carga la informacion
df <- iris

## Se describen las primeras líneas de los datos
head(df)

## Se grafican las 5 dimensiones de la información
## se colorea según la especie a la que pertenece cada flor
plot(df %>% select(-Species), col = df$Species)

## Se grafican 2 dimensiones y se colocan líneas arbitrarias
## como ejemplo de segmentación manual

plot(df$Petal.Length, df$Sepal.Length, col = df$Species)
abline(v = 2.5, h = 6, lty = 3)
abline(v = 4.8, col ="blue")



## Fase de entrenamiento
## Selección del 70% de la información

porcentaje <- 70/100

num_muestras <- porcentaje * nrow(df)

set.seed(4)
test_set<- df[sample(nrow(df), num_muestras), ]


## Ejecución del algoritmo de LDA

library(MASS)
source(file = 'Cap 2. Algoritmos de clasificacion lineal/diferencias_iris.R')

modelo <- lda(data =test_set, Species ~.)
prediccion <- predict(modelo)
plot(modelo)
plot(prediccion$x, col =prediccion$class)
plot(prediccion$x, col =test_set$Species)
diferentes <- diferencias_iris(prediccion, test_set)


