## Marketing retail clients segmentation

library(dplyr)

## Se carga la información
df <- read.csv("Online Retail.csv", stringsAsFactors = F) %>% 
  filter(Quantity >0, UnitPrice > 0) ## Se eliminan devoluciones de producto (notas de compra)


## Se eligen dos países
muestra <- df %>% filter(Country == "Poland" | Country == "Japan")
muestra$Country <- as.factor(muestra$Country)

## Se reescala la información en logaritmos
# plot(f$Quantity, f$UnitPrice, col = as.factor(f$Country))

muestra$Quantity[muestra$Quantity == 1] <- 2

## Construcción de los vectores de logaritmos
muestra$log_Quantity     <- log(muestra$Quantity)
muestra$log_UnitPrice    <- log(muestra$UnitPrice)

## Colores por país
plot(muestra$log_Quantity, muestra$log_UnitPrice, col = muestra$Country)

## Colores por cliente
plot(muestra$log_Quantity, muestra$log_UnitPrice, col = muestra$CustomerID)


## Selección de información relevante

muestra <- muestra %>% dplyr::select(Country, log_Quantity, log_UnitPrice)

## Fase de entrenamiento
## Selección del 70% de la información

porcentaje <- 70/100

num_muestras <- porcentaje * nrow(muestra)

  set.seed(4)
test_set<- muestra[sample(nrow(muestra), num_muestras), ]


## Ejecución del algoritmo de LDA

library(MASS)

modelo <- lda(data =test_set, Country ~.)
prediccion <- predict(modelo)
plot(modelo)
plot(prediccion$x, col =prediccion$class)
plot(prediccion$x, col =test_set$Country)



# Workshop 
# 
# library(dplyr)
# 
# l <-a %>% select(CustomerID, 
#                Country, UnitPrice) %>% 
#   group_by(CustomerID, Country) %>% 
#   summarise(total_comprado = sum(UnitPrice))
# 
# f <-l %>% group_by(Country) %>% summarise(total = sum(total_comprado))
