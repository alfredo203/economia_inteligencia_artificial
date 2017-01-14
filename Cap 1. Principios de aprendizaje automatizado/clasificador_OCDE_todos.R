## Clasificador para todos los países

#install.packages("dplyr") ## librería básica de manejo de datos
library(dplyr) ## se carga la librería

## Juntando bases de datos para flag OCDE
A <- read.csv(file = "Cap 1. Principios de aprendizaje automatizado/data/all_contries.csv", 
              stringsAsFactors = F)

B <- read.csv(file = "Cap 1. Principios de aprendizaje automatizado/data/ocde_members.csv", 
             stringsAsFactors = F) %>% select(-Date)

df2 <- left_join(A, B)

## Se pintan los casos que no son miembros de la OCDE con color 1 (negro)
df2$OCDE_member[is.na(df2$OCDE_member)] <- 1


## Cambiando los nombres de todas las variables
## se utiliza una lista de nombres más sencilla de interpretar
names(df2) <-c("Nombre_pais",
               "Codigo_pais",
               "GNI_PC_PPP",
               "Tasa_mortalidad",
               "OCDE_member")
df2$OCDE_member[df2$Nombre_pais == "World"] <- 3


par(mfrow=c(1,1))
plot(df2$Tasa_mortalidad, 
     df2$GNI_PC_PPP ,
     col = df2$OCDE_member,
     xlab = "Tasa de Mortalidad infantil por cada mil nacimientos (2015)",
     ylab = "Ingreso Nacional Bruto precios corrientes PPP (2015) ")
abline(a, col = "blue", lwd = 3)

## Se pueden agregar otras líneas para mejorar el poder de clasificación del algoritmo

# a$coefficients <- a$coefficients * -2
# abline(a, col = "blue", lwd = 3 )
# 
# a$coefficients <- abs(a$coefficients) + 100
# abline(a, col = "blue", lwd = 3 )

## Se crean puntos hipotéticos
x <- c(5, 7, 9)
y <- c(17000, 70000,32000)
p<- data.frame(x, y)

## Se pintan los puntos al gráfico anterior
points(p, col = "cyan", lwd = 10)
