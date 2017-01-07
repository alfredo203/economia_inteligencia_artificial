### clasificador de países OCDE

## Entrenamiento

## Lectura del archivo de entrenamiento
df <- read.csv(file = "Cap 1. Principios de aprendizaje automatizado/data_countries.csv")

## Se observan las variables
names(df)

## Cambiando los nombres de todas las variables
## se utiliza una lista más sencilla de interpretar

names(df) <- c("Nombre_pais",
               "Codigo_pais",
               "Tasa_mortalidad",
               "GNI_PC_PPP",
               "OCDE_member")

## Gráfica de dispersión de los datos seleccionados, sin distinción por clase
par(mfrow=c(1,2))

plot(df$Tasa_mortalidad, 
     df$GNI_PC_PPP, 
     xlab = "Tasa de Mortalidad infantil por cada mil nacimientos (2015)",
     ylab = "Ingreso Nacional Bruto precios corrientes PPP (2015) ")


## Estimación del modelo de regresión lineal.
## se especifica que el ingreso nacional bruto = f(Tasa de mortalidad)

a <- lm(data = df,  GNI_PC_PPP ~ Tasa_mortalidad)

## Se pintan los casos que están mal clasificados
df$OCDE_member[df$Tasa_mortalidad > 75] <- 3

plot(df$Tasa_mortalidad, 
     df$GNI_PC_PPP, col = df$OCDE,
     xlab = "Tasa de Mortalidad infantil por cada mil nacimientos (2015)",
     ylab = "Ingreso Nacional Bruto precios corrientes PPP (2015) ")

abline(a, col = "blue", lwd = 3)


## Para todos los países
library(dplyr)

## Juntando bases de datos para flag OCDE

A <- read.csv("Cap 1. Principios de aprendizaje automatizado/all_contries.csv", stringsAsFactors = F)
B<- read.csv("Cap 1. Principios de aprendizaje automatizado/ocde_members.csv", stringsAsFactors = F) %>% select(-Date)

df2 <- left_join(A, B)

df2$OCDE_member[is.na(df2$OCDE_member)] <- 1

names(df2) <-c("Nombre_pais",
               "Codigo_pais",
               "GNI_PC_PPP",
               "Tasa_mortalidad",
               "OCDE_member")
df2$OCDE_member[df2$Nombre_pais == "World"] <- 3


par(mfrow=c(1,1))
plot(df2$Tasa_mortalidad, 
     df2$GNI_PC_PPP, 
     col = df2$OCDE_member,
     xlab = "Tasa de Mortalidad infantil por cada mil nacimientos (2015)",
     ylab = "Ingreso Nacional Bruto precios corrientes PPP (2015) ")
abline(a, col = "blue", lwd = 3)

a$coefficients <- a$coefficients * -2
abline(a, col = "blue", lwd = 3 )

a$coefficients <- abs(a$coefficients) + 100
abline(a, col = "blue", lwd = 3 )