### clasificador de países OCDE

## Lectura del archivo
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


## Gráfica de dispersión de los datos seleccionados
plot(df$Tasa_mortalidad, 
     df$GNI_PC_PPP, col = df$OCDE)

## Estimación del modelo de regresión lineal.
## se especifica que el ingreso nacional bruto = f(Tasa de mortalidad)

a <- lm(data = df,  GNI_PC_PPP ~ Tasa_mortalidad)
abline(a, col = "blue", lwd = 3)


## Para todos los países
library(dplyr)

## Juntando bases de datos para flag OCDE

A <- read.csv("Cap 1. Principios de aprendizaje automatizado/all_contries.csv", stringsAsFactors = F)
B<- read.csv("Cap 1. Principios de aprendizaje automatizado/ocde_members.csv", stringsAsFactors = F) %>% select(-Date)

df <- left_join(A, B)

df$OCDE_member[is.na(df$OCDE_member)] <- 1

names(df) <-c("Nombre_pais",
               "Codigo_pais",
               "GNI_PC_PPP",
               "Tasa_mortalidad",
               "OCDE_member")

plot(df$Tasa_mortalidad, 
     df$GNI_PC_PPP, 
     col = df$OCDE_member,
     xlab = "Tasa de Mortalidad infantil por cada mil nacimientos (2015)",
     ylab = "Ingreso Nacional Bruto precios corrientes PPP (2015) ")

b <- lm(data = df2,  GNI_PC_PPP ~ Tasa_mortalidad)
abline(b, col = "red", lwd = 3)

df3<-anti_join(df2, df)

c <- lm(data = df3,  GNI_PC_PPP ~ Tasa_mortalidad)
abline(b, col = "black", lwd = 3)

