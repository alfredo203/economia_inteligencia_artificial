### clasificador de países OCDE

## Entrenamiento

## Lectura del archivo de entrenamiento
df <- read.csv(file = "Cap 1. Principios de aprendizaje automatizado/data/non_OCDE_countries.csv")

## Se observan las variables
names(df)

## Cambiando los nombres de todas las variables
## se utiliza una lista de nombres más sencilla de interpretar

names(df) <- c("Nombre_pais",
               "Codigo_pais",
               "Tasa_mortalidad",
               "GNI_PC_PPP",
               "OCDE_member")

## Gráfica de dispersión de los datos seleccionados, sin distinción por clase

par(mfrow=c(1,2)) ## Se especifica que se quieren dos gráficas en un mismo renglón

## Gráfica 1 de 2 Mortalidad infantil e ingres nacional bruto.
plot(x = df$Tasa_mortalidad,  ## Variable X
     y = df$GNI_PC_PPP,       ## Variable Y
     xlab = "Tasa de Mortalidad infantil por cada mil nacimientos (2015)",
     ylab = "Ingreso Nacional Bruto precios corrientes PPP (2015) ")


## Estimación del modelo de regresión lineal.
## se especifica que el ingreso nacional bruto = f(Tasa de mortalidad)
## la variable Y del modelo es ingreso nacional bruto
## la variable X del modelo es Tasa de mortalidad

a <- lm(data = df,  GNI_PC_PPP ~ Tasa_mortalidad)

## Se pintan los casos que están mal clasificados con color 3 (verde)

df$OCDE_member[df$Tasa_mortalidad > 75] <- 3

## Gráfica 2 de 2 Mortalidad infantil e ingres nacional bruto, colores por clasificación de país

plot(df$Tasa_mortalidad, 
     df$GNI_PC_PPP, col = df$OCDE,
     xlab = "Tasa de Mortalidad infantil por cada mil nacimientos (2015)",
     ylab = "Ingreso Nacional Bruto precios corrientes PPP (2015) ")

abline(a, col = "blue", lwd = 3)
