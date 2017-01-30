## LDA para datos de Fisher 1936

## Se carga la informacion
df <- iris

## Se describen las primeras líneas de los datos
head(df)

## Se grafican las 5 dimensiones de la información
## se colorea según la especie a la que pertenece cada flor
plot(df %>% select(-Species), col = df$Species)

## S

plot(df$Petal.Length, df$Sepal.Length, col = df$Species)
abline(v = 2.5, h = 6, lty = 3)
abline(v = 4.8, col ="blue")
