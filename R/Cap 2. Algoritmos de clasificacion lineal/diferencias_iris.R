## Diferencias Iris
## Función para encontrar las diferencias de clasificación para
## los datos de iris 

diferencias_iris <- function(prediccion, test_set){
  f <- data.frame(prediccion$x, 
                  prediccion$class, 
                  test_set$Species)
  
  names(f) <- c("LD1", "LD2", "prediccion", "original")
  f$prediccion <- as.character(f$prediccion)
  f$original <- as.character(f$original)
  
  f[f == "versicolor"] <- 1
  f[f == "setosa"] <- 2
  f[f == "virginica"] <- 3
  
  f$prediccion <- as.numeric(f$prediccion)
  f$original <- as.numeric(f$original)
  f$diferencia <- f$prediccion - f$original
  
  diferentes <- f %>% filter(!diferencia == 0)
  
  diferentes$prediccion[diferentes$prediccion == 3 ] <- "virginica"
  diferentes$prediccion[diferentes$prediccion == 2 ] <- "setosa"
  diferentes$prediccion[diferentes$prediccion == 1 ] <- "versicolor"
  
  diferentes$original[diferentes$original == 3 ] <- "virginica"
  diferentes$original[diferentes$original == 2 ] <- "setosa"
  diferentes$original[diferentes$original == 1 ] <- "versicolor"
  
  diferentes
  
}
