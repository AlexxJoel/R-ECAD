# Importar el dataset con encabezados
dengue_dataset <- read.csv('C:/Users/anna/Desktop/9 cuatrimestre/DENGUE/DENGUE.csv', header = TRUE, sep = ",")

# Verificar si hay valores faltantes
print(colSums(is.na(dengue_dataset)))

# Convertir las columnas a tipo numérico si es necesario
dengue_dataset$CASOS_PROBABLES <- as.numeric(dengue_dataset$CASOS_PROBABLES)
dengue_dataset$CASOS_CONFIRMADOS_ACUMULADOS <- as.numeric(dengue_dataset$CASOS_CONFIRMADOS_ACUMULADOS)
dengue_dataset$CASOS_PROBABLES_ACUMULADOS <- as.numeric(dengue_dataset$CASOS_PROBABLES_ACUMULADOS)
dengue_dataset$CASOS_CONFIRMADOS <- as.numeric(dengue_dataset$CASOS_CONFIRMADOS)

# Manejar valores faltantes (reemplazar con la media de la columna)
dengue_dataset$CASOS_PROBABLES[is.na(dengue_dataset$CASOS_PROBABLES)] <- 
  mean(dengue_dataset$CASOS_PROBABLES, na.rm = TRUE)
dengue_dataset$CASOS_CONFIRMADOS_ACUMULADOS[is.na(dengue_dataset$CASOS_CONFIRMADOS_ACUMULADOS)] <- 
  mean(dengue_dataset$CASOS_CONFIRMADOS_ACUMULADOS, na.rm = TRUE)
dengue_dataset$CASOS_PROBABLES_ACUMULADOS[is.na(dengue_dataset$CASOS_PROBABLES_ACUMULADOS)] <- 
  mean(dengue_dataset$CASOS_PROBABLES_ACUMULADOS, na.rm = TRUE)
dengue_dataset$CASOS_CONFIRMADOS[is.na(dengue_dataset$CASOS_CONFIRMADOS)] <- 
  mean(dengue_dataset$CASOS_CONFIRMADOS, na.rm = TRUE)

# Revisar la estructura del dataset
str(dengue_dataset)


# Variables independientes y dependiente
x <- as.matrix(cbind(1, dengue_dataset$CASOS_PROBABLES,
                     dengue_dataset$CASOS_CONFIRMADOS_ACUMULADOS, dengue_dataset$CASOS_PROBABLES_ACUMULADOS))
y <- as.numeric(dengue_dataset$CASOS_CONFIRMADOS)

# Verificar que x y y sean matrices numéricas
print(head(x))
print(head(y))


# Coeficientes beta usando álgebra matricial
beta <- solve(t(x) %*% x) %*% t(x) %*% y


# Predicción de valores
y_pred <- x %*% beta

# Cálculo de R-cuadrado
ss_total <- sum((y - mean(y))^2)
ss_residual <- sum((y - y_pred)^2)
r_squared <- 1 - (ss_residual / ss_total)

# Cálculo de MSE
mse <- mean((y - y_pred)^2)

# Resultados
resultados <- list(beta0 = beta[1], beta1 = beta[2], beta2 = beta[3], beta3 = beta[4], r_squared = r_squared)
print(resultados)


# Nuevos Valores para predicción
CASOS_PROBABLES <- 406
CASOS_CONFIRMADOS_ACUMULADOS <- 533
CASOS_PROBABLES_ACUMULADOS <- 6587

# Vector de nuevos valores
nuevos_valores <- c(1, CASOS_PROBABLES, CASOS_CONFIRMADOS_ACUMULADOS, CASOS_PROBABLES_ACUMULADOS)

# Predicción
prediccion_mpg <- sum(nuevos_valores * beta)
print(prediccion_mpg)

# Gráfico 3D de dispersión y plano de regresión
library(scatterplot3d)
scatterplot3d(dengue_dataset$CASOS_PROBABLES, dengue_dataset$CASOS_CONFIRMADOS_ACUMULADOS,
              pch = 19, type = "h", color = "blue",
              angle = 55)
scatterplot3d(dengue_dataset$CASOS_PROBABLES, dengue_dataset$CASOS_PROBABLES_ACUMULADOS,
              pch = 19, type = "h", color = "blue",
              angle = 55)
