#-------  Cálculo de medias y varianzas --------#
# Cargar los datos
data("iris")

# Definimos el conjunto de x y el conjunto de y
x <- iris$Sepal.Length # Variable independiente
y <- iris$Petal.Length # Variable dependiente

# Obtener la media de cada variable
x_avg <- mean(x)
y_avg <- mean(y)

# Obtener la varianza de x
var_x <- sum((x - x_avg)^2)

# Obtener la covarianza entre x e y
cov_xy <- sum((x - x_avg) * (y - y_avg))

#--- Cálculo de los coeficientes de regresión ---#

# Calcular el valor de Beta 1 (pendiente)
beta_1 <- cov_xy / var_x

# Calcular el valor de Beta 0 (intercepto)
beta_0 <- y_avg - (beta_1 * x_avg)

#--- Predicción de valores y evaluación del modelo ---#

# Calcular los valores predichos
y_calculo <- beta_0 + beta_1 * x

# Calcular el coeficiente de determinación R-cuadrado
ss_total <- sum((y - y_avg)^2)
ss_residual <- sum((y - y_calculo)^2)
r_cuadrado <- 1 - (ss_residual / ss_total)

# Calcular el Error Cuadrático Medio (MSE)
mse <- mean((y - y_calculo)^2)

# Resultados
resultados <- list(beta0 = beta_0, beta1 = beta_1, r_squared = r_cuadrado, mse = mse)
print(resultados)

#--- Gráfico de dispersión y línea de regresión ---#

# Crear el gráfico de dispersión
plot(x, y, main = "Regresión lineal simple", xlab = "Sepal", ylab = "Petal")
# Añadir la línea de regresión
abline(beta_0, beta_1, col = 'red')

#--- Predicción para un nuevo valor  ---#

# Nuevo valor
valor_predecir <- 5.5

# Calcular la predicción
prediccion <- beta_0 + beta_1 * valor_predecir
print(paste("Predicción de ", valor_predecir, ":", prediccion))
