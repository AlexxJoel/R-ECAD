# Cargar el dataset de los rock
data("rock")

# Ajusta el modelo polinomial de grado a usando de area
modelo <- lm(rock$perm ~ poly(rock$area, 2), data = rock)

# Resumen del modelo usando de summary
summary(modelo)

# Predicción de nuevo valor de nuevo prediccion
nuevo_peso <- data.frame(area = c(10,20,80))
prediccion <- predict(modelo, nuevo_peso)
print(prediccion)

# Cálculo del R-Cuadrado que se toma desde de rock
y_pred <- predict(modelo, rock)
ss_total <- sum((rock$perm - mean(rock$perm)) ^ 2)
ss_residual <- sum((rock$perm - y_pred) ^2 )
r_squared <- 1 - (ss_residual / ss_total)
r_squared

# Gráfico de dispersión y línea de regresión polinomial
plot(
  rock$area, rock$perm, main = "Regresión polinomial",
  xlab = "area",
  ylab = "permn"
)
lines(sort(rock$area), fitted(modelo)[order(rock$area)], col = "red")