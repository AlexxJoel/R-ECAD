data(mtcars)

# Ajustar el modelo polinomial de grado 2
modelo <- lm(mpg ~ poly(wt, 2), data = mtcars)

# Resumen del modelo
summary(modelo)

# predicción de valores
nuevo_peso <- data.frame(wt = c(3.0, 3.5, 4.0))
prediccion <- predict(modelo, nuevo_peso)
print(prediccion)

# Cálculo del R-Cuadrado
y_pred <- predict(modelo, mtcars)
ss_total <- sum((mtcars$mpg - mean(mtcars$mpg)) ^ 2)
ss_residual <- sum((mtcars$mpg - y_pred) ^ 2)
r_squared <- 1 - (ss_residual / ss_total)
r_squared

# Gráfico de dispersión y línea de regresión polinomial
plot(
  mtcars$wt, mtcars$mpg, main = "Regresion polinomial",
  xlab = "Peso (wt)",  # Etiqueta del eje X
  ylab = "Millas por galon (mpg)"    # Etiqueta del eje Y
)
lines(sort(mtcars$wt), fitted(modelo)[order(mtcars$wt)], col = "red")  # Añade la línea de regresión polinomial en rojo

# Nuevo valor para predicción
nuevo_peso <- data.frame(wt = 3.5)

# Predicción
prediccion <- predict(modelo, nuevo_peso)
print(prediccion)