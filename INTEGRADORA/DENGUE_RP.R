# Importación de DATA
dengue_dataset <- 
  read.csv('C:/Users/LENOVO/Documents/9IDGS_psfs/ECBD/Actividades/Integradora/DENGUE.csv', 
           header = TRUE, sep = ",")

# Selección de variables
# Variable independiente (Predictora): SEMANA
# Variable dependiente (Respuesta): CASOS_CONFIRMADOS
x <- dengue_dataset$SEMANA # Variable independiente (Predictora)
y <- dengue_dataset$CASOS_CONFIRMADOS # Variable dependiente (Respuesta)

# Cálculo de media
y_media <- mean(y)

# Ajusta el modelo polinomial de grado 10
# Se elgió el grado 10 dado a que de ahí en adelante, el valor
# obtenido no dista mucho en cuanto a decimales.
modelo <- lm(y ~ poly(x, 10), data = dengue_dataset)

# Predicción de valores
# Se predicen posibles valores para las semanas 1,2 y 3
nuevo_valor <- data.frame(x = c(1,2,3))
prediccion <- predict(modelo, nuevo_valor)

# Evaluación del modelo
# Cálculo del R-Cuadrado 
y_pred <- predict(modelo, dengue_dataset)
ss_total <- sum((y - y_media) ^ 2)
ss_residual <- sum((y - y_pred) ^2 )
r_squared <- 1 - (ss_residual / ss_total)

# Cálculo del MSE
mse <- mean((y - y_pred) ^ 2)

# Predicción del modelo
# Gráfico de dispersión y línea de regresión polino<mial
plot(
  x, y, main = "Regresión polinomial",
  xlab = "Semanas",
  ylab = "Casos confirmados"
)
lines(sort(x), fitted(modelo)[order(x)], col = "red")

# Impresión de resultados
print("Modelo")
print(modelo)

# Resumen del modelo
print("Resumen del modelo")
print(summary(modelo))

print("Predicción")
print(prediccion)

print("R cuadrada")
print(r_squared)

print("ss_residual")
print(ss_residual)

print("ss_total")
print(ss_total)

print("MSE")
print(mse)

