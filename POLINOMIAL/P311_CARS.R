#Herrera Hernández Joel Alejandro 9A

# Cargar el dataset
data("cars")  # Carga el conjunto de datos 'cars'

# Variables
x <- cars$speed  # Asigna la columna 'speed' del dataset a la variable x
y <- cars$dist   # Asigna la columna 'dist' del dataset a la variable y
y_media <- mean(y)  # Calcula la media de y (distancias)
dataset <- cars  # Asigna el dataset completo a la variable 'dataset'

# Ajusta el modelo polinomial de grado 2
modelo <- lm(y ~ poly(x, 2), data = dataset)  # polinomial de grado 2

# Resumen del modelo
summary(modelo)  # Muestra un resumen del modelo ajustado, incluyendo coeficientes y estadísticas de ajuste

# Predicción de valores
nuevo_valor <- data.frame(x = c(7.0, 4.0, 3.0)) # 'speed' para predicción
prediccion <- predict(modelo, nuevo_valor)  # Predice las distancias para los nuevos valores de 'speed'
print(prediccion)

# Cálculo del R-Cuadrado
y_pred <- predict(modelo, dataset)  # Predice las distancias
ss_total <- sum((y - y_media) ^ 2)
ss_residual <- sum((y - y_pred) ^ 2) # residuales (variación no explicada por el modelo)
r_squared <- 1 - (ss_residual / ss_total)  # Calcula el R-cuadrado (proporción de variación explicada por el modelo)
r_squared  # Imprime el valor del R-cuadrado

# Gráfico de dispersión y línea de regresión polinomial
plot(
  x, y, main = "Regresion polinomial",
  xlab = "Velocidad (speed)",  # Etiqueta del eje X
  ylab = "Distancia (dist)"    # Etiqueta del eje Y
)
lines(sort(x), fitted(modelo)[order(x)], col = "red")  # Añade la línea de regresión polinomial en rojo