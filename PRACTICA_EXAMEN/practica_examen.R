#-------------------------------------identificar la correlacion--------------------------------------------------------
cor(mtcars$mpg, mtcars[, c("disp", "hp", "drat", "qsec", "wt", "vs", "am", "gear", "carb")])

#--------------------------------------Regresión lineal simple--------------------------------------------------

#Cargar el archivo desde un csv
dengue_dataset <- 
  read.csv('C:/Users/anna/Desktop/9 cuatrimestre/DENGUE/DENGUE.csv', 
           header = TRUE, sep = ",")

#Creamos y cargamos el dataset
data("dengue_dataset");


#Se deben seleccionar las variables 
#Variable independiente x (predictora)
#Variable dependiente y (respuesta)

#Para obtener solo un conjunto o columna de valores de un dataset se hace de la siguiente manera
x <- dengue_dataset$SEMANA; #Variable Independiente (Predictora)
y <- dengue_dataset$CASOS_CONFIRMADOS; #Variable Dependiente (Respuesta)

#Despues se hace el calculo de medias
x_media <- mean(x);
y_media <- mean(y);

#Calculo de varianza
var_x <- sum((x - x_media)^2);

#Cálculo de la covarianza
cov_xy <- sum((x-x_media)*(y-y_media))

#Cálculo de los coeficientes de regresión
#Obtener el valor de beta 1
beta_1 <- cov_xy /var_x;

#Obtener el valor de beta 0
beta_0 <- y_media - (beta_1*x_media)


#Predicción de un nuevo valor
#Valor de semana
x_nuevo <- 20;

#Prediccion del nuevo valor
y_predicted <- beta_0 + (beta_1 * x)

y_predicted_nuevo <- beta_0 + (beta_1 * x_nuevo);

#Cálculo de R^2 y MSE
#Calcular SS total
ss_total <- sum((y - mean(y))^2);

#Calcular ss residual
ss_residual <- sum((y - y_predicted)^2)


#Caculo de R^2
r_cuadrada <- 1 - (ss_residual/ss_total)

#Cálculo de MSE
mse <- mean((y - y_predicted) ^2)


plot(x,y, main = "Regresión Lineal Simple Panorama Epidemiológico de Dengue",
     xlab = "SEMANA", ylab = "CASOS CONFIRMADOS")

abline(beta_0, beta_1, col = "red")


#--------------------------------------Regresión lineal multiple--------------------------------------------------

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

#--------------------------------------Regresión polinomial--------------------------------------------------
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

#--------------------------------------KNN--------------------------------------------------
# ----------------------------------------------
# 1. CARGA DE LIBRERÍAS Y DEFINICIÓN DE FUNCIONES
# ----------------------------------------------

library(class) # Para usar la función knn

# Función para calcular el umbral alto normalizado (entre 0 y 1)
calcular_umbral_alto_normalizado <- function(casos_confirmados, umbral_casos) {
  umbral_alto_normalizado <- umbral_casos / max(casos_confirmados)
  return(umbral_alto_normalizado)
}

# Función para normalizar datos (escalado min-max)
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# ----------------------------------------------
# 2. CARGA Y PREPARACIÓN DE DATOS
# ----------------------------------------------

# Importar el conjunto de datos
dengue_dataset <- 
  read.csv('C:/Users/LENOVO/Documents/9IDGS_psfs/ECBD/Actividades/Integradora/DENGUE.csv', 
           header = TRUE, sep = ",")

# Seleccionar las variables relevantes
dengue_data <- dengue_dataset[, c("CASOS_CONFIRMADOS", "CASOS_PROBABLES")]

# Normalizar las variables 
# La normalización ayuda a que el modelo KNN aprenda patrones en los datos sin 
# verse afectado por las diferencias en las escalas de las variables.
dengue_data_norm <- as.data.frame(lapply(dengue_data, normalize))

# Crear etiquetas de gravedad (Alta/Baja)
# Se colo 100 como un umbral alto de casos confirmados
umbral_alto <- calcular_umbral_alto_normalizado(dengue_data$CASOS_CONFIRMADOS, 100) 
dengue_labels <- ifelse(dengue_data_norm$CASOS_CONFIRMADOS > umbral_alto, "Alta", "Baja")

# ----------------------------------------------
# 3. DIVISIÓN DE DATOS (ENTRENAMIENTO Y PRUEBA)
# ----------------------------------------------

# Plantar semilla de reproducibilidad
set.seed(123) 

# Dividir el dataset en un 80% para entrenamiento y un 20% para prueba
# - train_data y train_labels: Contienen los datos y etiquetas del 80% de las 
# filas seleccionadas aleatoriamente (conjunto de entrenamiento).
# - test_data y test_labels: Contienen los datos y etiquetas del 20% restante de 
# las filas (conjunto de prueba).
sample_index <- sample(1:nrow(dengue_data_norm), 0.8 * nrow(dengue_data_norm))

train_data <- dengue_data_norm[sample_index, ]
test_data <- dengue_data_norm[-sample_index, ]

train_labels <- dengue_labels[sample_index]
test_labels <- dengue_labels[-sample_index]

# ----------------------------------------------
# 4. APLICACIÓN DEL MODELO KNN
# ----------------------------------------------

# Número de vecinos a considerar
k <- 3 

# Aplicación de knn
predicciones <- knn(train = train_data, 
                    test = test_data, 
                    cl = train_labels, 
                    k = k)

# ----------------------------------------------
# 5. EVALUACIÓN DEL MODELO
# ----------------------------------------------

# Matriz de confusión
tabla_confusion <- table(predicciones, test_labels)
cat("Tabla de confusión", "\n")
print(tabla_confusion)

# Precisión (Accuracy)
accuracy <- sum(diag(tabla_confusion)) / sum(tabla_confusion)
cat("Precisión del modelo (Accuracy):", accuracy, "\n")

# Error de Clasificación 
error_clasificacion <- 1 - accuracy
cat("Error de Clasificación:", error_clasificacion, "\n")

cat("Predicciones", "\n")
print(predicciones)

# ----------------------------------------------
# 6. VISUALIZACIÓN DE RESULTADOS
# ----------------------------------------------

plot(dengue_data_norm$CASOS_CONFIRMADOS, dengue_data_norm$CASOS_PROBABLES,
     col = ifelse(dengue_labels == "Alta", "red", "blue"),
     pch = 16,
     xlab = "Casos Confirmados (Normalizados)",
     ylab = "Casos Probables (Normalizados)",
     main = "Clasificación de Semanas Epidémicas")
legend("topright", legend = c("Alta", "Baja"), col = c("red", "blue"), pch = 16)

#--------------------------------------Clasificador de Bayes--------------------------------------------------
# ----------------------------------------------
# 1. CARGA DE LIBRERÍAS Y DEFINICIÓN DE FUNCIONES
# ----------------------------------------------

library(e1071)  # Para usar la función naiveBayes

# Función para calcular el umbral alto normalizado (entre 0 y 1)
calcular_umbral_alto_normalizado <- function(casos_confirmados, umbral_casos) {
  umbral_alto_normalizado <- umbral_casos / max(casos_confirmados)
  return(umbral_alto_normalizado)
}

# Función para normalizar datos (escalado min-max)
normalize <- function(x, min_val = NULL, max_val = NULL) {
  if (is.null(min_val)) min_val <- min(x)
  if (is.null(max_val)) max_val <- max(x)
  return ((x - min_val) / (max_val - min_val))
}

# ----------------------------------------------
# 2. CARGA Y PREPARACIÓN DE DATOS
# ----------------------------------------------

# Importar el conjunto de datos
dengue_dataset <- 
  read.csv('C:/Users/LENOVO/Documents/9IDGS_psfs/ECBD/Actividades/Integradora/DENGUE.csv', 
           header = TRUE, sep = ",")

# Seleccionar las variables relevantes
dengue_data <- dengue_dataset[, c("CASOS_CONFIRMADOS", "CASOS_PROBABLES")]

# Normalizar las variables 
# La normalización ayuda a que el modelo Naive Bayes aprenda patrones en los datos sin 
# verse afectado por las diferencias en las escalas de las variables.
min_confirmados <- min(dengue_data$CASOS_CONFIRMADOS)
max_confirmados <- max(dengue_data$CASOS_CONFIRMADOS)
min_probables <- min(dengue_data$CASOS_PROBABLES)
max_probables <- max(dengue_data$CASOS_PROBABLES)

dengue_data_norm <- data.frame(
  CASOS_CONFIRMADOS = normalize(dengue_data$CASOS_CONFIRMADOS, min_confirmados, max_confirmados),
  CASOS_PROBABLES = normalize(dengue_data$CASOS_PROBABLES, min_probables, max_probables)
)

# Crear etiquetas de gravedad (Alta/Baja)
# Se colo 100 como un umbral alto de casos confirmados
umbral_alto <- calcular_umbral_alto_normalizado(dengue_data$CASOS_CONFIRMADOS, 100) 
dengue_labels <- ifelse(dengue_data_norm$CASOS_CONFIRMADOS > umbral_alto, "Alta", "Baja")

# ----------------------------------------------
# 3. DIVISIÓN DE DATOS (ENTRENAMIENTO Y PRUEBA)
# ----------------------------------------------

# Plantar semilla de reproducibilidad
set.seed(123) 

# Dividir el dataset en un 80% para entrenamiento y un 20% para prueba
# - train_data y train_labels: Contienen los datos y etiquetas del 80% de las 
# filas seleccionadas aleatoriamente (conjunto de entrenamiento).
# - test_data y test_labels: Contienen los datos y etiquetas del 20% restante de 
# las filas (conjunto de prueba).
sample_index <- sample(1:nrow(dengue_data_norm), 0.8 * nrow(dengue_data_norm))

train_data <- dengue_data_norm[sample_index, ]
test_data <- dengue_data_norm[-sample_index, ]

train_labels <- dengue_labels[sample_index]
test_labels <- dengue_labels[-sample_index]

# ----------------------------------------------
# 4. APLICACIÓN DEL MODELO NAIVE BAYES
# ----------------------------------------------

# Entrenar el modelo Naive Bayes
modelo <- naiveBayes(train_data, as.factor(train_labels))

# Hacer predicciones
predicciones <- predict(modelo, test_data)

# ----------------------------------------------
# 5. EVALUACIÓN DEL MODELO
# ----------------------------------------------

# Matriz de confusión
tabla_confusion <- table(predicciones, test_labels)
cat("Tabla de confusión", "\n")
print(tabla_confusion)

# Precisión (Accuracy)
accuracy <- sum(diag(tabla_confusion)) / sum(tabla_confusion)
cat("Precisión del modelo (Accuracy):", accuracy, "\n")

# Error de Clasificación 
error_clasificacion <- 1 - accuracy
cat("Error de Clasificación:", error_clasificacion, "\n")

cat("Predicciones", "\n")
print(predicciones)

# ----------------------------------------------
# 6. EJEMPLO DE PREDICCIÓN CON UN NUEVO DATO
# ----------------------------------------------

nuevo_dato <- data.frame(
  CASOS_CONFIRMADOS = normalize(c(90), min_confirmados, max_confirmados),
  CASOS_PROBABLES = normalize(c(90), min_probables, max_probables)
)
prediccion_nuevo <- predict(modelo, nuevo_dato)
cat("Nuevo dato", "\n")
print(nuevo_dato)
cat(paste("Predicción para el nuevo dato:", prediccion_nuevo), "\n")

# ----------------------------------------------
# 7. VISUALIZACIÓN DE RESULTADOS
# ----------------------------------------------

plot(dengue_data_norm$CASOS_CONFIRMADOS, dengue_data_norm$CASOS_PROBABLES,
     col = ifelse(dengue_labels == "Alta", "red", "blue"),
     pch = 16,
     xlab = "Casos Confirmados (Normalizados)",
     ylab = "Casos Probables (Normalizados)",
     main = "Clasificación de Semanas Epidémicas")
legend("topright", legend = c("Alta", "Baja"), col = c("red", "blue"), pch = 16)


#----------------------------------------------------KNN version profe-------------------------------------------------
  # Función para calcular la distancia euclidiana
  euclidean_distance <- function(point1, point2) {
    sqrt(sum((point1 - point2)^2))
  }

# Función principal del clasificador KNN
knn_classifier <- function(train_data, train_labels, test_point, k) {
  distances <- sapply(1:nrow(train_data), function(i) {
    euclidean_distance(test_point, train_data[i,])
  })
  
  k_nearest_indices <- order(distances)[1:k]
  k_nearest_labels <- train_labels[k_nearest_indices]
  
  predicted_label <- names(which.max(table(k_nearest_labels)))
  return(predicted_label)
}

# Datos de entrenamiento: [goles_por_temporada, asistencias_por_temporada]
train_data <- matrix(c(
  20, 5,   # Jugador 1 del Equipo A
  15, 10,  # Jugador 2 del Equipo A
  18, 7,   # Jugador 3 del Equipo A
  22, 3,   # Jugador 4 del Equipo A
  12, 12,  # Jugador 5 del Equipo A
  8, 15,   # Jugador 1 del Equipo B
  10, 12,  # Jugador 2 del Equipo B
  6, 18,   # Jugador 3 del Equipo B
  9, 14,   # Jugador 4 del Equipo B
  7, 16    # Jugador 5 del Equipo B
), ncol=2, byrow=TRUE)

# Etiquetas de los equipos
train_labels <- c(rep("Equipo A", 5), rep("Equipo B", 5))

# Nuevo jugador a clasificar
nuevo_jugador <- c(14, 9)  # 14 goles, 9 asistencias por temporada
k <- 3

# Predicción
equipo_predicho <- knn_classifier(train_data, train_labels, nuevo_jugador, k)

cat("Predicción para el nuevo jugador (", 
    nuevo_jugador[1], "goles,", nuevo_jugador[2], "asistencias):\n",
    "El jugador probablemente pertenece al", equipo_predicho, "\n")

# Visualización de los datos
plot(train_data[,1], train_data[,2], 
     col = ifelse(train_labels == "Equipo A", "red", "blue"),
     pch = 16, 
     xlab = "Goles por temporada", 
     ylab = "Asistencias por temporada",
     main = "Clasificación de jugadores")

# Añadir el nuevo jugador con una etiqueta
points(nuevo_jugador[1], nuevo_jugador[2], col = "green", pch = 8, cex = 2)
text(nuevo_jugador[1], nuevo_jugador[2], 
     labels = paste("Nuevo\n", equipo_predicho), 
     pos = 3, col = "green", offset = 1)

legend("topright", 
       legend = c("Equipo A", "Equipo B", "Nuevo jugador"),
       col = c("red", "blue", "green"),
       pch = c(16, 16, 8))

#------------------------------------------- bayes version profe---------------------------------------
  # Simular datos
  set.seed(123)
n <- 100
clase <- rep(c("Masculino", "Femenino"), each = n/2)
altura <- c(rnorm(n/2, mean = 175, sd = 10), rnorm(n/2, mean = 165, sd = 10))
peso <- c(rnorm(n/2, mean = 70, sd = 10), rnorm(n/2, mean = 60, sd = 10))
datos <- data.frame(clase, altura, peso)
head(datos)


# Calcular probabilidades previas
P_Masculino <- sum(datos$clase == "Masculino") / n
P_Femenino <- sum(datos$clase == "Femenino") / n

# Calcular medias y desviaciones estándar por clase
mean_altura_m <- mean(datos$altura[datos$clase == "Masculino"])
sd_altura_m <- sd(datos$altura[datos$clase == "Masculino"])
mean_peso_m <- mean(datos$peso[datos$clase == "Masculino"])
sd_peso_m <- sd(datos$peso[datos$clase == "Masculino"])

mean_altura_f <- mean(datos$altura[datos$clase == "Femenino"])
sd_altura_f <- sd(datos$altura[datos$clase == "Femenino"])
mean_peso_f <- mean(datos$peso[datos$clase == "Femenino"])
sd_peso_f <- sd(datos$peso[datos$clase == "Femenino"])




# Función de densidad de la normal
dnorm_custom <- function(x, mean, sd) {
  (1 / (sqrt(2 * pi) * sd)) * exp(-0.5 * ((x - mean) / sd)^2)
}

# Nueva observación
nuevo_dato <- data.frame(altura = 170, peso = 65)

# Calcular verosimilitudes
P_X_given_Masculino <- dnorm_custom(nuevo_dato$altura, mean_altura_m, sd_altura_m) * 
  dnorm_custom(nuevo_dato$peso, mean_peso_m, sd_peso_m)
P_X_given_Femenino <- dnorm_custom(nuevo_dato$altura, mean_altura_f, sd_altura_f) * 
  dnorm_custom(nuevo_dato$peso, mean_peso_f, sd_peso_f)

# Calcular probabilidades posteriores
P_Masculino_given_X <- P_X_given_Masculino * P_Masculino
P_Femenino_given_X <- P_X_given_Femenino * P_Femenino





# Normalizar las probabilidades posteriores
P_total <- P_Masculino_given_X + P_Femenino_given_X
P_Masculino_given_X <- P_Masculino_given_X / P_total
P_Femenino_given_X <- P_Femenino_given_X / P_total

# Asignar clase basada en la mayor probabilidad posterior
prediccion <- ifelse(P_Masculino_given_X > P_Femenino_given_X, "Masculino", "Femenino")
print(prediccion)










