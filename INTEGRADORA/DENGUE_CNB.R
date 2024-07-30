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
