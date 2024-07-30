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