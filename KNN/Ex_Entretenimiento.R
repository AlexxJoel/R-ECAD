# Datos de entrenamiento: Goles por temporada, asistencia por temporada

train_data <- matrix(c(
  20, 5,  # Jugador 1 del equipo A
  15, 10,  # Jugador 2 del equipo A
  18, 7,   # Jugador 3 del equipo A
  22, 3,   # Jugador 4 del equipo A
  12, 12,  # Jugador 5 del equipo A
  8, 15,    # Jugador 1 del equipo B
  10, 12,  # Jugador 2 del equipo B
  6, 18,  # Jugador 3 del equipo B
  9, 14,  # Jugador 4 del equipo B
  7, 16  # Jugador 5 del equipo B
), ncol = 2, byrow = TRUE)

# ncol es el número de columnas
# byrow es si se llena por fila o por columna
#etiquetar los conjuntos
train_labels <- c(rep("A", 5), rep("B", 5))

print(train_data)
print(train_labels)

# Funcion para calcular la distancia euclidiana
euclidean_distance <- function(x, y) {
  # la raiz cuadrada de la suma de las diferencias al cuadrado
  sqrt(sum((x - y)^2))
}

# Funcion KNN
clasificador_knn <- function(new_data, labels, test_point, k) {
  # Calcular la distancia entre el punto de prueba y todos los puntos de entrenamiento
  distances <- sapply(1:nrow(new_data), function(i){
    # Hace una iteracion por cada fila de new_data
    euclidean_distance(test_point, new_data[i,])
  })

  # Ordenar los indices de las distancias
  sorted_indices <- order(distances)

  # Obtener los k vecinos mas cercanos
  k_nearest_labels <- labels[sorted_indices[1:k]]

  # Contar las ocurrencias de cada etiqueta
  label_counts <- table(k_nearest_labels)

  # Obtener la etiqueta con mas ocurrencias
  most_common_label <- names(which.max(label_counts))

  return(most_common_label)
}