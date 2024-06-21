# Made by: Joel Alejandro Herrera Hernandez

# ---------------------- Estadisticas Descriptivas -----------------------
# Generate 100 random numbers with normal distribution
random_numbers <- rnorm(100, mean = 50, sd = 10)
cat("Numeros aleatorios: ", random_numbers, "\n")

# Calculate mean
mean_value <- mean(random_numbers)
cat("Media: ", mean_value, "\n")

# Calculate median
median_value <- median(random_numbers)
cat("Mediana: ", median_value, "\n")

# Calculate standard deviation
sd_value <- sd(random_numbers)
cat("Desviacion estandar: ", sd_value, "\n")

# Calculate variance
variance_value <- var(random_numbers)
cat("Varianza: ", variance_value, "\n")

# Calculate range
range_value <- range(random_numbers)
cat("Rango: ", range_value, "\n")

# Calculate quartiles
quartiles_value <- quantile(random_numbers)
cat("Cuartiles: ", quartiles_value, "\n")

# Calculate minimum
min_value <- min(random_numbers)
cat("Minimo: ", min_value, "\n")

# Calculate maximum
max_value <- max(random_numbers)
cat("Maximo: ", max_value, "\n")

