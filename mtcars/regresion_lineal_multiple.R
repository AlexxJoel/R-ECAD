
data(mtcars)

# Variables independientes
x <- as.matrix(cbind(1, mtcars$wt, mtcars$hp))
y <- mtcars$mpg

#coefcinete beta usando algebra matricial
beta <- solve(t(x) %*% x) %*% t(x) %*% y
#coeficiente de regresion
beta0 <- beta[1]
beta1 <- beta[2]
beta2 <- beta[3]

# Predicción de valores
y_pred <- x %*% beta
#cálculo de r cuadrado
ss_total <- sum((y - mean(y))^2)
ss_residual <- sum((y - y_pred)^2)
r_squared <- 1 - (ss_residual / ss_total)
#Calculo de MSE
mse <- mean((y - y_pred)^2)
# Resultados
list(beta0 = beta0, beta1 = beta1, beta2 = beta2, r_squared = r_squared, mse = mse)

# nuevos valores para predicción
nuevo_peso <- 3.5
nuevos_hp <- 150

# vector de nuevos valores
nuevos_valores <- c(1, nuevo_peso, nuevos_hp)

# Predicción
prediccion_mpg <- sum(nuevos_valores * beta)
prediccion_mpg

# Gráfico 3D de dispersión y plano de regresión
library(scatterplot3d)
scatterplot3d(mtcars$wt, mtcars$hp, mtcars$mpg, pch = 19, type = "h", color = "blue", angle = 55)


