#carga del Dataset
data("airquality")

# Imputar valores faltantes con la media de cada columna
airquality_imputed <- airquality
for(i in 1:ncol(airquality_imputed)) {
  airquality_imputed[is.na(airquality_imputed[, i]), i] <- mean(airquality_imputed[, i], na.rm = TRUE)
}

# Verificar que no hay valores faltantes
colSums(is.na(airquality_imputed))
# Eliminar filas con valores faltantes
airquality_clean <- na.omit(airquality)

# Verificar que no hay valores faltantes
colSums(is.na(airquality_clean))
#variables
#En este caso se extiende la variables en x
x<- as.matrix(cbind(1,airquality_clean$Temp,airquality_clean$Wind))
y<- airquality_clean$Ozone
#Los 1 son para conservar y marcar para banderas solo para ocupar el espacio y meta el ruido

#Coeficientes beta usando álgebra matricial
beta <- solve(t(x)%*% x) %*% t(x) %*% y
#Es decir que las coeficientes de regresión son de acuerdo a los betas declaradas en x
#Coeficientes de regresión
beta0<- beta[1]
beta1<-beta[2]
beta2 <- beta[3]

#Predicción de valores
y_pred <- x %*% beta

#Cálculo de R-cuadrado
ss_total <- sum((y-mean(y))^2)
ss_residual<- sum((y-y_pred)^2)
r_squared <- 1-(ss_residual / ss_total)

# Cálculo de MSE
mse <- mean((y-y_pred)^2)

#Resultados
list(beta0 = beta0,beta1=beta1,beta2=beta2,r_squared=r_squared)

#Nuevos Valores para predicción
temp <- 90
wind <- 10

# Vector de nuevos valores
nuevos_valores <- c(1, temp,wind)

#Predicción
prediccion_mpg <- sum(nuevos_valores * beta)
prediccion_mpg

# Gráfico 3D de dispersión y plano de regresiónss
# (requiere liberías adicionales)
library(scatterplot3d)
scatterplot3d(airquality_clean$Ozone,airquality_clean$Temp,airquality_clean$Wind,
              pch = 19, type = "h", color = "blue",
              angle = 55)



