#Importación de DATA
# Se utiliza header = FALSE porque el archivo no contiene encabezados
dengue_dataset <- 
  read.csv('C:/Users/anna/Desktop/9 cuatrimestre/DENGUE/DENGUE.csv', 
           header = TRUE, sep = ",")

data("dengue_dataset");

#Selección de variables
#Variable Independiente (Predictora): Semana.
#Variable Dependiente (Respuesta): Casos Confirmados.
x <- dengue_dataset$SEMANA; #Variable Independiente (Predictora)
y <- dengue_dataset$CASOS_CONFIRMADOS; #Variable Dependiente (Respuesta)

#Cálculo de medias
x_media <- mean(x);
y_media <- mean(y);

#Cálculo de varianza
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



