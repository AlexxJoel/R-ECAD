# Made by: Joel Alejandro Herrera Hernandez
# ---------------------- Aprobado o Reprobado -----------------------
calificacion <- 7
if (calificacion >= 8){
  print("Aprobado")
} else {
  print("Reprobado")
}

# ---------------------- Numero Positivo, Negativo o Cero --------------
numero <- -5
if (numero > 0){
  print("Positivo")
} else if (numero < 0){
  print("Negativo")
} else {
  print("Cero")
}

# ---------------------- Descuento de una tienda -----------------------
compra_total <- 1000
descuento <- 0
if (compra_total > 100){
  # get 15% discount
    descuento <- compra_total * 0.15
    print(paste("Descuento: ", descuento))
} else {
  print("Pago normal")
}

# print total to pay, with discount
print(paste("Total a pagar: ", compra_total - descuento))

# ---------------------- Numero mayor y menor -------------------------
num1 <- 5
num2 <- 10
num3 <- 15

# check first number is greater
if (num1 > num2 && num1 > num3){
  print(paste("El numero mayor es: ", num1))
} else if (num2 > num1 && num2 > num3){
  print(paste("El numero mayor es: ", num2))
} else {
  print(paste("El numero mayor es: ", num3))
}
