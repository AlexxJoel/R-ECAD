# made by: Joel Alejandro Herrera Hernandez

# ---------------- Calcula area y perimetro ----------------
base <- 10
altura <- 20
# area de un rectangulo
area <- base * altura
perimetro <- 2 * (base + altura)
print(paste("El area del rectangulo es: ", area))
print(paste("El perimetro del rectangulo es: ", perimetro))

# ---------------- Numero par o impar ----------------
numero <- 10
if (numero %% 2 == 0) {
  print("El numero es par")
}else {
  print("El numero es impar")
}

# ---------------- Comparacion de edades ----------------
edad_1 <- 20
edad_2 <- 30

if (edad_1 > edad_2) {
  print("La primera persona es mayor")
}else if (edad_1 < edad_2) {
  print("La segunda persona es mayor")
}else if (edad_1 == edad_2) {
  print("Las personas tienen la misma edad")
}else {
  print("Las personas tienen edades diferentes")
}

# ----------------Evaluación logica ----------------
a <- TRUE
b <- FALSE
c <- TRUE

# && both values must be true
if (a && b) {
  print("a y b son verdaderos")
}

# || at least one value must be true
if (a || b) {
  print("a o b son verdaderos")
}else {
  print("a y b son falsos")
}

# ! negation
if (!a) {
  print("a es falso")
}else {
  print("a es verdadero")
}

if (a && b && c) {
  print("a, b y c son verdaderos")
}else{
  print("Al menos uno de los valores es falso")
}



