# made by: Joel Alejandro Herrera Hernandez

# function to calculate the IMC
calcular_imc <- function(peso, altura) {
  imc <- peso / altura^2
  return(imc)
}

# function to check the status of the IMC
check_status <- function(peso, altura) {
  imc <- calcular_imc(peso, altura)
  print("Tu IMC es: ")
  print(imc)
  print("Tu estado es: ")
  if (imc < 18.5) {
    print("Bajo peso")
  }else if (imc >= 18.5 && imc < 25) {
    print("Peso normal")
  }else if (imc >= 25 && imc < 30) {
    print("Sobre peso")
  }else if (imc >= 30) {
    print("Obesidad")
  }else {
    print("Error")
  }
}

# test the functions with the calcular_imc function
print(calcular_imc(70, 1.75))
print(calcular_imc(100, 1.70))

# test case Peso normal
check_status(70, 1.75)

# test case Sobre peso
check_status(90, 1.80)

# test case Bajo peso
check_status(50, 1.75)

# test case Obesidad
check_status(120, 1.70)


