#########################
## Tarea 1 Econometría ##
#########################

# Librerias ---------------------------------------------------------------
library(readr)
library(dplyr)
library(car)

# Importacion de datos ----------------------------------------------------
# Se utiliza Import Dataset para importar las bases de datos anteriormente descargadas
# Cambiamos los nombres de las bases de datos
Matricula <- Resumen_Matricula_EE_Oficial_2019
Prioritarios <- Preferentes_Prioritarios_y_Beneficiarios_SEP_2019
Simce <- simce8b2019_rbd

# Pregunta 1 --------------------------------------------------------------
### 1.1
# Se juntan pripero las bases de datos teniendo a el establecimiento educacional en comun
nalumnos_prioritarios = merge(Matricula, Prioritarios)
# Encontramos un numero de proporcion entre alumnos y alumnos prioritarios
# Prioritarios = Alumnos que tienen dificultades socioeconomicos
proporcion_estudiantes_prioritarios = nalumnos_prioritarios$N_PRIO / nalumnos_prioritarios$MAT_TOTAL
# Unimos esta variable a la primera tabla
nalumnos_prioritarios = cbind(nalumnos_prioritarios, proporcion_estudiantes_prioritarios)
# Histograma
hist(nalumnos_prioritarios$proporcion_estudiantes_prioritarios, main = "Proporción Alumnos Prioritarios Según Establecimiento", xlab = "Proporción", col = "blue", )
#Mediana proporciones
median(nalumnos_prioritarios$proporcion_estudiantes_prioritarios)
# La proporción que tendría la mediana sería 0.6034483
### 1.2
# Estandarizamos los valores del simce de matemáticas
# https://www.statology.org/standardize-data-in-r/
Simce = Simce %>% mutate_at(c('prom_mate8b_rbd'), ~(scale(.) %>% as.vector))
colnames(Simce)[which(names(Simce) == 'prom_mate8b_rbd')] <- "puntaje_SIMCE"
sd(Simce$puntaje_SIMCE, na.rm = TRUE)
mean(Simce$puntaje_SIMCE, na.rm = TRUE)

# Pregunta 2 --------------------------------------------------------------
# Utilizaremos COD_DEPE2 de Simce, donde 1 es Municipal, 2 Particular Subvencionado y 3 Particular Pagado. Descartamos 4 y 5 debido a que no es lo que pide el enunciado.
muni = Simce[Simce$cod_depe2 == 1,]
subv = Simce[Simce$cod_depe2 == 2,]
part = Simce[Simce$cod_depe2 == 3,]
punt_muni_subv = append(muni$puntaje_SIMCE,subv$puntaje_SIMCE)
prom_muni = mean(muni$puntaje_SIMCE, na.rm = TRUE)
prom_subv = mean(subv$puntaje_SIMCE, na.rm = TRUE)
prom_part = mean(part$puntaje_SIMCE, na.rm = TRUE)
prom_muniysubv = mean(punt_muni_subv, na.rm = TRUE)
print("Brecha particular vs subv ") 
print(prom_part-prom_subv)
print("Brecha particular vs muni ")
print(prom_part-prom_muni)
print("Brecha particular vs subv y muni ")
print(prom_part-prom_muniysubv)

# Pregunta 3 --------------------------------------------------------------
### 3.1
# Agregar tabla Simce segun rbd
Datos = merge(nalumnos_prioritarios, Simce, by.x = "RBD", by.y = "rbd")
# Aplicamos scatterplot
Puntaje_Simce = Datos$puntaje_SIMCE
Estudiantes_Prioritarios = Datos$proporcion_estudiantes_prioritarios 
plot(Estudiantes_Prioritarios, Puntaje_Simce, pch=19, col="black")
### 3.2
# Aplicamos Binscater con lo visto en la ayudantía 1
# Ordenamos los datos
Datos_Ordenados = Datos[order(Datos[,"puntaje_SIMCE"], Datos[,"proporcion_estudiantes_prioritarios"]), ]
n = length(Datos_Ordenados$puntaje_SIMCE) # Numero de observaciones 
vent = 20 # Numero de sets (Elegido este numero para que la cantidad de datos sea divisible)
di = n/vent # Cantidad de observaciones por set
nueva_prioritarios = vector()
nueva_simce = vector()
inicio = 0
# Separamos en "beans"
for (i in 1:20)
{
  desde = (inicio)*di+1
  hasta = i*di
  nueva_prioritarios <- append(nueva_prioritarios, mean(Datos_Ordenados[desde:hasta, "proporcion_estudiantes_prioritarios"]))
  nueva_simce <- append(nueva_simce, mean(Datos_Ordenados[desde:hasta, "puntaje_SIMCE"]))
  inicio = inicio + 1
}
# Graficamos
plot(nueva_prioritarios, nueva_simce, pch = 19, col = "black")
### 3.3

# Pregunta 4 --------------------------------------------------------------
### 4.1 Regresión lineal 

## x variable independiente proporción estudiantes prioritarios
## y puntaje simce matemáticas (lo que varía)
# Hacemos la regresión mediante el método de mínimos cuadrados para estimar los parámetros

#y = Xbeta + e
#CON INTERCEPTO
#CREAR LA MATRIZ

## Basado en ejercicios de ayudantía

x = Datos_Ordenados$proporcion_estudiantes_prioritarios
y = Datos_Ordenados$puntaje_SIMCE
  
  #Se procede a calcular el cov
cov = mean(x*y, na.rm = TRUE)-(mean(x, na.rm = TRUE)*mean(y, na.rm = TRUE))

#CALCULAMOS LA VAR
var = mean(x^2)-(mean(x))^2

# Se aplica la fórmula de la regresión 
beta_plot = cov/var
beta_0_plot = mean(y, na.rm = TRUE) - mean(x, na.rm = TRUE)*beta_plot

#A continuación se procede a estimar el error
#EPSILON = Y-Xbeta
epsilon = y - (x*rep(beta_plot, length(x))) - beta_0_plot
print(mean(epsilon, na.rm = TRUE)) # -0.004977955

plot(Datos_Ordenados$proporcion_estudiantes_prioritarios, Datos_Ordenados$puntaje_SIMCE, pch = 19, col = "Blue", main="Proporsion Prioritarios vs Ptje Simce Mat", xlab = "Proporcion", ylab = "Ptje")
abline(a=beta_0_plot, b=beta_plot, col = "red")
print(beta_0_plot) # 1.376421
print(beta_0) # 3.733565
# Ahora calculamos con los 20 datos del biscaterr
cov = mean(nueva_prioritarios*nueva_simce, na.rm = TRUE)-(mean(nueva_prioritarios, na.rm = TRUE)*mean(nueva_simce, na.rm = TRUE))
var = mean(nueva_prioritarios^2, na.rm = TRUE)-(mean(nueva_prioritarios, na.rm = TRUE))^2

# Se aplica la fórmula de la regresión 

beta = cov/var
beta_0 = mean(nueva_simce, na.rm = TRUE) - mean(nueva_prioritarios, na.rm = TRUE)*beta

#A continuación se procede a estimar el error
#EPSILON = Y-Xbeta
epsilon = nueva_simce - (x*rep(beta, length(nueva_prioritarios))) - beta_0

plot(nueva_prioritarios, nueva_simce, pch = 19, col = "Blue", main="Proporsion Prioritarios vs Ptje Simce Mat", xlab = "Proporcion", ylab = "Ptje")
abline(a=beta_0, b=beta, col = "red")
print(beta) # -5.330212
print(beta_0) # 2.900186
# Pregunta 5 --------------------------------------------------------------

# 5.1
betas = vector()
for (i in 1:10000){
  random_prioritarios = vector()
  random_simce = vector()
  set.seed(i)
  vector_random = sample(5886, 100, replace = FALSE)
  for (i in vector_random){
    random_prioritarios <- append(random_prioritarios, Datos_Ordenados$proporcion_estudiantes_prioritarios[i])
    random_simce <- append(random_simce, Datos_Ordenados$puntaje_SIMCE[i])
  }
  cov = mean(random_prioritarios*random_simce, na.rm = TRUE)-(mean(random_prioritarios, na.rm = TRUE)*mean(random_simce, na.rm = TRUE))
  var = var(random_prioritarios, na.rm = TRUE)
  betas <- append(betas, cov/var)
}
hist(betas, main = "Coeficiente MCO", xlab = "Coeficientes", col = "blue")
cont_0 = 0
for (i in betas){
  if (i < 0){
    cont_0 = cont_0 + 1
  }
}
proporcion_menor_0 = cont_0/length(betas)
cont_24 = 0
for (i in betas){
  if (i < -2.4){
    cont_24 = cont_24 + 1
  }
}
proporcion_menor_24 = cont_24/length(betas)
print(proporcion_menor_0) # 1
print(proporcion_menor_24) # 0.5429

# 5.2
betas = vector()
for (i in 1:10000){
  random_prioritarios = vector()
  random_simce = vector()
  set.seed(i)
  vector_random = sample(5886, 1000, replace = FALSE)
  for (i in vector_random){
    random_prioritarios <- append(random_prioritarios, Datos_Ordenados$proporcion_estudiantes_prioritarios[i])
    random_simce <- append(random_simce, Datos_Ordenados$puntaje_SIMCE[i])
  }
  cov = mean(random_prioritarios*random_simce, na.rm = TRUE)-(mean(random_prioritarios, na.rm = TRUE)*mean(random_simce, na.rm = TRUE))
  var = var(random_prioritarios, na.rm = TRUE)
  betas <- append(betas, cov/var)
}
hist(betas, main = "Coeficiente MCO", xlab = "Coeficientes", col = "blue")
cont_0 = 0
for (i in betas){
  if (i < 0){
    cont_0 = cont_0 + 1
  }
}
proporcion_menor_0 = cont_0/length(betas)
cont_24 = 0
for (i in betas){
  if (i < -2.4){
    cont_24 = cont_24 + 1
  }
}
proporcion_menor_24 = cont_24/length(betas)
print(proporcion_menor_0) # 1
print(proporcion_menor_24) # 0.7241
# Pregunta 6 --------------------------------------------------------------
# Obtenemos la diferencia de resultados Simce promedio entre un prioritario y alguien que no sea prioritario

prom_prioritarios = mean(Datos$proporcion_estudiantes_prioritarios*Datos$puntaje_SIMCE, na.rm = TRUE)
prom_no_prioritarios = mean((1-Datos$proporcion_estudiantes_prioritarios)*Datos$puntaje_SIMCE, na.rm = TRUE)
diferencia_ptje = prom_prioritarios-prom_no_prioritarios
print(prom_prioritarios)
print(prom_no_prioritarios)
print(diferencia_ptje)

# Hay una diferencia de 0.3074853, por lo que supondremos que el subcidio aumenta los puntajes de los prioritarios en un 0.15
# Para esto crearemos nuevos valores de puntajes simce que tengan en cuenta el subcidio.
subcidio_v = vector()
nuevo_ptje = 0
for (i in 1:5886) {
  nuevo_ptje = 0
  nuevo_ptje = Datos$proporcion_estudiantes_prioritarios[i]*(Datos$puntaje_SIMCE[i] + 0.15) + (1-Datos$proporcion_estudiantes_prioritarios[i])*Datos$puntaje_SIMCE[i]
  subcidio_v <- append(subcidio_v, nuevo_ptje)
}
Datos = cbind(Datos, subcidio_v)
# Calculamos RL
# Ordenamos
Datos = Datos[order(Datos[,"subcidio_v"], Datos[,"proporcion_estudiantes_prioritarios"]), ]
x = Datos$proporcion_estudiantes_prioritarios
y_new = subcidio_v

cov_new = mean(x*y_new, na.rm = TRUE)-(mean(x, na.rm = TRUE)*mean(y_new, na.rm = TRUE))
var_new = var(x, na.rm = TRUE)
beta_new = cov_new/var_new
beta_0_new = mean(y_new, na.rm = TRUE) - mean(x, na.rm = TRUE)*beta_new

plot(x, y_new, pch = 19, col = "blue", main="Proporcion Prioritarios vs Ptje Simce Mat Subcidio", xlab = "Proporcion", ylab = "Ptje")
abline(a=beta_0_new, b=beta_new, col = "yellow")
abline(a=beta_0_plot, b=beta_plot, col = "red")
# Podemos ver como la linea azul muestra que los estudiantes prioritarios aumentaron su puntaje en comparacion a la regresion pasada (rojo)en donde no hubo subcidio.


