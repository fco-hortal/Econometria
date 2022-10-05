##############################################
## T4 Francisco Hortal, Juan Ignacio García ##
##############################################

# Importacion de Datos y Preliminar -------------------------------------------
library(dplyr) 
# Descargamos la tabla
base = earthquake_T4
length(base$city)
base_0 = filter(base, post == 0 & city == 0)
base_1 = filter(base, post == 0 & city == 1)
base_2 = filter(base, post == 0 & city == 2)


# P1 ----------------------------------------------------------------------
 
# Para el analisis de esta pregunta utilizaremos T = 1 

# Creamos una base de datos con Tiempo = 1 para la ciudad 0 
time1city0  <- base %>% filter(city == "0" & post == "1")

# Creamos una base de datos con Tiempo = 1 para la ciudad 1 
time1city1  <- base %>% filter(city == "1" & post == "1")

# Creamos una base de datos con Tiempo = 1 para la ciudad 2 
time1city2  <- base %>% filter(city == "2" & post == "1")

# Ahora procederemos a calcular los promedios de las variables de interes

# Ciudad 0 

mujeres0 = mean(time1city0$female)
# 0.5061279
sdmujeres0 = sd(time1city0$female)
# 0.5000453


immig0 = mean(time1city0$immig)
# 0.09307718
sdimmig0 = sd(time1city0$immig)
# 0.2905887

edad0 = mean(time1city0$age)
# 38.57138
sdedad0 = sd(time1city0$age)
# 6.126872

educ0 = mean(time1city0$educ)
# 8.078503
sdeduc0 = sd(time1city0$educ)
# 2.121273

exp0 = mean(time1city0$exp)
# 24.52435
sdexp0 = sd(time1city0$exp)
# 6.531338

log_income_0 = mean(time1city0$log_income)
# 2.291271
sdlog_income_0 = sd(time1city0$log_income)
# 0.5753521

# Ciudad 1 

mujeres1 = mean(time1city1$female)
# 0.5018364
sdmujeres1 = sd(time1city1$female)
# 0.5000801


immig1 = mean(time1city1$immig)
# 0.1048414
sdimmig1 = sd(time1city1$immig)
# 0.3064001

edad1 = mean(time1city1$age)
# 40.19299
sdedad1 = sd(time1city1$age)
# 6.463051


educ1 = mean(time1city1$educ)
# 7.573623
sdeduc1 = sd(time1city1$educ)
# 2.185496

exp1 = mean(time1city1$exp)
# 26.64341
sdexp1 = sd(time1city1$exp)
# 6.880582

log_income_1 = mean(time1city1$log_income)
# 2.42741
sdlog_income_1 = sd(time1city1$log_income)
# 0.6067065

# Ciudad 2 

mujeres2 = mean(time1city2$female)
# 0.4884596
sdmujeres2 = sd(time1city2$female)
# 0.4999295

immig2 = mean(time1city2$immig)
# 0.0938284
sdimmig2 = sd(time1city2$immig)
# 0.2916264

edad2 = mean(time1city2$age)
# 38.40843
sdedad2 = sd(time1city2$age)
# 6.357858

educ2 = mean(time1city2$educ)
# 8.552183
sdeduc2 = sd(time1city2$educ)
# 2.19213

exp2 = mean(time1city2$exp)
# 23.88585
sdexp2 = sd(time1city2$exp)
# 6.714935

log_income_2 = mean(time1city2$log_income)
# 2.223015
sdlog_income_2 = sd(time1city2$log_income)
# 0.6333247

# Ahora procederemos a calcular los p-value
t1city12 = filter(base, post == 1 & city != 0)
t1city01 = filter(base, post == 1 & city != 2)
t1city02 = filter(base, post == 1 & city != 1)

# Variable female
t_city0 = t.test(t1city12$female, time1city0$female)
t_city0$p.value # 0.273459

t_city1 = t.test(t1city02$female, time1city1$female)
t_city1$p.value # 0.5976635

t_city2 = t.test(t1city01$female, time1city2$female)
t_city2$p.value # 0.1283138

# Variable immig
t_city0 = t.test(t1city12$immig, time1city0$immig)
t_city0$p.value # 0.3907173

t_city1 = t.test(t1city02$immig, time1city1$immig)
t_city1$p.value # 0.0855064

t_city2 = t.test(t1city01$immig, time1city2$immig)
t_city2$p.value # 0.3957224

# Variable age
t_city0 = t.test(t1city12$age, time1city0$age)
t_city0$p.value # 9.127573e-06

t_city1 = t.test(t1city02$age, time1city1$age)
t_city1$p.value # 3.935689e-34

t_city2 = t.test(t1city01$age, time1city2$age)
t_city2$p.value # 8.266057e-14

# Variable educ
t_city0 = t.test(t1city12$educ, time1city0$educ)
t_city0$p.value # 0.2520585

t_city1 = t.test(t1city02$educ, time1city1$educ)
t_city1$p.value # 4.256325e-58

t_city2 = t.test(t1city01$educ, time1city2$educ)
t_city2$p.value # 1.317611e-58

# Variable exp
t_city0 = t.test(t1city12$exp, time1city0$exp)
t_city0$p.value # 0.0001726216

t_city1 = t.test(t1city02$exp, time1city1$exp)
t_city1$p.value # 4.468007e-61

t_city2 = t.test(t1city01$exp, time1city2$exp)
t_city2$p.value # 1.874419e-34

# Variable log_income
t_city0 = t.test(t1city12$log_income, time1city0$log_income)
t_city0$p.value # 0.132127

t_city1 = t.test(t1city02$log_income, time1city1$log_income)
t_city1$p.value # 3.783484e-39

t_city2 = t.test(t1city01$log_income, time1city2$log_income)
t_city2$p.value # 7.010974e-27

# P2 ----------------------------------------------------------------------

# Filtramos los datos de las tres ciudades en el tiempo 1 por las razones 
# descritas en la P1
base_1 = filter(base, post == 1)

# Procedemos a calcular tres modelos diferentes en donde iremos agregando
# variables para ver el cambio en coeficientes
reg_1 = lm(log_income ~ educ,data=base_1)
summary(reg_1)

# Vemos que la Regresion de Mincer toma en cuenta la experiencia y la 
# experiencia al cuadrado, por lo que las agregamos al modelo
reg_2 = lm(log_income ~ educ + exp + exp2,data=base_1)
summary(reg_2)

# Agregamos aun más variabes
reg_3 = lm(log_income ~ educ + exp + exp2 + female + age + city + immig, 
           data=base_1)
summary(reg_3)

# Ahora utilizaremos los datos con todos los tiempos y los mismos modelos
reg_1 = lm(log_income ~ educ,data=base)
summary(reg_1)

reg_2 = lm(log_income ~ educ + exp + exp2,data=base)
summary(reg_2)

reg_3 = lm(log_income ~ educ + exp + exp2 + female + age + city + immig, 
           data=base)
summary(reg_3)

# P3 ----------------------------------------------------------------------

# Para este problema crearemos un diseño en el que solo se tomarán en cuenta los 
# habitantes de la ciudad 1 para los periodos de tiempo 0, 1 y 2. Compararemos 
# la información de t 0 y 1 con la de t 2.
city1_t01 = filter(base, post != 2 & city == 1)
city1_t2 = filter(base, post == 2 & city == 1)

# Obtenemos los promedios de los salarios y el pvalue
mean(city1_t01$log_income) # 2.331713
mean(city1_t2$log_income) # 1.962028

t_city1 = t.test(city1_t2$log_income, city1_t01$log_income)
t_city1$p.value # 1.013577e-140

# El pvalue es menor a 0.05 con creces
# Procedemos a analizar otras variables para ver si estas son estadisticamente
# significativas y afectan más que el terremoto en el salario. Para esto creamos 
# una base de datos controlada para que sean solo de la ciudad 1 en todos los 
# periodos de tiempo y generamos un modelo de regresión lineal que compare 
# salario con muchas otras variables teniendo como variable dependiente si
# ocurrio terremoto o no.

city_1 = filter(base, city == 1)
reg_city1 = lm(log_income ~ earthquake + educ + exp + exp2
               + female + age + immig, data=city_1)
summary(reg_city1)
# Vemos que el terremoto es la variable que tiene el beta con mayor diferencia
# a 0, por lo que es la variable que más influye en el salario disminuyendolo. 
# La variable que le sigue en nivel de influencia es la educación, pero la 
# variable terremoto afecta mucho más. -0.4576 del beta del terremoto comparado
# con 0.252 de la educación.

# P4 ----------------------------------------------------------------------
# Mediremos los salarios de los migrantes y no migrantes en los tres periodos
# t=0, t=1 y t=2 y en la ciudad 1. A la ves encontraremos los pvalues para la
# diferencia entre t=0 con t=1 y t=1 con t=2. Además calcularemos pvalue para 
# las diferencias entre salarios de migrantes y no migrantes manteniendo el 
# periodo. Con esto trataremos de magnificar la diferencia de salarios debido al
# subcidio viendo si hay otros factores por variable imigración o terremoto.

mig_t0 = filter(base, post==0 & city==1 & immig==1) # Size = 321
mig_t1 = filter(base, post==1 & city==1 & immig==1) # Size = 314
mig_t2 = filter(base, post==2 & city==1 & immig==1) # Size = 318
nomig_t0 = filter(base, post==0 & city==1 & immig==0) # Size = 2679
nomig_t1 = filter(base, post==1 & city==1 & immig==0) # Size = 2681
nomig_t2 = filter(base, post==2 & city==1 & immig==0) # Size = 2661

# Sacamos los promedios
mean(mig_t0$log_income) # 2.13803
mean(mig_t1$log_income) # 2.295228
mean(mig_t2$log_income) # 1.608146
mean(nomig_t0$log_income) # 2.247935
mean(nomig_t1$log_income) # 2.442892
mean(nomig_t2$log_income) # 2.004318

# Sacamos los p.value en diferencia de tiempos
t01_mig = t.test(mig_t0$log_income, mig_t1$log_income); t01_mig$p.value 
# 0.001049358
t12_mig = t.test(mig_t1$log_income, mig_t2$log_income); t12_mig$p.value 
# 8.329937e-36
t01_mig = t.test(nomig_t0$log_income, nomig_t1$log_income); t01_mig$p.value 
# 3.407552e-32
t12_nomig = t.test(nomig_t1$log_income, nomig_t2$log_income); t12_nomig$p.value 
# 3.660812e-140

# Sacamos los p.value en diferencias de nacionalidad para los distintos t
t0_mig = t.test(mig_t0$log_income, nomig_t0$log_income); t0_mig$p.value 
# 0.001947157
t1_mig = t.test(mig_t1$log_income, nomig_t1$log_income); t1_mig$p.value 
# 5.362464e-05
t2_mig = t.test(mig_t2$log_income, nomig_t2$log_income); t2_mig$p.value 
# 2.406909e-20

# Creamos dos regresiones. Una con la variable terremoto y otra sin
city_1_t2 = filter(base, city == 1 & post == 2)
reg_1_p4 = lm(log_income ~ immig + educ + exp + exp2
              + female + age, data = city1_t2)
summary(reg_1_p4)


# P5 ----------------------------------------------------------------------
# En pdf

# P6 ----------------------------------------------------------------------
# Primero debemos separar a las personas que se trasladan de cidad. Creamos 6 
# data frames según las ciudades de traslado con la información de las personas
# en el periodo t=1 y t=2 debido a que aquí es cuando ocurre el terremoto.

mov_01 = rbind()
mov_02 = rbind()
mov_10 = rbind()
mov_12 = rbind()
mov_20 = rbind()
mov_21 = rbind()

for (i in 1:10000){
  x = filter(base, id == i & post == 1)
  y = filter(base, id == i & post == 2)
  if (x$city == 0 & y$city == 1){
    mov_01 = rbind(mov_01, x)
    mov_01 = rbind(mov_01, y)
  }
  if (x$city == 0 & y$city == 2){
    mov_02 = rbind(mov_02, x)
    mov_02 = rbind(mov_02, y)
  }
  if (x$city == 1 & y$city == 0){
    mov_10 = rbind(mov_10, x)
    mov_10 = rbind(mov_10, y)
  }
  if (x$city == 1 & y$city == 2){
    mov_12 = rbind(mov_12, x)
    mov_12 = rbind(mov_12, y)
  }
  if (x$city == 2 & y$city == 0){
    mov_20 = rbind(mov_20, x)
    mov_20 = rbind(mov_20, y)
  }
  if (x$city == 2 & y$city == 1){
    mov_21 = rbind(mov_21, x)
    mov_21 = rbind(mov_21, y)
  }
}

# Procedemos a sacar los promedios de los log_income para los distintos
# traslados y periodos

mean((filter(mov_01, post == 1))$log_income) # 2.361932
mean((filter(mov_01, post == 2))$log_income) # 1.86331
mean((filter(mov_02, post == 1))$log_income) # 2.176769
mean((filter(mov_02, post == 2))$log_income) # 2.191883
mean((filter(mov_10, post == 1))$log_income) # 2.306172
mean((filter(mov_10, post == 2))$log_income) # 2.180475
mean((filter(mov_12, post == 1))$log_income) # 2.301259
mean((filter(mov_12, post == 2))$log_income) # 2.320958
mean((filter(mov_20, post == 1))$log_income) # 2.261711
mean((filter(mov_20, post == 2))$log_income) # 2.024945
mean((filter(mov_21, post == 1))$log_income) # 2.223302
mean((filter(mov_21, post == 2))$log_income) # 1.625185

# Ahora sacamos los p.value

t_city01 = t.test((filter(mov_01, post == 1))$log_income,
                  (filter(mov_01, post == 2))$log_income)
t_city01$p.value

t_city02 = t.test((filter(mov_02, post == 1))$log_income,
                  (filter(mov_02, post == 2))$log_income)
t_city02$p.value

t_city10 = t.test((filter(mov_10, post == 1))$log_income,
                  (filter(mov_10, post == 2))$log_income)
t_city10$p.value

t_city12 = t.test((filter(mov_12, post == 1))$log_income,
                  (filter(mov_12, post == 2))$log_income)
t_city12$p.value

t_city20 = t.test((filter(mov_20, post == 1))$log_income,
                  (filter(mov_20, post == 2))$log_income)
t_city20$p.value

t_city21 = t.test((filter(mov_21, post == 1))$log_income,
                  (filter(mov_21, post == 2))$log_income)
t_city21$p.value