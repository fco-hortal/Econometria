##############################################
## T3 Francisco Hortal, Juan Ignacio García ##
##############################################

# Importacion de Datos y Preliminar -------------------------------------------

# Descargamos las tablas y las unimos en una base mediante merge segun RBD
require(reshape)
library(dplyr)
simce8b2019_rbd = rename(simce8b2019_rbd, c(rbd = "RBD"))
prog_educ = rename(prog_educ, c(rbd = "RBD"))
base = merge(x = Resumen_Matricula_EE_Oficial_2019, 
             y = Preferentes_Prioritarios_y_Beneficiarios_SEP_2019)
base = merge(x = base, y = simce8b2019_rbd, by = c("RBD"))
base = merge(x = base, y = prog_educ, by = c("RBD"))

# Eliminamos missings de variables relevantes
base = base[!is.na(base$prom_mate8b_rbd),]
base = base[!is.na(base$prom_lect8b_rbd),]

# Creación variable proporsión de estudiantes prioritarios
PROP_PRIO = base$N_PRIO/base$MAT_TOTAL
base = cbind(base, PROP_PRIO)

# Creacion variable puntaje simce mat estandarizado.
pje_simce_mat = (base$prom_mate8b_rbd - 
                   mean(base$prom_mate8b_rbd))/sd(base$prom_mate8b_rbd)
# Se utilizan valores estandarizados como variable puntaje_simce
base = cbind(base, pje_simce_mat)
# Lo mismo para lenguaje
pje_simce_leng = (base$prom_lect8b_rbd - 
                    mean(base$prom_lect8b_rbd))/sd(base$prom_lect8b_rbd)
# Se utilizan valores estandarizados como variable puntaje_simce
base = cbind(base, pje_simce_leng)

# Agregamos variable de proporcion de mujeres y hombres
p_mujeres = base$MAT_MUJ_TOT/base$MAT_TOTAL
base = cbind(p_mujeres,base)
p_hombres = base$MAT_HOM_TOT/base$MAT_TOTAL
base = cbind(p_hombres,base)

# P1 ----------------------------------------------------------------------
# Nos basamos en la ayudantía 5
# 1.1
# El modelo correctamente especificado corresponde a:
reg_correct = lm(pje_simce_mat ~ MAT_TOTAL + PROP_PRIO, data = base)
# En este caso, el coeficiente asociado a PROP_PRIO tiene valor de -2.263092 
beta_PROP_PRIO_correct = reg_correct$coefficients[3]
beta_PROP_PRIO_correct

# El modelo con variable omitida:
reg_omit = lm(pje_simce_mat ~ PROP_PRIO, data=base)
# En este caso, el coeficiente asociado a PROP_PRIO tiene valor de -2.514957  
beta_PROP_PRIO_omit = reg_omit$coefficients[2]
beta_PROP_PRIO_omit

# El sesgo puede ser calculado como la diferencia entre ambos, ya que 
# sabemos que b_omit = b_correct + sesgo => sesgo = b_omit - b_correct:
sesgo = beta_PROP_PRIO_omit - beta_PROP_PRIO_correct
sesgo
# Luego el sesgo tiene un valor de -0.2518647 

# 1.2

# Comprobamos el cálculo teórico:
beta_MAT_TOTAL = reg_correct$coefficients[2]
beta_MAT_TOTAL # 0.0002942866 

# Notamos que es positivo, luego esperaríamos de acuerdo al cálculo anterior 
# que la covarianza entre la proporción de alumnos prioritarios y el total de 
# alumnos matriculados sea negativa, ya que la varianza siempre es positiva:
cov_MAT_TOTAL_PROP_PRIO = cov(base$PROP_PRIO, base$MAT_TOTAL)
cov_MAT_TOTAL_PROP_PRIO # -52.83594

# El valor efectivamente es negativo, lo que significa que su correlación 
# es negativa. Luego, en promedio, un aumento de la proporción de alumnos 
# prioritarios refleja una disminución del total de alumnos. Ahora estamos en 
# condiciones de calcular el sesgo:
var_PROP_PRIO = var(base$PROP_PRIO)
var_PROP_PRIO # 0.06173516
sesgo_teo = beta_MAT_TOTAL*cov_MAT_TOTAL_PROP_PRIO/var_PROP_PRIO
sesgo_teo # -0.2518647 

# y nos da exactamente el mismo valor calculado anteriormente de manera empírica

# 1.3

# Obtenemos establecimientos aleatorios de la base según una semilla
set.seed(10)
base_1500 = sample(base, 1500, replace = TRUE)

# Regresión corta
reg_corta = lm(pje_simce_mat ~ PROP_PRIO, data=base_1500)
beta_corto = reg_corta$coefficients[2] # -2.515116 
beta_corto

# Regresión larga
reg_larga = lm(pje_simce_mat ~ MAT_TOTAL + PROP_PRIO, data=base)
beta_largo = reg_larga$coefficients[3] # -2.263104
beta_largo

# Sesgo
beta_corto-beta_largo # -0.2520118 

# Ahora realizamos 1000 de estas regresiones
betas_corto = cbind()
betas_largo = cbind()
sesgos = cbind()

for(i in 1:1000){
  set.seed(i)
  base_1500 = sample(base, 1500, replace = TRUE)
  # Regresión corta
  reg_corta = lm(pje_simce_mat ~ PROP_PRIO, data=base_1500)
  beta_corto = reg_corta$coefficients[2] 
  betas_corto = cbind(betas_corto, beta_corto)
  
  # Regresión larga
  reg_larga = lm(pje_simce_mat ~ MAT_TOTAL + PROP_PRIO, data=base_1500)
  beta_largo = reg_larga$coefficients[3] 
  betas_largo = cbind(betas_largo, beta_largo)
  
  # Sesgo
  sesgo = beta_corto-beta_largo
  sesgos = cbind(sesgos, sesgo)
}

# Histogramas
plot(density(betas_corto))
plot(density(betas_largo))
plot(density(sesgos))

# P2 ----------------------------------------------------------------------

# Al comienzo del archivo se agregó la nueva base de datos a base

# Para realizar esta comparación utilizaremos los siguientes 4 modelos:

# Modelo 1:Relación entre puntaje estandarizado simce matemática,
# tasa de profesores por estudiante y porcentaje de estudiantes prioritarios

reg = lm(pje_simce_mat ~ PROP_PRIO + n_prof, data=base)
reg$coefficients[3] # -8.246576
summary(reg)

# Modelo 2: Relación entre puntaje estandarizado simce matemática,
# tasa de profesores por estudiante y porcentaje de estudiantes mujeres
# respecto al total de alumnos del establecimiento

reg = lm(pje_simce_mat ~ p_mujeres + n_prof, data=base)
reg$coefficients # -46.8373 
summary(reg)

# Modelo 3: Relación entre puntaje estandarizado simce matemática,
# tasa de profesores por estudiante y puntaje simce lenguaje estandarizado

reg = lm(pje_simce_mat ~  pje_simce_leng + n_prof, data=base)
reg$coefficients #-30.7237097
summary(reg)

# Modelo 4: Relación entre puntaje estandarizado simce matemática,
# tasa de profesores por estudiante y si el establecimiento es rural o no

reg = lm(pje_simce_mat ~ RURAL_RBD + n_prof, data=base)
reg$coefficients #-35.1311277
summary(reg)


# P3 ----------------------------------------------------------------------

# 3.1

#Separamos según treat
base_0 = base[base$treat==0,] # Tamaño 4623
base_1 = base[base$treat==1,] # Tamaño 1192

# Matriculas Totales
sum(base_0$MAT_TOTAL) # 2305764
sum(base_1$MAT_TOTAL) # 573582

# Comparación promedio de pje simce mat estandarizado
mean(base_0$pje_simce_mat) # -0.004214702
mean(base_1$pje_simce_mat) # 0.0169356
t_simce_mat = t.test(base_1$pje_simce_mat, base_0$pje_simce_mat);t_simce_mat
t_simce_mat$p.value # 0.5177415

# Comparación promedio de pje simce lenguaje estandarizado
mean(base_0$pje_simce_leng) # -0.004430001
mean(base_1$pje_simce_leng) # 0.01718112
t_simce_leng = t.test(base_1$pje_simce_leng, base_0$pje_simce_leng);t_simce_leng
t_simce_leng$p.value # 0.5059704

#Otro tipo de colegios son contados en el total (dividendo)
# Porcentaje Particular
sum(base_0$COD_DEPE2==3)/length(base_0$COD_DEPE2) # 0.07289639
sum(base_1$COD_DEPE2==3)/length(base_1$COD_DEPE2) # 0.0738255
t_particular = t.test(base_1$COD_DEPE2==3, base_0$COD_DEPE2==3);t_particular
t_particular$p.value # 0.9128392

# Porcentaje Subvencionado
sum(base_0$COD_DEPE2==2)/length(base_0$COD_DEPE2) # 0.431538
sum(base_1$COD_DEPE2==2)/length(base_1$COD_DEPE2) # 0.4354027
t_subv = t.test(base_1$COD_DEPE2==2, base_0$COD_DEPE2==2);t_subv
t_subv$p.value # 0.8104186

# Porcentaje Municipal
sum(base_0$COD_DEPE2==1)/length(base_0$COD_DEPE2) # 0.4726368
sum(base_1$COD_DEPE2==1)/length(base_1$COD_DEPE2) # 0.4748322
t_mun = t.test(base_1$COD_DEPE2==1, base_0$COD_DEPE2==1);t_mun
t_mun$p.value # 0.8104186

# Porcentaje Hombres (N°Hombres/Matriculas Totales)
t_hom = t.test(base_1$p_hombres, base_0$p_hombres);t_hom
# mean_1: 0.5197820
# mean_0: 0.5176446 
t_hom$p.value # 0.5059932

# Porcentaje Mujeres (N°Mujeres/Matriculas Totales)
t_muj = t.test(base_1$p_mujeres, base_0$p_mujeres);t_muj
# mean_1: 0.4802180
# mean_0: 0.4823548
t_muj$p.value

# Porcentaje Convenio SEP Vigente 
sum(base_0$CONVENIO_SEP)/length(base_0$CONVENIO_SEP) # 0.8617781
sum(base_1$CONVENIO_SEP)/length(base_1$CONVENIO_SEP) # 0.8330537
t_sep = t.test(base_1$CONVENIO_SEP, base_0$CONVENIO_SEP);t_sep
t_sep$p.value # 0.8104186

# Promedio Proporción Estudiantes Prioritarios
mean(base_0$PROP_PRIO) # 0.5629648
mean(base_1$PROP_PRIO) # 0.5625222
t_prio = t.test(base_1$PROP_PRIO, base_0$PROP_PRIO);t_prio
t_prio$p.value # 0.8104186

# Porcentaje de establecimientos rurales
sum(base_0$RURAL_RBD)/length(base_0$RURAL_RBD) # 0.235345
sum(base_1$RURAL_RBD)/length(base_1$RURAL_RBD) # 0.2474832
t_rural = t.test(base_1$RURAL_RBD, base_0$RURAL_RBD);t_rural
t_rural$p.value # 0.3852026

# Porcentaje establecimientos de la RM
sum(base_0$COD_REG_RBD==13)/length(base_0$COD_REG_RBD) # 0.2825005
sum(base_1$COD_REG_RBD==13)/length(base_1$COD_REG_RBD) # 0.2919463
t_rm = t.test(base_1$COD_REG_RBD==13, base_0$COD_REG_RBD==13);t_rm
t_rm$p.value # 0.5218593

# Porcentaje grupo social alto
sum(base_0$cod_grupo==5)/length(base_0$cod_grupo) # 0.0709496
sum(base_1$cod_grupo==5)/length(base_1$cod_grupo) # 0.07298658
t_sa = t.test(base_1$cod_grupo==5, base_0$cod_grupo==5);t_sa
t_sa$p.value # 0.8090977

# Porcentaje grupo social bajo
sum(base_0$cod_grupo==1)/length(base_0$cod_grupo) # 0.1992213
sum(base_1$cod_grupo==1)/length(base_1$cod_grupo) # 0.2005034
t_sb = t.test(base_1$cod_grupo==1, base_0$cod_grupo==1);t_sb
t_sb$p.value # 0.9214755

# 3.2
# Creamos las 4 regresiones lineales
# Iremos agragando cada vez más variables para ver el efecto en treat

# Partimos con la proporcion de mujeres
reg_1 = lm(pje_simce_mat ~ treat + p_mujeres, data=base)
summary(reg_1)

# Agregamos la proporción de alumnos prioritarios
reg_2 = lm(pje_simce_mat ~ treat + p_mujeres + PROP_PRIO, data=base)
summary(reg_2)

# Agragamos si es rural
reg_3 = lm(pje_simce_mat ~ treat + p_mujeres + PROP_PRIO + RURAL_RBD, data=base)
summary(reg_3)

# Por último agragamos convenio sep y puntaje simce lenguaje
reg_4 = lm(pje_simce_mat ~ treat + p_mujeres + PROP_PRIO + RURAL_RBD + 
             CONVENIO_SEP + pje_simce_leng, data=base)
summary(reg_4)
