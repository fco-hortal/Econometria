library(utils)

# Importamos la base de datos:
bdd = read.csv("C:/Users/nacho/Desktop/2022-1/Ayudantía ICS2562/Ayudantía 22 de Abril/Ayudantía 6/penguins_size.csv")

# Eliminamos NA de las variables que nos interesan:
bdd = bdd[!is.na(bdd$culmen_depth_mm),]
bdd = bdd[!is.na(bdd$culmen_length_mm),]

# Realizamos la regresión de culmen_length sobre culmen_depth:
reg = lm(culmen_depth_mm ~ culmen_length_mm, data=bdd)

# Graficamos los datos y la regresión ajustada:
plot(bdd$culmen_length_mm, bdd$culmen_depth_mm, col='red', xlab = "Culmen Length (mm)", ylab = "Culmen Depth (mm)",
     main = "Culmen Comparison")
abline(reg, col = "blue")

reg$coefficients
# Notamos que existe una relación negativa entre las variables, es decir, por un aumento en el largo del pico de los
# pinguinos se tiene una disminución en la profundidad de ellos.

# Comparemos ahora las mismas variables, pero considerando separaciones por especies de pinguinos:
adelie = bdd[bdd$species=="Adelie",]
chinstrap = bdd[bdd$species=="Chinstrap",]
gentoo = bdd[bdd$species=="Gentoo",]

# Realizamos tres regresiones, una para cada especie:
reg_adelie = lm(culmen_depth_mm ~ culmen_length_mm, data=adelie)
reg_chinstrap = lm(culmen_depth_mm ~ culmen_length_mm, data=chinstrap)
reg_gentoo = lm(culmen_depth_mm ~ culmen_length_mm, data=gentoo)

# Tema dimensional:
xdelim = c(min(bdd$culmen_length_mm),max(bdd$culmen_length_mm) + 20)
ydelim = c(min(bdd$culmen_depth_mm),max(bdd$culmen_depth_mm))

# Graficamos las mismas variables, pero diferenciando en especies.
plot(adelie$culmen_length_mm, adelie$culmen_depth_mm, col='red', xlab = "Culmen Length (mm)", ylab = "Culmen Depth (mm)",
     main = "Culmen Comparison by Species", xlim = xdelim, ylim = ydelim)
points(chinstrap$culmen_length_mm, chinstrap$culmen_depth_mm, col = "blue")
points(gentoo$culmen_length_mm, gentoo$culmen_depth_mm, col = "black")
abline(reg_adelie, col = "red")
abline(reg_chinstrap, col = "blue")
abline(reg_gentoo, col = "black")
legend("bottomright", legend = c("Adelie", "Chinstrap", "Gentoo"), lwd = 3, col = c("red", "blue", "black"), 
       title = "Species")

# Ahora la relación entre las variables es positiva. Qué paso?
