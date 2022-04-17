#-------------------------------------------------
# Trabajo Practico N°1
# Integrantes: Tomas Santucci y Fernando Silva
# ------------------------------------------------

library(knitr)
# vemos nuestro lugar de trabajo
getwd()
# seteamos nuestro lugar de trabajo y luego mostramos el lugar del trabajo

# leemos el csv de recorridos
recorridos <- read.csv("./recorridos6.csv")
# leemos el csv de usuarios
usuarios <- read.csv("usuarios6.csv")

# combinamos los csv
bicicletas <- merge(usuarios, recorridos, all = TRUE)

# mostramos la informacion de las bicicletas
View(bicicletas)
View(usuarios)

# GENERO
# Utilizamos un grafico Barplot para representar los generos de cada usuario:

genero <- table(usuarios$genero_usuario)[2:4]

barplot(
  sort(
    genero, decreasing = TRUE,
  ),
  ylim = c(0,100),
  xlim = c(0,5),
  names.arg = c("Femenino","Otro", "Masculino"),
  col = c("blue","green","red")
)

# EDAD
# Grafico para la edad un histograma
edad <- usuarios$edad_usuario
breakpoints <- c(15,20,25,30,35,40,45,50,55,60,72)
titulo <- "EDAD DE LOS USUARIOS DE ECOBICI\nCABA 2020"

# Histograma
hist(edad,breaks = breakpoints, right = TRUE,
     xlim = c(10,80), ylim = c(0,0.05), xlab = "Edad", ylab = "Densidad",
     main = titulo, col = "pink")

rng_edad <- cut(edad, breakpoints, right = TRUE)
tabla_rng_edad <- table(rng_edad)
n <- sum(tabla_rng_edad)

# Poligono de frecuencias
x <- breakpoints
y <- c(0,tabla_rng_edad)/n
plot(x,y,type="l", xlim = c(15,72), ylim = c(0,0.25),
     main = titulo,
     ylab = "Frecuencia relativa", xlab = "Edad",lwd = 1.5)
for (i in seq(0,0.25,by=0.05)) abline(a=i,b=0,lty=2,lwd=0.5)

# Poligono acumulativo
y = cumsum(y)
plot(x,y,type="l", xlim = c(15,72), main = titulo,
     ylab = "Frecuencia relativa acumulada", xlab = "Edad", lwd = 1.5)
for (i in seq(0,1,by=0.2)) abline(a=i,b=0,lty=2,lwd=0.5)


# ESTACION DE ORIGEN

frecAbs <- table(bicicletas$direccion_estacion_origen)
frecAbs <- sort(frecAbs, decreasing = TRUE)
frecAbs <- frecAbs[1:15]
frecRel <- frecAbs/sum(frecAbs)
frecAbsAcum <- cumsum(frecAbs)
frecRelAcum <- cumsum(frecRel)
frecRel <- round(frecRel, digits = 4)
frecRelAcum <- round(frecRelAcum, digits = 4)
tabla <- cbind(frecAbs,frecRel,frecAbsAcum,frecRelAcum)
kable(tabla, caption = "Tabla de frecuencias")

barplot(
  frecAbs,
  horiz = TRUE,
  las = 1,
  xlim = c(0,25),
  cex.names = 0.5,
  col = "pink",
)

# DISTANCIA

distancia <- bicicletas$distancia/1000
summary(distancia)
hist(distancia,breaks = c(0,3,5,7,10,12,111), right = TRUE)
boxplot(distancia, horizontal = TRUE, outline = FALSE,
        xlab = "Distancia en kilometros", boxfill = "pink")

#distancia[distancia>40000]

#DURACION

duracion <- bicicletas$duracion_recorrido/60
summary(duracion)
hist(duracion,breaks = c(0,10,20,30,45,60,100,500), right = TRUE)
boxplot(duracion, horizontal = TRUE, outline = TRUE,
        xlab = "Duracion en minutos", boxfill = "pink")

