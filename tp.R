#-------------------------------------------------
# Trabajo Practico N°1
# Integrantes: Tomas Santucci y Fernando Silva
# ------------------------------------------------

# Librerias
library(knitr)


getwd()
setwd("/home/silva/Documentos/ecobici-estadistica-descriptiva/")

# leemos el csv de recorridos
recorridos <- read.csv("./recorridos6.csv")

# leemos el csv de usuarios
usuarios <- read.csv("./usuarios6.csv")

# combinamos los csv
bicicletas <- merge(usuarios, recorridos, all = TRUE)
View(bicicletas)
# damos un orden a los dias
dia <- factor(bicicletas$dia, levels = c("Lunes","Martes","Miércoles","Jueves", "Viernes", "Sábado", "Domingo"))

# ====================================================

# VARIABLE GENERO
# Utilizamos un grafico de torta para representar 
# el porcentaje de usuarios que utilizan el sistema
# segun su genero

var_genero <- function(){
  genero <- table(usuarios$genero_usuario)[2:4]
  
  label <- c("Femenino","Otro","\n\nMasculino")
  porcentajes <- round((genero/sum(genero))*100, digits = 2)
  label <- paste(label,"\n\t",porcentajes,"%")
  color <- c("#FFC09F","#A0CED9","#FFEE93")
  
  pie(
    genero,
    main = "GENERO DE LOS USUARIOS DE ECOBICI \n CABA 2020",
    labels = label,
    sub="Fuente: Datos sustraidos de las estaciones de las bicicletas publicas de CABA",
    cex.sub = 0.9,
    col = color
  )
}

var_genero()

  #=================================================

# VARIABLE EDAD
# Utilizamos un grafico de histograma y graficos de poligonos de frecuencia
# para representar la variacion de edad de los usuarios del sistema

edad <- usuarios$edad_usuario
breakpoints <- c(15,20,25,30,35,40,45,50,55,60,72)
summary(edad)
titulo <- "EDAD DE LOS USUARIOS DE ECOBICI\nCABA 2020"

# Histograma

hist(edad,breaks = breakpoints, right = TRUE,
     xlim = c(10,80), ylim = c(0,0.05), xlab = "Edad", ylab = "Densidad",
     main = titulo,
     sub="Fuente: Datos sustraidos de las estaciones de las bicicletas publicas de CABA",
     cex.sub = 0.9,
     col = "pink")



rng_edad <- cut(edad, breakpoints, right = TRUE)
tabla_rng_edad <- table(rng_edad)
n <- sum(tabla_rng_edad)

# Poligono de frecuencias
x <- breakpoints
y <- c(0,tabla_rng_edad)/n
plot(x,y,type="l", xlim = c(15,72), ylim = c(0,0.25),
     main = titulo,
     sub="Fuente: Datos sustraidos de las estaciones de las bicicletas publicas de CABA",
     cex.sub = 0.9,
     ylab = "Frecuencia relativa", xlab = "Edad",lwd = 1.5)
for (i in seq(0,0.25,by=0.05)) abline(a=i,b=0,lty=2,lwd=0.5)


# Poligono acumulativo
y = cumsum(y)
plot(x,y,type="l", xlim = c(15,72), main = titulo,
     sub="Fuente: Datos sustraidos de las estaciones de las bicicletas publicas de CABA",
     cex.sub = 0.9,
     ylab = "Frecuencia relativa acumulada", xlab = "Edad", lwd = 1.5)
for (i in seq(0,1,by=0.2)) abline(a=i,b=0,lty=2,lwd=0.5)

# ===========================================
# GRUPO ETARIO

edad <- bicicletas$edad_usuario

agrupar <- function(x){
  if (x <= 25) {
    return ("Adulto menor")
  } else if(x <= 45) {
    return ("Adulto medio")
  } else {
    return ("Adulto mayor")
  }
}

etario <- sapply(edad,agrupar)
frecAbs <- table(etario)
frecAbs <- sort(frecAbs, decreasing = TRUE)
#frecAbs <- frecAbs[1:15]
frecRel <- frecAbs/sum(frecAbs)
frecAbsAcum <- cumsum(frecAbs)
frecRelAcum <- cumsum(frecRel)
frecRel <- round(frecRel, digits = 4)
frecRelAcum <- round(frecRelAcum, digits = 4)

tabla <- cbind(names(frecAbs),frecAbs,frecRel,frecAbsAcum,frecRelAcum)
names = c("Grupo etario", "Frecuencia absoluta","Frecuencia relativa","Frecuencia absoluta acumulada",
          "Frecuencia relativa acumulada")
kable(tabla, caption = "Tabla de frecuencias", col.names = names, row.names = FALSE, align = c('l','r','r','r','r'))


# ============================================
# DIAS
# Para representar la frecuencia de uso de las bicicletas
# segun el dia de la semana utilizamos un barplot

semana <- table(dia)

barplot(semana, main = "DIA DE LOS RECORRIDOS DE LAS ECOBICI\nCABA 2020",
        sub="Fuente: Datos sustraidos de las estaciones de las bicicletas publicas de CABA",
        cex.sub = 0.9,
        ylim = c(0,150), col = c("lightblue"))

#===============================================

# ESTACION DE ORIGEN
# Representamos la estacion de origen con un barplot
# y adicionalmente mostramos una tabla de las frecuencias del mismo

frecAbs <- table(bicicletas$direccion_estacion_origen)
frecAbs <- sort(frecAbs, decreasing = TRUE)
#frecAbs <- frecAbs[1:15]
frecAbs <- sort(frecAbs, decreasing = TRUE)
frecRel <- frecAbs/sum(frecAbs)
frecAbsAcum <- cumsum(frecAbs)
frecRelAcum <- cumsum(frecRel)
frecRel <- round(frecRel, digits = 4)
frecRelAcum <- round(frecRelAcum, digits = 4)

tabla <- cbind(names(frecAbs),frecAbs,frecRel,frecAbsAcum,frecRelAcum)
names = c("Estación", "Frecuencia absoluta","Frecuencia relativa","Frecuencia absoluta acumulada",
          "Frecuencia relativa acumulada")

tabla
frecAbs
length(frecAbs)
kable(tabla, caption = "Tabla de frecuencias", col.names = names, row.names = FALSE)

par(mar = c(6,13,4,4))
barplot(
  frecAbs,
  main = "ESTACIONES DE ORIGEN MAS USADAS DE LAS ECOBICI \nCABA 2020",
  sub="Fuente: Datos sustraidos de las estaciones de las bicicletas publicas de CABA",
  cex.sub = 0.9,
  horiz = TRUE,
  las = 1,
  xlim = c(0,25),
  cex.names = 0.6,
  col = "pink",
)
par(mar = c(5,4,4,2) + 0.1)

#===============================================

# ESTACION DE DESTINO
# Representamos la estacion de destino con un barplot
# y adicionalmente mostramos una tabla de las frecuencias del mismo

frecAbs <- table(bicicletas$direccion_estacion_destino)
frecAbs <- sort(frecAbs, decreasing = TRUE)
#frecAbs <- frecAbs[1:15]
frecAbs <- sort(frecAbs, decreasing = FALSE)
frecRel <- frecAbs/sum(frecAbs)
frecAbsAcum <- cumsum(frecAbs)
frecRelAcum <- cumsum(frecRel)
frecRel <- round(frecRel, digits = 4)
frecRelAcum <- round(frecRelAcum, digits = 4)

tabla <- cbind(names(frecAbs),frecAbs,frecRel,frecAbsAcum,frecRelAcum)
names = c("Estación", "Frecuencia absoluta","Frecuencia relativa","Frecuencia absoluta acumulada",
          "Frecuencia relativa acumulada")
kable(tabla, caption = "Tabla de frecuencias", col.names = names, row.names = FALSE)

par(mar = c(6,14,4,4))
barplot(
  frecAbs,
  main = "ESTACIONES DESTINO MAS USADAS DE LAS ECOBICI \nCABA 2020",
  sub="Fuente: Datos sustraidos de las estaciones de las bicicletas publicas de CABA",
  cex.sub = 0.9,
  horiz = TRUE,
  las = 1,
  xlim = c(0,25),
  cex.names = 0.6,
  col = "pink",
)
par(mar = c(5, 4, 4, 2) + 0.1)

#==========================================

# DISTANCIA
# vamos a mostrar la frecuencia de la variable distancia
# con un boxplot

distancia <- bicicletas$distancia/1000
distancia <- distancia[distancia<20]
summary(distancia)
boxplot(distancia, horizontal = TRUE, outline = FALSE,
        main = "DISTANCIA DE LOS RECORRIDOS DE ECOBICI\nCABA 2020",
        sub="Fuente: Datos sustraidos de las estaciones de las bicicletas publicas de CABA",
        cex.sub = 0.9,
        xlab = "Distancia en kilometros", boxfill = "pink")

#===============================================

# DURACION
# vamos a mostrar la frecuencia de la variable 
# Duracion con un Boxplot

duracion <- bicicletas$duracion_recorrido/60
duracion <- duracion[duracion<100]
summary(duracion)
boxplot(duracion, horizontal = TRUE, outline = FALSE,
        main = "DURACION DE LOS RECORRIDOS DE ECOBICI\nCABA 2020",
        sub="Fuente: Datos sustraidos de las estaciones de las bicicletas publicas de CABA",
        cex.sub = 0.9,
        xlab = "Duración en minutos", boxfill = "pink")

# ========================================

# CANTIDAD DE VIAJES POR USUARIO
# Vamos a mostrar la cantidad de viajes por usuarios
# con un grafico de baras

viajes <- table(bicicletas$id_usuario)
View(viajes)
View(table(viajes))
plot(table(viajes), 
     main = "CANTIDAD DE VIAJES POR USUARIO\nCABA 2020",
     sub="Fuente: Datos sustraidos de las estaciones de las bicicletas publicas de CABA",
     cex.sub = 0.9,
     ylim = c(0,100), xlab = c("Viajes") ,ylab = c("Cantidad de Usuarios"))
n <- sum(table(viajes))
v <- table(viajes)
v <- append(v,c(0,0,0,0),after =16)
v <- append(v,c(0,0),after = 21)
v <- append(v,0,after=25)
v <- append(v,c(0,0,0,0),after=27)

titulo <- "Grafico 9: FRECUENCIA RELATIVA ACUMULADA DE LA\n CANTIDAD DE VIAJES POR USUARIO\nCABA 2020"
plot(c(0,cumsum(v))/n, type = "s",xlim=c(0,32), main = titulo,
     sub = "Fuente: Datos sustraidos de las estaciones de las bicicletas publicas de CABA",
     cex.sub = 0.9, ylab = "Frecuencia relativa acumulada", xlab = "Cantidad de viajes",lwd=1.5)
for (i in seq(0,1,by=0.2)) abline(a=i,b=0,lty=2,lwd=0.5)

# =============================
# Analisis Bivariado
# ===============================

# ======= Dia c respecto al genero ================

conteo <- table(bicicletas$genero_usuario, dia)
conteo
barplot(conteo[2:4,], beside = TRUE, col = c("lightblue","lightgreen", "pink"))
legend("topright", legend = c("F", "M", "OTRO"), fill = c("lightblue","lightgreen", "pink"))

# =================================================


# ====== Edad con respecto a distancia ==============

edad <- bicicletas$edad_usuario

agrupar <- function(x){
  if (x <= 21) {
    return ("[16-21]")
  } else if(x <= 30) {
    return ("[22-30]")
  } else if(x <= 45){
    return ("[30-45]")
  } else if(x <= 72){
    return("[45-72]")
  }
}

edad <- sapply(edad,agrupar)


distancia <- bicicletas$duracion/60
genero <- bicicletas$genero_usuario
bivar <- data.frame(edad,distancia,dia,genero)

bivar$edad <- factor(bivar$edad, levels = c("[16-21]","[22-30]","[30-45]","[45-72]"))
boxplot((bivar$distancia)~(bivar$edad), horizontal = TRUE, outline = FALSE,
        main = "DISTANCIA RECORRIDA SEGUN LA EDAD DEL USUARIO\nCABA 2020",
        sub="Fuente: Datos sustraidos de las estaciones de las bicicletas publicas de CABA",
        cex.sub = 0.9,
        ylab = "Rango Hetario",
        xlab = "Distancia en KM", boxfill = "pink")

# =========================================================

# ====== Edad con respecto al genero ======================

edad <- usuarios[usuarios$genero_usuario == 'M',]$edad_usuario
breakpoints <- c(15,20,25,30,35,40,45,50,55,60,72)
rng_edad <- cut(edad, breakpoints, right = TRUE)
tabla_rng_edad <- table(rng_edad)
n <- sum(tabla_rng_edad)

# Poligono de frecuencias
x <- breakpoints
y <- c(0,tabla_rng_edad)/n
plot(x,y,type="l", xlim = c(15,72), ylim = c(0,0.30),
     main = "Edad del usuario según el género\n Poligono de frecuencias relativas",
     sub="Fuente: Datos sustraidos de las estaciones de las bicicletas publicas de CABA",
     cex.sub = 0.9,
     ylab = "Frecuencia relativa", xlab = "Edad",lwd = 1.5, col = "green")
for (i in seq(0,0.30,by=0.05)) abline(a=i,b=0,lty=2,lwd=0.5)

edad <- usuarios[usuarios$genero_usuario == 'F',]$edad_usuario
rng_edad <- cut(edad, breakpoints, right = TRUE)
tabla_rng_edad <- table(rng_edad)
n <- sum(tabla_rng_edad)

# Poligono de frecuencias
y <- c(0,tabla_rng_edad)/n
lines(x,y, col = "red")

edad <- usuarios[usuarios$genero_usuario == 'OTRO',]$edad_usuario
rng_edad <- cut(edad, breakpoints, right = TRUE)
tabla_rng_edad <- table(rng_edad)
n <- sum(tabla_rng_edad)

# Poligono de frecuencias
y <- c(0,tabla_rng_edad)/n
lines(x,y, col = "blue")

edad <- usuarios$edad_usuario
rng_edad <- cut(edad, breakpoints, right = TRUE)
tabla_rng_edad <- table(rng_edad)
n <- sum(tabla_rng_edad)

# Poligono de frecuencias
y <- c(0,tabla_rng_edad)/n
lines(x,y, col = "black")

# ====================================================================


# ==== Dia con respecto a edad ================

edad <- bicicletas$edad_usuario
bivar <- data.frame(edad,distancia,dia,genero)

hist(bivar[bivar$dia == 'Lunes',]$edad,breaks = c(15,21,30,45,72), right = TRUE,
     xlim = c(10,80), ylim = c(0,0.06), xlab = "Edad", ylab = "Densidad",
     main = "lunes",
     sub="Fuente: Datos sustraidos de las estaciones de las bicicletas publicas de CABA",
     cex.sub = 0.9,
     col = "pink")

hist(bivar[bivar$dia == 'Martes',]$edad,breaks = c(15,21,30,45,72), right = TRUE,
     xlim = c(10,80), ylim = c(0,0.06), xlab = "Edad", ylab = "Densidad",
     main = "martes",
     sub="Fuente: Datos sustraidos de las estaciones de las bicicletas publicas de CABA",
     cex.sub = 0.9,
     col = "pink")

hist(bivar[bivar$dia == 'Miércoles',]$edad,breaks = c(15,21,30,45,72), right = TRUE,
     xlim = c(10,80), ylim = c(0,0.06), xlab = "Edad", ylab = "Densidad",
     main = "mie",
     sub="Fuente: Datos sustraidos de las estaciones de las bicicletas publicas de CABA",
     cex.sub = 0.9,
     col = "pink")

hist(bivar[bivar$dia == 'Jueves',]$edad,breaks = c(15,21,30,45,72), right = TRUE,
     xlim = c(10,80), ylim = c(0,0.06), xlab = "Edad", ylab = "Densidad",
     main = "jueves",
     sub="Fuente: Datos sustraidos de las estaciones de las bicicletas publicas de CABA",
     cex.sub = 0.9,
     col = "pink")

hist(bivar[bivar$dia == 'Viernes',]$edad,breaks = c(15,21,30,45,72), right = TRUE,
     xlim = c(10,80), ylim = c(0,0.06), xlab = "Edad", ylab = "Densidad",
     main = "viernes",
     sub="Fuente: Datos sustraidos de las estaciones de las bicicletas publicas de CABA",
     cex.sub = 0.9,
     col = "pink")

hist(bivar[bivar$dia == 'Sábado',]$edad,breaks = c(15,21,30,45,72), right = TRUE,
     xlim = c(10,80), ylim = c(0,0.06), xlab = "Edad", ylab = "Densidad",
     main = "sab",
     sub="Fuente: Datos sustraidos de las estaciones de las bicicletas publicas de CABA",
     cex.sub = 0.9,
     col = "pink")

hist(bivar[bivar$dia == 'Domingo',]$edad,breaks = c(15,21,30,45,72), right = TRUE,
     xlim = c(10,80), ylim = c(0,0.06), xlab = "Edad", ylab = "Densidad",
     main = "domingo",
     sub="Fuente: Datos sustraidos de las estaciones de las bicicletas publicas de CABA",
     cex.sub = 0.9,
     col = "pink")

# =======================================================================================