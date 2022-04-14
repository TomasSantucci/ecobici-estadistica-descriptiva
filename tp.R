#-------------------------------------------------
# Trabajo Practico N°1
# Integrantes: Tomas Santucci y Fernando Silva
# ------------------------------------------------

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



# Usuarios
table(bicicletas$id_usuario)
table(usuarios$genero_usuario)
table(usuarios$edad_usuario)

# Bicicletas
table(bicicletas$direccion_estacion_origen)
table(bicicletas$direccion_estacion_destino)
table(bicicletas$duracion_recorrido)
table(bicicletas$distancia)
table(bicicletas$dia)

table(bicicletas$id_usuario, bicicletas$genero_usuario)
