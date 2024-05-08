###################################################################################################################
                      # Introducción a datos multivariante
###################################################################################################################

#------------------------------------------------------------------------------------------------------------------
#### Cargar datos ####
# Para este ejemplo se puede utilizar el conjunto de datos llamado State.x77. 
# Estos datos son métricas de los 50 estados de EEUU.

?state.x77 #SI quieres saber más de este conjunto de datos

datos <- as.data.frame(state.x77)
attach(datos)
#------------------------------------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------------------------------
#### Visualización de datos ####

# Gráficos de boxplot 
par(mfrow=c(2,4))
sapply(seq(1,8),function(j)boxplot(datos[,j],main=colnames(datos)[j],xlab="",col="blue"))

# Histogramas para las variables 
par(mfrow=c(2,4))
sapply(seq(1,8),function(j)hist(datos[,j],main=colnames(datos)[j],xlab="",col="blue",breaks = "Sturges"))

# Si usamos un kernel gaussiano y Epanechnikov para representar mejor la distribución del histograma
sapply(seq(1,8),function(j){
  plot(density(datos[,j],kernel="gaussian"),main=colnames(datos)[j],xlab="",col="blue",lwd=2)
  lines(density(datos[,j],kernel="epanechnikov"),main=colnames(datos)[j],xlab="",col="green",lwd=2)}
)

# Correlaciones en todos los datos 
pairs(datos,pch=19,col="blue")
par(mfrow=c(1,1))

# Si deseo conocer una relación entre variables, pero de forma tridimensional

install.packages("Rcpp")
library(rgl)
open3d() # Ventana emergente, para que salga el gráfico corra la linea siguiente
plot3d(Income,`Life Exp`,Murder,size=3) # Se pueden rotar los ejes.
install.packages("plotly")
library(plotly)
plot_ly(datos,x=~Income,y=~`Life Exp`,z=~Murder) # Otra posibilidad

# Se puede notar la profunidad de las variables

# Ahora, vamos a visualizar las variables en su relación 
install.packages("MASS")
library(MASS) 
parcoord(datos,col="blue",var.label = TRUE)

#------------------------------------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------------------------------

#### Mdedidas multivariantes ####
# Vector de medias
(vector_medias <- colMeans(datos))

# Mediana 

# Para la mediana se considera la profuncidad de Tukey. Recuerde que el dato con la mayor
# profundidad es la mediana
install.packages("ddalpha")
library(ddalpha)
profundidad_datos <- depth.halfspace(datos,datos,num.directions=100000,seed=123) # LA librería realiza la búsqueda de las produndidadesObtener la soluci�n aproximada basada en 100000 proyecciones
ordenar_profundidades <- sort(profundidad_datos,decreasing=TRUE,index.return=TRUE) # Se ordena las profundidades
(ordenar_profundidades_para_filas <- ordenar_profundidades$x) # Las profundidades ordenadas
ordenar_profundidades_filas <- ordenar_profundidades$ix # Las filas asociadas a las profundidades ya ordenadas

# La mediana en los datos es

datos[ordenar_profundidades_filas [1],]

# Matriz de Covarianza 
install.packages("pgirmess")
library(pgirmess)
(matriz_cov <- cov(datos))
# Visualizar la matriz de covarianza como correlagrama
cormat(datos)

# Escalar valores univariante
escalar_datos <- scale(datos)
pairs(escalar_datos,pch=19,col="blue")
# Escalar valores multivariante
escalar_multivariante <- solve(matriz_cov)
e <- eigen(escalar_multivariante)
V <- e$vectors
B <- V %*% diag(sqrt(e$values)) %*% t(V)
Xtil <- scale(datos,scale = FALSE)
SX <- Xtil %*% B
colMeans(SX)
cov(SX)
pairs(SX,pch=19,col="blue")



