###################################################################################################################
# MANOVA
###################################################################################################################

#------------------------------------------------------------------------------------------------------------------
#### Cargar datos ####
# Para este ejemplo se puede utilizar el conjunto de datos llamado IRIS. 
# Estos datos son datos de un conjunto de flores con diferentes.

# Instalar y cargar los paquetes necesarios
install.packages("ggplot2")
install.packages("GGally")
#------------------------------------------------------------------------------------------------------------------
# Para este primer ejmplo se considerae los datos de iris. Los cuales son medidas de tres diferentes tipos de flores
# Se ha considerado todos los datos para lo mismo

#### Visualización de datos ####
# Librerías
library(ggplot2)
library(GGally)

# Visaulización Utilizar el conjunto de datos iris
data <- iris

# Crear el gráfico combinado
hist(data$Sepal.Length) # Grafico Univariante. Se ve que los datos se mezclan entre si
ggpairs(data, columns = 1:2, mapping = ggplot2::aes(color = Species))
# Podemos ver las diferentes distribuciones que existen en los datos. Además la relación de los datos 

# Usando ggpairs para crear el gráfico ajustado
ggpairs(data, columns = c("Sepal.Length", "Sepal.Width"), 
        aes(color = Species),
        upper = list(continuous = wrap("density", alpha = 0.5)),
        lower = list(continuous = "points"), 
        diag = list(continuous = "barDiag"))
#------------------------------------------------------------------------------------------------------------------

#### Ejemplo MANOVA ####
# Cargar la librería necesaria
library(stats) # No necesita instalar esta librería ya que esta instalada por defecto
### MANOVA
# Se requiere probar las cuatro variables ancho y largo de sépalo y pétalo  son diferentes en las diferentes especies. 
# Las variables dependientes son: Sepal.Length, Sepal.Width, Petal.Length, Petal.Width
# Las variables independientes son: el tipo de especie de flor.

manova_result <- manova(cbind(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width) ~ Species, data = iris)

# Ver los resultados

summary(manova_result)

# De los resultados podemos ver: 
# Pillai's Trace: Esta es una medida de la magnitud de la diferencia entre grupos. En este caso, el valor de Pillai es 1.1919, 
# lo que indica que hay una diferencia sustancial entre los grupos.

# Approx F: Este es el valor aproximado de la estadística F, que sigue una distribución F de Fisher. 
# Se utiliza para probar la hipótesis nula de que no hay diferencias entre los grupos en términos de las variables dependientes. 
# Un valor grande de F indica que hay diferencias significativas entre los grupos. En este caso, el valor de F es 53.466, lo que indica que 
# hay diferencias significativas entre las especies de iris en términos de las variables medidas.
# Pr(>F): Este es el valor p asociado con la estadística F. Indica la probabilidad de observar un valor de F igual o más extremo que 
# el observado bajo la hipótesis nula de que no hay diferencias entre los grupos. Un valor p pequeño (generalmente <0.05)
# sugiere que rechacemos la hipótesis nula y concluyamos que hay diferencias significativas entre los grupos. 
# En este caso, el valor p es < 2.2e-16, lo que indica que hay diferencias significativas entre las especies de iris en términos de las variables medidas

summary.aov(manova_result) # veo cuál de las variables dependientes es el que provoca la diferencia

#------------------------------------------------------------------------------------------------------------------

#### Ejemplo MANOVA ####
# Caso ratones 

# Hipótesis a contrastar:
  
  # Hipótesis nula (H0): No hay diferencias significativas entre los tratamientos (A y B) en términos de peso y altura de los ratones.
  # Hipótesis alternativa (H1): Hay diferencias significativas entre al menos uno de los tratamientos (A o B) en términos de peso y altura de los ratones.

# Cargar la librería necesaria

# Generar datos de ejemplo
set.seed(123) # Para reproducibilidad
n <- 50 # Número de ratones por grupo
tratamiento <- factor(rep(c("A", "B"), each = n)) # Matriz X
peso <- c(rnorm(n, mean = 25, sd = 3), rnorm(n, mean = 28, sd = 4)) #Matriz Y
altura <- c(rnorm(n, mean = 15, sd = 2), rnorm(n, mean = 16, sd = 3)) # Matriz Y
data <- data.frame(tratamiento, peso, altura) 

# Realizar MANOVA
manova_result <- manova(cbind(peso, altura) ~ tratamiento, data = data)

# Ver los resultados
summary(manova_result)
# Pillai's Traza (R de Pillai): Esta es una medida de la magnitud de la diferencia entre los grupos. Un valor más cercano a 1 indica una mayor diferencia entre los grupos. 
# En este caso, el valor de R de Pillai es 0.30997, lo que sugiere que hay una diferencia moderada entre los grupos 
# en términos de peso y altura de los ratones.
# Approx F: se utiliza para probar la hipótesis nula de que no hay diferencias entre los grupos. 
# Un valor grande de F indica que hay diferencias significativas entre los grupos. En este caso, el valor de F es 21.787, 
# lo que indica que hay diferencias significativas entre los tratamientos en términos de peso y altura de los ratones.
# Pr(>F): Este es el valor p asociado con la estadística F. Indica la probabilidad de observar un valor de F igual o 
# más extremo que el observado bajo la hipótesis nula de que no hay diferencias entre los tratamientos. 
# Un valor p pequeño (generalmente <0.05) sugiere que rechacemos la hipótesis nula y concluyamos que hay diferencias significativas entre los tratamientos. 
# En este caso, el valor p es muy pequeño (1.532e-08), lo que indica que hay diferencias significativas entre los tratamientos en términos de peso y altura de los ratones.

summary.aov(manova_result) # Son los anovas individuales de las variables dependientes

# Efecto 
# El tamaño del efecto de un ANOVA, es el valor que permite medir cuanta varianza en la variable dependiente cuantitativa
# es resultado de la influencia de la variable cualitativa independiente, o lo que es lo mismo, 
# cuanto afecta la variable independiente (factor) a la variable dependiente.


#install.packages("lsr") # para el caso univariante
#library(lsr) # Para el caso Univariante
#etaSquared(anova_result) #Para el caso univariante
install.packages("heplots") # Caso multivariante
library(heplots) # Caso multivariante
etasq <- etasq(manova_result)
etasq
#Los niveles de clasificación más empleados para el tamaño del efecto son:
# 0.01 = pequeño
# 0.06 = mediano
# 0.14 = grande
# Identificar el factor con el valor más alto de Eta cuadrado
factor_mas_representativo <- names(etasq)[which.max(etasq)]
print(factor_mas_representativo)
#------------------------------------------------------------------------------------------------------------------
# En este ejemplo, estamos analizando la diferencia entre empresas agrícolas (Empresa A, Empresa B, Empresa C) en 
# términos de tres variables dependientes: rendimiento de cultivos, uso de fertilizantes y costos operativos.

# Generar datos de ejemplo
set.seed(123)  # Para reproducibilidad
n <- 50  # Número de observaciones por empresa
empresas <- factor(rep(c("Empresa A", "Empresa B", "Empresa C"), each = n))
rendimiento <- c(rnorm(n, mean = 50, sd = 5), rnorm(n, mean = 55, sd = 6), rnorm(n, mean = 60, sd = 4))
fertilizantes <- c(rnorm(n, mean = 20, sd = 3), rnorm(n, mean = 18, sd = 2), rnorm(n, mean = 22, sd = 4))
costos <- c(rnorm(n, mean = 2000, sd = 200), rnorm(n, mean = 1800, sd = 250), rnorm(n, mean = 2200, sd = 180))
data <- data.frame(empresas, rendimiento, fertilizantes, costos)

# Realizar MANOVA
manova_result <- manova(cbind(rendimiento, fertilizantes, costos) ~ empresas, data = data)

# Ver los resultados
summary(manova_result)
summary.aov(manova_result)
#------------------------------------------------------------------------------------------------------------------
