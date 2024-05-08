###################################################################################################################
# Pruebas de Normalidad
###################################################################################################################

#------------------------------------------------------------------------------------------------------------------
#### Cargar datos ####
# Para este ejemplo se puede utilizar el conjunto de datos Swiss 
install.packages("datasets")
library(datasets)
swis<- swiss
??swiss # Son datos de 6 variables demograficas de 47 provincias de habla francesa en Suiza.
attach(swis)
# Datos multivariantes
# Grafica de los datos
boxplot(swiss, main= "Boxplot de las variables")
bagplot(swiss)
colMeans(swiss) # Vector de Medias 
sapply(swiss, var) # Vector de varianzas
var(swiss[ , 2 : 3]) # Se puede comprobar con la diagonal

#------------------------------------------------------------------------------------------------------------------

#### Pruebas de Normalidad de Datos Univaraintes ####

## Chi cuadrada de Pearson

install.packages("fBasics")
library(fBasics)
library(interp)
pchiTest(swis$Fertility,description="Prueba univariante de Fertilidad")

## Jarque - Bera
jbTest(swis$Agriculture,
         title="Prueba univariante de Agricultura")
# The jbTest() calculates both a ﬁnite sample (LM) and asymptotic (ALM) version of the JB statistic as well as their corresponding p-values.

## Kolmogorov - Smirnov
install.packages("nortest")
library(nortest)
lillieTest(swis$Examination)
# Unless we have strong a priori concerns, the two-sided alternative hypothesis is the preferred approach.

## Shapiro Test

shapiro.test(swis$Education)

#------------------------------------------------------------------------------------------------------------------

#### Pruebas de Normalidad de Datos Bivariantes ####
install.packages("goft")# tambien se puede usar la librería mvShapiroTest, use install.packages(mvShapiroTest)
library(goft) # Tiene muchas pruebas de bondad de ajuste, incluídas las lognormal
data(goats) # Cargar datos de medidas zoométricas de diferentes cabras.
mvshapiro_test(as.matrix(goats))
# Normalidad
data(goats) # Cargar datos
apply(goats,2,normal_test) # testing normality on each variable of the "goats" data set

# También se puede usar la prueba de Henze-Zirkler test,
# En términos más específicos:
# H0: Los datos multivariados siguen una distribución normal multivariante.
# H1: Los datos multivariados no siguen una distribución normal multivariante.
# es una prueba estadística utilizada para evaluar si un conjunto de datos multivariados proviene de una distribución normal multivariante
# Sirve cuando la muestra es pequeña
install.packages("MVN")
library(MVN) # Una librería que entrega mucha información, incluída la univariante
mvn(goats[,c(1,2)],mvnTest = "hz",multivariateOutlierMethod = "quan") 

#### Pruebas de Normalidad de Datos Multivariantes ####

## Otro ejemplo en ciencias ambientales

library(datasets)
data(airquality) # Estos datos sonDaily air quality measurements in New York, May to September 1973.
pairs(airquality)
# Visualización

library(MVN)

# Seleccionar solo las variables numéricas relevantes para la prueba
datos_ambientales <- airquality[, c("Ozone", "Solar.R", "Wind", "Temp")]

# Realizar la prueba de normalidad multivariante,, acá colocó la prueba para datos mayores a 50
(resultado_prueba <- mvn(datos_ambientales, mvnTest = "hz", univariateTest ="Lillie",multivariateOutlierMethod = "quan")) # Para los outliers usa Mahalanobis distance

#------------------------------------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------------------------------



