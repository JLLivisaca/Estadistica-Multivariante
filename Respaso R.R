2*pi*2
#Variables
radio = 3
2*pi*radio
Radio = c(149,173,1536)
2*pi*Radio
circunferencia = 2*pi*Radio
circunferencia # atajo ctrl + enter
(circunferencia = 2*pi*radio)
circunferencia[2]
# Graficos
numeros = rnorm(100,mean=2,sd=1)
plot(numeros) # Gráfica
boxplot(numeros,main="Gráfico boxplot")
hist(numeros,main = "Histograma de datos")
# Instalar paquetes
install.packages("nortest")
library(nortest)
??nortest
library(readr)
datos <- read_csv("Cursos CEDIA/2 Análisis multivariante en la investigación/Script/BostonHousing.csv")
View(datos)

