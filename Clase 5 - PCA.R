###################################################################################################################
# PCA
###################################################################################################################

#------------------------------------------------------------------------------------------------------------------
#### Cargar datos ####

# Cargar las librerías necesarias
install.packages("ggplot2")
install.packages("factoextra")
library(ggplot2)
library(factoextra)

# Cargar el conjunto de datos iris
data(iris)
head(iris)

# Seleccionar las columnas numéricas para PCA
iris_data <- iris[, 1:4]

# Estandarizar los datos
data_std <- scale(iris_data)

# Aplicar PCA
pca_result <- prcomp(data_std, center = TRUE, scale. = TRUE)

# Resumen del PCA
summary(pca_result)

# Visualización de la proporción de varianza explicada
fviz_eig(pca_result)
# Obtener la matriz de carga factorial
loadings <- pca_result$rotation

# Aplicar la rotación Varimax
varimax_result <- varimax(loadings)

# Ver los resultados de la rotación Varimax
print(varimax_result$loadings)

# Transformar los datos utilizando los componentes rotados
rotated_components <- as.data.frame(pca_result$x %*% varimax_result$loadings)

# Renombrar las columnas para mayor claridad
colnames(rotated_components) <- c("PC1_Rotado", "PC2_Rotado", "PC3_Rotado", "PC4_Rotado") #"PC5_Rotado", "PC6_Rotado", "PC7_Rotado", "PC8_Rotado"

# Visualización de los datos en el espacio de los componentes rotados
plot(rotated_components$PC1_Rotado, rotated_components$PC2_Rotado,
     xlab = "Componente Principal 1 Rotado", ylab = "Componente Principal 2 Rotado",
     main = "Distribución de Vinos en el Espacio de Componentes Principales Rotados")


# Visualización de los resultados
# Grafico de Scree para visualizar la varianza explicada por cada componente
fviz_eig(pca_result)

# Plot de individuos
fviz_pca_ind(pca_result, geom.ind = "point", pointshape = 21, 
             pointsize = 2, fill.ind = iris$Species, 
             col.ind = "black", palette = "jco", 
             addEllipses = TRUE, ellipse.level = 0.95, 
             repel = TRUE)

# Plot de variables
fviz_pca_var(pca_result, col.var = "blue", repel = TRUE)


##### Ejemplo Encuesta #####


# Instalar y cargar paquetes necesarios
install.packages("psych")
install.packages("factoextra")
install.packages("readxl")
library(psych)
library(factoextra)
library(readxl)
library(readxl)
# Leer el archivo de Excel modificado

data <- read_excel("GitHub/estadistica-inferencia/Modelos_Multivariantes/Estad-stica-Multivariante/Variables para PCA.xlsx")


# Seleccionar solo las columnas numéricas
data_numeric <- data[, sapply(data, is.numeric)]
data_clean <- na.omit(data_numeric)
# Estandarizar los datos
data_std <- data.frame(scale(data_clean))

# Realizar PCA
pca_result <- prcomp(data_std, center = TRUE, scale. = TRUE)

# Resumen de los resultados del PCA
summary(pca_result)

# Visualización de la proporción de varianza explicada
fviz_eig(pca_result)

# Obtener la matriz de carga factorial
loadings <- pca_result$rotation

fviz_pca_var(pca_result,
             col.var = "contrib", # Color por contribución
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE) # Evitar superposición de etiquetas

# Longitud de las Flechas: Cada variable se representa como una flecha (vector). La longitud de la flecha indica la importancia de la variable en ese componente principal
# Ángulo entre Flechas: El ángulo entre dos flechas indica la correlación entre las variables correspondientes. Flechas con ángulos pequeños (cercanas) indican variables que están altamente correlacionadas positivamente, mientras que flechas con ángulos de 180 grados (opuestas) indican una correlación negativa.
# Dim1 (38.4%): El primer componente principal (PC1) explica el 38.4% de la varianza total en los datos.
# Dim2 (18.7%): El segundo componente principal (PC2) explica el 18.7% de la varianza total
# Variables con alta contribución a Dim1:

    #Costo de producción
    #Porcentaje de Ventas Cumplidas
    #Nivel de Inventario
    #Demanda de un producto
# Variables con alta contribución a Dim2:

    # Costo fijo de envío del distribuidor al cliente
    # Costo fijo asociado con la operación del distribuidor involucrado en el envío
    # Costo fijo de envío desde la fábrica hasta el distribuidor por tipo de transporte usado

# El gráfico sugiere que:

# Dim1 está principalmente influenciado por variables relacionadas con la gestión de inventarios, ventas y costos de producción.
# Dim2 está principalmente influenciado por variables relacionadas con los costos logísticos y operativos de envío.

# Visualización del biplot (individuos y variables)
fviz_pca_biplot(pca_result,
                col.ind = "cos2", # Color por calidad de representación
                col.var = "contrib", # Color por contribución
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                repel = TRUE) # Evitar superposición de etiquetas


# Aplicar la rotación Varimax
varimax_result <- varimax(loadings)

# Ver los resultados de la rotación Varimax
print(varimax_result$loadings)


