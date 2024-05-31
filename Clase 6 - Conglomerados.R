###################################################################################################################
# MANOVA
###################################################################################################################

#------------------------------------------------------------------------------------------------------------------
# Instalar y cargar los paquetes necesarios
install.packages("ggplot2")
install.packages("factoextra")
install.packages("cluster")
install.packages("fpc")
library(ggplot2)
library(factoextra)
library(cluster)
library(fpc)

#### Cargar datos ####
# Instalar y cargar las librerías necesarias
# Crear el dataframe con los datos corregidos
data <- data.frame(
  CCAA = c("Andalucía", "Aragón", "Asturias", "Baleares", "Canarias", "Cantabria", 
           "Cast. Mancha", "Cast. León", "Cataluña", "Valencia", "Extremadura", 
           "Galicia", "Madrid", "Murcia", "Navarra", "País Vasco", "La Rioja"),
  N_Cines = c(448, 76, 55, 68, 94, 26, 211, 102, 585, 300, 69, 166, 474, 88, 37, 171, 22),
  N_Peliculas = c(330, 310, 383, 523, 394, 315, 295, 234, 502, 435, 309, 341, 764, 358, 441, 385, 309),
  Espectadores_Esp = c(1380202, 580526, 207100, 280851, 345213, 190540, 1049698, 404716, 2179229, 1267581, 
                       226139, 570921, 3188742, 326445, 245750, 730241, 120135),
  Espectadores_Ext = c(13976149, 3513294, 1524423, 2081987, 4056725, 1149257, 5319556, 2406798, 19324988, 
                       9849692, 1614986, 4465381, 9126469, 2669391, 1403940, 5277214, 769674),
  Recaudacion = c(7709721, 2370874, 1000709, 2288764, 847231, 3464668, 1490303, 14234196, 6061359, 912405, 
                  2680531, 15282573, 1647870, 981839, 3673712, 526496, 14234196)
)

# Normalizar los datos (escalar)
data_scaled <- scale(data[,-1])

# Clusterización Jerárquica
# Calcular la matriz de distancias
dist_matrix <- dist(data_scaled, method = "euclidean")

# Realizar el clustering jerárquico
hc <- hclust(dist_matrix, method = "ward.D2")

# Cortar el dendrograma en 4 clusters
clusters_hc <- cutree(hc, k = 4)
# Visualizar el dendrograma
fviz_dend(hc, k = 4, # Cortar el árbol en 3 clusters
          rect = TRUE, # Dibujar rectángulos alrededor de los clusters
          rect_border = "jco", # Color de los rectángulos
          rect_fill = TRUE, # Rellenar los rectángulos
          labels_track_height = 0.1,ggtheme=theme_gray()) # Altura de las etiquetas

# Validación del Índice de Silueta para Clustering Jerárquico
sil_hc <- silhouette(clusters_hc, dist_matrix)
avg_sil_hc <- mean(sil_hc[, 3])
cat("Índice de Silueta (Clusterización Jerárquica):", avg_sil_hc, "\n")

# Visualizar el índice de Silueta para Clustering Jerárquico
fviz_silhouette(sil_hc)


# Clusterización No Jerárquica - DIANA
# Determinar el número óptimo de clusters usando el método de la silueta
fviz_nbclust(data_scaled, kmeans, method = "silhouette") +
  labs(subtitle = "Número óptimo de clusters según el método de la silueta")

# También se puede hacer con CLARA
set.seed(123)  # Para reproducibilidad
fviz_nbclust(data_scaled, clara, method = "silhouette") +
  labs(subtitle = "Número óptimo de clusters según el método de la silueta (CLARA)")

# Realizar el clustering DIANA
diana_result <- diana(dist_matrix)

# Cortar el dendrograma de DIANA en 3 clusters
clusters_diana <- cutree(as.hclust(diana_result), k = 2)

# Validación del Índice de Silueta para DIANA
sil_diana <- silhouette(clusters_diana, dist_matrix)
avg_sil_diana <- mean(sil_diana[, 2])
cat("Índice de Silueta (DIANA):", avg_sil_diana, "\n")

# Visualizar el índice de Silueta para DIANA
fviz_silhouette(sil_diana)

# Clusterización No Jerárquica - PAM
# Realizar el clustering PAM
pam_result <- pam(data_scaled, k = 3)

# Validación del Índice de Silueta para PAM
sil_pam <- silhouette(pam_result$clustering, dist_matrix)
avg_sil_pam <- mean(sil_pam[, 3])
cat("Índice de Silueta (PAM):", avg_sil_pam, "\n")

# Visualizar el índice de Silueta para PAM
fviz_silhouette(sil_pam)

# Aplicar el algoritmo CLARA
set.seed(123)  # Para reproducibilidad
clara_result <- clara(data_scaled, k = 3, samples = 5, pamLike = TRUE)
# Ver los resultados
print(clara_result)

# Visualizar los clusters
fviz_cluster(diana_result, data = data_scaled, ellipse.type = "convex", palette = "jco", ggtheme = theme_minimal())

# Validación del Índice de Silueta para CLARA
sil_clara <- silhouette(clara_result$clustering, dist(data_scaled))
avg_sil_clara <- mean(sil_clara[, 3])
cat("Índice de Silueta (CLARA):", avg_sil_clara, "\n")

# Visualizar el índice de Silueta para CLARA
fviz_silhouette(sil_clara)



##### Otro ejemplo

data(iris)
iris_data <- iris[, -5] # Excluir la columna de especies

# Para este ejemplo se puede utilizar el conjunto de datos llamado IRIS. 
# Estos datos son datos de un conjunto de flores con diferentes.

# Calcular la matriz de distancias
dist_matrix <- dist(iris_data, method = "euclidean")

# Realizar el clustering jerárquico
hc <- hclust(dist_matrix, method = "ward.D2")

# Visualizar el dendrograma
fviz_dend(hc, k = 3, # Cortar el árbol en 3 clusters
          rect = TRUE, # Dibujar rectángulos alrededor de los clusters
          rect_border = "jco", # Color de los rectángulos
          rect_fill = TRUE, # Rellenar los rectángulos
          labels_track_height = 0.5) # Altura de las etiquetas

# Establecer el número de clusters
set.seed(123) # Para reproducibilidad
km <- kmeans(iris_data, centers = 3, nstart = 25)

# Resumen de los resultados
print(km)
# Visualizar los clusters
fviz_cluster(km, data = iris_data,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())

#Comparación de Resultados

# Jeararquicos

# Cortar el dendrograma para obtener 3 clusters
sub_grp <- cutree(hc, k = 3)

# Añadir los clusters a los datos originales
iris_data_hc <- iris_data
iris_data_hc$cluster <- as.factor(sub_grp)

# Visualizar los clusters jerárquicos
fviz_cluster(list(data = iris_data, cluster = sub_grp),
             palette = "jco",
             ggtheme = theme_minimal())

# No Jeararquicos

# Añadir los clusters de K-means a los datos originales
iris_data_km <- iris_data
iris_data_km$cluster <- as.factor(km$cluster)

# Visualizar los clusters de K-means
fviz_cluster(km, data = iris_data,
             palette = "jco",
             ggtheme = theme_minimal())

