

# Cargar datos
data(mtcars)

# Ver los primeros registros del conjunto de datos
head(mtcars)
# Escalar los datos
mtcars_scaled <- scale(mtcars)

# Calcular la matriz de distancias euclideanas
dist_euclidean <- dist(mtcars_scaled)

# Convertir a matriz y obtener distancias al centroide
dist_euclidean_matrix <- as.matrix(dist_euclidean)
center <- colMeans(mtcars_scaled)
euclidean_distances <- apply(mtcars_scaled, 1, function(row) sqrt(sum((row - center)^2)))

# Establecer umbral (por ejemplo, percentil 95 de las distancias euclideanas)
threshold_euclidean <- quantile(euclidean_distances, 0.95)

# Identificar puntos atípicos usando distancia euclideana
(outliers_euclidean <- euclidean_distances > threshold_euclidean)
# Convertir a matriz
dist_euclidean_matrix <- as.matrix(dist_euclidean)
# Calcular la media y la matriz de covarianza
mean_mtcars <- colMeans(mtcars)
cov_mtcars <- cov(mtcars)

#### Mahalanobis

# Calcular las distancias de Mahalanobis para cada punto
dist_mahalanobis <- mahalanobis(mtcars, mean_mtcars, cov_mtcars)
# Umbral de chi-cuadrado para el percentil 95
threshold <- qchisq(0.95, df = ncol(mtcars))

# Identificar puntos atípicos
outliers <- dist_mahalanobis > threshold

# Ver los índices de los datos atípicos
which(outliers)
# Visualización de datos con ggplot2
library(ggplot2)

# Añadir columna de outliers al conjunto de datos
mtcars$outlier <- outliers

# Crear gráfico de dispersión
ggplot(mtcars, aes(x = mpg, y = hp, color = outlier)) +
  geom_point() +
  labs(title = "Outliers usando la distancia de Mahalanobis",
       x = "Millas por galón (mpg)",
       y = "Caballos de fuerza (hp)")
