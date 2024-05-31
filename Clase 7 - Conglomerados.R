###################################################################################################################
# RFM
###################################################################################################################

#------------------------------------------------------------------------------------------------------------------

####librerías utilizadas####
library(rfm)
library(readr)
library(dplyr)
library(ggplot2)
library(knitr)
library(kableExtra)
library(dtplyr)
library(tidyr)
library(lattice)
library(caret)
library(gridExtra)
library(PerformanceAnalytics)
library(nortest)
library(factoextra)
library(cluster)
library(stats)
library(corrr)
library(car)
library(simstudy)
library(data.table)
library(tidyverse)

####Cargar datos ####
data <- read_csv("GitHub/estadistica-inferencia/Modelos_Multivariantes/Estad-stica-Multivariante/Datos-supermercado.csv")
# Convertir la columna InvoiceDate a formato fecha
data$InvoiceDate <- dmy(data$InvoiceDate)

ecom <- mutate(data[,c(-2, -7)], # gets only columns we need
               Revenue = UnitPrice*Quantity)
####Eliminación de NA, valores negativos en la BD####
summary(ecom) %>% 
  kable() %>% 
  kable_minimal()
ecom %>% 
  ggplot(aes(Quantity, UnitPrice)) +
  geom_point(alpha=.3) + 
  geom_jitter() + 
  labs(title="Comparing UnitPrice against Quantity", 
       tag = "Figure 1") + theme_classic()+theme(text = element_text(family = "serif"))
#Elimino NA
ecom %>% 
  filter(UnitPrice <= 0 | Quantity <= 0 ) %>% 
  arrange(UnitPrice)  %>% 
  head(10) %>% 
  kable() %>% 
  kable_classic_2()
ecom_clean <- ecom %>%  
  drop_na(CustomerID) %>% # Removes entries with out customer information
  filter(Quantity > 0,
         UnitPrice > 0 )  #Removes entries with negative quantity or Unit Price


data <- ecom_clean
# Definir la fecha de referencia para calcular la recencia
reference_date <- max(data$InvoiceDate) + days(1)

# Calcular las métricas RFM para cada cliente
rfm <- data %>%
  group_by(CustomerID) %>%
  summarise(
    Recency = as.numeric(difftime(reference_date, max(InvoiceDate), units = "days")),
    Frequency = n_distinct(InvoiceNo),
    Monetary = sum(UnitPrice)
  )

# Graficar Recency
plot_recency <- ggplot(rfm, aes(x = Recency)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black") +
  labs(title = "Distribución de la Recencia", x = "Recency (días)", y = "Frecuencia") +
  theme_minimal()

# Graficar Frequency
plot_frequency <- ggplot(rfm, aes(x = Frequency)) +
  geom_histogram(binwidth = 1, fill = "green", color = "black") +
  labs(title = "Distribución de la Frecuencia", x = "Frequency (número de compras)", y = "Frecuencia") +
  theme_minimal()

# Graficar Monetary
plot_monetary <- ggplot(rfm, aes(x = Monetary)) +
  geom_histogram(binwidth = 10, fill = "red", color = "black") +
  labs(title = "Distribución del Valor Monetario", x = "Monetary (total gastado)", y = "Frecuencia") +
  theme_minimal()

# Mostrar las gráficas en una sola disposición
grid.arrange(plot_recency, plot_frequency, plot_monetary, nrow = 3)

###las mejoras compras de clientes en el local
ecom_clean %>% 
  arrange(desc(Revenue)) %>%  
  head(10) %>% 
  kable() %>% 
  kable_classic_2()



# para saber cuántos cliente regresan
n<-nrow(rfm)
tot_freq <- 1
i <-1
while(i < n){
  tot_freq<-tot_freq + rfm$Frequency[i]
  i<-i+1
}
tot_freq
nrow(data)
percentage <- tot_freq/nrow(data)
percentage*100 # solo regresan el 19.26% de clientes regresan



#### Análisis de clústers####

#K-mean
df_RFM2 <-   ecom_clean[,-3] 
summary(df_RFM2)
n.test <- apply(X = df_RFM2, MARGIN = 2, FUN = nortest::lillie.test)
n.test

# número de clústers por el silhouette
set.seed(123)
factoextra::fviz_nbclust(df_RFM2, kmeans, method = "silhouette")
fviz_nbclust(df_RFM2, FUN = hcut, method = "wss") #elbow method
#kmeans
res <- kmeans(df_RFM2, 3, nstart = 25)
factoextra::fviz_cluster(res, data = df_RFM2,
                         geom = "point",
                         ellipse.type = "convex", 
                         ggtheme = theme_bw()
)
df_RFM2 <- 
  df_RFM2 %>% 
  dplyr::mutate(cluster = as.factor(res$cluster)) %>% 
  tidyr::drop_na()
data_long <- 
  df_RFM2 %>% 
  tidyr::gather(key = 'var', 
                value = 'valor', c(1:3), 
                factor_key = TRUE)
# resumen por clúster
df_RFM2 %>%
  dplyr::group_by(cluster) %>%
  dplyr::group_map(~ summary(.))

data_long %>% 
  dplyr::filter(var %in% 'Monetary') %>% 
  dplyr::group_by(cluster) %>% 
  ggplot2::ggplot() + 
  ggplot2::aes(x = cluster, y = valor, fill = cluster) + 
  ggplot2::geom_boxplot() + 
  ggplot2::theme_minimal() + 
  ggplot2::labs(title = "Customer segmentation-Monetary", 
                x = "Cluster", 
                y = "Monetary") +
  ggplot2::theme(legend.position="bottom",text = ggplot2::element_text(family = 'serif'))

data_long %>%
  dplyr::group_by(cluster) %>% 
  ggplot2::ggplot() + 
  ggplot2::aes(x = cluster, y = valor, fill = cluster) + 
  ggplot2::geom_boxplot() + 
  ggplot2::theme_minimal() + 
  ggplot2::theme(text = ggplot2::element_text(family = 'serif')) + 
  ggplot2::facet_grid(var ~ ., scales = 'free')

k=kmeans(df_RFM[,-1],centers = 3, iter.max = 100 )
#los centroides
k$centers
table(k$cluster) # cuántas personas son parte de los segmentos
library(MASS)

# distribución de centroides
centers= k$centers
mini= apply(df_RFM[,-1], 2, min)
maxi= apply(df_RFM[,-1],2,max)
maxi=(as.numeric(maxi))
mini=(as.numeric(mini))
centersde=t(mini+t(centers)*(maxi-mini))
data_centers=data.frame(centersde)
names(data_centers)[1]="Recency"
names(data_centers)[2]="Frequency"
names(data_centers)[3]="Monetary"
parcoord(data_centers, col=1:3, var.label = T)
legend(x = "bottom",         # Posición
       legend = c("Grupo 1", "Grupo 2", "Grupo 3"), # Textos de la leyenda
       lty = c(2),          # Tipo de líneas
       col = c(1,2,3),          # Colores de las líneas
       lwd = 2,
       bty = "n", 
       cex = 0.9,
       inset = c(0, -0.25),
       horiz = T)     

par(xpd=T)

library(GGally)
# Plot
ggparcoord(data_centers,
           columns = 1:3, groupColumn = 1, showPoints = TRUE,
           alphaLines = 0.3)


# se puede notar que la segmenatción con k= 3, no muestra la información adecuada, por lo que 
# se toma otra estrategia para los clústers

#### Calidad el cluster kmeans####
calidad.kmean <- scale(df_RFM[,-1])
km_clusters <- eclust(x = calidad.kmean, FUNcluster = "kmeans", k = 3, seed = 123,
                      hc_metric = "euclidean", nstart = 50, graph = FALSE)
fviz_silhouette(sil.obj = km_clusters, print.summary = TRUE, palette = "jco",
                ggtheme = theme_classic()) 
library(dplyr)
km_clusters$silinfo$widths %>% filter(sil_width <= 0)
# se puede notar que en el cluster 2 hay una medida mal clasficada, cluster 2 y 3 hay una
# Duun indice
library(fpc)
calidad.kmean <- scale(df_RFM[,-1])
# K-means clustering con k = 3
set.seed(321)
km_clusters <- kmeans(x = dist(calidad.kmean, method = "euclidean"), centers = 3,
                      nstart = 50)
# Cálculo de índices
km_indices <- cluster.stats(d = dist(calidad.kmean, method = "euclidean"), 
                            clustering = km_clusters)
# Medidas de homogeneidad y separación
km_indices$average.within
# Índice Dunn
km_indices$dunn



#### Clústerización por CLARA ####

df <- data.frame(segment)
df$segment=as.factor(df$segment)
levels(df$segment)
df.2 <- df[,-2]
tipo.cliente <- df[,2]
pairs(segment[,6:9], col = tipo.cliente,lower.panel = NULL)
par(xpd = TRUE)
legend(x = 0.05, y = 0.4, cex = 2,
       legend=as.character(levels(tipo.cliente)),
       fill = unique(tipo.cliente))
par(xpd = NA)

dfc=data.frame(df)
dfc=dfc[,6:9]
DDA= data.frame(tipo.cliente)
dfc=data.frame(DDA,dfc)
dfc %>%
  split(.$tipo.cliente) %>% 
  map(select, -c(tipo.cliente)) %>% 
  map(cor) 
#clusterización por el Método de CLARA
clara_clusters <- clara(x = dfc, k = 3, metric = "manhattan", stand = TRUE,
                        samples = 60, pamLike = TRUE)
clara_clusters$sample
clara_clusters$medoids
clara_clusters$i.med
clara_clusters$clustering
clara_clusters$clusinfo
table(clara_clusters$clustering)
fviz_cluster(object = clara_clusters, ellipse.type = "t", geom = "point",
             pointsize = 1.5) +  theme_bw() +
  labs(title = "Resultados clustering CLARA") +
  theme(legend.position = "none")

#### Calidad el cluster Clara####

#silohutte
calidad.clara<- clara_clusters
fviz_silhouette(sil.obj = calidad.clara, print.summary = TRUE, palette = "jco",
                ggtheme = theme_classic()) 

library(dplyr)
calidad.clara$silinfo$widths # con esto veo si está malclasificado, los valores menores a cero son los que se clasificaron mal
# se puede notar que en el cluster 3 hay una medida mal clasficada, cluster 0 y 1 hay una

# Duun indice
library(fpc)
set.seed(321)
km_clusters_clara <- clara_clusters$clustering
# Cálculo de índices
km_indices <- cluster.stats(d = dist(dfc, method = "euclidean"))
# Medidas de homogeneidad y separación
km_indices$average.within
# Índice Dunn
km_indices$dunn


library(clValid)
Dist2 <- dist(dfc[,-1],method="euclidean")
km_clusters_clara <- clara_clusters$clustering
dunn(Dist2, km_clusters_clara)

####Algortimo hierarchical clustering aglomerativo ####
datos <- scale(dfc[,2:5])

# Matriz de distancias euclídeas
mat_dist <- dist(x = datos, method = "euclidean")
# Dendrogramas con linkage complete y average
hc_euclidea_complete <- hclust(d = mat_dist, method = "complete")
hc_euclidea_average  <- hclust(d = mat_dist, method = "average")
cor(x = mat_dist, cophenetic(hc_euclidea_complete))
cor(x = mat_dist, cophenetic(hc_euclidea_average))
# se puede elegir más distancias
datj <- scale(dfc[,-1])
datj<-datos
rownames(datj) <- dfc[,1]
# Matriz de distancias euclideas
mat_dist <- dist(x = datj)
# Dendrogramas con linkage complete y average
hc_euclidea_complete <- hclust(d = mat_dist, method = "complete")
hc_euclidea_average  <- hclust(d = mat_dist, method = "average")
hc_euclidea_single  <- hclust(d = mat_dist, method = "single")
hc_euclidea_ward.D2  <- hclust(d = mat_dist, method = "ward.D2")
hc_euclidea_median  <- hclust(d = mat_dist, method = "median")
hc_euclidea_centroid  <- hclust(d = mat_dist, method = "centroid")
hc_euclidea_mcquitty  <- hclust(d = mat_dist, method = "mcquitty")
cor(x = mat_dist, cophenetic(hc_euclidea_complete))
cor(x = mat_dist, cophenetic(hc_euclidea_average)) # tiene la mayor correlación
cor(x = mat_dist, cophenetic(hc_euclidea_single))
cor(x = mat_dist, cophenetic(hc_euclidea_ward.D2))
cor(x = mat_dist, cophenetic(hc_euclidea_median))
cor(x = mat_dist, cophenetic(hc_euclidea_centroid)) 
cor(x = mat_dist, cophenetic(hc_euclidea_mcquitty))

library(factoextra)
set.seed(133)
hc_euclidea_completo <- hclust(d = dist(x = datos, method = "euclidean"),
                               method = "complete")

fviz_dend(x = hc_euclidea_completo, k = 3, cex = 0.6) +
  geom_hline(yintercept = 5, linetype = "dashed") +
  labs(title = "Herarchical clustering",
       subtitle = "Euclidean distance, K=3") # corte en 5 para tener los 3 grupos

#hacer zoom al dendograma
set.seed(5665)
fviz_dend(x = hc_euclidea_completo,
          k = 3,
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800"),
          color_labels_by_k = TRUE,
          cex = 0.5,
          main = "Zoom del area x = 1, 20, y = -50, 200",
          xlim = c(1,20),
          ylim = c(-50,200))


#se corta el árbol
cutree(hc_euclidea_completo, k = 3)
cutree(hc_euclidea_completo, h = 5)
clusters <- cutree(tree = hc_euclidea_completo, k = 3)
table(clusters, datos[, "amount"], dnn = list("clusters", "tipo de cliente"))
a=table(cutree(hc_euclidea_completo, h = 3), datos[, "amount"])
a=as.data.frame(a)

#Gap statistic method, número óptimo de clústers

set.seed(133)
kmeans_gap <- clusGap(x = datos, 
                      FUNcluster = kmeans,
                      K.max = 15,
                      B = 500,
                      verbose = FALSE,
                      nstart = 50)

# Preprocesado y modelado
# ==============================================================================
library(tree)

datos_rf = segment[,-1]
catgoriadatos=factor(datos_rf$segment)
levels(catgoriadatos)
str(catgoriadatos)
catgoriadatos=factor(catgoriadatos, labels = c("1","2","3","4","5","6","7","8"))
levels(catgoriadatos)
str(catgoriadatos)
catgoriadatos=as.data.frame(catgoriadatos)
datos_rf=cbind(catgoriadatos, datos_rf)
head(datos_rf, 3)
datos_rf$catgoriadatos=factor(datos_rf$catgoriadatos)
skim(datos_rf)
datos_rf=datos_rf[,-2]
skim(datos_rf)


# División de los datos en train y test
# ==============================================================================
set.seed(123)
train <- sample_frac(datos_rf,.7)
prueba <- setdiff(datos_rf, train)
library(randomForest)


# Creación y entrenamiento del modelo
# ==============================================================================
set.seed(123)
library(tidyverse)
library(rpart)
library(rpart.plot)
library(caret)
#arbol de clasificación con random forest
# Tratamiento de datos
# ==============================================================================
library(ISLR)
library(dplyr)
library(tidyr)
library(skimr)

# Gráficos
# ==============================================================================
library(ggplot2)
library(ggpubr)

# Preprocesado y modelado
# ==============================================================================
library(tidymodels)
library(ranger)
library(doParallel)
# Grid de hiperparámetros evaluados
# ==============================================================================
param_grid = expand_grid(
  'num_trees' = c(50, 100, 500, 1000, 5000),
  'mtry'      = c(3, 5, 7, ncol(train)-1),
  'max_depth' = c(1, 3, 10, 20)
  
)

# Loop para ajustar un modelo con cada combinación de hiperparámetros
# ==============================================================================

oob_error = rep(NA, nrow(param_grid))

for(i in 1:nrow(param_grid)){
  
  modelo <- ranger(
    formula   = catgoriadatos ~ .,
    data      = train, 
    num.trees = param_grid$num_trees[i],
    mtry      = param_grid$mtry[i],
    max.depth = param_grid$max_depth[i],
    seed      = 123
  )
  
  oob_error[i] <- modelo$prediction.error
}


# Resultados
# ==============================================================================
resultados <- param_grid
resultados$oob_error <- oob_error
resultados <- resultados %>% arrange(oob_error)
head(resultados, 1)
#número de arboles = 100
#mtry = 3, max dett = 10, ooberror=0




library(parsnip)
library(randomForest)
# modelo 
set.seed(121)
modelo <- randomForest(catgoriadatos ~ ., data=train,
                       mode  = "classification",
                       mtry  = 3,
                       trees = 100)
print(modelo)
plot(modelo)
mod=randomForest(x=train[,2:8], 
                 y=train[,1], 
                 ntree=100, 
                 keep.forest = T)

pred= predict(mod, prueba[,2:8], type="class" )
table(prueba[,"catgoriadatos"],pred,dnn=c("Actual", "Predicho"))
probs= predict(mod, prueba[,2:8], type="prob" )

#un solo arból de clasificación

set.seed(123)
arbol_clasificacion <- tree::tree(
  formula = catgoriadatos ~ .,
  data    = train,
  split   = "deviance",
  mincut  = 20,
  minsize = 50
)
print(arbol_clasificacion)
summary(arbol_clasificacion)
par(mar = c(1,1,1,1))
plot(x = arbol_clasificacion, type = "proportional")
text(x = arbol_clasificacion, splits = TRUE, pretty = 0, cex = 0.8, col = "firebrick")


#mincut: número mínimo de observaciones que debe de tener al menos uno de los nodos hijos para que se produzca la división.
#minsize: número mínimo de observaciones que debe de tener un nodo para que pueda dividirse.


summary(arbol_clasificacion)




# random forest clasifica muy bien 
# el mejor algortimo
library(clValid)
datos <- scale(dfc[,2:5])
datos<- as.data.frame(datos)
comparacion <- clValid(
  obj        = datos,
  nClust     = 2:8,
  clMethods  = c("hierarchical", "kmeans", "pam", "CLARA", "randomForest"),
  validation = c("stability", "internal"),
  method="complete"
)
summary(comparacion)


