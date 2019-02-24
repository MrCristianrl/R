library(readr)
library(dplyr)
library(ggplot2)

data <- read.csv("~/R/WisconsinCancer.csv")

head(data)

cancer <- as.matrix(data[3:32])


row.names(cancer) <- data$id

diagnosis <- as.numeric(data$diagnosis=="M")

#Veamos si hay que escalar los datos

colMeans(cancer) %>% summary()

apply(cancer, 2, sd) %>% summary()

#Escalamos los datos pues existe mucha diferencia entre las medias y las desviaciones típicas.

pr.cancer <- prcomp(cancer, center=TRUE, scale=TRUE)

summary(pr.cancer)

#Creamos un biplot.

biplot(pr.cancer, cex=0.6)

#Graficamos la componente 1 y la 2.

plot(pr.cancer$x[, c(1,2)], col=(diagnosis+1), xlab="PC1", ylab="PC2")

pve <- pr.cancer$sdev^2
pva <- pve/sum(pve)

par(mfrow=(c(1,2)))
  
#Graficamos la proporción de la varianza y la acumulada para cada componente principal.

plot(1:30, pva, ylim= c(0,1), type= "b", xlab="Componente principal", ylab="Proporción de la varianza explicada")

plot(1:30, cumsum(pva), ylim=c(0,1), type="b", xlab="Componente principal", ylab="Proporción de la varianza acumulada")

#Escalamos el dataframe para calcular la matriz distancia.

cancer_scaled <- scale(cancer)

cancer_dist <- dist(cancer_scaled)

cancer_hclust <- hclust(cancer_dist)

plot(cancer_hclust)

#Tomamos h=20, lo que nos permite dividir en 4 clusters.

cancer_clusters <- cutree(cancer_hclust, h=20)

#Comparamos con el vector diagnóstico

table(cancer_clusters, diagnosis)

#Creamos ahora un modelo de k-medias clustering

cancer_km <- kmeans(scale(cancer), centers=2, nstart=20)

table(cancer_km$cluster, diagnosis)

table(cancer_km$cluster, cancer_clusters)

#El cluster 1 mediante el método de los kmeans puede ser equivalente a los clusters 1,2 y 4. Al mismo tiempo, el cluster 2 del método kmeans parece ser equivalente al cluster número 3.

#Tomamos el número mínimo de componentes principales para describir al menos el 90% de la variabilidad de los datos.

plot(cumsum(pva))

#Tomamos el número de componentes principales como 7

cancer_pr_hclust <- hclust(dist(pr.cancer$x[,1:7], method="euclidean"))

cancer_pr_hclust_clusters <- cutree(cancer_pr_hclust, k=4)

table(cancer_pr_hclust_clusters, diagnosis)

table(cancer_pr_hclust_clusters, diagnosis)

table(cancer_km$cluster, diagnosis)

