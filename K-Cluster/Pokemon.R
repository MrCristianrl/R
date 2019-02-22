library(readr)
library(dplyr)

Pokemon <- read_csv("R/Pokemon.csv", col_types = cols(Generation = col_skip(), 
                                                      Legendary = col_skip(), Name = col_skip(), 
                                                      Number = col_skip(), Total = col_skip(), 
                                                      Type1 = col_skip(), Type2 = col_skip()))

head(Pokemon)

str(Pokemon)

#Vamos a buscar el número de clusters recomendado para este dataset basándonos en la función codo. Para ello, tomamos primero la suma de los cuadrados totales como 0

wss <- 0

for (i in 1:15){
  km.out <- kmeans(Pokemon, centers=i, nstart=20, iter.max=50)
  wss[i] <- km.out$tot.withinss
}

#Dibujamos la gráfica para cada cluster.

plot(1:15, wss, xlab="cluster", ylab="wss", type="b")

k <- 3

km.out <- kmeans(Pokemon, centers = k, nstart = 20, iter.max = 50)

km.out

#Graficamos la comparación entre la defensa y la velocidad coloreada por clusters.

plot(Pokemon[, c("Defense", "Speed")],
     col = km.out$cluster,
     main = paste("k-means clustering de Pokemon con", k, "clusters"),
     xlab = "Defensa", ylab = "Velocidad")

#Observamos la media y desviación típica de las columnas.

colMeans(Pokemon)

apply(Pokemon, 2, sd)

pokemon.scaled <- scale(Pokemon)

#Realizamos ahora la selección de los clusters mediante el método de hcluster y comparamos con el resultado obtenido mediante el kmedias.

hclust.pokemon <- hclust(dist(Pokemon))

cut.pokemon <- cutree(hclust.pokemon, k=3)

table(cut.pokemon, km.out$cluster)


