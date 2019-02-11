library(tibble)
library(tidyr)
library(ggplot2)
library(cluster)

#El dataframe OES contiene el salario anual, desde el año 2001 hasta el 2016, para 22 ocupaciones distintas.

oes <- readRDS('~/R/oes.rds')

head(oes)

summary(oes)

table(is.na(oes))

#No hay valores NA ni hay que escalar las variables pues todas pueden ser comparadas entre ellas mismas. Calculamos la matriz de distancias. 

dist_oes <- dist(oes)

hc_oes <- hclust(dist_oes, method="average")

dend_oes <- as.dendrogram(hc_oes)

plot(dend_oes)

dend_colored <- color_branches(dend_oes, h=100000)

plot(dend_colored)

#Creamos el dataframe a partir de la matriz y colocamos el nombre de las filas como columna.

df_oes <- rownames_to_column(as.data.frame(oes), var = 'occupation')

#Asignamos los clusters con una altura de 100000

cut_oes <- cutree(hc_oes, h = 100000)

#Añadimos al dataframe anterior la columna correspondiente de su cluster.

clust_oes <- mutate(df_oes, cluster = cut_oes)


#Agrupamos los años en una columna.

gathered_oes <- gather(data = clust_oes, 
                       key = year, 
                       value = mean_salary, 
                       -occupation, -cluster)

#Graficamos la evoluación del salario medio cada año.

ggplot(gathered_oes, aes(x = year, y = mean_salary, color = factor(cluster))) + 
  geom_line(aes(group = occupation))

#Parece que dividiendo en 3 clusters tiene sentido los resultados. Veamos mediante el método del codo.

tot_withinss <- map_dbl(1:10,  function(k){
  model <- kmeans(x = oes, centers = k)
  model$tot.withinss
})

#Creamos el dataframe con la columna tot.withinss, que es la suma total de los cuadrados.

elbow_df <- data.frame(
  k = 1:10,
  tot_withinss = tot_withinss
)

#Graficamos para detectar el codo.

ggplot(elbow_df, aes(x=k, y=tot_withinss)) + geom_line() + scale_x_continuous(breaks=1:10)

#Mediante este método obtenemos que el "mejor" número de clusters es 2.

fit_c <- kmeans(oes, centers=2)
clusters <- fit_c$cluster

clust_oes_elbow <- mutate(df_oes, cluster=clusters)

gathered_oes_elbow <- gather(data = clust_oes_elbow, 
                       key = year, 
                       value = mean_salary, 
                       -occupation, -cluster)

ggplot(gathered_oes_elbow, aes(x=year, y=mean_salary, col=factor(cluster))) + geom_line(aes(group=occupation))

#Veamos ahora con el coeficiente de la silueta.

sil_width <- map_dbl(2:10, function(k){
  model <- pam(oes, k=k)
  model$silinfo$avg.width 
  })

sil_df <- data.frame(k=2:10, sil_width=sil_width)


ggplot(sil_df, aes(x=k, y=sil_width)) + geom_line() + scale_x_continuous(breaks=2:10)

#En este caso obtenemos que el mejor número de clusters es 7, aunque el 2 está cerca.

fit_sil <- pam(oes, k=7)

clust_oes_silhouette <- mutate(df_oes, cluster=fit_sil$clustering)

gathered_oes_silhouette <- gather(data = clust_oes_elbow, 
                                  key = year, 
                                  value = mean_salary, 
                                  -occupation, -cluster)

ggplot(gathered_oes_silhouette, aes(x=year, y=mean_salary, col=factor(cluster))) + geom_line(aes(group=occupation))

#Ninguna de las 3 distribuciones en distintos números de clusters es incorrecta pues dependerá del posterior estudio que se quiera hacer con esos datos.

  
  
