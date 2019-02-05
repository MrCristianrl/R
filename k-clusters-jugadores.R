#Primera práctica con k-clusters en la identificación de los jugadores del mismo equipo en un partido de 6vs6 sabiendo las posiciones de los jugadores al inicio.

library(dplyr)
library(ggplot2)

lineup <- data.frame("x"=c(-1,-2,8,7,-12,-15,-13,15,21,12,-25,26),"y"=c(1,-3,6,-8,8,0,-10,16,2,-15,1,0))

#Calculamos la distancia euclídea entre todos los jugadores.

dist_players <- dist(lineup)

#Mediante las dos siguientes funciones, agrupamos los jugadores mediante el máximo enlace completo e identificamos a qué equipo corresponde cada jugador.

hc_players <- hclust(dist_players, method="complete")
cluster_k2 <- cutree(hc_players, k=2)

#Agrupamos el vector cluster_k2 al dataframe de antes para visualizar los datos juntos.

lineup_k2_complete <- lineup %>% mutate(cluster=cluster_k2)

#Graficamos cada jugador por equipo.

ggplot(lineup_k2_complete, aes(x=x,y=y, col=factor(cluster))) + geom_point()
