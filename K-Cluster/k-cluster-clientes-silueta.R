#library(dendextend)
library(dplyr)
library(cluster)
library(purrr)
library(ggplot2)

#Buscamos mediante el método del análisis de silueta el número más apropiado de clusters para el dataframe de los clientes.

ws_customers <- readRDS("~/R/ws_customers.rds")

sil_width <- map_dbl(2:10,  function(k){
  model <- pam(x = ws_customers, k = k)
  model$silinfo$avg.width
})

#Creamos un dataframe para visualizar con cada cluster la media del ancho de la silueta.

sil_df <- data.frame(
  k = 2:10,
  sil_width = sil_width
)

#Graficamos.

ggplot(sil_df, aes(x = k, y = sil_width)) +
  geom_line() +
  scale_x_continuous(breaks = 2:10)
