#EN ESTE ARCHIVO SE OBTIENEN EN EL DATAFRAME data_j LA LISTA DE JUGADORES DEL COMUNIO EN LA TEMPORADA 2018/19 EN LA LIGA SANTANDER USADA POR LA PÁGINA WEB COMUNIAZO.

library(rvest) # github version
library(dplyr)

url <- "https://www.comuniazo.com/comunio/jugadores"

tmp <- read_html(url)
tmp <- html_nodes(tmp, "table")
jugadores <- html_table(tmp[[1]])

colnames(jugadores)
colnames(jugadores) <- c("Nombre","Puntos_totales","Media","Puntos_casa","Media_casa","Puntos_fuera","Media_fuera","Valor","Racha_últimos_5_partidos")

j2 <- jugadores %>% mutate(Media = gsub(",",".", Media), Media_casa = gsub(",",".", Media_casa), Media_fuera = gsub(",",".", Media_fuera), Valor=gsub("\\.","", Valor), Racha_últimos_5_partidos=gsub("-",0,Racha_últimos_5_partidos))

j3 <- j2 %>% mutate(Racha_últimos_5_partidos=sapply(strsplit(Racha_últimos_5_partidos," "), function(x) sum(as.numeric(x))))

head(j3)

j4 <- j3 %>% mutate(Puntos_totales=as.integer(Puntos_totales), Media =as.numeric(Media), Puntos_casa = as.integer(Puntos_casa), Media_casa= as.numeric(Media_casa), Puntos_fuera= as.integer(Puntos_fuera), Media_fuera=as.numeric(Media_fuera), Valor=as.numeric(Valor), Racha_últimos_5_partidos=as.numeric(Racha_últimos_5_partidos))

data_j <- j4[complete.cases(j4), ]

summary(data_j)

