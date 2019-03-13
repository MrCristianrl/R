  #EN ESTE ARCHIVO SE OBTIENEN EN EL DATAFRAME jugadores_comunio LA LISTA DE JUGADORES DEL COMUNIO EN LA TEMPORADA 2018/19 EN LA LIGA SANTANDER USADA POR LA PÁGINA WEB COMUNIAZO.

#Cargamos los paquetes para hacer scraping a la web de comuniazo. Actualizado a día 13/03/19.

library(rvest) # github version
library(dplyr)

url <- "https://www.comuniazo.com/comunio/jugadores"

tmp <- read_html(url)
tmp <- html_nodes(tmp, "table")
jugadores_comunio_raw <- html_table(tmp[[1]])

#Modificamos el nombre de las columnas del dataframe

colnames(jugadores_comunio_raw)
colnames(jugadores_comunio_raw) <- c("Nombre","Puntos_totales","Media","Puntos_casa","Media_casa","Puntos_fuera","Media_fuera","Valor","Racha_últimos_5_partidos")


#Sustituímos en las columnas numéricas los _ por . y quitamos los puntos a la columna de valor.

j2 <- jugadores_comunio_raw %>% mutate(Media = gsub(",",".", Media), Media_casa = gsub(",",".", Media_casa), Media_fuera = gsub(",",".", Media_fuera), Valor=gsub("\\.","", Valor), Racha_últimos_5_partidos=gsub("-",0,Racha_últimos_5_partidos))

#La columna de racha, que viene a indicar los puntos obtenidos en los últimos 5 partidos, nos interesa sumarlos para obtener una sola cifra.

j3 <- j2 %>% mutate(Racha_últimos_5_partidos=sapply(strsplit(Racha_últimos_5_partidos," "), function(x) sum(as.numeric(x))))

head(j3)

#Le damos formato a las columnas numéricas.

j4 <- j3 %>% mutate(Puntos_totales=as.integer(Puntos_totales), Media =as.numeric(Media), Puntos_casa = as.integer(Puntos_casa), Media_casa= as.numeric(Media_casa), Puntos_fuera= as.integer(Puntos_fuera), Media_fuera=as.numeric(Media_fuera), Valor=as.numeric(Valor), Racha_últimos_5_partidos=as.numeric(Racha_últimos_5_partidos))

#Eliminamos las filas con valores NA.

jugadores_comunio <- j4[complete.cases(j4), ]

#Vemos que efectivamente el dataframe ya está para trabajar con él.

summary(jugadores_comunio)
