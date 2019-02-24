#############################


#Este dataset contiene un ránking del top 5000 de canales de youtube elaborado por Socialblade. 

#La columnas:
#Rank: Indica el ránking.
#Grade: Calidad del canal generada por Socialblade
#Channel.name: Nombre del canal
#Video uploads: Número de videos subidos en el canal
#Subscribers: Número de suscriptores en el canal
#Video views: Número de visualizaciones de los videos del canal.

#Obtenido de: https://www.kaggle.com/mdhrumil/top-5000-youtube-channels-data-from-socialblade

#Haremos un análisis de los datos proporcionados por el dataset.
###############################

library(readr)
library(stringr)
library(dplyr)
library(ggplot2)
library(corrplot)
library(forecast)

youtube <- read.csv("~/R/data.csv", stringsAsFactors = FALSE)

#Exploramos el dataset

str(youtube)

summary(youtube)

head(youtube)

#Convertimos a formato numérico a las columnas correspondientes y eliminamos los NA's

youtube_procesado <- youtube %>% mutate(Rank=as.numeric(str_extract(str_remove(youtube$Rank,"\\,"), "\\d+")), Grade=as.character(Grade),Video.Uploads=as.numeric(Video.Uploads), Subscribers=as.numeric(Subscribers))

youtube_procesado <- youtube_procesado[complete.cases(youtube_procesado),]

#Veamos cómo están distribuídos los canales en función de su calidad.

youtube_procesado %>% group_by(Grade) %>% ggplot(aes(x=Grade)) + geom_bar(fill="lightgreen", colour="green") + xlab("calidad") + ylab("Apariciones")

youtube_procesado %>% group_by(Grade) %>% summarize(Total=n())

#Los canales de calidad B+ triplica al segundo calidad con más presencia en el ránking

#Veamos cómo está distribuído el número de visitas, los suscriptores y los videos subidos en función del calidad del canal.

youtube_procesado %>% group_by(Grade) %>% summarize(Visitas_totales_grupo = sum(Video.views)) %>% ggplot(aes(x=Grade, y=Visitas_totales_grupo)) + geom_bar(stat="identity", colour="green", fill="lightgreen") + xlab("calidad") + ylab("Visitas totales") + ggtitle("Visitas totales por calidad")

youtube_procesado %>% group_by(Grade) %>% summarize(Subidas_totales_grupo = sum(Video.Uploads)) %>% ggplot(aes(x=Grade, y=Subidas_totales_grupo)) + geom_bar(stat="identity", colour="green", fill="lightgreen") + xlab("calidad") + ylab("Subidas totales") + ggtitle("Subidas totales por calidad")

youtube_procesado %>% group_by(Grade) %>% summarize(Suscriptores_por_grupo = sum(Subscribers)) %>% ggplot(aes(x=Grade, y=Suscriptores_por_grupo)) + geom_bar(stat="identity", colour="green", fill="lightgreen") + xlab("calidad") + ylab("Suscriptores") + ggtitle("Suscriptores totales por calidad")

#Podemos apreciar un mayor número de visitas y subscriptores en los canales de calidad A, así como mayor número de videos subidos en canales de calidad B+

#Graficamos el top 10 de canales con mayor número de visitas, suscriptores y videos subidos.

youtube_procesado %>% top_n(10) %>%ggplot(aes(x=reorder(Channel.name, Video.Uploads), y=Video.Uploads, fill=Grade)) + geom_col(stat="identity") + coord_flip() + xlab("Videos subidos") +ylab("Canal") + ggtitle("Top 10 canales en con mayor videos subidos")

youtube_procesado %>% top_n(10) %>%ggplot(aes(x=reorder(Channel.name, Video.views), y=Video.views, fill=Grade)) + geom_col(stat="identity") + coord_flip() + xlab("Visitas en los videos") +ylab("Canal") + ggtitle("Top 10 canales en mayor número de visitas")

youtube_procesado %>% top_n(10) %>%ggplot(aes(x=reorder(Channel.name, Subscribers), y=Subscribers, fill=Grade)) + geom_col(stat="identity") + coord_flip() + xlab("Subscriptores") + ylab("Canal") + ggtitle("Top 10 canales con mayor número de suscriptores")

#Los canales de calidad A++ son los que más videos han subido y más visitas tienen. Sin embargo, el canal que más suscriptores tienes es uno de calidad A-.

#Calculamos la matriz de correlación entre las columnas numéricas.

correlacion <- cor(youtube_procesado[, 4:6])

corrplot(correlacion, method="square")

#Como cabía esperar, se aprecia una  correlación (0.7912) entre el número de visitas y el número de subscriptores.

youtube_procesado %>% ggplot(aes(Subscribers, Video.views)) + geom_point() + geom_smooth(method="lm", se=FALSE) + xlab("Suscriptores") + ylab("Visitas en los videos") + ggtitle("Modelo lineal en relación al número de suscriptores y visitas en los videos")
