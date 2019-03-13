#Esta función obtiene todos los jugadores de la Liga Santander junto a su equipo, nacionalidad y posición.

library(data.table)

#Cargamos los 20 enlaces con las plantillas de los 20 equipos.

lista_plantillas <- list("http://www.footballsquads.co.uk/spain/2018-2019/laliga/alaves.htm", "http://www.footballsquads.co.uk/spain/2018-2019/laliga/abilbao.htm", "http://www.footballsquads.co.uk/spain/2018-2019/laliga/amadrid.htm", "http://www.footballsquads.co.uk/spain/2018-2019/laliga/barce.htm", "http://www.footballsquads.co.uk/spain/2018-2019/laliga/betis.htm", "http://www.footballsquads.co.uk/spain/2018-2019/laliga/celta.htm","http://www.footballsquads.co.uk/spain/2018-2019/laliga/eibar.htm", "http://www.footballsquads.co.uk/spain/2018-2019/laliga/espanyol.htm","http://www.footballsquads.co.uk/spain/2018-2019/laliga/getafe.htm","http://www.footballsquads.co.uk/spain/2018-2019/laliga/girona.htm","http://www.footballsquads.co.uk/spain/2018-2019/laliga/huesca.htm","http://www.footballsquads.co.uk/spain/2018-2019/laliga/leganes.htm","http://www.footballsquads.co.uk/spain/2018-2019/laliga/levante.htm","http://www.footballsquads.co.uk/spain/2018-2019/laliga/rayo.htm","http://www.footballsquads.co.uk/spain/2018-2019/laliga/rmadrid.htm","http://www.footballsquads.co.uk/spain/2018-2019/laliga/sociedad.htm","http://www.footballsquads.co.uk/spain/2018-2019/laliga/sevilla.htm", "http://www.footballsquads.co.uk/spain/2018-2019/laliga/valencia.htm","http://www.footballsquads.co.uk/spain/2018-2019/laliga/vallad.htm","http://www.footballsquads.co.uk/spain/2018-2019/laliga/villar.htm")

plantillas <- vector("list", length = 20)

for (i in 1:20){
  url <- lista_plantillas[[i]]
  tmp <- read_html(url)
  tmp <- html_nodes(tmp,"table")
  plantilla <- html_table(tmp[[1]], header=T)
  plantilla <- select(plantilla, 2:4)
  club = gsub("(.*/|\\.htm)", "", url)
  
  plantilla <- plantilla[!apply(plantilla=="",1,all), ]
  
  j <- which(plantilla=="Name")
  k <- j-2
  plantilla_club <- plantilla[1:k, ] %>% mutate(Club=club)
  plantillas[[i]] <- plantilla_club
}

jugadores_liga_santander <- rbindlist(plantillas)
