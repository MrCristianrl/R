#En este dataframe modificaremos el tipo de columnas del dataframe de jugadores_liga_santander y posteriormente crearemos el dataframe data, que contiene el nombre de los jugadores obtenidos en la web comuniazo y los de la web de footballsquads.
#Nos encontramos con el problema de que los nombres obtenidos de una y otra página no coinciden, por lo que no podemos unir los dataframes mediante joins, así que hay que usaremos la función regex_left_join, que une los nombres similares. 
#Ahora el problema es que algunos jugadores aparecen con nombre NA y otros se repiten, como por ejemplo: "Balenziaga", que tiene dos antecedentes, el propio "Balenziaga" y "Bale" del Madrid.

library(fuzzyjoin)

str(jugadores_liga_santander)

#Modificamos el nombre de las columnas y le damos formato.

names(jugadores_liga_santander) <- c("Nombre", "Nacionalidad","Posición","Club")

jugadores_liga_santander$Nacionalidad <- as.factor(jugadores_liga_santander$Nacionalidad)
jugadores_liga_santander$Posición <- as.factor(jugadores_liga_santander$Posición)
levels(jugadores_liga_santander$Posición) <- c("Defensa","Delantero","Portero","Centrocampista")
jugadores_liga_santander$Club <- as.factor(jugadores_liga_santander$Club)
levels(jugadores_liga_santander$Club) <- c("ATH", "ALV","ATM","FCB","BET","CEL","EIB","ESP","GET","GIR", "HUE", "LEG", "LEV", "RVAL","RMA","SEV","RSOC","VAL","VALD","VIL" )

#Como los nombres no coinciden, vamos a sustituir los nombres en el jugadores_comunio para que sí que coincidan.

data <- jugadores_liga_santander %>%
  regex_left_join(jugadores_comunio, by = c(Nombre = "Nombre")) %>%
  select(Nombre.comunio = Nombre.y, Nombre.plantilla = Nombre.x)
