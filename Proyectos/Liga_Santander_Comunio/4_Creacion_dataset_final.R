##EN ESTE ARCHIVO SE MODIFICAN LOS NOMBRES DE ALGUNAS FILAS QUE APARECÍAN COMO NA Y AL CRUZAR LOS NOMBRES DE LAS TABLAS DE AQUELLOS JUGADORES CON EL MISMO NOMBRE PROVOCABAN UN ERROR.

#Sustituímos los nombres de algunos jugadores que no coinciden en ambos datasets, pero sin embargo están en ambos. Esto es debido a que muchos son nombres compuestos o tienen algún caracter especial. Los cambiamos a mano después de buscar las filas cuyo valor en la columna Nombre.comunio es NA.
#No es el mejor método si lo pensamos en datasets muy grandes, pero en este caso se puede hacer a ojo ya que el dataset lo permite.

#Sustituímos los nombres de algunos jugadores que no coinciden en ambos datasets, pero sin embargo están en ambos. Esto es debido a que muchos son nombres compuestos o tienen algún caracter especial. Los cambiamos a mano después de buscar las filas cuyo valor en la columna Nombre.comunio es NA.
#No es el mejor método si lo pensamos en datasets muy grandes, pero en este caso se puede hacer a ojo ya que el dataset lo permite.

#Sustituímos los nombres de algunos jugadores que no coinciden en ambos datasets, pero sin embargo están en ambos. Esto es debido a que muchos son nombres compuestos o tienen algún caracter especial. Los cambiamos a mano después de buscar las filas cuyo valor en la columna Nombre.comunio es NA.
#No es el mejor método si lo pensamos en datasets muy grandes, pero en este caso se puede hacer a ojo ya que el dataset lo permite.

data2 <- data
jugadores_comunio[!jugadores_comunio$Nombre %in% data$Nombre.comunio,]

data2$Nombre.comunio[10] = "Diego Rolan"
data2$Nombre.comunio[16] = "Ximo Navarro"
data2$Nombre.comunio[20] = "Brasanac"
data2$Nombre.comunio[23] = "Jony Rodríguez"
data2$Nombre.comunio[34] = "Iñigo Martínez"
data2$Nombre.comunio[35] = "Yeray Álvarez"
data2$Nombre.comunio[71] = "Saúl Ñíguez"
data2$Nombre.comunio[72] = "Kalinic"
data2$Nombre.comunio[77] = "Rodrigo Hernández"
data2$Nombre.comunio[78] = "Savic"
data2$Nombre.comunio[81] = "Juanfran Torres"
data2$Nombre.comunio[111] = "Ter Stegen"
data2$Nombre.comunio[112] = "Nelson Semedo"
data2$Nombre.comunio[114] = "Rakitic"
data2$Nombre.comunio[131] = "Aleñá"
data2$Nombre.comunio[165] = "Junior Firpo"
data2$Nombre.comunio[178] = "Emerson de Souza"
data2$Nombre.comunio[187] = "Maxi Gómez"
data2$Nombre.comunio[205] = "Dmitrovic"
data2$Nombre.comunio[218] = "José Ángel Cote"
data2$Nombre.comunio[220] = "Kike García"
data2$Nombre.comunio[226] = "Joan Jordan"
data2$Nombre.comunio[231] = "Roberto Jiménez"
data2$Nombre.comunio[233] = "Víctor Sánchez"
data2$Nombre.comunio[242] = "Dídac Vilà"
data2$Nombre.comunio[246] = "Javier López"
data2$Nombre.comunio[248] = "Álex López Moreno"
data2$Nombre.comunio[256] = "Lluis López"
data2$Nombre.comunio[264] = "Bruno González"
data2$Nombre.comunio[269] = "Ángel Rodríguez"
data2$Nombre.comunio[278] = "Maksimovic"
data2$Nombre.comunio[284] = "Miguel Ángel Rubio"
data2$Nombre.comunio[295] = "Aday Benítez"
data2$Nombre.comunio[297] = "Bono"
data2$Nombre.comunio[301] = "Choco Lozano"
data2$Nombre.comunio[318] = "Adrián Diéguez"
data2$Nombre.comunio[323] = "Cucho Hernández"
data2$Nombre.comunio[324] = "Juanjo Camacho"
data2$Nombre.comunio[352] = "A.Szymanowski"
data2$Nombre.comunio[358] = "José Arnaiz"
data2$Nombre.comunio[369] = "Óscar Rodríguez"
data2$Nombre.comunio[378] = "Koke Vegas"
data2$Nombre.comunio[381] = "Toño García"
data2$Nombre.comunio[382] = "Rober Pier"
data2$Nombre.comunio[405] = "Alberto García"
data2$Nombre.comunio[428] = "Franco di Santo"
data2$Nombre.comunio[449] = "Mariano Díaz"
data2$Nombre.comunio[452] = "Modric"
data2$Nombre.comunio[456] = "Fede Valverde"
data2$Nombre.comunio[481] = "Zaldua"
data2$Nombre.comunio[486] = "Juanmi Jiménez"
data2$Nombre.comunio[491] = "Moyà"
data2$Nombre.comunio[497] = "Theo Hernández"
data2$Nombre.comunio[498] = "Kevin Rodrigues"
data2$Nombre.comunio[513] = "Kjaer"
data2$Nombre.comunio[549] = "Jaume Doménech"
data2$Nombre.comunio[562] = "Kang-In Lee"
data2$Nombre.comunio[579] = "Joaquín Fernández"
data2$Nombre.comunio[581] = "Kiko Olivas"
data2$Nombre.comunio[580] = "Moi Delgado"
data2$Nombre.comunio[595] = "Cop"
data2$Nombre.comunio[596] = "Michel Herrero"
data2$Nombre.comunio[610] = "Mario Gaspar"
data2$Nombre.comunio[614] = "Víctor Ruiz"
data2$Nombre.comunio[619] = "Jaume Costa"
data2$Nombre.comunio[627] = "Bruno Soriano"
data2$Nombre.comunio[632] = "Samu Chukwueze"

jugadores_comunio[!jugadores_comunio$Nombre %in% data2$Nombre.comunio,]

#Miramos los nombres.comunio duplicados, ya que distintos jugadores cogen el mismo nombre.comunio
#Los NA los trataremos luego.

data2 <- data2 %>% mutate(id=row_number())

lista_duplicados <- count(data2[!is.na(data2$Nombre.comunio),], Nombre.comunio) %>% filter(n>1)

data2 %>% filter(Nombre.comunio %in% lista_duplicados$Nombre.comunio) %>% arrange(Nombre.comunio)

#Eliminamos los duplicados y nombre erróneos del data

data3 <- data2[-c(4, 54, 74, 90, 104, 134, 237, 356, 380),]

rownames(data3) <- 1:nrow(data3)

lista_duplicados <- count(data3[!is.na(data4$Nombre.comunio),], Nombre.comunio) %>% filter(n>1)

#Volvemos a mirar los duplicados, aunque en esta ocasión será de jugadores que sí que tienen el nombre correcto.

data3<- data3 %>% mutate(id=row_number())

data3 %>% filter(Nombre.comunio %in% lista_duplicados$Nombre.comunio) %>% arrange(Nombre.comunio)

#Modificamos manualmente esos nombres.

data3$Nombre.comunio[17] = "Borja Bastón"
data3$Nombre.comunio[291] = "Raúl García Carnero"
data3$Nombre.comunio[204] = "Sergio Álvarez Díaz"
data3$Nombre.comunio[287] = "Borja García"

#Ahora hay que modificar aquellos jugadores que tienen el nombre.comunio = NA y en el nombre.plantilla tienen nombre pero son jugadores que aparecen en el data original.

lista_jugadores <- jugadores_liga_santander[jugadores_liga_santander$Nombre %in% data3$Nombre.comunio==FALSE, ] %>% select(Nombre) 

data4 <- data3

data4$Nombre.comunio[c(47,524,306, 301, 30)] <- c("De Marcos","Gnagnon","Seung-Ho Paik", "Kevin Soni","Alex Remiro")

#Eliminamos los NAs.

Jugadores <- data4[complete.cases(data4), ]

Jugadores <- Jugadores[,1:2]

Jugadores2<-left_join(Jugadores, jugadores_liga_santander, by=c(Nombre.plantilla="Nombre"))

#Este es el dataframe final cruzando los datos de los jugadores del comunio y de las plantillas obtenidas.                  

Jugadores_final <- left_join(Jugadores2, jugadores_comunio, by=c(Nombre.comunio="Nombre"))


