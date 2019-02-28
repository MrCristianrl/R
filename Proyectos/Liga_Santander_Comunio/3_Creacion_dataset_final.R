##EN ESTE ARCHIVO SE MODIFICAN LOS NOMBRES DE ALGUNAS FILAS QUE APARECÍAN COMO NA Y AL CRUZAR LOS NOMBRES DE LAS TABLAS DE AQUELLOS JUGADORES CON EL MISMO NOMBRE PROVOCABAN UN ERROR.

#Sustituímos los nombres de algunos jugadores que no coinciden en ambos datasets, pero sin embargo están en ambos. Esto es debido a que muchos son nombres compuestos o tienen algún caracter especial. Los cambiamos a mano después de buscar las filas cuyo valor en la columna Nombre.comunio es NA.
#No es el mejor método si lo pensamos en datasets muy grandes, pero en este caso se puede hacer a ojo ya que el dataset lo permite.

data3 <- data2
data_j[!data_j$Nombre %in% data3$Nombre.comunio,]

data3$Nombre.comunio[10] = "Diego Rolan"
data3$Nombre.comunio[16] = "Ximo Navarro"
data3$Nombre.comunio[20] = "Brasanac"
data3$Nombre.comunio[23] = "Jony Rodríguez"
data3$Nombre.comunio[34] = "Iñigo Martínez"
data3$Nombre.comunio[35] = "Yeray Álvarez"
data3$Nombre.comunio[71] = "Saúl Ñíguez"
data3$Nombre.comunio[72] = "Kalinic"
data3$Nombre.comunio[77] = "Rodrigo Hernández"
data3$Nombre.comunio[78] = "Savic"
data3$Nombre.comunio[81] = "Juanfran Torres"
data3$Nombre.comunio[111] = "Ter Stegen"
data3$Nombre.comunio[112] = "Nelson Semedo"
data3$Nombre.comunio[114] = "Rakitic"
data3$Nombre.comunio[131] = "Aleñá"
data3$Nombre.comunio[165] = "Junior Firpo"
data3$Nombre.comunio[178] = "Emerson de Souza"
data3$Nombre.comunio[187] = "Maxi Gómez"
data3$Nombre.comunio[205] = "Dmitrovic"
data3$Nombre.comunio[218] = "José Ángel Cote"
data3$Nombre.comunio[220] = "Kike García"
data3$Nombre.comunio[226] = "Joan Jordan"
data3$Nombre.comunio[231] = "Roberto Jiménez"
data3$Nombre.comunio[233] = "Víctor Sánchez"
data3$Nombre.comunio[242] = "Dídac Vilà"
data3$Nombre.comunio[246] = "Javier López"
data3$Nombre.comunio[248] = "Álex López Moreno"
data3$Nombre.comunio[256] = "Lluis López"
data3$Nombre.comunio[264] = "Bruno González"
data3$Nombre.comunio[269] = "Ángel Rodríguez"
data3$Nombre.comunio[278] = "Maksimovic"
data3$Nombre.comunio[284] = "Miguel Ángel Rubio"
data3$Nombre.comunio[295] = "Aday Benítez"
data3$Nombre.comunio[297] = "Bono"
data3$Nombre.comunio[301] = "Choco Lozano"
data3$Nombre.comunio[318] = "Adrián Diéguez"
data3$Nombre.comunio[323] = "Cucho Hernández"
data3$Nombre.comunio[324] = "Juanjo Camacho"
data3$Nombre.comunio[352] = "A.Szymanowski"
data3$Nombre.comunio[358] = "José Arnaiz"
data3$Nombre.comunio[369] = "Óscar Rodríguez"
data3$Nombre.comunio[378] = "Koke Vegas"
data3$Nombre.comunio[381] = "Toño García"
data3$Nombre.comunio[382] = "Rober Pier"
data3$Nombre.comunio[405] = "Alberto García"
data3$Nombre.comunio[428] = "Franco di Santo"
data3$Nombre.comunio[449] = "Mariano Díaz"
data3$Nombre.comunio[452] = "Modric"
data3$Nombre.comunio[456] = "Fede Valverde"
data3$Nombre.comunio[481] = "Zaldua"
data3$Nombre.comunio[486] = "Juanmi Jiménez"
data3$Nombre.comunio[491] = "Moyà"
data3$Nombre.comunio[497] = "Theo Hernández"
data3$Nombre.comunio[498] = "Kevin Rodrigues"
data3$Nombre.comunio[513] = "Kjaer"
data3$Nombre.comunio[549] = "Jaume Doménech"
data3$Nombre.comunio[562] = "Kang-In Lee"
data3$Nombre.comunio[579] = "Joaquín Fernández"
data3$Nombre.comunio[581] = "Kiko Olivas"
data3$Nombre.comunio[580] = "Moi Delgado"
data3$Nombre.comunio[595] = "Cop"
data3$Nombre.comunio[596] = "Michel Herrero"
data3$Nombre.comunio[610] = "Mario Gaspar"
data3$Nombre.comunio[614] = "Víctor Ruiz"
data3$Nombre.comunio[619] = "Jaume Costa"
data3$Nombre.comunio[627] = "Bruno Soriano"
data3$Nombre.comunio[632] = "Samu Chukwueze"

#Miramos los nombres.comunio duplicados, ya que distintos jugadores cogen el mismo nombre.comunio
#Los NA los trataremos luego.

data3 <- data3 %>% mutate(id=row_number())

lista_duplicados <- count(data3[!is.na(data3$Nombre.comunio),], Nombre.comunio) %>% filter(n>1)

data3 %>% filter(Nombre.comunio %in% lista_duplicados$Nombre.comunio) %>% arrange(Nombre.comunio)

data4 <- data3[-c(4, 54, 74, 90, 104, 134, 237, 356, 380),]

rownames(data4) <- 1:nrow(data4)

lista_duplicados <- count(data4[!is.na(data4$Nombre.comunio),], Nombre.comunio) %>% filter(n>1)

data4 <- data4 %>% mutate(id=row_number())

data4 %>% filter(Nombre.comunio %in% lista_duplicados$Nombre.comunio) %>% arrange(Nombre.comunio)

data4$Nombre.comunio[17] = "Borja Bastón"
data4$Nombre.comunio[291] = "Raúl García Carnero"
data4$Nombre.comunio[204] = "Sergio Álvarez Díaz"
data4$Nombre.comunio[287] = "Borja García"

#Ahora hay que modificar aquellos jugadores que tienen el nombre.comunio = NA y el nombre.plantilla no pero son jugadores que aparecen en el data_j original.

lista_jugadores <- data_j[data_j$Nombre %in% data4$Nombre.comunio==FALSE, ] %>% select(Nombre) 

data5 <- data4

data5$Nombre.comunio[c(47,524,306, 301, 30)] <- c("De Marcos","Gnagnon","Seung-Ho Paik", "Kevin Soni","Alex Remiro")

Jugadores <- data5[complete.cases(data5), ]

Jugadores <- Jugadores[,1:2]

Jugadores2<-left_join(Jugadores, data_j, by=c(Nombre.comunio="Nombre"))

#Este es el dataframe final cruzando los datos de los jugadores del comunio y de las plantillas obtenidas.                  


Jugadores_final <- left_join(Jugadores2, data, by=c(Nombre.plantilla="Nombre"))
