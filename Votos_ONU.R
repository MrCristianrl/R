#Dataset obtenido de la web de la ONU.

library(dplyr)
library(tidyr)
library(countrycode)
library(ggplot2)
library(purrr)
library(broom)

votes <- readRDS("~/R/Votes/votes.rds")
head(votes, 10)
glimpse(votes)

#La columna vote viene dada por los números 1,2,3,8, 9, que significan:
#1 = Yes
#2 = Abstain
#3 = No
#8 = Not present
#9 = Not a member

#Borramos las líneas que no nos interesan, correspondientes a los dígitos 8 y 9 que son países que no se presentaron o no eran miembros.

votes <- votes %>% 
  filter(vote<=3)

#Dado que se comenzó a votar en 1945 en UN, la columna session se refiere al año de cada voto, por lo tanto, para hacer más claro el dataset, crearemos una columna nueva con el año correspondiente de la votación.

votes <- votes %>% mutate(year=1945+session)

#Creamos una columna con el nombre de cada país dada por la columna ccode

votes_processed <- votes %>%
    mutate(year = session + 1945) %>%
      mutate(year = session + 1945, country=countrycode(ccode, "cown", "country.name"))

#Calculamos el porcentajes de votos que fueron sí para cada país.

votes_processed %>% summarize(total=n(), percent_yes= mean(vote==1))

#Agrupamos por años para ver el número de síes.

by_year <- votes_processed %>% group_by(year) %>% summarize(total=n(), percent_yes= mean(vote==1))

#Análogamente, por país.

by_country <- votes_processed %>% group_by(country) %>% summarize(total=n(), percent_yes= mean(vote==1))

#Ordenamos el dataframe anterior mediante la columna yes_percent, de manera ascendente y descendente.

arrange(by_country, percent_yes)
arrange(by_country, desc(percent_yes))

#Elminamos países con menos de 50 votos, ya que por ejemplo Zanzibar tiene 2 y no va a significar nada importante en el estudio.

by_country %>% arrange(percent_yes) %>% filter(total>100)

#Gráfica de la evolución del porcentaje de votos síes a lo largo de los años.

ggplot(by_year, aes(x=year, y=percent_yes)) + geom_line()

#Agrupamos por año y país para analizar la evolución del porcentaje de síes en España.

by_year_country <- votes_processed %>% group_by(year, country) %>% summarize(total = n(), percent_yes = mean(vote == 1))

spain_by_year <- by_year_country %>% filter(country=="Spain")

ggplot(spain_by_year, aes(x=year, y=percent_yes)) + geom_line()

#Comparación entre varios países gráficamente.

v <- c("United States","United Kingdom","Spain", "India")

myf <- function(v){
  by_year_country %>% filter(country %in% v) %>% ggplot(aes(x=year, y=percent_yes, col=country)) + geom_line() + stat_smooth(method="lm", se=FALSE) + facet_wrap(~country, scale="free_y")
}

#Veamos si existe una relación entre los votos síes en España a lo largo de los años.

Spain_by_year <- by_4_paises %>% filter(country=="Spain")

model <- lm(percent_yes~year, Spain_by_year)

ggplot(Spain_by_year, aes(x=year, y=percent_yes)) + geom_line() + geom_smooth(method='lm')

#Existe una tendencia positiva entre ambas variables a lo largo de los años. Veamos cómo podemos observar esta tendencia en todos los países.

#Creamos un dataframe con los coeficientes y modelos de la regresión lineal para cada país.

coeficientes_paises <- ungroup(by_year_country) %>% 
  nest(-country) %>% 
    mutate(model=map(data, ~lm(percent_yes~year, data=.))) %>%
      mutate(tidied=map(model, tidy)) %>%
        unnest(tidied)

#Tomamos las filas que nos interesan, con el valor de la pendiente y ajustamos su p-valor, debido a que como hay tantos, pueden darse casos de falsos positivos.

coef_pendientes <- coeficientes_paises %>% filter(term=="year")

coef_pendientes_ajustados <- coef_pendientes %>% mutate(p.adjusted= p.adjust(p.value))

#Ahora tomamos aquellas filas que tengan un p-valor ajustado menor que el 0.05.

coef_paises_final <- coef_pendientes_ajustados %>% filter(p.adjusted<.05)

#Eliminamos la variable NA que se ha creado.

coef_paises_final <- coef_paises_final %>% filter(!is.na(country))

#Observamos qué países tienen una tendencia menor

coef_paises_final %>% arrange(estimate)

#Análogamente para ver cuáles tienen una tendencia mayor.

coef_paises_final %>% arrange(desc(estimate))

#Juntamos los dataframes para hacer la tabla final.

tabla_final <- inner_join(by_year_country, coef_paises_final)
