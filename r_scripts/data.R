library(dplyr)
library(tidyr)
library(readr)
library(stringr)

CANT_MESAS <- 8653

mesas <- read_csv("resultados-test.csv")
colnames(mesas) <- str_replace_all(tolower(colnames(mesas))," ", "_")

resultados <- mesas %>%
  select(1:20) %>% 
  gather(key="nombre_lista",value = "votos", 7:20) %>% 
  mutate(lista = stringr::word(nombre_lista, -1))

posiciones <- resultados %>%
  group_by(nombre_lista) %>% 
  summarise(total_votos = sum(votos)) %>%
  arrange(desc(total_votos)) 

############# Circuitos
# url_circuitos <- "https://docs.google.com/feeds/download/spreadsheets/Export?key=1Afrsxj_eJwk4-eF8A3I9uEJG57i5-twdLBDVI15I_RU&exportFormat=csv&gid=0"
# column_types = cols(LONGITUD = col_double(), LATITUD = col_double())
# circuitos_raw <- read_csv(url_circuitos, locale = locale(decimal_mark = ","), col_types = column_types)
# circuitos_geo <- circuitos_raw %>% 
#   select(circuito_id = circuito, circuito_nombre = Circuito, lng = LONGITUD, lat = LATITUD)
# write_csv(circuitos_geo, "circuitos_geo.csv")

circuitos_geo <- read_csv("circuitos_geo.csv")

circuitos_resultados <- resultados %>% 
  left_join(circuitos_geo, by=c("codigo_circuito" = "circuito_id")) %>%
  group_by(codigo_circuito, lng, lat) %>%
  summarise(votos = sum(votos))
