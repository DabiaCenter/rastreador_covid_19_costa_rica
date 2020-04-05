# Dependencias 

if(!require("pacman")) install.packages("pacman")
library(tidyverse)
library(lubridate)

# Los siguientes datos son recuperados de la UNED, 
# "http://geovision.uned.ac.cr/oges/archivos_covid"

# Creando url dinamico 

mes <- Sys.time() %>% 
  lubridate::month()

dia <- Sys.time() %>% 
  lubridate::day() 

# 
agrega_cero <- function(valor) {
  if (valor < 10){
    valor <- str_c("0", as.character(valor))
  }  else{
    valor <- as.character(valor)
  }
}

mes <- agrega_cero(mes)

#quitar menos 1 cuando se programe para que se corra las 6 PM 
dia <- agrega_cero(dia -1)

# url para datos de hoy 

url_today <- str_c("http://geovision.uned.ac.cr/oges/archivos_covid/", 
                   mes, 
                   "_", 
                   dia, 
                   "/",
                   mes, 
                   "_", 
                   dia,
                   "_CSV.csv")

# descarga lo datos crudos 
temp_cr_caso <- read.csv(url_today) %>% 
  select(-c("cod_provin","cod_canton")) 

# limpia nombres de provincias y cantones 
tem_cr_caso_pc <- temp_cr_caso %>% 
  mutate(provincia = case_when(
    str_detect(provincia, pattern = "^San Jos.$") ~ "SAN JOSE",
    str_detect(provincia, pattern = "^Lim.n$") ~ "LIMON",
               TRUE ~ toupper(as.character(provincia)))) %>% 
  mutate(canton = case_when(
    str_detect(canton, pattern = "^San Jos.$") ~ "SAN JOSE",
    str_detect(canton, pattern = "^Aserr.$") ~ "ASERRI",
    str_detect(canton, pattern = "^Bel.n$") ~ "BELEN",
    str_detect(canton, pattern = "^Ca.as$") ~ "CANAS",
    str_detect(canton, pattern = "^Gu.cimo$") ~ "GUACIMO",
    str_detect(canton, pattern = "^Jim.nez$") ~ "JIMENEZ",
    str_detect(canton, pattern = "^La Uni.n$") ~ "LA UNION",
    str_detect(canton, pattern = "^Le.*Castro$") ~ "LEON CORTES",
    str_detect(canton, pattern = "^Lim.n$") ~ "LIMON",
    str_detect(canton, pattern = "^Par.*so$") ~ "PARAISO",
    str_detect(canton, pattern = "^Esca.*$") ~ "ESCAZU",
    str_detect(canton, pattern = "^Po.*s$") ~ "POAS",
    str_detect(canton, pattern = "^Pococ.$") ~ "POCOCI",
    str_detect(canton, pattern = "^P.rez.*$") ~ "PEREZ ZELEDON",
    str_detect(canton, pattern = "^R.o.*Cuarto$") ~ "RIO CUARTO",
    str_detect(canton, pattern = "^San Ram.*n$") ~ "SAN RAMON", 
    str_detect(canton, pattern = "^Sarapi.*$") ~ "SARAPIQUI", 
    str_detect(canton, pattern = "^Sarch.*$") ~ "SARCHI", 
    str_detect(canton, pattern = "^Tarraz.*$") ~ "TARRAZU", 
    str_detect(canton, pattern = "^Tib.*$") ~ "TIBAS",
    str_detect(canton, pattern = "^Tila.*n$") ~ "TILARAN",
    str_detect(canton, pattern = "V.*nado$") ~ "VAZQUEZ DE CORONADO",
    TRUE ~ toupper(as.character(canton)))) %>% 
  pivot_longer(-c(provincia, canton),
             names_to = "fecha", values_to = "Total") %>% 
  filter(canton != "DESCONOCIDO") %>% 
  filter(!is.na(Total)) 

# limipia fechas 
cr_caso_limpio <- tem_cr_caso_pc %>% 
  mutate(fecha = str_replace_all(fecha,"\\.", "-"),
         fecha = str_remove(fecha,"X"),
         fecha = as.Date(fecha, format = "%d-%m-%Y")) %>% 
  rename(total = Total)

cr_caso_provincia <- cr_caso_limpio %>% 
  group_by(provincia, fecha) %>% 
  summarize(total = sum(total))

saveRDS(cr_caso_limpio, file = "datos/cr_caso_limpio.RDS")
saveRDS(cr_caso_provincia, file = "datos/cr_caso_provincia.RDS")





