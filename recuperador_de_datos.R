# Dependencias 

if(!require("pacman")) install.packages("pacman")
library(tidyverse)
library(lubridate)
library(readxl)

tem_cr_caso_pc <- read.csv("datos/covid19_cantones_cr.csv")
tem_cr_caso_pc %>% View()

# limipia fechas 
cr_caso_limpio <- tem_cr_caso_pc %>% 
  pivot_longer(-c(provincia, canton),
               names_to = "fecha", values_to = "total") %>% 
  filter(canton != "DESCONOCIDO") %>% 
  filter(!is.na(total)) %>% 
  mutate(fecha = str_replace_all(fecha,"\\.", "-"),
         fecha = str_remove(fecha,"X"),
         fecha = as.Date(fecha, format = "%d-%m-%Y"))

cr_caso_provincia <- cr_caso_limpio %>% 
  group_by(provincia, fecha) %>% 
  summarize(total = sum(total))

saveRDS(cr_caso_limpio, file = "datos/cr_caso_limpio.RDS")
saveRDS(cr_caso_provincia, file = "datos/cr_caso_provincia.RDS")

temp_casos_general <- read.csv("datos/covid19_general_cr.csv") %>% 
  mutate(casos_anteriores = lag(Confirmados), 
         casos_nuevos = Confirmados - casos_anteriores) %>% 
  mutate(casos_nuevos = if_else(!is.na(casos_nuevos), 
                                as.double(casos_nuevos),
                                1))
saveRDS(temp_casos_general, file = "datos/casos_general.RDS")

