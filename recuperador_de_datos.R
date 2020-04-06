# Dependencias 

if(!require("pacman")) install.packages("pacman")
library(tidyverse)
library(lubridate)

####### llamado a data por cantones

tem_cr_caso_pc <- read.csv("datos/covid19_cantones_cr.csv")

####### limpia fechas

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


######### almacena datos

saveRDS(cr_caso_limpio, file = "datos/cr_caso_limpio.RDS")
saveRDS(cr_caso_provincia, file = "datos/cr_caso_provincia.RDS")

######## carga datos generales

temp_casos_general <- read.csv("datos/covid19_general_cr.csv")

####### modificacion datos generales

temp_casos_general <- temp_casos_general %>%
  mutate(anterior = lag(Confirmados),
         anterior = if_else(is.na(anterior),
                            0,
                            as.numeric(anterior)),
         casos_activos = Confirmados - Fallecidos - Recuperados,
         casos_nuevos = Confirmados - anterior,
         periodo_de_duplicacion = round(casos_activos/casos_nuevos, 0),
         periodo_de_duplicacion = if_else(is.infinite(periodo_de_duplicacion),
                                          0,
                                          periodo_de_duplicacion),
         casosdia = casos_nuevos + 1,
         logcasos = log(casosdia),
         dias = 1:nrow(temp_casos_general))

temp_casos_general$Fecha <- as.Date(as.character(temp_casos_general$Fecha), 
                                    format = "%d/%m/%Y")

names(temp_casos_general)[names(temp_casos_general) == "casos_nuevos"] <- "Casos"

######### almacena datos generales

saveRDS(temp_casos_general, file = "datos/casos_general.RDS")

######### seccion de modelo loglinear

estimacion<-function(x0,b,t){
  return(x0*(b^t))
}

######## crear modelo

modelo_log <- lm(logcasos~dias,data = temp_casos_general)

######## transfromar variables

x0 <- exp(modelo_log$coefficients[[1]])
b <- exp(modelo_log$coefficients[[2]])


######## datos ajustados

temp_casos_general <- temp_casos_general %>%
  mutate(ajuste = estimacion(x0 = x0,
                             b = b,
                             t = (1:length(casosdia))),
         casosdia = casosdia - 1)

names(temp_casos_general)[names(temp_casos_general) %in% c("casosdia","ajuste")] <- c("Casos Reales", "Estimado")

prediccion <- round(estimacion(x0 = x0,
                               b = b,
                               t = (nrow(temp_casos_general):(nrow(temp_casos_general) + 6))),0)

prediccion <- data.frame(
  "Casos_estimados"= prediccion,
  time = (nrow(temp_casos_general):(nrow(temp_casos_general) + 6))
)

prediccion <- prediccion %>%
  mutate(
    Fecha = temp_casos_general[1,"Fecha"] + days(time - 1)
  ) %>%
  select(
    Fecha, Casos_estimados
  )

colnames(prediccion)<-c("Fecha","Casos Estimados")

prediccion$Fecha <- as.character(prediccion$Fecha)

######## almacena la tabla para el output

saveRDS(prediccion, file = "datos/prediccion.RDS")

ajuste_prediccion <- data.frame(
  time = (1:(nrow(temp_casos_general) + 6)),
  casos = round(estimacion(x0 = x0,b = b,t = (1:(nrow(temp_casos_general) + 6))))
)

colnames(ajuste_prediccion) <- c("time","Estimado")

###### agregar fechas

ajuste_prediccion <- ajuste_prediccion %>%
  mutate(Fecha = temp_casos_general[1,"Fecha"] + days(time - 1))

ajuste_prediccion <- ajuste_prediccion %>%
  e_charts(Fecha) %>%
  e_line(Estimado) %>%
  e_title("Modelo de crecimiento exponencial","casos") %>%
  e_tooltip() %>%
  e_data(temp_casos_general) %>%
  e_scatter(Casos) %>%
  e_x_axis(name = "Fecha")

######### almacena el grafico para el output

saveRDS(ajuste_prediccion, file = "datos/ajuste_prediccion.RDS")

######## seccion mapa

json <- jsonlite::read_json("mapas-json/provincias.geojson")

json$features <- json$features %>% 
  map(function(x){ 
    x$properties$name <- x$properties$NPROVINCIA 
    return(x)})

prov_map <- cr_caso_provincia %>%
  group_by(fecha) %>%
  e_charts(provincia, timeline = TRUE) %>%
  e_map_register("cr_provincia", json) %>%
  e_map(total, map = "cr_provincia", name = "Confirmados") %>%
  e_visual_map(min = 0, 
               max = max(cr_caso_provincia$total),
               inRange = list(color = c('yellow','orange', 'orangered', 'red')),
               show = FALSE) %>%
  e_tooltip() %>%
  e_timeline_opts(axis_type = "category",
                  playInterval = 1000)

saveRDS(prov_map, file = "datos/mapa_provincia.RDS")

