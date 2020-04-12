# Dependencias 

if(!require("pacman")) install.packages("pacman")
library(tidyverse)
library(lubridate)
library(echarts4r)
library(echarts4r.maps)
library(easynls)

####### Cargar/tratar datos -----------

#### llamado a data por cantones

tem_cr_caso_pc <- read.csv("datos/covid19_cantones_cr.csv")

#### limpia fechas

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


#### almacena datos

saveRDS(cr_caso_limpio, file = "datos/cr_caso_limpio.RDS")
saveRDS(cr_caso_provincia, file = "datos/cr_caso_provincia.RDS")

#### carga datos generales

temp_casos_general <- read.csv("datos/covid19_general_cr.csv")

#### modificacion datos generales

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

#### almacena datos generales

saveRDS(temp_casos_general, file = "datos/casos_general.RDS")


######### seccion "general" -----------------
ultima_fila = tail(temp_casos_general,1)
maximo_casos <- max(temp_casos_general$Casos)

#Codigo para obtener infectados por genero
genero <- factor(x = c("Hombres", "Mujeres"))
dfgeneros <- data.frame(Genero = genero, 
                        Infectados = c(ultima_fila$Hombres, ultima_fila$Mujeres)) 

#Codigo para obtener infectados por genero
nacionalidad <- factor(x = c("Extranjeros", "Costarricenses"))
dfnacionalidad <- data.frame(Nacionalidad = nacionalidad, 
                             Infectados = c(ultima_fila$Extranjeros, ultima_fila$Costarricenses)) 

#Codigo para obtener Recuperados y Fallecimientos
dfestado <- data.frame(
  Estado = c("Recuperados", "Fallecidos"),
  Infectados = c(ultima_fila$Recuperados, ultima_fila$Fallecidos)
) 

#Codigo para obtener infectados por grupos etarios
dfedad <- data.frame(
  Grupos = c("Adultos", "Adultos mayores", "Menores"),
  Infectados = c(ultima_fila$Adultos, ultima_fila$Adultos.Mayores, ultima_fila$Menores)
) 

#Grafico comparativo entre infectados por dia e infectados acumulados
graf_infectados <- temp_casos_general %>%
  select(Fecha, Confirmados, Casos) %>%
  `colnames<-`(c("Fecha", "Acumulados", "Diarios")) %>%
  e_charts(Fecha) %>%
  e_line(Acumulados) %>% 
  e_area(Diarios) %>%
  e_tooltip(
    axisPointer = list(
      type = "cross"
    )
  ) %>%
  e_mark_point("Acumulados", data = list(type = "max")) %>%
  e_mark_point("Diarios", data = list(type = "max")) %>%
  e_legend(right = 0) %>%
  e_title("Infectados", top = 0) %>% 
  e_x_axis(name = "Fecha", nameLocation = "center", nameGap = 40) %>%
  e_y_axis(name = "Cantidad") %>%
  e_text_style(fontSize = 12)

saveRDS(graf_infectados, file = "datos/graf_infectados.RDS")

#Grafico cantidad descartados
graf_descartados <- temp_casos_general %>%
  e_charts(Fecha) %>% 
  e_line(Descartados) %>%
  e_tooltip(
    axisPointer = list(
      type = "cross"
    )
  ) %>%
  e_mark_point("Descartados", data = list(type = "max")) %>%
  e_legend(right = 0) %>%
  e_title("Descartados") %>% 
  e_x_axis(name = "Fecha", nameLocation = "center", nameGap = 40) %>%
  e_y_axis(name = "Cantidad") %>%
  e_text_style(fontSize = 12)

saveRDS(graf_descartados, file = "datos/graf_descartados.RDS")

#Mapa de calor: cantidad de infecciones por dia
graf_calendario <- temp_casos_general %>% 
  e_charts(Fecha) %>% 
  e_calendar(range = c(temp_casos_general[1,1], temp_casos_general[nrow(temp_casos_general), 1]), 
             dayLabel = list(nameMap = c("D", "L", "K", "M", "J", "V", "S")), 
             monthLabel = list(nameMap = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")),
             left = "25%",
             width = "50%",
             yearLabel = list(position = "right")) %>% 
  e_heatmap(Casos, coord_system = "calendar") %>% 
  e_visual_map(max = maximo_casos, top = 60)  %>% 
  e_title("Calendario: nuevos casos por día") %>%
  e_tooltip(formatter = htmlwidgets::JS("
              function(params){
                return('Fecha: ' + params.value[0] + 
                '</strong><br />Infectados: ' + params.value[1]) 
                        }
                  "))

saveRDS(graf_calendario, file = "datos/graf_calendario.RDS")

#Grafico top 10 cantones
cr_caso_limpio$canton <- as.character(cr_caso_limpio$canton)

graf_top10 <- cr_caso_limpio %>%
  group_by(canton) %>%
  summarize(Casos = max(total))  %>%
  arrange(desc(Casos)) %>%
  head(n = 10) %>%
  arrange(Casos) %>%
  e_charts(canton) %>%
  e_bar(Casos) %>%
  e_tooltip(trigger = "item") %>%
  e_flip_coords() %>%
  e_legend(right = 0) %>%
  e_title("Top 10 de casos por cantones") 

saveRDS(graf_top10, file = "datos/graf_top10.RDS")

#Grafico cantidad recuperados y fallecidos
graf_estados <- dfestado %>% 
  e_charts(Estado) %>% 
  e_bar(Infectados) %>% 
  e_title("Recuperados y fallecidos") %>%
  e_legend(right = 0) %>%
  e_flip_coords() %>%
  e_tooltip()# flip axis

saveRDS(graf_estados, file = "datos/graf_estados.RDS")

#Grafico comparativo entre infectados por genero
graf_genero <- dfgeneros %>% 
  e_charts(Genero) %>% 
  e_pie(Infectados, radius = c("50%", "70%")) %>% 
  e_title("Infectados según género") %>%
  e_tooltip(axisPointer = list(type = "cross"))

saveRDS(graf_genero, file = "datos/graf_genero.RDS")

#Grafico comparativo entre infectados por nacionalidad
graf_nacionalidad <- dfnacionalidad %>% 
  e_charts(Nacionalidad) %>% 
  e_pie(Infectados, radius = c("50%", "70%")) %>% 
  e_title("Infectados según nacionalidad") %>%
  e_tooltip(axisPointer = list(type = "cross")) 

saveRDS(graf_nacionalidad, file = "datos/graf_nacionalidad.RDS")

#Grafico comparativo infectados adultos, adultos mayores y menores
graf_edades <- dfedad  %>% 
  e_charts(Grupos) %>% 
  e_pie(Infectados, radius = c("50%", "70%")) %>% 
  e_title("Infectados según grupo etario") %>%
  e_tooltip(axisPointer = list(type = "cross"))

saveRDS(graf_edades, file = "datos/graf_edades.RDS")


######### seccion de modelo loglinear -----------------

estimacion<-function(x0,b,t){
  return(x0*(b^t))
}

#### crear modelo

modelo_log <- lm(logcasos~dias,data = temp_casos_general)

#### transfromar variables

x0 <- exp(modelo_log$coefficients[[1]])
b <- exp(modelo_log$coefficients[[2]])


#### datos ajustados

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

colnames(prediccion)<-c("Fecha","Casos estimados")

prediccion$Fecha <- as.character(prediccion$Fecha)

#### almacena la tabla para el output

saveRDS(prediccion, file = "datos/prediccion.RDS")

ajuste_prediccion <- data.frame(
  time = (1:(nrow(temp_casos_general) + 6)),
  casos = round(estimacion(x0 = x0,b = b,t = (1:(nrow(temp_casos_general) + 6))))
)

colnames(ajuste_prediccion) <- c("time","Estimado")

#### agregar fechas

ajuste_prediccion <- ajuste_prediccion %>%
  mutate(Fecha = temp_casos_general[1,"Fecha"] + days(time - 1))

ajuste_prediccion <- ajuste_prediccion %>%
  e_charts(Fecha) %>%
  e_line(Estimado) %>%
  e_tooltip() %>%
  e_data(temp_casos_general) %>%
  e_scatter(Casos) %>%
  e_x_axis(name = "Fecha", nameLocation = "center", nameGap = 40) %>%
  e_y_axis(name = "Casos") 

#### almacena el grafico para el output

saveRDS(ajuste_prediccion, file = "datos/ajuste_prediccion.RDS")


######### seccion de modelo gompertz -----------------

# Definir de funciones ---------------------------

gompertz_mod = function(params, t) {
  
  (params[1] * exp(-params[2] * exp(-params[3] * (t - 1))))
}

cuando_acaba <- function(params, t, ultima_fecha) {
  
  i = 0
  
  while (TRUE) {
    
    model_max <- gompertz_mod(param, t)
    finales   <- tail(model_max,2)
    ultimo    <- round(finales[2])
    penultimo <- round(finales[1])
    
    if (ultimo == penultimo) {
      break()
    }
    
    t <- 1:(length(t) + 1)
    i = i + 1
  }
  
  return(ultima_fecha + i)
  
}

# Cargar y preparar datos ---------------------------

df <- temp_casos_general %>%
  select(Confirmados) %>%
  tibble::rownames_to_column("Dia") %>%
  mutate(Dia = lag(Dia),
         Dia = if_else(is.na(Dia),
                       0,
                       as.numeric(Dia)
         )
  )

tiempo <- 1:(length(df$Dia) + 6)

ultima_fecha <- tail(temp_casos_general$Fecha, 1)

# Ajustar modelo ---------------------------

modelo <- nlsfit(df, 
                 model = 10,
                 start = c(a = 761.049823156006, 
                           b = 6.63469882635722, 
                           c = 0.08376468439801
                 )
)

param <- modelo$Parameters[1:3,1]

# Obtener salidas ---------------------------

modelado <- gompertz_mod(param, tiempo)

predicciones <- data.frame(seq.Date(ultima_fecha,
                                    by = "day",
                                    length.out = 7),
                           round(tail(modelado, 7), 1))

colnames(predicciones) <- c("Fecha", "Casos acum. estimados")

InfoExtra <- data.frame(cuando_acaba(param, tiempo, ultima_fecha),
                        modelo$Parameters[7,1])

colnames(InfoExtra) <- c("Fecha final de la epidemia", "Coef. de Determinacion (R-cuadrado)")

# Graficar datos ---------------------------

modelo_gompertz <- temp_casos_general %>%
  select(Fecha, Confirmados) %>%
  add_row(Fecha = seq.Date(ultima_fecha + 1, 
                           length.out = 6, 
                           by = "day") ) %>%
  mutate(Estimados = modelado) %>%
  e_charts(Fecha) %>%
  e_scatter(Confirmados, symbol_size = 7) %>%
  e_line(Estimados) %>%
  e_legend(right = 0) %>%
  e_tooltip(trigger = "axis") %>%
  e_title("Modelo de Gompertz", "Casos totales")

saveRDS(modelo_gompertz, file = "datos/modelo_gompertz.RDS")

######## seccion mapa -----------------

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
               show = TRUE) %>%
  e_tooltip() %>%
  e_timeline_opts(axis_type = "category",
                  playInterval = 1000, currentIndex = length(unique(cr_caso_provincia$fecha))-1)

saveRDS(prov_map, file = "datos/mapa_provincia.RDS")

