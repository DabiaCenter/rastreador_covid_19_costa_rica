# Dependencias 

if(!require("pacman")) install.packages("pacman")
library(tidyverse)
library(lubridate)
library(echarts4r)
library(echarts4r.maps)
library(easynls)
library(reticulate)

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
         dias = 1:nrow(temp_casos_general),
         descartados_anterior = lag(Descartados),
         descartados_anterior = if_else(is.na(descartados_anterior),
                                        0,
                                        as.numeric(descartados_anterior)),
         descartados_por_dia = Descartados - descartados_anterior
         )

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

#Codigo para obtener infectados por nacionalidad
nacionalidad <- factor(x = c("Extranjeros", "Costarricenses"))
dfnacionalidad <- data.frame(Nacionalidad = nacionalidad, 
                             Infectados = c(ultima_fila$Extranjeros, ultima_fila$Costarricenses)) 

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
  e_line(Descartados, name = "Acumulados") %>%
  e_area(descartados_por_dia, name = "Diarios") %>%
  e_tooltip(
    axisPointer = list(
      type = "cross"
    )
  ) %>%
  e_mark_point("Acumulados", data = list(type = "max")) %>%
  e_mark_point("Diarios", data = list(type = "max")) %>%
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

#Tabla top 10 cantones
cr_caso_limpio$canton <- as.character(cr_caso_limpio$canton)

tabla_top10 <- cr_caso_limpio %>%
  group_by(canton) %>%
  summarize(Casos = max(total),
            first(provincia))  %>%
  arrange(desc(Casos)) %>%
  head(n = 10)  

colnames(tabla_top10) <- c("Canton", "Infectados", "Provincia")

saveRDS(tabla_top10, file = "datos/tabla_top10.RDS")

#Grafico comparativo entre infectados por genero
graf_genero <- dfgeneros %>% 
  e_charts(Genero) %>% 
  e_bar(Infectados) %>% 
  e_title("Infectados según género") %>%
  e_legend(right = 0) %>%
  e_tooltip()

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

######### Grafico hospitalizados 

graf_hosp <- temp_casos_general %>%
  select(Fecha, Hospitalizados, CI) %>%
  `colnames<-`(c("Fecha", "Hospitalizados", "C.Intensivos")) %>%
  filter(Fecha > "2020-03-30") %>%
  e_charts(Fecha) %>%
  e_bar(Hospitalizados) %>% 
  e_bar(C.Intensivos) %>%
  e_tooltip(
    trigger = "axis"
  ) %>%
  e_x_axis(name = "Fecha", nameLocation = "center", nameGap = 40) %>%
  e_y_axis(name = "Cantidad") %>%
  e_text_style(fontSize = 12)

saveRDS(graf_hosp, file = "datos/graf_hosp.RDS")

######### definición de funciones para la sección de modelos -----------------

#Modelo exponencial
estimacion<-function(x0,b,t){
  return(x0*(b^t))
}

#Modelo gompertz
gompertz_mod = function(params, t) {
  
  (params[1] * exp(-params[2] * exp(-params[3] * (t - 1))))
}

#Definir funcion logistica en R
logistic_model<-function(params, x){
  return (params[3]/(1+exp(-(x-params[2])/params[1])))
}

#Función pronóstico fecha de aplanamiento de la curva
cuando_acaba <- function(params, t, ultima_fecha, idmodel) {
  
  i = 0
  
  while (TRUE) {
    if (idmodel == 2) {
      model_max <- gompertz_mod(params, t)
    } else if (idmodel == 3) {
      model_max <- logistic_model(params, t)
    }
    finales   <- tail(model_max,2)
    ultimo    <- round(finales[2])
    penultimo <- round(finales[1])
    
    if (ultimo == penultimo) {
      break()
    }
    
    t <- 1:(length(t) + 1)
    i = i + 1
  }
  
  #return(data.frame(model_max, t))
  return(ultima_fecha + i)
  
}


######### seccion de modelo exponencial -----------------

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
                               t = ((nrow(temp_casos_general) + 1):(nrow(temp_casos_general) + 7))),0)

prediccion <- data.frame(
  "Casos_estimados"= prediccion,
  time = ((nrow(temp_casos_general) + 1):(nrow(temp_casos_general) + 7))
)

prediccion <- prediccion %>%
  mutate(Fecha = as.Date(temp_casos_general[1,"Fecha"] + days(time - 1), format = "%Y-%m-%d")) %>%
  mutate(Fecha = paste(month(Fecha, label = TRUE, abbr = FALSE), day(Fecha))) %>%
  select(
    Fecha, Casos_estimados
  )

colnames(prediccion) <- c("Fecha", "Casos diarios estimados")

#### almacena la tabla para el output

saveRDS(prediccion, file = "datos/prediccion.RDS")

ajuste_prediccion <- data.frame(
  time = (1:(nrow(temp_casos_general) + 7)),
  casos = round(estimacion(x0 = x0,b = b,t = (1:(nrow(temp_casos_general) + 7))))
)

colnames(ajuste_prediccion) <- c("time","Estimados")

#### agregar fechas

ajuste_prediccion <- ajuste_prediccion %>%
  mutate(Fecha = as.Date(temp_casos_general[1,"Fecha"] + days(time - 1), format = "%Y-%m-%d")) %>%
  mutate(Fecha = paste(month(Fecha, label = TRUE, abbr = FALSE), day(Fecha)))

general_temporal <- temp_casos_general %>%
  mutate(Fecha = paste(month(Fecha, label = TRUE, abbr = FALSE), day(Fecha))) #Eliminar esto apenas se haya modificado la base de datos general

ajuste_prediccion <- ajuste_prediccion %>%
  e_charts(Fecha) %>%
  e_line(Estimados) %>%
  e_tooltip(trigger = "axis") %>%
  e_data(general_temporal) %>%
  e_scatter(Casos, symbol_size = 7, name = "Confirmados") %>%
  e_legend(right = 0) %>%
  e_x_axis(name = "Fecha", nameLocation = "center", nameGap = 40) %>%
  e_y_axis(name = "Diarios")

#### almacena el grafico para el output

saveRDS(ajuste_prediccion, file = "datos/ajuste_prediccion.RDS")

### Error medio absoluto exponencial

infoextra_exponencial<-data.frame(c("No aplica"),
                                  round(mean(abs(general_temporal$`Casos Reales`- general_temporal$Estimado)),2),
                                  c("No aplica")
)

colnames(infoextra_exponencial) <- c("Pronóstico del aplanamiento de la curva", 
                                     "Error medio absoluto (MAE)", 
                                     "Error porcentual medio absoluto (MAPE %)")

saveRDS(infoextra_exponencial,file="datos/infoextra_exponencial.RDS")

######### seccion de modelo gompertz -----------------

#### Cargar y preparar datos 

df <- temp_casos_general %>%
  select(Confirmados) %>%
  tibble::rownames_to_column("Dia") %>%
  mutate(Dia = lag(Dia),
         Dia = if_else(is.na(Dia),
                       0,
                       as.numeric(Dia)
         )
  )

tiempo <- 1:(length(df$Dia) + 7)
tiemporeal <- 1:length(df$Dia)

ultima_fecha <- tail(temp_casos_general$Fecha, 1)

#### Ajustar modelo 

modelo <- nlsfit(df, 
                 model = 10,
                 start = c(a = 761.049823156006, 
                           b = 6.63469882635722, 
                           c = 0.08376468439801
                 )
)

param_gompertz <- modelo$Parameters[1:3,1]

#### Graficar datos

modelado <- gompertz_mod(param_gompertz, tiempo)

predicciones_gompertz <- temp_casos_general %>%
  select(Fecha) %>%
  add_row(Fecha = seq.Date(ultima_fecha + 1, 
                           length.out = 7, 
                           by = "day")
  ) %>%
  mutate(Estimados = round(modelado, 0),
         Fecha = paste(month(Fecha, label = TRUE, abbr = FALSE), day(Fecha)))

modelo_gompertz <- predicciones_gompertz %>%
  e_charts(Fecha) %>%
  e_line(Estimados) %>%
  e_data(general_temporal) %>% #Este es un data.frame con los casos reales acum. de covid y se crea en la seccion del modelo exponencial
  e_scatter(Confirmados, symbol_size = 7) %>%
  e_legend(right = 0) %>%
  e_tooltip(trigger = "axis") %>%
  e_x_axis(name = "Fecha", nameLocation = "center", nameGap = 40) %>%
  e_y_axis(name = "Acumulados")

saveRDS(modelo_gompertz, file = "datos/modelo_gompertz.RDS")

#### Obtener salidas 

colnames(predicciones_gompertz) <- c("Fecha", "Casos acum. estimados")

saveRDS(tail(predicciones_gompertz, 7), file = "datos/predicciones_gompertz.RDS")

infoextra_gompertz <- data.frame(as.character(cuando_acaba(param_gompertz, tiemporeal, ultima_fecha, 2)),
                                 round(mean(abs(
                                   general_temporal$Confirmados - modelado[1:nrow(general_temporal)]
                                 )), 2),
                                 round(mean( ( abs(
                                   general_temporal$Confirmados - modelado[1:nrow(general_temporal)]
                                 ) / general_temporal$Confirmados ) ), 4)
)

colnames(infoextra_gompertz) <- c("Pronóstico del aplanamiento de la curva", 
                                  "Error medio absoluto (MAE)", 
                                  "Error porcentual medio absoluto (MAPE %)")

saveRDS(infoextra_gompertz, file = "datos/infoextra_gompertz.RDS")

######### seccion de modelo logistico -----------------
dias<-temp_casos_general$dias
confirmados<-temp_casos_general$Confirmados
#Funcion que me permite usar python dentro de R de manera interactiva
repl_python()

import numpy as np
import scipy.optimize as optim
#Definir la funcion logistica a optimizar
def logistic_model(x,a,b,c):
  return c/(1+np.exp(-(x-b)/a))
#Establecer valores aleatorios y limites para los parametros
p0=np.random.exponential(size=3)
bounds=(0,[100000.,10000.,4900000.])

#Estimar el valor de los parametros a partir de los datos originales
ajuste = optim.curve_fit(logistic_model,r.dias,r.confirmados,p0=p0,bounds=bounds)
#salir de python
exit

#guardar parametros del modelo logístico
a=as.numeric(py$ajuste[[1]][1])
b=as.numeric(py$ajuste[[1]][2])
c=as.numeric(py$ajuste[[1]][3])

param_logistico <- c(a, b, c)

#Generar ajuste y predicciones del modelo logístico
ajuste_regresion_logistica <- logistic_model(params = param_logistico, x=(1:(nrow(temp_casos_general) + 7)))

#Generar DataFrame para el grafico de regresion logística
data_regresion_logistica <- data.frame(
  Estimados = ajuste_regresion_logistica,
  time = (1:(nrow(temp_casos_general) + 7))
)
#cambiar fecha para gráfico acumulado
ajuste_logistico_acum <- data_regresion_logistica%>%
  mutate(
    Fecha = as.Date(temp_casos_general[1,"Fecha"] + days(time - 1), format = "%Y-%m-%d"),
    Estimados = round(Estimados)
  ) %>%
  mutate(Fecha = paste(month(Fecha, label = TRUE, abbr = FALSE), day(Fecha))) %>%
  select(
    Fecha, Estimados
  )

#gráfico acumulado
modelo_logistico <- ajuste_logistico_acum %>%
  e_charts(Fecha) %>%
  e_line(Estimados) %>%
  e_data(general_temporal) %>% #Este es un data.frame con los casos reales acum. de covid y se crea en la seccion del modelo exponencial
  e_scatter(Confirmados, symbol_size = 7) %>%
  e_legend(right = 0) %>%
  e_tooltip(trigger = "axis")  %>%
  e_y_axis(name = "Acumulados") %>%
  e_x_axis(name = "Fecha",nameLocation="center",nameGap = 40)

colnames(ajuste_logistico_acum) <- c("Fecha", "Casos acum. estimados")

saveRDS(modelo_logistico,file = "datos/modelo_logistico.RDS")

predicciones_logistica <- ajuste_logistico_acum[((nrow(temp_casos_general) + 1):(nrow(temp_casos_general) + 7)) ,]

saveRDS(predicciones_logistica, file="datos/predicciones_logistica.RDS")

#Error medio absoluto logístico

infoextra_logistico <- data.frame(as.character(cuando_acaba(param_logistico, tiemporeal, ultima_fecha, idmodel = 3)),
                                  round(mean(
                                    abs(general_temporal$Confirmados - ajuste_regresion_logistica[1 : nrow(general_temporal)])),2),
                                  round(mean( ( 
                                    abs(general_temporal$Confirmados - ajuste_regresion_logistica[1 : nrow(general_temporal)]
                                    ) / general_temporal$Confirmados ) 
                                  ), 4)
)

colnames(infoextra_logistico)<-c("Pronóstico del aplanamiento de la curva", 
                                 "Error medio absoluto (MAE)", 
                                 "Error porcentual medio absoluto (MAPE %)")

saveRDS(infoextra_logistico,file="datos/infoextra_logistico.RDS")

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

