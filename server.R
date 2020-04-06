

library(devtools)
library(shiny)
library(dplyr)
library(echarts4r.maps)
library(echarts4r)
library(shinydashboard)
library(tidyr)
library(shinyMobile)
library(waiter)
library(jsonlite)
library(purrr)
library(shinyscroll)
library(deSolve)
library(lubridate)

cr_caso_limpio <- readRDS("datos/cr_caso_limpio.RDS")
cr_caso_provincia <- readRDS("datos/cr_caso_provincia.RDS")

#Codigo Alexis
#iniciar fecha
comienzo <- "6/03/2020"
#hacerle transformacion logaritmica a los datos
datos<-read_excel("datos/datos.xlsx")
datos<-datos%>%
    mutate(casosdia=casosdia+1,Fecha = dmy(comienzo) + days(dias - 1))%>%
    mutate(logcasos=log(casosdia))

#funcion exponencial
estimacion<-function(x0,b,t){
    return(x0*(b^t))
}
#crear modelo
modelo_log<-lm(logcasos~dias,data=datos)

#transfromar variables
x0<-exp(modelo_log$coefficients[[1]])
b<-exp(modelo_log$coefficients[[2]])


#datos ajustados
datos<-datos%>%
    select(dias,casosdia,Fecha)%>%
    mutate(ajuste=estimacion(x0=x0,b=b,t=(1:length(casosdia))))%>%
    mutate(casosdia=casosdia-1)


#prediciones para los proximos 8 dias
prediccion<-round(estimacion(x0=x0,b=b,t=(nrow(datos):(nrow(datos)+6))),0)

prediccion<-data.frame(
    "Casos_estimados"=prediccion,
    time=(nrow(datos):(nrow(datos)+6))
)

prediccion <- prediccion %>%
    mutate(
        Fecha = dmy(comienzo) + days(time - 1)
    )%>%
    select(
        Fecha,Casos_estimados
    )

colnames(prediccion)<-c("Fecha","Casos Estimados")
colnames(datos)<-c("dias","Casos","Fecha","ajuste")

prediccion$Fecha<-as.character(prediccion$Fecha)

ajuste_prediccion<-data.frame(
    time=(1:(nrow(datos)+6)),
    casos=round(estimacion(x0=x0,b=b,t=(1:(nrow(datos)+6))))
    
)

colnames(ajuste_prediccion)<- c("time","Estimado")

#agregar fechas
ajuste_prediccion<-ajuste_prediccion%>%
    mutate(Fecha = dmy(comienzo) + days(time - 1))
#
# Codigo debe ir aparte en otro Script

SIR <- function(time, state, parameters) {
    par <- as.list(c(state, parameters))
    with(par, {
        dS <- -beta * I * S / N
        dI <- beta * I * S / N - gamma * I
        dR <- gamma * I
        list(c(dS, dI, dR))
    })
}

cr_caso_general <- readRDS("datos/casos_general.RDS")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
            output$modelo_log_lin<-renderEcharts4r({
                ajuste_prediccion %>%
                    e_charts(Fecha)%>%
                    e_line(Estimado) %>%
                    e_title("Modelo de crecimiento exponencial","casos") %>%
                    e_tooltip()%>%
                    e_data(datos)%>%
                    e_scatter(Casos)%>%
                    e_x_axis(name="Fecha")
                
            })
            output$estimacion_log_lin<-renderTable({prediccion
            })
            
            output$map <- renderEcharts4r({
                
                json <- jsonlite::read_json("mapas-json/provincias.geojson")
                
                json$features <- json$features %>% 
                    map(function(x){ 
                        x$properties$name <- x$properties$NPROVINCIA 
                        return(x)})
                
                cr_caso_provincia %>%
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
                })
            
            url_to_geojson <- function(x){
                x <- tolower(x)
                x <- ifelse(x == "san jose", "sanjose", x)
                paste0("mapas-json/", x, ".geojson")
                }
            
            output$canton <- echarts4r::renderEcharts4r({
                
                if(is.null(input$map_clicked_data)) {
                    selected <- "ALAJUELA"
                    } else {
                        selected <- input$map_clicked_data$name
                        shinyscroll::scroll("Cantones")
                        }
                
                geojson <- url_to_geojson(selected) %>%
                    jsonlite::read_json()
        
                cr_caso_limpio %>%
                    filter(provincia == selected) %>%
                    group_by(fecha) %>%
                    e_charts(canton, timeline = TRUE) %>%
                    e_map_register("cr_canton", geojson) %>%
                    e_map(total, map = "cr_canton", name = "Confirmados") %>%
                    e_visual_map(min = 0, 
                                 max = max(cr_caso_limpio$total),
                                 inRange = list(color = c('lightskyblue', 'yellow', 'orangered', 'red'))) %>%
                    e_tooltip() %>%
                    e_timeline_opts(axis_type = "category",
                                    playInterval = 1000) %>%
                    e_title(selected, top = 10, left = 10) %>% 
                    e_show_loading(color = "#000000", mask_color = '#ffffff')
                })
            
            #Parte del server de modelaje del Modelo SIR
            valores <- reactive({
                req(input$poblacion, input$beta, input$gamma)
                ode(y = c(S = input$poblacion, I = 1, R = 0),
                    times = seq(1:200),
                    func = SIR,
                    parms = c(beta = input$beta, gamma = input$gamma, N = input$poblacion))
            })
            
            
            output$SIR <- renderEcharts4r({
                val <- as.data.frame(valores())
                
                comienzo <- "6/03/2020"
                val <- val %>%
                    mutate(
                        Fecha = dmy(comienzo) + days(time - 1)
                    )
                
                val %>%
                    e_charts(Fecha)%>%
                    e_x_axis(name = "Fecha") %>%
                    e_y_axis(name = "Población") %>%
                    e_text_style(fontSize = 18) %>%
                    e_line(S) %>%
                    e_line(I) %>%
                    e_line(R) %>%
                    e_title("Modelo SIR del Covid-19 para Costa Rica") %>%
                    e_tooltip() 
            })
            
            
            output$indicadores <- renderTable({
                val1 <- as.data.frame(valores())
                
                comienzo <- "6/03/2020"
                val1 <- val1 %>%
                    mutate(
                        Fecha = dmy(comienzo) + days(time - 1)
                    )%>%
                    select(Fecha, I)
                
                ro = input$beta/input$gamma
                infectados = ifelse(
                    (1 -(1/ro)) < 0, 
                    "No aplica", 
                    ((1 -(1/ro))*100))
                pico = ifelse(
                    (1 -(1/ro)) < 0, 
                    "No aplica", 
                    as.character(val1[max(val1$I) == val1$I,]$Fecha))
                muertes = ifelse(
                    (1 -(1/ro)) < 0,
                    "No aplica",
                    max(val1$I) * max(cr_caso_general$Fallecidos)/cr_caso_general$Confirmados[nrow(cr_caso_general)])
                df <- data.frame("Tasa de contacto" = ro, 
                                 "Porcentaje de la poblacion a infectarse (%)" = infectados,
                                 "Pico de la pandemia" = pico,
                                 "Posible cantidad de muertes" = round(muertes,))
                colnames(df) <- c("Tasa de contacto", 
                                  "Porcentaje de la población a infectarse (%)", 
                                  "Pico de la pandemia", 
                                  "Posible cantidad de muertes")
                df
            })
    })
        
