

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
cr_caso_general <- readRDS("datos/casos_general.RDS")
pred <- readRDS("datos/prediccion.RDS")
ajuste <- readRDS("datos/ajuste_prediccion.RDS")
mapa_p <- readRDS("datos/mapa_provincia.RDS")

## Codigo debe ir aparte en otro Script

SIR <- function(time, state, parameters) {
    par <- as.list(c(state, parameters))
    with(par, {
        dS <- -beta * I * S / N
        dI <- beta * I * S / N - gamma * I
        dR <- gamma * I
        list(c(dS, dI, dR))
    })
}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
            output$modelo_log_lin <- renderEcharts4r({
                
                ajuste
                
            })
            
            output$estimacion_log_lin<-renderTable({
                
                pred
                
            })
            
            output$map <- renderEcharts4r({
                
                mapa_p
                
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
                
                cn <- cr_caso_limpio %>%
                    filter(provincia == selected) %>%
                    group_by(fecha)
        
                cn %>%
                    e_charts(canton, timeline = TRUE) %>%
                    e_map_register("cr_canton", geojson) %>%
                    e_map(total, map = "cr_canton", name = "Confirmados") %>%
                    e_visual_map(min = 0, 
                                 max = max(cn$total),
                                 inRange = list(color = c('yellow','orange', 'orangered', 'red'))) %>%
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
        
