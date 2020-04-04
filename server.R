

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

cr <- read.csv("https://raw.githubusercontent.com/DabiaCenter/Covid19_CostaRica/master/covid19_cantones_cr.csv")



# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
            
            output$map <- renderEcharts4r({
                
                json <- jsonlite::read_json("mapas-json/provincias.geojson")
                
                json$features <- json$features %>% 
                    map(function(x){ 
                        x$properties$name <- x$properties$NPROVINCIA 
                        return(x)})
                
                cr_pr <- cr
                
                cr_pr$Provincia <- as.character(cr_pr$Provincia)
                
                cr_pr <- cr_pr %>%
                    filter(Canton != "DESCONOCIDO") %>%
                    select(-2) %>%
                    group_by(Provincia) %>%
                    tidyr::gather("Fecha",  "Confirmados", -1) %>%
                    group_by(Provincia, Fecha) %>%
                    summarise(Total = sum(Confirmados))
                
                cr_pr$Fecha <- sub("X","",cr_pr$Fecha)
                
                cr_pr <- cr_pr %>% 
                    separate(col = Fecha, c("Dia", "Mes", "Año")) %>%
                    unite(Dia,Mes,Año, col = "Fecha", sep = "-")
                
                cr_pr$Fecha <- as.Date(cr_pr$Fecha, format = "%d-%m-%Y")
                
                cr_pr %>%
                    group_by(Fecha) %>%
                    e_charts(Provincia, timeline = TRUE) %>%
                    e_map_register("cr_provincia", json) %>%
                    e_map(Total, map = "cr_provincia", name = "Confirmados") %>%
                    e_visual_map(min = 0, 
                                 max = max(cr_pr$Total),
                                 inRange = list(color = c('yellow','orange', 'orangered', 'red')),
                                 show = FALSE) %>%
                    e_tooltip() %>%
                    e_timeline_opts(axis_type = "category",
                                    playInterval = 1000)
                })
            
            url_to_geojson <- function(canton){
                canton <- tolower(canton)
                canton <- ifelse(canton == "san jose", "sanjose", canton)
                paste0("mapas-json/", canton, ".geojson")
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
                
                cr_cn <- cr %>% 
                    filter(Provincia == selected) %>%
                    group_by(Canton) %>%
                    select(-1) %>%
                    tidyr::gather("Fecha",  "Confirmados", -1) %>%
                    group_by(Fecha)
                
                cr_cn$Fecha <- sub("X","",cr_cn$Fecha)
        
                cr_cn <- cr_cn %>% 
                    separate(col = Fecha, c("Dia", "Mes", "Año")) %>%
                    unite(Dia,Mes,Año, col = "Fecha", sep = "-")
        
                cr_cn$Fecha <- as.Date(cr_cn$Fecha, format = "%d-%m-%Y")
        
                cr_cn %>%
                    group_by(Fecha) %>%
                    e_charts(Canton, timeline = TRUE) %>%
                    e_map_register("cr_canton", geojson) %>%
                    e_map(Confirmados, map = "cr_canton", name = "Confirmados") %>%
                    e_visual_map(min = 0, 
                                 max = max(cr_cn$Confirmados),
                                 inRange = list(color = c('lightskyblue', 'yellow', 'orangered', 'red')),
                                 show = FALSE) %>%
                    e_tooltip() %>%
                    e_timeline_opts(axis_type = "category",
                                    playInterval = 1000) %>%
                    e_title(selected, top = 10, left = 10) %>% 
                    e_show_loading(color = "#000000", mask_color = '#ffffff')
                })
    })
        
