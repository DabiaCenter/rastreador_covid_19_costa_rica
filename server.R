

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

cr_caso_limpio <- readRDS("datos/cr_caso_limpio.RDS")
cr_caso_provincia <- readRDS("datos/cr_caso_provincia.RDS")


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
            
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
    })
        
