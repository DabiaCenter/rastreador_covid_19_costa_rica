

library(devtools)
library(shiny)
library(dplyr)
library(echarts4r.maps)
library(echarts4r)
library(tidyr)
library(shinyMobile)
library(waiter)
library(jsonlite)
library(purrr)
library(deSolve)
library(lubridate)

cr_caso_limpio <- readRDS("datos/cr_caso_limpio.RDS")
cr_caso_provincia <- readRDS("datos/cr_caso_provincia.RDS")
cr_caso_general <- readRDS("datos/casos_general.RDS")
pred <- readRDS("datos/prediccion.RDS")
ajuste <- readRDS("datos/ajuste_prediccion.RDS")
mapa_pr <- readRDS("datos/mapa_provincia.RDS")
graf_calendario <- readRDS("datos/graf_calendario.RDS")
graf_descartados <- readRDS("datos/graf_descartados.RDS")
graf_edades <- readRDS("datos/graf_edades.RDS")
graf_estados <- readRDS("datos/graf_estados.RDS")
graf_genero <- readRDS("datos/graf_genero.RDS")
graf_infectados <- readRDS("datos/graf_infectados.RDS")
graf_nacionalidad <- readRDS("datos/graf_nacionalidad.RDS")
graf_top10 <- readRDS("datos/graf_top10.RDS")

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
    
    #Conteos de estadísticas actuales
    
    output$conf <- renderCountup({
        countup(max(cr_caso_general$Confirmados), duration = 7)
    })
    
    output$fall <- renderCountup({
        countup(max(cr_caso_general$Fallecidos), duration = 7)
    })
    
    output$desc <- renderCountup({
        countup(max(cr_caso_general$Descartados), duration = 7)
    })
    
    output$recu <- renderCountup({
        countup(max(cr_caso_general$Recuperados), duration = 7)
    })
    
    #Grafico comparativo entre infectados por dia e infectados acumulados
    output$graf_infectados <- renderEcharts4r({
        
        graf_infectados
    })
    
    output$graf_descartados <- renderEcharts4r({
        
        #Grafico cantidad descartados
        graf_descartados
    })
    
    output$graf_calendario <- renderEcharts4r({
        
        #Mapa de calor: cantidad de infecciones por dia
        graf_calendario
    })
    
    output$graf_top10 <- renderEcharts4r({
        
        #Grafico top 10 cantones
        graf_top10
    })
    
    output$graf_estados <- renderEcharts4r({
        
        #Grafico cantidad recuperados y fallecidos
        graf_estados
    })
    
    output$graf_genero <- renderEcharts4r({
        
        #Grafico comparativo entre infectados por genero
        graf_genero
    })
    
    output$graf_nacionalidad <- renderEcharts4r({
        
        #Grafico comparativo entre infectados por nacionalidad
        graf_nacionalidad
    })
    
    output$graf_edades <- renderEcharts4r({
        
        #Grafico comparativo infectados adultos, adultos mayores y menores
        graf_edades
    })
    
    output$modelo_log_lin <- renderEcharts4r({
        
        ajuste
        
    })
    
    output$estimacion_log_lin<-renderTable({
        
        pred
        
    })
    
    output$map <- renderEcharts4r({
        
        mapa_pr
        
    })
    
    url_to_geojson <- function(x){
        x <- tolower(x)
        x <- ifelse(x == "san jose", "sanjose", x)
        paste0("mapas-json/", x, ".geojson")
    }
    
    selected <- reactive({
        selected <- input$smartsel
        return(selected)
    })
    
    output$canton <- echarts4r::renderEcharts4r({
        
        geojson <- url_to_geojson(selected()) %>%
            jsonlite::read_json()
        
        cn <- cr_caso_limpio %>%
            filter(provincia == selected()) %>%
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
                            playInterval = 1000, currentIndex = length(unique(cr_caso_provincia$fecha))-1) %>%
            e_show_loading(text = "Cargando...",color = "#000000", mask_color = '#ffffff')
        
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
            e_x_axis(name = "Fecha", nameLocation = "center", nameGap = 40) %>%
            e_y_axis(name = "Población") %>%
            e_text_style(fontSize = 12) %>%
            e_line(S) %>%
            e_line(I) %>%
            e_line(R) %>%
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
        
        df <- data.frame("Tasa de contacto" = ro, 
                         "Porcentaje de la poblacion a infectarse (%)" = infectados,
                         "Pico de la pandemia" = pico)
        
        colnames(df) <- c("Tasa de contacto", 
                          "Porcentaje de la población a infectarse (%)", 
                          "Pico de la pandemia")
        df
    })
    
    output$pedup <- renderEcharts4r({
        cr_caso_general %>%
            tail(n=5)%>%
            e_charts(Fecha) %>%
            e_scatter(periodo_de_duplicacion,name="Período de Duplicación",symbolSize =25) %>%
            e_tooltip(trigger="item") %>%
            e_x_axis(name="Fecha", nameLocation = "center", nameGap = 40)%>%
            e_y_axis(name="Días")%>%
            e_text_style(fontSize=13)
    })
    
    #Popups de la explicación de los modelos y período de duplicación
    
    observeEvent(input$sir, {
        f7TogglePopup(id = "popup1")
    })
    
    observeEvent(input$exp, {
        f7TogglePopup(id = "popup2")
    })
    
    observeEvent(input$periodo, {
        f7TogglePopup(id = "popup3")
    })
   
})
        
