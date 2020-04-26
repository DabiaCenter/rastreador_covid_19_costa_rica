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
library(DT)

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
tabla_top10 <- readRDS("datos/tabla_top10.RDS")
modelo_gompertz <- readRDS("datos/modelo_gompertz.RDS")
infoextra_gompertz <- readRDS("datos/infoextra_gompertz.RDS")
predicciones_gompertz <- readRDS("datos/predicciones_gompertz.RDS")
modelo_logistico1<-readRDS("datos/modelo_logistico.RDS")
predicciones_logistica1<-readRDS("datos/predicciones_logistica.RDS")
infoextra_exponencial<-readRDS("datos/infoextra_exponencial.RDS")
infoextra_logistico<-readRDS("datos/infoextra_logistico.RDS")
graf_hosp <- readRDS("datos/graf_hosp.RDS")

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

cr_caso_general <- readRDS("datos/casos_general.RDS")

N <- 4900000
init <- c(
    S = N - cr_caso_general$Confirmados[1],
    I = cr_caso_general$Confirmados[1],
    R = 0
)

Day <- 1:(length(cr_caso_general$Confirmados))

RSS <- function(parameters) {
    names(parameters) <- c("beta", "gamma")
    out <- ode(y = init, times = Day, func = SIR, parms = parameters)
    fit <- out[, 3]
    sum((cr_caso_general$Confirmados - fit)^2)
}


Opt <- optim(c(0.5, 0.5),
             RSS,
             method = "L-BFGS-B",
             lower = c(0, 0),
             upper = c(1, 1)
)

Opt_par <- setNames(Opt$par, c("beta", "gamma"))
Opt_par
beta_val = Opt$par[1]
gamma_val = Opt$par[2]

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    #Conteos de estadísticas actuales ----
    
    output$conf <- renderCountup({
        countup(max(cr_caso_general$Confirmados), duration = 7)
    })
    
    output$fall <- renderCountup({
        countup(max(cr_caso_general$Fallecidos), duration = 5)
    })
    
    output$desc <- renderCountup({
        countup(max(cr_caso_general$Descartados), 
                duration = 5)
    })
    
    output$recu <- renderCountup({
        countup(max(cr_caso_general$Recuperados), 
                duration = 5)
    })
    
    output$hosp <- renderCountup({
        countup(cr_caso_general$Hospitalizados[nrow(cr_caso_general)],
                duration = 5)
    })
    
    output$ci <- renderCountup({
        countup(cr_caso_general$CI[nrow(cr_caso_general)], 
                duration = 5)
    })
    
    #Graficos seccion "General" ----
    
    output$graf_infectados <- renderEcharts4r({
        #Grafico comparativo entre infectados por dia e infectados acumulados
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
    
    output$tabla_top10 <- renderDataTable({
        
        #Grafico top 10 cantones
        datatable(tabla_top10, rownames = FALSE, options = list(
            columnDefs = list(list(className = 'dt-center', targets = 1)),
            dom = 't',
            order = FALSE
        )) %>% formatStyle("Infectados",
                           fontWeight = 'bold',
                           background = styleColorBar(tabla_top10$Infectados, 'lightblue'),
                           backgroundSize = '65% 55%',
                           backgroundRepeat = 'no-repeat',
                           backgroundPosition = 'center')
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
    
    ##### Grafico hospitalizados
    
    output$graf_hosp <- renderEcharts4r({
        
        graf_hosp
        
    })
    
    #Seccion modelos de regresion ----
    
    output$modelo_regresion <- renderEcharts4r({
        
        if (input$variable == "Exponencial") {
            ajuste
        } else if (input$variable == "Gompertz") {
            modelo_gompertz
        } else if (input$variable =="Logístico"){
            modelo_logistico1
        }
        
    })
    
    output$info_adicional <- renderDataTable({
        if (input$variable == "Exponencial") {
            datatable(infoextra_exponencial, rownames = FALSE, options = list(
                columnDefs = list(list(className = 'dt-center', targets = "_all")),
                dom = 't',
                order = FALSE
            )) 
        } else if (input$variable == "Gompertz") {
            datatable(infoextra_gompertz, rownames = FALSE, options = list(
                columnDefs = list(list(className = 'dt-center', targets = "_all")),
                dom = 't',
                order = FALSE
            )) %>% formatPercentage(columns = 3, 2)
        } else if (input$variable == "Logístico"){
            datatable(infoextra_logistico, rownames = FALSE, options = list(
                columnDefs = list(list(className = 'dt-center', targets = "_all")),
                dom = 't',
                order = FALSE
            )) %>% formatPercentage(columns = 3, 2)
        }
        
    }) 
    
    output$estimacion_regresion <- renderDataTable({
        if (input$variable == "Exponencial") {
            datatable(pred, rownames = FALSE, options = list(
                columnDefs = list(list(className = 'dt-center', targets = "_all")),
                dom = 't',
                order = FALSE
            ))
        } else if (input$variable == "Gompertz") {
            datatable(predicciones_gompertz, rownames = FALSE, options = list(
                columnDefs = list(list(className = 'dt-center', targets = "_all")),
                dom = 't',
                order = FALSE
            ))
        } else if (input$variable =="Logístico"){
            datatable(predicciones_logistica1, rownames = FALSE, options = list(
                columnDefs = list(list(className = 'dt-center', targets = "_all")),
                dom = 't',
                order = FALSE
            ))
        }
        
    })
    
    
    #Seccion mapa  ----
    
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
            e_timeline_opts(axis_type = "time",
                            playInterval = 1000, 
                            currentIndex = length(unique(cr_caso_provincia$fecha))-1,
                            symbolSize = 4, 
                            label = list(
                                show = FALSE
                            ),
                            checkpointStyle = list(
                                symbol = "pin",
                                symbolSize = 25
                            )) %>%
            e_show_loading(text = "Cargando...",
                           color = "#000000", 
                           mask_color = '#ffffff')
        
    })
    
    #Parte del server de modelaje del Modelo SIR  ----
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
            e_line(S, name = "Susceptibles") %>%
            e_line(I, name = "Infectados") %>%
            e_line(R, name = "Recuperados")
    })
    
    
    output$indicadores <- renderDataTable({
        
        val1 <- as.data.frame(valores())
        
        comienzo <- "6/03/2020"
        
        val1 <- val1 %>%
            mutate(
                Fecha = dmy(comienzo) + days(time - 1)
            )%>%
            select(Fecha, I)
        
        ro = round(input$beta/input$gamma, 2)
        
        infectados = ifelse(
            (1 -(1/ro)) < 0, 
            "No aplica", 
            ((1 -(1/ro))))
        
        pico = ifelse(
            (1 -(1/ro)) < 0, 
            "No aplica", 
            as.character(val1[max(val1$I) == val1$I,]$Fecha))
        
        df <- data.frame("Tasa de contacto" = ro, 
                         "Porcentaje de la poblacion a infectarse" = infectados,
                         "Pico de la pandemia" = pico)
        
        colnames(df) <- c("Tasa de contacto", 
                          "Porcentaje de la población a infectarse", 
                          "Pico de la pandemia")
        datatable(df, rownames = FALSE, options = list(
            columnDefs = list(list(className = 'dt-center', targets = "_all")),
            dom = 't',
            order = FALSE
        )) %>% formatPercentage(columns = 2, 2)
    })
    
    #Botón de reseteo
    
    observeEvent(input$reset, {
        updateSliderInput(session, "beta", value = beta_val)
        updateSliderInput(session, "gamma", value = gamma_val)
        updateSliderInput(session, "poblacion", value = 4900000)
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
    
    #Popups de la explicación de los modelos y período de duplicación ---------
    
    observeEvent(input$sir, {
        f7TogglePopup(id = "popup1")
    })
    
    observeEvent(input$info_model_reg, {
        if (input$variable == "Exponencial") {
            f7TogglePopup(id = "popup2")
        } else if (input$variable == "Gompertz") {
            f7TogglePopup(id = "popup3")
        }
        else if (input$variable == "Logístico") {
            f7TogglePopup(id = "popup4")
        }
    })
    
    observeEvent(input$periodo, {
        f7TogglePopup(id = "popup5")
    })
   
})
        
