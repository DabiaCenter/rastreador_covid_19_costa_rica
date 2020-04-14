#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyMobile)


ui<-f7Page(
 # use_waiter(include_js = FALSE),
  title = "COVID-19 CR",
  dark_mode = FALSE,
  init = f7Init(
    skin = "md",
    theme = "light",
    color = "blue"
  ),
  f7Tabs(
    animated = TRUE,
    id = "tabs",
    f7Tab(
      tabName = "General",
      icon = f7Icon("info_circle", old = FALSE),
      active = TRUE,
      swipeable = FALSE,
      #waiter_show_on_load(html = loader),
      f7Card(
        f7BlockHeader(img(src = "https://www.grupodabia.com/nosotros/logo.jpg", height = 140, width = 140)) %>%
          f7Align("center"),
        h2("Estado Covid-19 Costa Rica", class = "center", align = "center"),
        p("Datos generales", class = "center", align = "center")
      ),
        f7Card(
            f7Popup(
              id="popup3",
              label = "Información del Período de Duplicación",
              title= "Información sobre el Periódo de Duplicación",
              p("El COVID-19 está marcando un antes y un después en la historia de la humanidad, existe información que puede brindar un acercamiento a la comprensión del comportamiento y dinámica de la pandemia."),
              div(),
              p("Los casos confirmados es una de estas informaciones en el país. Ahora bien, el número de casos positivos absolutos no es necesariamente el mejor indicador del estado actual de expansión de un brote de enfermedad, sobre todo en el caso de una infección que puede tener síntomas leves o incluso personas asintomáticas que pueden infectar a otros."),
              p("El",strong ("período de duplicación"), "es decir, el tiempo en el cual se duplica la cifra de personas testeadas positivas por una infección, es una medida utilizada a nivel internacional para ofrecer una imagen del desarrollo de una infección en un instante determinado."),
              div(),
              p("Si el período de duplicación fuese de dos días con cien infectados, quiere decir que pasaría de cien infectados a doscientos en dos días. Este indicador se obtiene:"),
              hr(),
              
              img(src="periodo_duplicacion", align = "center"),
              
              p("La variable de",strong("casos activos"),"es igual al número de casos confirmados del día actual menos las personas fallecidas y recuperadas del día actual. Estos casos activos son en efecto los que representan potenciales fuentes de transmisión viral.",
                strong("Casos nuevos"), "deriva de la resta entre los casos confirmados del día de anterior menos los casos confirmados del día actual. Se obtiene como Resultado el período de duplicación aproximado (en días).
                "),
              div(),
              p("En la gráfica se muestra el período de duplicación al lado de cada barra de los últimos 5 días actualizados."),
              div(),
              p("Un período corto de duplicación de 4-5 días, no es lo ideal, dado que podría provocar una saturación en el sistema de salud. Significa que la enfermedad se está propagando muy rápidamente.
                Por otro lado, si el período supera 20 a 25 días, se considera un período prolongado que representa un crecimiento más lento de aparición de casos infectados, por el hecho que en el mismo período se disminuyen los casos activos reales. Por ello, el Ministro de Salud y el Gobierno actual han insistido en “aplanar la curva”, es decir, ralentizar la tasa de crecimiento de los nuevos casos de infección para conservar la capacidad del sector de salud estable al mantener un período de duplicación prolongado e impactar de manera más sutil sobre las repercusiones económicas y sociales del país.
                "),
                div(),
                p("Es importante mencionar que este indicador es meramente teórico y simplificado. No es una predicción perfecta, también por causa de ausencia de suficientes datos epidemiológicos, por el factor de incertidumbre en las personas asintomáticas que no necesariamente están testeadas y cuyo porcentaje real se desconoce. Se trata de un predictor para ofrecer una imagen aproximada de la situación actual que tienen márgenes de error."),
                div(),
                p("Sin embargo, como costarricenses debemos responsabilizarnos y ser conscientes que con el distanciamiento social y acatamiento de las órdenes dadas por las autoridades se puede reducir la tasa de transmisión y aumentar efectivamente el período de duplicación disminuyendo la tasa de infección en la población no infectada."),
                div(),
                p("Léase:"),
                a(href="Nunes-Vaz, R. (2020). Visualising the doubling time of COVID-19 allows comparison of the success of containment measures. Global Biosecurity. Retrieved from https://jglobalbiosecurity.com/articles/10.31646/gbio.61/"),
                a(href="Roser, M., Ritchie, H., & Ortiz, E. (2020). Coronavirus Disease (COVID-19) – Statistics and Research. Our World in Data. Retrieved from https://ourworldindata.org/coronavirus"),
                a(href=" Woolhouse, M. (2011, July 12). How to make predictions about future infectious disease risks. Royal Society. Retrieved from https://royalsocietypublishing.org/doi/10.1098/rstb.2010.0387"),
                a(href="Wu, J. T., Leung, K., & Leung, G. M. (2020, February 29). Nowcasting and forecasting the potential domestic and. Lancet, 395, 689-695. Obtenido de https://www.thelancet.com/pdfs/journals/lancet/PIIS0140-6736(20)30260-9.pdf")
                

      ) 
     )
    )
  
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
}

# Run the application 
shinyApp(ui = ui, server = server)   
   

