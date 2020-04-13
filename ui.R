
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
library(remotes)
library(deSolve)
library(lubridate)
library(countup)
library(shinyWidgets)

# La siguiente seccion es una modificación del codigo de John Coene que 
# puede ser consultado en el siguiente link: https://github.com/JohnCoene/coronavirus

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

act <- max(cr_caso_general$Fecha)

loader <- tagList(
  waiter::spin_half(),
  br(),
  h3("Cargando datos...")
)

shinyUI(
  f7Page(
    use_waiter(include_js = FALSE),
    title = "COVID-19 CR",
    dark_mode = FALSE,
    init = f7Init(
      skin = "md",
      theme = "light",
      color = "blue"
    ),
    manifest = "www/manifest.json",
    icon = f7Icon("ant-fill", old = FALSE),
    favicon = f7Icon("ant-fill", old = FALSE),
    f7TabLayout(
      navbar = f7Navbar(
        title = "Rastreador COVID-19 de Costa Rica",
        hairline = FALSE,
        shadow = TRUE,
        left_panel = TRUE,
        right_panel = FALSE
      ),
      panels = tagList(
        f7Panel(
          title = "Acerca de",
          side = "left",
          theme = "light",
          effect = "cover",
          p("Registro del estado del COVID-19 en Costa Rica, elaborado por la comunidad de interés DABIA, conformada por estudiantes del Instituto Tecnológico de Costa Rica."),
          f7Link(label = "Autor", src = "https://grupodabia.com", external = TRUE),
          f7Link(label = "GitHub", src = "https://github.com/DabiaCenter", external = TRUE)
        )
      ),
      f7Tabs(
        animated = TRUE,
        id = "tabs",
        f7Tab(
          tabName = "General",
          icon = f7Icon("ant", old = FALSE),
          active = TRUE,
          swipeable = FALSE,
           waiter_show_on_load(html = loader),
          f7Card(
            f7BlockHeader(img(src = "https://www.grupodabia.com/nosotros/logo.jpg", height = 140, width = 140)) %>%
              f7Align("center"),
            h2("Estado Covid-19 Costa Rica", class = "center", align = "center"),
            p("Datos generales", class = "center", align = "center"),
            p(paste("Datos actualizados a la fecha:", act), class = "center", align = "center")
          ),
          f7Row(
            f7Col(
              f7Card(
                h2(
                  align = "center",
                  tags$span(countup::countupOutput("conf")),
                  br(),
                  span(tags$small("Confirmados"))
                )
              )
            ),
            f7Col(
              f7Card(
                h2(
                  align = "center",
                  tags$span(countup::countupOutput("fall")),
                  br(),
                  span(tags$small("Fallecidos"))
                )
              )
            )
          ),
          f7Row(
            f7Col(
              f7Card(
                h2(
                  align = "center",
                  tags$span(countup::countupOutput("desc")),
                  br(),
                  span(tags$small("Descartados"))
                )
              )
            ),
            f7Col(
              f7Card(
                h2(
                  align = "center",
                  tags$span(countup::countupOutput("recu")),
                  br(),
                  tags$span(tags$small("Recuperados"))
                )
               )
              )
            ),
          f7Card(
            title = "Casos confirmados y descartados por COVID-19",
            id = "confirm_descart",
            footer = "Deslice hacia la izquierda para ver el contenido",
            f7Swiper(
              id = "swiper1",
              centered = TRUE, 
              spaceBetween = 60,
              slidePerView = 1,
              f7Slide(
                echarts4rOutput("graf_infectados", height = "50vh")
              ),
              f7Slide(
                echarts4rOutput("graf_descartados", height = "50vh")
              ),
              f7Slide(
                echarts4rOutput("graf_calendario", height = "50vh")
              ),
              f7Slide(
                echarts4rOutput("graf_top10", height = "50vh")
              )
            )
          ),
          f7Card(
            title = "Agrupamiento de los infectados por COVID-19",
            id = "agrupaciones",
            footer = "Deslice hacia la izquierda para ver el contenido",
            f7Swiper(
              id = "swiper2",
              centered = TRUE, 
              spaceBetween = 60,
              slidePerView = 1,
              f7Slide(
                echarts4rOutput("graf_genero", height = "60vh")
              ),
              f7Slide(
                echarts4rOutput("graf_nacionalidad", height = "60vh")
              ),
              f7Slide(
                echarts4rOutput("graf_edades", height = "60vh")
              )
            )
          )
        ),
        f7Tab(
          tabName = "Mapa",
          icon = f7Icon("map", old = FALSE),
          active = FALSE,
          swipeable = FALSE,
          waiter_hide_on_render("map"),
          f7Card(
            title = "Casos Confirmados por provincia en el tiempo",
            echarts4rOutput("map", height = "80vh")
          ),
          f7SmartSelect(
            inputId = "smartsel",
            label = "Seleccione una provincia:",
            selected = "ALAJUELA",
            choices = c("ALAJUELA", "SAN JOSE", "CARTAGO",
                        "GUANACASTE", "LIMON", "PUNTARENAS",
                        "HEREDIA"),
            openIn = "popover",
            searchbar = FALSE
          ),
          f7Card(
            title = "Casos por cantones",
            id = "Cantones",
            echarts4rOutput("canton", height = "80vh")
          )
        ),
        f7Tab(
          tabName = "Modelos",
          icon = f7Icon("waveform_path", old = FALSE),
          active = FALSE,
          swipeable = FALSE,
          f7Card(
            f7Button(
              inputId = "sir", 
              label = "Información acerca del modelo SIR"
              ),
            f7Popup(
              id = "popup1",
              title = "Información acerca del modelo SIR",
              p("Existen diferentes tipos de modelos epidemiológicos para modelar el comportamiento de enfermedades 
                contagiosas, pero por factores de una fácil comprensión, además de que modela de una manera acertada
                el comportamiento del Covid-19, se utiliza el modelo SIR."),
              div(),
              p("El modelo SIR, es un modelo compartimental que parte de la idea de dividir a la población en tres 
                grupos de personas, los cuales son:"),
              div(),
              p(strong("Susceptibles(S):"), "el grupo de personas que se encuentra sana pero puede ser contagiada."),
              div(),
              p(strong("Infectados (I):"), "el grupo de personas que se encuentran infectados y contagian."),
              div(),
              p(strong("Recuperados (R):"), "el grupo de personas que se recupera y tiene inmunidad ante la enfermedad."),
              div(),
              p("Este cuenta con dos parámetros importantes como lo es la", strong("tasa de contagios"), ", la cual indica la transición
                entre las personas susceptibles (S) y la infectadas (I), y está la", strong("tasa de recuperación"), ",la cual controla
                la transición entre las personas infectadas (I) y la recuperadas(R). Las tasas que aparece en el modelo hecho,
                tanto la de contagios y recuperación, muestra las tasas al día de hoy de acuerdo a los datos obtenidos del 
                comportamiento del coronavirus en el país, sin embargo, usted puede cambiar esta tasa para ver como puede cambiar
                el comportamiento si la tasa de contagios baja y la tasa de recuperación sube o viceversa. ¿Qué es lo que se busca?
                El fin de los esfuerzos del gobierno y toda la población es que la curva se aplane y esto se logra buscando que la tasa
                de contagios disminuya. Para el comportamiento del coronavirus, y de cualquier enfermedad que sea muy contagiosa, la tasa 
                de contagios siempre va a ser mayor a la tasa de recuperación."),
              div(),
              p("Este modelo SIR es un modelo de ecuaciones diferenciales que es muy flexible, ya que permite observar cómo cambia el número
                de persona susceptibles a contagiarse, las infectadas y las recuperadas, donde muestra la importancia y la gran búsqueda de lograr
                aplanar la curva de los infectados. Además, permite tener predicciones como el previsible del", strong("pico de contagios"), ", la", 
                strong("tasa de contacto"), "(R0) y el posible", strong("porcentaje de la población a infectarse"), ", a todo esto, lo que se quiere 
                es disminuirlo al máximo."),
              div(),
              p("La tasa de contacto (R0) o también conocida como el número de reproducción es muy importante porque lo que indica es
                el número de personas sanas o suceptibles que llegan a ser infectados por una persona que está contagiada con la enfermedad, es decir,
                si el número de reproducción (R0) es de 2, esto quiere decir que una persona infectada está contagiando a 2 personas sanas. De acuerdo,
                con Manuel de León, indica que lo ideal es que este número esté por debajo de 1 y a nivel mundial, este número para el Covid-19 está entre
                1.5 y 2.5."),
              div(),
              p("NOTA: Todas las predicciones realizadas están basadas en modelos matemáticos y no necesariamente son los números exactos que van a haber
                al final de esta pandemia en nuestro país, hay factores que lo modelos no toman en cuenta como la cuarentena que está realizando el país o
                todos los esfuerzos que hace el gobierno por controlar la pandemia, de parte de DABIA esperamos que estos números terminen siendo los más
                favorables al país y esta pandemia termine lo más pronto posible."),
              div(),
              p("Para más información visitar", 
                a(href = "https://www.statsandr.com/blog/covid-19-in-belgium/", "Stats and R"), "(www.statsandr.com/blog/covid-19-in-belgium)", 
                "y", a(href = "https://www.agenciasinc.es/Reportajes/Un-modelo-un-teorema-y-teoria-de-juegos-contra-el-coronavirus", "Un modelo, un teorema
                               y teoría de juegos contra el coronavirus."), "(www.agenciasinc.es/Reportajes/Un-modelo-un-teorema-y-teoria-de-juegos-contra-el-coronavirus)")
            ),
            wellPanel(
            fluidRow(
              column(1, 
                     sliderInput(
                       inputId = "beta",
                       label = "Tasa de contagios",
                       value = beta_val, min = 0, max = 1
                     )
              )
            ),
            fluidRow(
              column(1, 
                     sliderInput(
                       inputId = "gamma",
                       label = "Tasa de recuperación",
                       value = gamma_val, min = 0, max = 1
                     )
              )
            ),
            fluidRow(
              column(1, 
                     sliderInput(
                       inputId = "poblacion",
                       label = "Cantidad de habitantes(Default habitantes de Costa Rica)",
                       value = 4900000, min = 0, max = 10000000, step = 100000)
              )
            )
          )
          ),
          f7Card(
            title = "Modelo SIR del Covid-19 para Costa Rica",
            id = "modelo_SIR",
            echarts4rOutput("SIR", height = "65vh")
            ),
          f7Card(
            title = "Tabla de resumen",
            id = "tabla_resumen",
            f7BlockHeader(h4(tableOutput("indicadores"))) %>%
               f7Align("center")
            ),
          f7Radio("variable", 
                 label = "Seleccione un modelo de regresion:", 
                 choices = c("Exponencial", "Gompertz"), 
                 selected = "Exponencial"),
          f7Card(
            f7Button(
              inputId = "info_model_reg", 
              label = "Información acerca del modelaje de regresión seleccionado"
            ),
            f7Popup(
              id="popup2",
              title="Observaciones generales del modelo de crecimiento exponencial",
                p("En el periodo inicial de una pandemia los casos suelen aumentar de manera exponencial. 
                  Para estimar los casos en un periodo definido se utiliza el factor de crecimiento (cuantas 
                  personas infecta una persona) y el número de casos al inicio. Con el fin de estimar el factor
                  de crecimiento y el valor inicial de los casos,que mejor representan el comportamiento de los 
                  datos, se obtuvo el logaritmo de los datos y se realizó un modelo de regresión lineal.  Una vez 
                  con estos valores estimados, se calculó el exponencial de cada uno, y se procedió a ajustar el 
                  modelo de crecimiento exponencial a los datos,y a realizar una estimación para los próximos siete
                  días. No obstante, en algún momento los casos dejan de aumentar, por lo que como se mencionó este 
                  modelo ajusta solo al inicio del periodo de infección."),
                div(),
                p("Importante resaltar que los resultados obtenidos en este estudio no representan la situación real
                  del país. Simplemente es una demostración de uno de modelos matemáticos que representan 
                  el comportamiento de un virus como el COVID-19, esto no quiere decir que sea el mejor. Al contrario, 
                  se incita al usuario a utilizar este modelo, e investigar sobre sobre otros modelos usados en epidemiología. "),
                a("Referencias","https://towardsdatascience.com/modeling-exponential-growth-49a2b6f22e1f")
            ),
            f7Popup(
              id="popup3",
              title="Observaciones generales del modelo Gompertz",
              p("El modelo de Gompertz es un modelo de regresión no lineal, perteneciente a la familia modelos logísticos 
                  generalizados denominados 'modelos de Richard'. Consiste en una función exponencial doble, de tres parámetros 
                  en su forma más simple y su diferencia con otros modelos logísticos es que, cuando la población modelada aumenta, 
                  la tasa de crecimiento decrece exponencialmente, lo que la hace asimétrica. Lo anterior hace de Gompertz un modelo
                  muy adecuado para describir fenómenos biológicos (como es el caso de una epidemia o el crecimiento de tumores, 
                  área en la que este modelo es muy aplicado). "),
              div(),
              p("Este modelo es una función de crecimiento, por lo que representa los casos confirmados acumulados de Covid-19
                  por día aproximadamente, no toma en cuenta los casos de recuperados o fallecidos ni otros aspectos epidemiológicos.
                  La presente aplicación calibra los parámetros del modelo con los nuevos casos de cada día, optimizando sus predicciones."),
              div(),
              p("Importante resaltar que las estimaciones obtenidas no son más que predicciones matemáticas obtenidas con base
                  en un modelo de regresión adoptado (en este caso, el modelo Gompertz), de manera que los resultados pueden 
                  desviarse de la realidad si el crecimiento del número de casos reales deja de presentar un comportamiento 
                  similar al modelo, esto puede pasar en cualquier momento ya que la evolución de la cantidad de contagios 
                  depende de muchos factores, algunos dependientes del azar (por ejemplo: los casos no detectados) y otros 
                  de naturaleza caótica. "),
              div(),
              p("Por demás, se insta al usuario a experimentar con este modelo y a investigar sobre otros modelos aplicables. "),
              div(),
              p("Referencias:"),
              p("Loria, S. (29 de marzo de 2020). Una explicación breve del modelo de Gompertz para modelar el crecimiento del Coronavirus (COVID19). Obtenido de: https://www.youtube.com/watch?v=5a4qjvVEmqo"),
              p("Espinoza, J. (7 de abril de 2020) Evolución de casos de Covid-19 en Costa Rica: Reporte al martes 7 de abril de 2020. Escuela de Matemática, Instituto Tecnológico de Costa Rica.")
            ),
            title = "Modelo de regresión seleccionado",
            echarts4rOutput("modelo_regresion", height = "65vh")
          ),
          f7Card(
            title = "Predicción próximos 7 días",
            f7BlockHeader(h4(tableOutput("estimacion_regresion"))) %>%
              f7Align("center")
            ) 
          ),
          f7Tab(
            tabName = "Período",
            icon = f7Icon("sort_up",old = FALSE),
            active = FALSE,
            swipeable=FALSE,
            f7Card(
              f7Button(
                inputId = "periodo", 
                label = "Información acerca del Período de Duplicación"
              ),
              f7Popup(
                id="popup4",
                title= "Información sobre el Periódo de Duplicación",
                p("El COVID-19 está marcando un antes y un después en la historia de la humanidad, existe información que puede brindar un acercamiento a la comprensión del comportamiento y dinámica de la pandemia."),
                div(),
                p("Los casos confirmados es una de estas informaciones en el país. Ahora bien, el número de casos positivos absolutos no es necesariamente el mejor indicador del estado actual de expansión de un brote de enfermedad, sobre todo en el caso de una infección que puede tener síntomas leves o incluso personas asintomáticas que pueden infectar a otros."),
                p("El",strong ("período de duplicación"), "es decir, el tiempo en el cual se duplica la cifra de personas testeadas positivas por una infección, es una medida utilizada a nivel internacional para ofrecer una imagen del desarrollo de una infección en un instante determinado."),
                div(),
                p("Si el período de duplicación fuese de dos días con cien infectados, quiere decir que pasaría de cien infectados a doscientos en dos días. Este indicador se obtiene: período de duplicación es igual a casos activos divido entre casos nuevos."),
                div(),
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
                a("Nunes-Vaz, R. (2020). Visualising the doubling time of COVID-19 allows comparison of the success of containment measures. Global Biosecurity. Obtenido de https://jglobalbiosecurity.com/articles/10.31646/gbio.61/"),
                a("Roser, M., Ritchie, H., & Ortiz, E. (2020). Coronavirus Disease (COVID-19) – Statistics and Research. Our World in Data. Obtenido de https://ourworldindata.org/coronavirus"),
                a("Woolhouse, M. (2011, July 12). How to make predictions about future infectious disease risks. Royal Society. Obtenido de https://royalsocietypublishing.org/doi/10.1098/rstb.2010.0387"),
                a("Wu, J. T., Leung, K., & Leung, G. M. (2020, February 29). Nowcasting and forecasting the potential domestic and. Lancet, 395, 689-695. Obtenido de https://www.thelancet.com/pdfs/journals/lancet/PIIS0140-6736(20)30260-9.pdf")
              ), 
              title = "Período de Duplicación COVID-19 en Costa Rica",
              echarts4rOutput("pedup")
            )
          )
        )
      )
    )
  )
