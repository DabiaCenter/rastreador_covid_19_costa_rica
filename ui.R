
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
            p("Datos generales", class = "center", align = "center")
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
                echarts4rOutput("graf_estados", height = "60vh")
              ),
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
          f7Card(wellPanel(
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
          f7Card(
            title = "Modelo de crecimiento exponencial",
            echarts4rOutput("modelo_log_lin", height = "65vh")
          ),
          f7Card(
            title = "Predicción próximos 7 días",
            f7BlockHeader(h4(tableOutput("estimacion_log_lin"))) %>%
              f7Align("center")
            ) 
          ),
          f7Tab(
            tabName = "Período",
            icon = f7Icon("sort_up",old = FALSE),
            active = FALSE,
            swipeable=FALSE,
            f7Card(
              title = "Período de Duplicación COVID-19 en Costa Rica",
              echarts4rOutput("pedup")
            )
          )
        )
      )
    )
  )
