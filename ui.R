
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
library(remotes)
library(deSolve)
library(lubridate)


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

shinyUI(
  f7Page(
    title = "Tab Layout",
    dark_mode = FALSE,
    init = f7Init(
      skin = "md",
      theme = "light"
    ),
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
          p("Registro del estado del COVID-19 en Costa Rica, elaborado por el equipo de DABIA"),
          f7Link(label = "Autor", src = "https://grupodabia.com", external = TRUE),
          f7Link(label = "GitHub", src = "https://github.com/DabiaCenter", external = TRUE)
          )
        ),
      f7Tabs(
        animated = TRUE,
        id = "tabs",
        f7Tab(
          tabName = "General",
          icon = f7Icon("calendar", old = FALSE),
          active = TRUE,
          swipeable = TRUE
        ),
        f7Tab(
          tabName = "Inicio",
          use_shinyscroll(),
          icon = f7Icon("email", old = FALSE),
          active = FALSE,
          swipeable = TRUE,
          f7Card(
            title = "Casos Confirmados por provincia en el tiempo",
            echarts4rOutput("map", height = "70vh")
            ),
          f7Card(
            title = "Cantón",
            id = "Cantones",
            echarts4rOutput("canton", height = "70vh")
            )
          ),
        f7Tab(
          tabName = "Modelaje",
          icon = f7Icon("email", old = FALSE),
          active = FALSE,
          swipeable = TRUE,
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
            ))
          ),
          f7Card(
            title = "Modelo SIR del Covid-19 para Costa Rica",
            id = "modelo_SIR",
            echarts4rOutput("SIR", height = "70vh")
          ),
          f7Card(
            title = "Tabla de resumen",
            id = "tabla_resumen",
            tableOutput("indicadores")
          ),
          f7Card(
            title = "Modelo de crecimiento exponencial",
            id = "modelo_exponencial",
            echarts4rOutput("modelo_log_lin", height = "70vh")
          ),
          f7Card(
            title = "Estimación de casos de los próximos 8 días",
            id = "tabla_exp",
            f7Slide(
              tableOutput("estimacion_log_lin")
            )
          )
        )
      )
    )
  )
)
