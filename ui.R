
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



# La siguiente seccion es una modificación del codigo de John Coene que 
# puede ser consultado en el siguiente link: https://github.com/JohnCoene/coronavirus

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
          tabName = "Inicio",
          use_shinyscroll(),
          icon = f7Icon("email", old = FALSE),
          active = TRUE,
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
          tabName = "Modelo",
          icon = f7Icon("email", old = FALSE),
          active = FALSE,
          swipeable = TRUE
          )
        )
      )
    )
  )
