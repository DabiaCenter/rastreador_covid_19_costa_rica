# Rastreador COVID-19 Costa Rica

![LogoDabia](https://raw.githubusercontent.com/DabiaCenter/rastreador_covid_19_costa_rica/master/images/dabia.png)

Aplicación creada en Shiny para rastrear el comportamiento de la pandemia del coronavirus en Costa Rica donde se presentan datos, gráficos interactivos,
mapas dinámicos con líneas de tiempo, modelo SIR y modelo de crecimiento exponencial.

## Comenzando 

### Paquetes a instalar 

Los paquetes a instalar son los siguientes

```
library(tidyverse)
library(lubridate)
library(echarts4r)
library(echarts4r.maps)
library(pacman)
library(devtools)
library(shiny)
library(dplyr)
library(tidyr)
library(shinyMobile)
library(waiter)
library(jsonlite)
library(purrr)
library(deSolve)
library(remotes)
library(lubridate)
library(countup)
library(shinyWidgets)
```

*SE DEBEN DE TENER INSTALADOS Y ACTUALIZADOS ESTOS PAQUETES*
NOTA: Los paquetes de echarts4r.maps, shinyMobile y shinyscroll se deben de instalar con la funcion remotes::install_github() 

### Funcionamiento de la aplicación
1. Instalar todos los paquetes solicitados.
2. Correr el script de recuperador_de_datos.
3. Correr el script generador-de-mapas.
4. Correr la aplicación desde el script ui o server.



