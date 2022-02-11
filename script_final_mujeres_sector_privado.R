#Porcentaje de puestos ocupados por mujeres, por sector de actividad
# https://datos.gob.ar/dataset/produccion-porcentaje-puestos-ocupados-por-mujeres-por-sector-actividad

#Librerías
library(readr)
library(tidyverse)
library(lubridate)
library(ggthemes)
library(plotly)

## 1. Cargo datos
#Sector productivo: dos digitos
sp_dos <- read.csv()