#Porcentaje de mujeres en facultades de la UBA. 2016.
#https://informacionestadistica.rec.uba.ar/graficos_facultades.html (datos transformados)

#Librería
library(tidyverse)

#Cargo datos
facultades_uba <-  read.csv("https://raw.githubusercontent.com/melinaschamberger/proyecto_shiny/main/Datos/trayectoria_ecucativa/facultades_uba.csv", sep=";", encoding = "UTF-8")

#Veo nombres de las columnas
colnames(facultades_uba)

#Transformo nombres
facultades_uba <- rename(facultades_uba, Facultad = X.U.FEFF.Facultad)

#Calculo porcentaje de mujeres por facultad
facultades_uba <- facultades_uba %>%
  group_by(Facultad) %>%
  mutate(Porcentaje = round (100* Cantidad/ sum(Cantidad),2))

write.csv(facultades_uba,"porc_facultades_uba.csv", row.names = F)
