#Porcentaje de mujeres en carreras de grado y pregrado. Todas las facultades. 2019.
#https://drive.google.com/file/d/1IiKj1ILMrR91J8-GCvVblA4UFjMZ3Eb9/view (datos transformados)

#Librería
library(tidyverse)

#Cargo datos
total <-  read.csv("https://raw.githubusercontent.com/melinaschamberger/proyecto_shiny/main/Datos/trayectoria_ecucativa/grado_pregrado.csv", sep=";", encoding = "UTF-8")

#Renombro variables
total <- rename(total, Categoría = X.U.FEFF.Categoría)

#Elimino variables que no voy a utilizar
total$X <- NULL

#Calculo porcentaje de participación femenina sobre el total
total <- total %>%
  group_by(Categoría, Gestión) %>%
  mutate(Porcentaje = round (100* Cantidad/ sum(Cantidad),2))

write.csv(total,"porc_grado_pregrado.csv", row.names = F)

